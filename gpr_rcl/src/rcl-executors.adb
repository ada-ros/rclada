with Ada.Calendar;
with Ada.Exceptions;

with RCL.Nodes;
with RCL.Logging;

package body RCL.Executors is

   ---------
   -- Add --
   ---------

   procedure Add (This :         in out Executor;
                  Node : aliased in out Nodes.Node'Class)
   is
   begin
      This.Nodes.Insert (Node'Unchecked_Access);
   end Add;

   ---------------------
   -- Common_Dispatch --
   ---------------------

   procedure Common_Dispatch (Node   : access Nodes.Node'Class;
                              Handle :        Callbacks.Handle) is
   begin
      Node.Trigger (Handle);
   end Common_Dispatch;

   ------------
   -- Remove --
   ------------

   procedure Remove (This :         in out Executor;
                     Node : aliased in out Nodes.Node'Class) is
   begin
      This.Nodes.Delete (Node'Unchecked_Access);
   end Remove;

   ----------
   -- Spin --
   ----------

   procedure Spin (This   : in out Executor;
                   Once   :        Boolean       := False;
                   During :        ROS2_Duration := 0.1)
   is
      use Ada.Calendar;
      Start : constant Time := Clock;
   begin
      loop
         if This.Spin_Once (During - (Clock - Start)) and then Once then
            exit;
         end if;

         exit when Clock - Start >= During;
      end loop;
   exception
      when E : others =>
         Logging.Error ("Executor.Spin caught: " &
                          Ada.Exceptions.Exception_Information (E));
         raise;
   end Spin;

   ---------------
   -- Spin_Once --
   ---------------

   function Spin_Once (This    : in out Executor;
                       Timeout :        ROS2_Duration;
                       Node    : access Nodes.Node'Class := null)
                       return Boolean
   is
      --  True if something was processed

      use all type Wait.Wait_Outcomes;

      CBs : Callbacks.Set;
   begin
      for N of This.Nodes loop
         if Node = null or else N = Node then
            N.Get_Callbacks (CBs);
         end if;
      end loop;

      if CBs.Is_Empty then
         Logging.Warn ("Nothing to spin on: sleeping for" & Timeout'Img & " seconds!");
         delay Timeout;
         return False;
      end if;

      declare
         Set : Wait.Set := Wait.Init (CBs);
      begin
         Logging.Debug ("Waiting on" & CBs.Length'Img & " callbacks");
         case Set.Wait (Timeout) is
            when Error     =>
               raise Program_Error with "Error in Set.Wait";

            when Wait.Timeout   =>
               return False;

            when Triggered =>
               for Triggered of Set loop
                  This.Trigger (CBs, Triggered);
               end loop;
               return True;

         end case;
      end;
   end Spin_Once;

   -------------
   -- Trigger --
   -------------

   procedure Trigger (This : in out Executor'Class;
                      Set  : in out Callbacks.Set;
                      T    :        Wait.Trigger) is
   begin
      This.Dispatch (Set.Get (T.Ptr).Node,
                     T.Ptr);
   end Trigger;

end RCL.Executors;
