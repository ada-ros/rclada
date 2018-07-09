with Ada.Calendar;
with Ada.Exceptions;

with RCL.Nodes;
with RCL.Nodes.Impl;
with RCL.Logging;
with RCL.Wait;

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
                              Handle :        Impl.Dispatchers.Handle) is
   begin
      Nodes.Impl.Trigger (Node.all, Handle);
   end Common_Dispatch;

   ------------
   -- Remove --
   ------------

   procedure Remove (This :         in out Executor;
                     Node : aliased in out Nodes.Node'Class) is
   begin
      This.Nodes.Delete (Node'Unchecked_Access);
   end Remove;

   -------------------
   -- Set_Allocator --
   -------------------

   procedure Set_Allocator (This      : in out Executor;
                            Allocator :        Allocators.Handle) is
   begin
      This.Allocator := Allocator;
   end Set_Allocator;

   ----------
   -- Spin --
   ----------

   procedure Spin (This      : in out Executor;
                   Once      :        Boolean       := False;
                   During    :        ROS2_Duration := 0.1)
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

      CBs   : Impl.Dispatchers.Set;
      Nodes : Node_Sets.Set;
   begin
      This.Nodes.Get (Nodes);

      for N of Nodes loop
         if Node = null or else N = Node then
            RCL.Nodes.Impl.Get_Callbacks (N.all, CBs);
         end if;
      end loop;

      if CBs.Is_Empty then
         Logging.Warn ("Nothing to spin on [executor]: sleeping for" & Timeout'Img & " seconds!");
         delay Timeout;
         return False;
      end if;

      declare
         Set : Wait.Set := Wait.Init (This.Allocator, CBs);
      begin
         Logging.Debug ("Waiting on" & CBs.Length'Img & " callbacks");
         case Set.Wait (Timeout) is
            when Error     =>
               raise Program_Error with "Error in Set.Wait";

            when Wait.Timeout   =>
               return False;

            when Triggered =>
               for Triggered of Set loop
                  CBs.Get (Triggered.Handle).Dispatch;
               end loop;
               return True;

         end case;
      end;
   end Spin_Once;

   --------------
   -- Node_Set --
   --------------

   protected body Node_Set is

      ------------
      -- Delete --
      ------------

      procedure Delete (Node  : Node_Access) is
      begin
         Nodes.Delete (Node);
      end Delete;

      ------------
      -- Insert --
      ------------

      procedure Insert (Node  : Node_Access) is
      begin
         Nodes.Insert (Node);
      end Insert;

      ---------
      -- Get --
      ---------

      procedure Get    (Nodes : out Node_Sets.Set) is
      begin
         Nodes := Node_Set.Nodes;
      end Get;

   end Node_Set;

end RCL.Executors;
