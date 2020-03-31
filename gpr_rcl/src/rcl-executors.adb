with Ada.Calendar;
with Ada.Exceptions;

with RCL.Contexts;
with RCL.Impl.Dispatchers.Maps;
with RCL.Impl.Wait;
use RCL.Impl;

with RCL.Nodes;
with RCL.Nodes.Impl;
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
      CBs   : aliased Impl.Dispatchers.Maps.Map (Default_Pending_Events_Per_Node);
      --  TODO: get this size from the proper nodes?
      Nodes : Set (This.Max_Nodes);

      use Impl.Dispatchers;
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
         Set : Impl.Wait.Set :=
                 Impl.Wait.Init (Allocator => This.Allocator,
                                 Context   => Contexts.Global_Context.all,
                                 Callbacks => CBs);
      begin
         Logging.Debug ("Waiting on" & CBs.Length'Img & " callbacks");
         case Set.Wait (Timeout) is
            when Impl.Wait.Error     =>
               raise Program_Error with "Error in Set.Wait";

            when Impl.Wait.Timeout   =>
               return False;

            when Impl.Wait.Triggered =>
               for Triggered of Set loop
                  Element (CBs.Element (Triggered.Handle)).Dispatch;
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
         for I in 1 .. Natural (Nodes.Length) loop
            if Nodes (I) = Node then
               Nodes.Delete (I);
               return;
            end if;
         end loop;

         raise Constraint_Error with "Node not in executor";
      end Delete;

      ------------
      -- Insert --
      ------------

      procedure Insert (Node  : Node_Access) is
      begin
         Nodes.Append (Node);
      end Insert;

      ---------
      -- Get --
      ---------

      procedure Get    (Nodes : out Set) is
      begin
         Nodes := Node_Set.Nodes;
      end Get;

      --------------
      -- Is_Empty --
      --------------

      function Is_Empty return Boolean is (Nodes.Is_Empty);

   end Node_Set;

end RCL.Executors;
