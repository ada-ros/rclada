with Ada.Containers;
with Ada.Exceptions;
with Ada.Calendar;

with RCL.Subscriptions;
with RCL.Wait;

package body RCL.Nodes is

   use all type Ada.Containers.Count_Type;

   ----------
   -- Init --
   ----------

   function Init (Name      : String;
                  Namespace : String  := "/";
                  Opt       : Options := Default_Options) return Node
   is
      pragma Unreferenced (Opt);

      Opts  : aliased constant Rcl_Node_Options_T :=
                Rcl_Node_Get_Default_Options;
   begin
      return This : Node do
         Check (Rcl_Node_Init
                  (This.Impl'Access,
                   To_C (Name).To_Ptr,
                   To_C (Namespace).To_Ptr,
                   Opts'Access));
      end return;
   end Init;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Node) is
   begin
      if To_Boolean (Rcl_Node_Is_Valid (This.Impl'Access, null)) then
         Check (Rcl_Node_Fini (This.Impl'Access));
      else
         null;
         --  Log attempt at finalizing finalized node
      end if;
   exception
      when E : others =>
         Put_Line ("Exception while finalizing node:");
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end Finalize;

   ----------
   -- Spin --
   ----------

   procedure Spin (This   : in out Node;
                   During :        Duration := 0.1)
   is
      use Ada.Calendar;
      Start : constant Time := Clock;

      -------------
      -- Process --
      -------------

      procedure Process (T : Wait.Trigger) is
         use all type Wait.Kinds;
      begin
         case T.Kind is
            when Subscription =>
               This.Subscriptions (T.Index).Dispatch;
         end case;
      end Process;

      ---------------
      -- Spin_Once --
      ---------------

      procedure Spin_Once is
         use all type Wait.Wait_Outcomes;

         Set : Wait.Set := Wait.Init
           (Num_Subscriptions => Natural (This.Subscriptions.Length));
      begin
         if This.Subscriptions.Length = 0 then
            raise Constraint_Error with "Nothing to spin on!";
         end if;

         for Sub of This.Subscriptions loop
            Set.Add (Sub.Subscription);
         end loop;

         case Set.Wait (During - (Clock - Start)) is
            when Error     => raise Program_Error with "Error in Set.Wait";
            when Timeout   => null;
            when Triggered =>
               for Trigger of Set loop
                  Process (Trigger);
               end loop;
         end case;
      end Spin_Once;

   begin
      loop
         Spin_Once;

         exit when Clock - Start >= During;
      end loop;
   end Spin;

   ---------------
   -- Subscribe --
   ---------------

   procedure Subscribe (This     : in out Node;
                        Msg_Type :        ROSIDL.Typesupport.Message_Support;
                        Topic    :        String;
                        Callback :        Callbacks.For_Subscription)
   is
      Sub : constant Subscriptions.Subscription :=
              Subscriptions.Init (This, Msg_Type, Topic);
   begin
      This.Subscriptions.Append
        (Subscription_Dispatcher'(Sub.To_C, Callback, Msg_Type));
   end Subscribe;

end RCL.Nodes;
