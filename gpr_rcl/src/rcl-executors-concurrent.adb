with Ada.Exceptions; use Ada.Exceptions;

with Rcl.Logging;

package body RCL.Executors.Concurrent is

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch (This   : in out Executor;
                       Node   : access Nodes.Node'Class;
                       Handle :        Callbacks.Handle) is
   begin
      This.Queue.Enqueue (Callable'(Node   => Node,
                                    Handle => Handle));
   end Dispatch;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Controller) is
   begin
      for I in This.Parent.Pool'Range loop
         This.Parent.Pool (I).Set_Parent (This.Parent);
      end loop;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize   (This : in out Controller) is
   begin
      for I in This.Parent.Pool'Range loop
         Logging.Info ("Stopping down runner" & I'Img & "...");
         This.Parent.Pool (I).Shutdown;
      end loop;
      Logging.Info ("All stopped");
   end Finalize;

   ------------
   -- Runner --
   ------------

   task body Runner is
      Done   : Boolean := False;
      Parent : Executor_Access;
   begin
      accept Set_Parent (Parent : in Executor_Access) do
         Runner.Parent := Parent;
      end Set_Parent;

      while not Done loop
         declare
            Element : Callable;
         begin
            select
               Parent.Queue.Dequeue (Element);
               Common_Dispatch (Element.Node,
                                Element.Handle);
            or
               delay 1.0;
            end select;
         exception
            when E : others =>
               Logging.Error ("Executors.Concurrent.Runner: " & Exception_Information (E));
         end;

         --  Every now and then, allow shutdown
         select
            accept Shutdown  do
               Done := True;
            end Shutdown;
         else
            null;
         end select;
      end loop;
   end Runner;

end RCL.Executors.Concurrent;
