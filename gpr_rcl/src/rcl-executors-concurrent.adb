with Ada.Exceptions; use Ada.Exceptions;

with RCL.Logging;
with RCL.Nodes;

package body RCL.Executors.Concurrent is

   --------------
   -- Dispatch --
   --------------

   procedure Dispatch (This   : in out Executor;
                       Node   : access Nodes.Node'Class;
                       Handle :        Dispatchers.Handle) is
   begin
      This.Queue.Enqueue (Callable'(Node   => Node.all'Unchecked_Access,
                                    Handle => Handle));
      Logging.Info ("ENQUEUE:" & This.Queue.Current_Use'Img);

      if not This.Started then
         This.Started := True;
         for I in This.Pool'Range loop
            This.Pool (I).Set_Parent (This.Self);
         end loop;
      end if;
   end Dispatch;

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
      Logging.Info ("Runner ready");
      accept Set_Parent (Parent : in Executor_Access) do
         Runner.Parent := Parent;
      end Set_Parent;
      Logging.Info ("Runner started");

      while not Done loop
         declare
            Element : Callable;
         begin
            select
               Parent.Queue.Dequeue (Element);
               Logging.Info ("QUEUED:" & Parent.Queue.Current_Use'Img);
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
   exception
      when E : others =>
         Logging.Error ("Executors.concurrent.runner [toplevel]:" & Exception_Information (E));
   end Runner;

end RCL.Executors.Concurrent;
