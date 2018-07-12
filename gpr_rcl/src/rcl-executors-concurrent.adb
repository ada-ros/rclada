with Ada.Exceptions; use Ada.Exceptions;
--  with Ada.Unchecked_Deallocation;

with RCL.Logging;

package body RCL.Executors.Concurrent is

   procedure Awake (This : in out Executor) is
   begin
      for I in This.Pool'Range loop
         This.Pool (I).Init (This.Self);
      end loop;
   end Awake;

   ----------
   -- Call --
   ----------

   procedure Call (This : in out Executor;
                   CB   :        Impl.Callbacks.Callback'Class) is
   begin
      if not This.Started then
         This.Awake;
         This.Started := True;
      end if;

      select
         This.Queue.Enqueue (CB_Holders.To_Holder (CB));
      else
         Logging.Warn ("Queue full! Events are being DISCARDED!");
      end select;
   end Call;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown (This : in out Executor) is
   begin
      Logging.Debug ("Executor shutting down...");

      if This.Queue.Current_Use > 0 then
         Logging.Warn ("Executor pool starting shutdown with" &
                         This.Queue.Current_Use'Img &
                         " queued pending calls!");
      end if;

      for I in This.Pool'Range loop
         Logging.Debug ("Stopping down runner" & I'Img & "...");
         This.Pool (I).Shutdown;
      end loop;

      Logging.Debug ("All stopped");

      if This.Queue.Current_Use > 0 then
         Logging.Warn ("Executor pool shut down with" &
                         This.Queue.Current_Use'Img &
                         " queued pending calls!");
      end if;
   end Shutdown;

   ------------
   -- Runner --
   ------------

   task body Runner is
      Parent : Executor_Access;
      Done   : Boolean := False;
   begin

      accept Init (Parent : Executor_Access) do
         Runner.Parent := Parent;
      end Init;

      Logging.Debug ("Runner started");

      while not Done loop
         declare
            CB : Callable;
         begin
            select
               Parent.Queue.Dequeue (CB);
               CB.Element.Call;
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
