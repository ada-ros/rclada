with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with RCL.Logging;

package body RCL.Executors.Concurrent is

   ----------
   -- Call --
   ----------

   procedure Call (This : in out Executor;
                   CB   :        Impl.Callbacks.Callback'Class) is
   begin
      select
         This.Queue.Enqueue (CB_Holders.To_Holder (CB));
      else
         Logging.Warn ("Queue full! Events are being DISCARDED!");
      end select;
   end Call;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize   (This : in out Controller) is
      procedure Free is new Ada.Unchecked_Deallocation (Runner, Runner_Access);
   begin
      if This.Parent.Queue.Current_Use > 0 then
         Logging.Warn ("Executor pool starting shutdown with" &
                         This.Parent.Queue.Current_Use'Img &
                         " queued pending calls!");
      end if;

      for I in This.Parent.Pool'Range loop
         Logging.Debug ("Stopping down runner" & I'Img & "...");
         This.Parent.Pool (I).Shutdown;
      end loop;

      for I in This.Parent.Pool'Range loop
         while not This.Parent.Pool (I)'terminated loop
            delay 0.1;
         end loop;
         Free (This.Parent.Pool (I));
      end loop;

      Logging.Debug ("All stopped");

      if This.Parent.Queue.Current_Use > 0 then
         Logging.Warn ("Executor pool shut down with" &
                         This.Parent.Queue.Current_Use'Img &
                         " queued pending calls!");
      end if;
   end Finalize;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize (This : in out Controller) is
   begin
      for I in This.Parent.Pool'Range loop
         This.Parent.Pool (I) := new Runner (This.Parent.all'Unchecked_Access);
      end loop;
   end Initialize;

   ------------
   -- Runner --
   ------------

   task body Runner is
      Done   : Boolean := False;
   begin
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
