with Ada.Containers.Bounded_Synchronized_Queues;
with Ada.Containers.Synchronized_Queue_Interfaces;

with System.Multiprocessors; use System.Multiprocessors;

package RCL.Executors.Concurrent is

   --  Thread pool

   type Executor (Max_Nodes  : Count_Type      := Default_Nodes_Per_Executor;
                  Queue_Size : Count_Type      := Count_Type (System.Multiprocessors.Number_Of_CPUs) * 32;
                  Threads    : Positive        := Positive   (System.Multiprocessors.Number_Of_CPUs);
                  Priority   : System.Priority := System.Max_Priority) is
     new Executors.Executor with private;

   overriding
   procedure Call (This : in out Executor;
                   CB   :        Impl.Callbacks.Callback'Class);

   overriding
   procedure Shutdown (This : in out Executor);

private

   subtype Callable is Impl.Callbacks.Definite_Callback;

   package Queue_Elements is new Synchronized_Queue_Interfaces (Callable);
   package Queues is new Bounded_Synchronized_Queues (Queue_Elements,
                                                      Default_Capacity => 0);

   type Executor_Access is access all Executor with Storage_Size => 0;

   task type Runner is
      entry Init (Parent : Executor_Access);
      entry Shutdown;
   end Runner;

   type Runner_Pool is array (Positive range <>) of Runner;

   type Executor (Max_Nodes  : Count_Type      := Default_Nodes_Per_Executor;
                  Queue_Size : Count_Type := Count_Type (System.Multiprocessors.Number_Of_CPUs) * 32;
                  Threads    : Positive   := Positive (System.Multiprocessors.Number_Of_CPUs);
                  Priority   : System.Priority := System.Max_Priority) is
     new Executors.Executor (Max_Nodes) with
      record
         Self  : access Executor := Executor'Unchecked_Access;
         Pool  : Runner_Pool  (1 .. Threads);
         Queue : Queues.Queue (Capacity => Queue_Size,
                               Ceiling  => Priority);
         Started : Boolean := False;
      end record;

end RCL.Executors.Concurrent;
