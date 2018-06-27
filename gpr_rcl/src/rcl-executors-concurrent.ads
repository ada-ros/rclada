with Ada.Containers.Bounded_Synchronized_Queues;
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Finalization;

with System.Multiprocessors; use System.Multiprocessors;

package RCL.Executors.Concurrent is

   use Ada.Containers;
   
   --  Thread pool
   
   type Executor (Queue_Size : Count_Type      := Count_Type (System.Multiprocessors.Number_Of_CPUs) * 32;
                  Threads    : Positive        := Positive   (System.Multiprocessors.Number_Of_CPUs);
                  Priority   : System.Priority := System.Max_Priority) is
     new Executors.Executor with private;

   overriding 
   procedure Call (This : in out Executor; 
                   CB   :        Impl.Callbacks.Callback'Class);
   
private    
   
   package CB_Holders is new Ada.Containers.Indefinite_Holders
     (Impl.Callbacks.Callback'Class, Impl.Callbacks."=");
   
   subtype Callable is CB_Holders.Holder;
   
   package Queue_Elements is new Synchronized_Queue_Interfaces (Callable);
   package Queues is new Bounded_Synchronized_Queues (Queue_Elements,
                                                      Default_Capacity => 0);
   
   type Executor_Access is access all Executor;
   
   task type Runner (Parent : access Executor) is
      entry Shutdown;
   end Runner;
   
   type Runner_Access is access Runner;
   
   type Runner_Pool is array (Positive range <>) of Runner_Access;
   
   type Controller (Parent : access Executor) is
     new Ada.Finalization.Limited_Controlled with null record;
   
   overriding procedure Initialize (This : in out Controller);
   overriding procedure Finalize   (This : in out Controller);
   
   type Executor (Queue_Size : Count_Type := Count_Type (System.Multiprocessors.Number_Of_CPUs) * 32;
                  Threads    : Positive   := Positive (System.Multiprocessors.Number_Of_CPUs);
                  Priority   : System.Priority := System.Max_Priority) is
     new Executors.Executor with 
      record
         Self  : access Executor := Executor'Unchecked_Access;
         Pool  : Runner_Pool  (1 .. Threads);
         Queue : Queues.Queue (Capacity => Queue_Size, 
                               Ceiling  => Priority);
         Control : Controller (Executor'Access);
         Started : Boolean := False;
      end record;
   
end RCL.Executors.Concurrent;
