package RCL.Executors.Sequential is

   --  Execute in current task
   
   type Executor is new Executors.Executor with null record;
   
   overriding 
   procedure Dispatch (This : in out Executor;
                       Call : in out Callbacks.Dispatcher'Class);
   
end RCL.Executors.Sequential;
