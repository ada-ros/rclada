package RCL.Executors.Sequential is

   --  Execute in current task
   
   type Executor is new Executors.Executor with null record;
   
   overriding procedure Call (This : in out Executor; 
                              CB   :        Impl.Callbacks.Callback'Class);
   
end RCL.Executors.Sequential;
