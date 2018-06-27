package RCL.Executors.Sequential is

   --  Execute in current task
   
   type Executor is new Executors.Executor with null record;
   
   overriding 
   procedure Dispatch (This   : in out Executor;
                       Node   : access Nodes.Node'Class;
                       Handle :        Dispatchers.Handle);
   
end RCL.Executors.Sequential;
