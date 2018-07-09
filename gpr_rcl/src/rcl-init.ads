with RCL.Allocators;

package RCL.Init is
   
   type Assurances is (Ensure_First, Dont_Care);
   
   procedure Initialize (Allocator : Allocators.Handle;
                         Assurance : Assurances);
   --  First initialize call sets the global allocator
   --  The allocator is ignored for subsequent calls!
   --  As a user, you do not need to call this directly since the node Init will
   --  do it for you.
   --  You can call it however to force a initial global allocator.
   --  If Ensure_First, it will raise if it is not! 
   --    (To be sure allocator is properly applied)
   
   procedure Finalize;
   --  Each type that calls Init should make a corresponding Finalize call
   
   function User_Count return Natural;
   --  Should be 0 after everything has shut down

end RCL.Init;
