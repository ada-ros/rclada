with RCL.Contexts;

package RCL.Init is

   procedure Initialize (Context   : aliased in out Contexts.Context);
   --  First initialize call sets the global allocator
   --  The allocator is ignored for subsequent calls!
   --  As a user, you need not to call this directly since the node Init will
   --  do it for you.

   --  TODO: remove Context for Node. Currently we have a context per node, in
   --  the old tradition of one node per process. At some point, RCLAda must
   --  embrace the multi-node process shenanigans.

   procedure Shutdown (Context : aliased in out Contexts.Context);
   --  Each type that calls Initialize should make a corresponding Shutdown
   --  call.

   function User_Count return Natural;
   --  Should be 0 after everything has shut down

end RCL.Init;
