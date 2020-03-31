with RCL.Contexts;

package RCL.Init is

   --  This package internally keeps track of the number of contexts that have
   --  been initialized. This is the number returned by User_Count.

   procedure Initialize (Context   : aliased in out Contexts.Context);
   --  As a user, you need not to call this directly since the node Init will
   --  do it for you.

   --  TODO: remove Context from Node. Currently we have a context per node, in
   --  the old tradition of one node per process. At some point, RCLAda must
   --  embrace the multi-node process shenanigans.

   procedure Shutdown (Context : aliased in out Contexts.Context);
   --  Each type that calls Initialize should make a corresponding Shutdown
   --  call.

   function User_Count return Natural;
   --  Should be 0 after everything has shut down

end RCL.Init;
