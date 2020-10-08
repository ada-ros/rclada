with Ada.Finalization;

with Rcl_Context_H;

package RCL.Contexts is

   --  A context encapsulates what was formerly global state outside nodes.

   type Context is new Ada.Finalization.Limited_Controlled with private;

   --  This package internally keeps track of the number of contexts that have
   --  been initialized. This is the number returned by User_Count.

  overriding procedure Initialize (Context : in out Contexts.Context);
   --  As a user, you need not to call this directly since the node Init will
   --  do it for you.

   --  TODO: remove Context from Node. Currently we have a context per node, in
   --  the old tradition of one node per process. At some point, RCLAda must
   --  embrace the multi-node process shenanigans.

   --  procedure Shutdown (Context : aliased in out Contexts.Context);
   --  --  Each type that calls Initialize should make a corresponding Shutdown
   --  --  call.

   function User_Count return Natural;
   --  Should be 0 after everything has shut down

   ---------------
   -- Low level --
   ---------------

   function To_C (This : aliased in out Context)
                  return access Rcl_Context_H.Rcl_Context_T;

   --  overriding procedure Initialize (This : in out Context);
   overriding procedure Finalize   (This : in out Context);

   --  TODO: remove the global context and allow users to manage contexts
   --  manually.
   function Global_Context return not null access Context;

private

   use Rcl_Context_H;

   type States is (Initialized, Finalized);

   type Context is new Ada.Finalization.Limited_Controlled with record
      Impl  : aliased Rcl_Context_T := Rcl_Get_Zero_Initialized_Context;
      State : States := Finalized;
   end record;

   function To_C (This : aliased in out Context)
                  return access Rcl_Context_H.Rcl_Context_T is
     (This.Impl'Access);

end RCL.Contexts;
