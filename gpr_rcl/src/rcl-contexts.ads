with Ada.Finalization;

with Rcl_Context_H;

package RCL.Contexts is

   --  A context encapsulates what was formerly global state outside nodes. Now
   --  there is not any global state; a context provides all state to a group
   --  of nodes.

   --  TODO: remove Context from Node. Currently we have a context per node, in
   --  the old tradition of one node per process. At some point, RCLAda must
   --  embrace the multi-node process shenanigans.

   type Context is new Ada.Finalization.Limited_Controlled with private;

   procedure Shutdown (Context : aliased in out Contexts.Context);
   --  Contexts are finalized automatically. If, for some reason, you need
   --  a premature termination of a context (e.g. to make the User Count go
   --  down), you can use this procedure.

   --  This package internally keeps track of the number of contexts that have
   --  been initialized. This is the number returned by User_Count.

   function User_Count return Natural;
   --  Should be 0 after everything has shut down

   ---------------
   -- Low level --
   ---------------

   function To_C (This : aliased in out Context)
                  return access Rcl_Context_H.Rcl_Context_T;


   overriding procedure Initialize (Context : in out Contexts.Context);
   --  As a user, you need not to call this directly since the node Init will
   --  do it for you.

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
