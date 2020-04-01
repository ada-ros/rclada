with Ada.Finalization;

with Rcl_Context_H;

package RCL.Contexts is

   --  A context encapsulates what was formerly global state outside nodes.

   type Context is new Ada.Finalization.Limited_Controlled with private;

   ---------------
   -- Low level --
   ---------------

   function To_C (This : aliased in out Context)
                  return access Rcl_Context_H.Rcl_Context_T;

   overriding procedure Initialize (This : in out Context);
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
