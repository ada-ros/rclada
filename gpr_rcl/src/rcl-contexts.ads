with Rcl_Context_H;

package RCL.Contexts is

   --  A context encapsulates what was formerly global state outside nodes.

   type Context is tagged limited private;

   ---------------
   -- Low level --
   ---------------

   function To_C (This : aliased in out Context)
                  return access Rcl_Context_H.Rcl_Context_T;

private

   use Rcl_Context_H;

   type Context is tagged limited record
      Impl : aliased Rcl_Context_T := Rcl_Get_Zero_Initialized_Context;
   end record;

   function To_C (This : aliased in out Context)
                  return access Rcl_Context_H.Rcl_Context_T is
     (This.Impl'Access);

end RCL.Contexts;
