with Rcl_Context_H;

package RCL.Contexts is

   --  A context encapsulates what was formerly global state outside nodes.

   type Context is limited private;

private

   use Rcl_Context_H;

   type Context is limited record
      Impl : aliased Rcl_Context_T := Rcl_Get_Zero_Initialized_Context;
   end record;

end RCL.Contexts;
