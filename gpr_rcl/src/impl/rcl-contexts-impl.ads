with Rcl_Context_H;

package RCL.Contexts.Impl is

   function To_C (This : aliased in out Context)
                  return access Rcl_Context_H.Rcl_Context_T;

private

   function To_C (This : aliased in out Context)
                  return access Rcl_Context_H.Rcl_Context_T is
     (This.Impl'Access);

end RCL.Contexts.Impl;
