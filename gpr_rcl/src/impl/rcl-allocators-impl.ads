package RCL.Allocators.Impl is

   type Allocator_Reference (Impl : access Rcl_Allocator_T) is limited null record
     with Implicit_Dereference => Impl;

   function To_C (This : aliased in out Allocator) return Allocator_Reference;
   --  The resulting allocator will be valid as long as the Ada allocator lives.
   --  This is kind of ugly but there's an inconsistent use of allocators
   --    through C RCL right now

private

   function To_C (This : aliased in out Allocator) return Allocator_Reference is
     (Impl => This.Impl'Access);

end RCL.Allocators.Impl;
