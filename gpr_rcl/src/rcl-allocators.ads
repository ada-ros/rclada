with Rcl_Allocator_H; use Rcl_Allocator_H;

package RCL.Allocators is

   subtype Allocator is Rcl_Allocator_T;

   function Get_Default_Allocator return Allocator with
     Import,
     Convention => C,
     External_Name => "rcutils_get_default_allocator";

   Default_Allocator : aliased Allocator := Get_Default_Allocator;

end RCL.Allocators;
