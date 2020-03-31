with Ada.Finalization; use Ada.Finalization;

with Rcl_Allocator_H; use Rcl_Allocator_H;

with System.Storage_Pools; use System.Storage_Pools;

package RCL.Allocators is

   type Pool_Access is access all Root_Storage_Pool'Class with Storage_Size => 0;
   pragma No_Strict_Aliasing (Pool_Access);

   type Allocator (Pool : Pool_Access) is new Limited_Controlled with private;
   --  A null pool can be used to defer initialization.

   type Handle is not null access all Allocator with Storage_Size => 0;

   --  An allocator in practice encapsulates an Ada Storage_Pool
   --  The pool will govern when the data in the Allocator is freed.
   --  A local pool shouldn't ever be set as the global allocator.
   --  (Accessibility levels should prevent you anyway).

   --  There is NO ALIGNMENT information coming from the C side.
   --  For now all requests will have alignment 1.
   --  This decision might be revisited
   --    (e.g. to select greatest alignment within 64 bits)

   --  IMPORTANT IMPLEMENTATION NOTE  --
   --  Since the C side does not provide the size of the bytes to deallocate,
   --  this has to be stored in the allocated blocks. This introduces a small
   --  memory overhead (one size_t).
   --  END OF IMPLEMENTATION NOTE --

   procedure Set_Pool (This : in out Allocator;
                       Pool :        Pool_Access);
   --  Deferred initialization, overrides the constraint pool

   ----------------------------------------------------
   --  Global allocator used through RCL by default  --

   function Global_Allocator return Handle;
   --  This is, unless overriden, the default ROS2 allocator (regular heap)

   procedure Set_Global_Allocator (Alloc : Handle);

   ---------------
   -- Low level --
   ---------------

   function To_C (This : aliased in out Allocator)
                  return access Rcl_Allocator_T;

private

   function Get_Default_C_Allocator return Rcl_Allocator_T with
     Import,
     Convention => C,
     External_Name => "rcutils_get_default_allocator";

   type Allocator (Pool : Pool_Access) is new Limited_Controlled with record
      Deferred_Pool :         Pool_Access;
      Impl          : aliased Rcl_Allocator_T := Get_Default_C_Allocator;
   end record;

   overriding procedure Initialize (This : in out Allocator);

   function Get_Pool (This : Allocator) return Pool_Access is
     (if This.Deferred_Pool /= null
      then This.Deferred_Pool
      else This.Pool);

   function To_C (This : aliased in out Allocator)
               return access Rcl_Allocator_T
   is (This.Impl'Access);

end RCL.Allocators;
