with Rcl_Allocator_H; use Rcl_Allocator_H;

with System.Storage_Pools; use System.Storage_Pools;

package RCL.Allocators is

   type Allocator (<>) is tagged private;

   type Pool_Access is access all Root_Storage_Pool'Class with
     Storage_Size => 0;
   pragma No_Strict_Aliasing (Pool_Access);

   function To_Allocator (Pool  : Pool_Access) return Allocator;
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

   function To_C (This : Allocator) return Rcl_Allocator_T;
   --  The resulting allocator can be copied since it will be valid as long
   --  as the Pool exists.

   ----------------------------------------------------
   --  Global allocator used through RCL by default  --

   function Global_Allocator return Allocator;
   --  This is unless overriden the default ROS2 allocator (common heap)

   procedure Set_Global_Allocator (Alloc : Allocator);

   --------------------------------
   --  C stuff to be isolated later

   subtype C_Allocator is Rcl_Allocator_T;

   Default_C_Allocator : constant access Rcl_Allocator_T;

private

   type Allocator (Pool : Pool_Access) is tagged null record;

   ------------------
   -- To_Allocator --
   ------------------

   function To_Allocator (Pool  : Pool_Access) return Allocator is
     (Pool => Pool);


   --  C stuff  --

   function Get_Default_C_Allocator return Rcl_Allocator_T with
     Import,
     Convention => C,
     External_Name => "rcutils_get_default_allocator";

   Default_C_Allocator_Instance : aliased C_Allocator := Get_Default_C_Allocator;

   Default_C_Allocator : constant access Rcl_Allocator_T :=
                                    Default_C_Allocator_Instance'Access;

end RCL.Allocators;
