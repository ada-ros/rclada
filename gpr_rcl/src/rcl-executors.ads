with Ada.Containers.Bounded_Vectors; use Ada.Containers;
with Ada.Unchecked_Conversion;

with RCL.Allocators;
with RCL.Impl.Callbacks;
with RCL.Impl.Dispatchers;
limited with RCL.Nodes;

with System;

package RCL.Executors is

   --  Executors encapsulate multithreading. They're not provided by the
   --  C RCL, but they exist in both rclcpp and rclpy.

   --  Essentially, they allow to have several nodes running simultaneously in
   --  the same process.

   --  They aren't necessarily multithreading. In rclada, two impls are provided,
   --  a single-task one and a multi-task one.

   --  Executors are expected to outlive nodes (they should be library-level).
   --  Nodes self-manage their registration-unregistration

   type Executor (Max_Nodes : Count_Type := Default_Nodes_Per_Executor) is
     abstract tagged limited private;
   --  To avoid use of dynamic memory, node space is preallocated.

   type Handle is access all Executor'Class with Storage_Size => 0;

   procedure Set_Allocator (This      : in out Executor;
                            Allocator :        Allocators.Handle);
   --  Set an specific allocator

   procedure Call (This : in out Executor;
                   CB   :        Impl.Callbacks.Callback'Class) is abstract;
   --  Only procedure to be overriden, receives a ready call to user code

   procedure Add (This :         in out Executor;
                  Node : aliased in out Nodes.Node'Class);

   procedure Remove (This :         in out Executor;
                     Node : aliased in out Nodes.Node'Class);
   --  Automatically called by the node on going out of scope, no need to
   --    do it manually.

   procedure Spin (This      : in out Executor;
                   Once      :        Boolean       := False;
                   During    :        ROS2_Duration := 0.1);

   function Spin_Once (This    : in out Executor;
                       Timeout :        ROS2_Duration;
                       Node    : access Nodes.Node'Class := null) return Boolean;
   --  Will spin on the given node or all of them when null

   procedure Shutdown (This : in out Executor) is null;
   --  Concurrent executors must be shut down or otherwise they do not finalize (tasks inside)
   --  Parent must be called if overriden.

private

   type Node_Access is access all Nodes.Node'Class;

   function To_Ptr is new Ada.Unchecked_Conversion (Node_Access, System.Address);

   use all type System.Address;

   function "<" (L, R : Node_Access) return Boolean is
      (To_Ptr (L) < To_Ptr (R));

   package Node_Sets is new Ada.Containers.Bounded_Vectors (Positive, Node_Access);
   subtype Set is Node_Sets.Vector;
   --  This should be (and was) a set, but bounded_sets cause some bizarre
   --    accessibility check (might be related to uninitialized components)

   protected type Node_Set (Max_Nodes : Count_Type;
                            Parent    : access Executor'Class) is
      procedure Delete (Node  : Node_Access);
      procedure Insert (Node  : Node_Access);
      procedure Get    (Nodes : out Set);
      function  Is_Empty return Boolean;
   private
      Nodes : Set (Max_Nodes);
   end Node_Set;

   type Executor (Max_Nodes : Count_Type := Default_Nodes_Per_Executor) is
     abstract tagged limited
      record
         Nodes     : Node_Set (Max_Nodes, Executor'Access);
         Allocator : Allocators.Handle := Allocators.Global_Allocator;
      end record;

   procedure Common_Dispatch (Node   : access Nodes.Node'Class;
                              Handle :        Impl.Dispatchers.Handle);
   --  The actual logic of the dispatch that derived dispatchers may use
   --    if they do not do anything special

end RCL.Executors;
