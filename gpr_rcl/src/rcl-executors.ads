with Ada.Containers.Ordered_Sets;
with Ada.Unchecked_Conversion;

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
   
   type Executor is abstract tagged limited private;
      
   procedure Add (This :         in out Executor; 
                  Node : aliased in out Nodes.Node'Class) is null;
   
   procedure Remove (This :         in out Executor; 
                     Node : aliased in out Nodes.Node'Class) is null;
   
private
   
   type Node_Access is not null access Nodes.Node'Class;
   
   function To_Ptr is new Ada.Unchecked_Conversion (Node_Access, System.Address);
   
   use all type System.Address;
   
   function "<" (L, R : Node_Access) return Boolean is
      (To_Ptr (L) < To_Ptr (R));
   
   package Node_Sets is new Ada.Containers.Ordered_Sets (Node_Access);
   
   type Executor is abstract tagged limited null record;

end RCL.Executors;
