with RCL.Impl.Dispatchers;

package RCL.Nodes.Impl is
   
   use RCL.Impl;

   type Handle is access all Node'Class with Storage_Size => 0;
   
   type Reference (Ptr : access Rcl_Node_T) is limited null record
     with Implicit_Dereference => Ptr;
   
   function To_C (This : aliased in out Node'Class) return Reference;      
   
   ---------------------------------------------------------------
   --  Extras for executor interaction, also not needed by clients 
   
   procedure Client_Free (This : in out Node;
                          Ptr  :        Dispatchers.Handle);
   
   procedure Client_Success (This : in out Node; Client : Dispatchers.Handle);   
   
   function Current_Executor (This : in out Node'Class) return Executors.Handle;
   
   procedure Get_Callbacks (This : in out Node'Class; Set : in out Dispatchers.Maps.Set);      
   
   procedure Trigger (This : in out Node'Class; Dispatcher : Dispatchers.Handle);
   
private    
            
   function Current_Executor (This : in out Node'Class) return Executors.Handle 
     renames Nodes.Current_Executor;
   
   function To_C (This : aliased in out Node'Class) return Reference is
     (Ptr => This.Impl'Access);   
   
   procedure Client_Free (This : in out Node;
                          Ptr  :        Dispatchers.Handle) renames Nodes.Client_Free;

end RCL.Nodes.Impl;
