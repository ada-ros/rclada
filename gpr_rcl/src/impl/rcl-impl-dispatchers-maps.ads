with Ada.Containers.Bounded_Ordered_Maps;
use Ada.Containers;

package RCL.Impl.Dispatchers.Maps is

   package Dispatcher_Maps is new Ada.Containers.Bounded_Ordered_Maps
     (Handle, Definite_Dispatcher);
   
   type Set is new Dispatcher_Maps.Map with null record;
   
   function Num_Clients       (This : Set) return Natural;
   function Num_Services      (This : Set) return Natural;
   function Num_Subscriptions (This : Set) return Natural;
   function Num_Timers        (This : Set) return Natural;

end RCL.Impl.Dispatchers.Maps;
