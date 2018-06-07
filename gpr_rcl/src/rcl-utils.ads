with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Finalization;

package RCL.Utils is

   --  Assorted helpers
   
   function Command_Name return String;
   --  Command name without path
   
   package String_Maps is new Ada.Containers.Indefinite_Ordered_Maps (String, String);
   subtype String_Map is String_Maps.Map;
   
   subtype Topics_And_Types   is String_Map;
   subtype Services_And_Types is String_Map;
   
   package String_Vectors is new Ada.Containers.Indefinite_Vectors (Positive, String);
   subtype String_Vector is String_Vectors.Vector;
   
   subtype Node_Name_Vector is String_Vector;
   
   type Parameterless is access procedure;
   
   type Initshut (On_Initialize : Parameterless;
                  On_Finalize   : Parameterless) is
     new Ada.Finalization.Limited_Controlled with null record;
   
   overriding procedure Initialize (This : in out Initshut);
   overriding procedure Finalize   (This : in out Initshut);

end RCL.Utils;
