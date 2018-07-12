with Ada.Strings.Bounded;

with RCL.Utils;

package body RCL.Logging is

   package Names is new Ada.Strings.Bounded.Generic_Bounded_Length (255);

   Global_Name : Names.Bounded_String;

   ---------
   -- Log --
   ---------

   procedure Log (Severity : Levels;
                  Message  : String;
                  Locate   : Boolean := False; -- see Include_Source_Location
                  Location : Log_Location := Logging.Location; -- Manual location
                  Name     : String  := ""     -- Identify a particular logger
                 )
   is
      pragma Unreferenced (Locate);

      Cloc : aliased constant Rcutils_Log_Location_T :=
               (To_C (Location.Subprogram).To_Ptr,
                To_C (Location.File_Name).To_Ptr,
                C.Size_T (Location.Line_Number));
   begin
      Rcutils_Log (Cloc'Access,
                   C.Int (Severity),
                   To_C ((if Name /= ""
                          then Name
                          else Names.To_String (Global_Name))).To_Ptr,
                   To_C (Message).To_Ptr);
   end Log;


   -----------
   -- Debug --
   -----------

   procedure Debug (Message  : String;
                    Locate   : Boolean      := False;
                    Location : Log_Location := Logging.Location;
                    Name     : String       := "") is
   begin
      Log (Levels (RCUTILS_LOG_SEVERITY_DEBUG),
           Message,
           Locate,
           Location,
           Name);
   end Debug;

   ----------
   -- Info --
   ----------

   procedure Info (Message  : String;
                    Locate   : Boolean      := False;
                    Location : Log_Location := Logging.Location;
                    Name     : String       := "") is
   begin
      Log (Levels (RCUTILS_LOG_SEVERITY_INFO),
           Message,
           Locate,
           Location,
           Name);
   end Info;

   ----------
   -- Warn --
   ----------

   procedure Warn (Message  : String;
                    Locate   : Boolean      := False;
                    Location : Log_Location := Logging.Location;
                    Name     : String       := "") is
   begin
      Log (Levels (RCUTILS_LOG_SEVERITY_WARN),
           Message,
           Locate,
           Location,
           Name);
   end Warn;

   -----------
   -- Error --
   -----------

   procedure Error (Message  : String;
                    Locate   : Boolean      := False;
                    Location : Log_Location := Logging.Location;
                    Name     : String       := "") is
   begin
      Log (Levels (RCUTILS_LOG_SEVERITY_ERROR),
           Message,
           Locate,
           Location,
           Name);
   end Error;

   -----------
   -- Fatal --
   -----------

   procedure Fatal (Message  : String;
                    Locate   : Boolean      := False;
                    Location : Log_Location := Logging.Location;
                    Name     : String       := "") is
   begin
      Log (Levels (RCUTILS_LOG_SEVERITY_FATAL),
           Message,
           Locate,
           Location,
           Name);
   end Fatal;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      if Initialized then
         Check (Rcutils_Logging_Shutdown);
      end if;
   end Shutdown;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Name : String) is
   begin
      Global_Name := Names.To_Bounded_String (Name);

      if not Initialized then
         Check (Rcutils_Logging_Initialize);
      end if;
   end Initialize;

   ------------------
   -- Init_Logging --
   ------------------

   procedure Init_Logging is
   begin
      Logging.Initialize (Utils.Command_Name);
   end Init_Logging;

   Initer : Utils.Initshut (On_Initialize => Init_Logging'Access,
                            On_Finalize   => null) with Unreferenced;
   --  This way we ensure that users cannot log without initialization

end RCL.Logging;
