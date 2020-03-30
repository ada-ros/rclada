with Ada.Unchecked_Conversion;

package RCL.Errors is

   type Error is
     (Ok,
      Unspecified,
      Timeout,
      Other)
     with Size => Integer'Size;

   Other_Value : constant := 9999;

   for Error use
     (Ok          => 0, -- FIXME RMW_RET_OK,
      Unspecified => 1, -- FIXME RMW_RET_ERROR,
      Timeout     => 2, -- FIXME RMW_RET_TIMEOUT,
      Other       => Other_value);

   function To_Error (E : Integer) return Error;

private

   function Unchecked_To_Error is new Ada.Unchecked_Conversion (Integer, Error);

   function To_Error (E : Integer) return Error is
     (if   Unchecked_To_Error (E)'Valid
      then Unchecked_To_Error (E)
      else Other);

end RCL.Errors;
