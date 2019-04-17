with Interfaces.C;

package stdio_h is

   --  Too complex to be fully auto generated, and brings in too many deps

   subtype void is Interfaces.C.int;

   type va_list is access all void with convention => C;

end stdio_h;