package body RCL.Impl.Dispatchers.Maps is

   -----------------
   -- Num_Clients --
   -----------------

   function Num_Clients       (This : Map) return Natural is
      Num : Natural := 0;
   begin
      for D of This loop
         if D.Kind = Client then
            Num := Num + 1;
         end if;
      end loop;
      return Num;
   end Num_Clients;

   ------------------
   -- Num_Services --
   ------------------

   function Num_Services      (This : Map) return Natural is
      Num : Natural := 0;
   begin
      for D of This loop
         if D.Kind = Service then
            Num := Num + 1;
         end if;
      end loop;
      return Num;
   end Num_Services;

   -----------------------
   -- Num_Subscriptions --
   -----------------------

   function Num_Subscriptions (This : Map) return Natural is
      Num : Natural := 0;
   begin
      for D of This loop
         if D.Kind = Subscription then
            Num := Num + 1;
         end if;
      end loop;
      return Num;
   end Num_Subscriptions;

   ----------------
   -- Num_Timers --
   ----------------

   function Num_Timers        (This : Map) return Natural is
      Num : Natural := 0;
   begin
      for D of This loop
         if D.Kind = Timer then
            Num := Num + 1;
         end if;
      end loop;
      return Num;
   end Num_Timers;

end RCL.Impl.Dispatchers.Maps;
