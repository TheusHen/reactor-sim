with Interfaces.C;
package Reactor is
   type Reactor_State is record
      Temperature : Interfaces.C.double;
      Pressure    : Interfaces.C.double;
      Flow        : Interfaces.C.double;
      Pump_On     : Interfaces.C.int;
      Valve_Open  : Interfaces.C.int;
      Failure     : Interfaces.C.int;
   end record;
   pragma Convention (C, Reactor_State);

   procedure Init;
   pragma Export (C, Init, "reactor_init");

   procedure Step;
   pragma Export (C, Step, "reactor_step");

   function Get_State return Reactor_State;
   pragma Export (C, Get_State, "reactor_get_state");

   procedure Set_Valve(Open : Interfaces.C.int);
   pragma Export (C, Set_Valve, "reactor_set_valve");

   procedure Set_Pump(On : Interfaces.C.int);
   pragma Export (C, Set_Pump, "reactor_set_pump");

   procedure Inject_Failure(F: Interfaces.C.int);
   pragma Export (C, Inject_Failure, "reactor_inject_failure");
end Reactor;
