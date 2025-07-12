package Reactor is
   type Reactor_State is record
      Core_Temperature : Float;
      Pressure         : Float;
      Coolant_Flow     : Float;
      Pump_On          : Boolean;
      Valve_Position   : Float;
      Control_Rods     : Float;
      Alert_Flag       : Boolean;
      Fault_Message    : String(1..100);
   end record;

   procedure Initialize;
   procedure Step;
   procedure Set_Pump_Speed(Speed : Float);
   procedure Set_Valve_Position(Pos : Float);
   procedure Set_Control_Rods(Pos : Float);
   procedure Inject_Fault(Fault_ID : Integer);
   function Get_State return Reactor_State;

   -- FFI Exports
   pragma Export(C, Initialize, "reactor_initialize");
   pragma Export(C, Step, "reactor_step");
   pragma Export(C, Set_Pump_Speed, "reactor_set_pump_speed");
   pragma Export(C, Set_Valve_Position, "reactor_set_valve_position");
   pragma Export(C, Set_Control_Rods, "reactor_set_control_rods");
   pragma Export(C, Inject_Fault, "reactor_inject_fault");
   pragma Export(C, Get_State, "reactor_get_state");
end Reactor;
