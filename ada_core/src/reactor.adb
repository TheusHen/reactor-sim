with Reactor; use Reactor;
with Physics;
with Faults;
package body Reactor is
   Current_State : Reactor_State;

   procedure Initialize is
   begin
      Current_State.Core_Temperature := 300.0;
      Current_State.Pressure := 100.0;
      Current_State.Coolant_Flow := 1.0;
      Current_State.Pump_On := True;
      Current_State.Valve_Position := 1.0;
      Current_State.Control_Rods := 0.5;
      Current_State.Alert_Flag := False;
      Current_State.Fault_Message := (others => ' ');
   end Initialize;

   procedure Step is
   begin
      Physics.Update_Physics(Current_State);
      Faults.Check_Faults(Current_State);
   end Step;

   procedure Set_Pump_Speed(Speed : Float) is
   begin
      Current_State.Coolant_Flow := Speed;
      Current_State.Pump_On := (Speed > 0.1);
   end Set_Pump_Speed;

   procedure Set_Valve_Position(Pos : Float) is
   begin
      Current_State.Valve_Position := Pos;
   end Set_Valve_Position;

   procedure Set_Control_Rods(Pos : Float) is
   begin
      Current_State.Control_Rods := Pos;
   end Set_Control_Rods;

   procedure Inject_Fault(Fault_ID : Integer) is
   begin
      Faults.Inject_Fault(Current_State, Fault_ID);
   end Inject_Fault;

   function Get_State return Reactor_State is
   begin
      return Current_State;
   end Get_State;
end Reactor;
