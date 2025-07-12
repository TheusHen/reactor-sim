with Reactor; use Reactor;
package body Faults is
   procedure Inject_Fault(State : in out Reactor_State; Fault_ID : Integer) is
   begin
      case Fault_ID is
         when 1 =>
            State.Coolant_Flow := 0.0;
            State.Pump_On := False;
            State.Alert_Flag := True;
            State.Fault_Message := "Pump Failure" & (1..88 => ' ');
         when 2 =>
            State.Valve_Position := 0.0;
            State.Alert_Flag := True;
            State.Fault_Message := "Valve Jammed" & (1..87 => ' ');
         when 3 =>
            State.Control_Rods := 0.0;
            State.Alert_Flag := True;
            State.Fault_Message := "Rod Stuck Out" & (1..86 => ' ');
         when others =>
            null;
      end case;
   end Inject_Fault;

   procedure Check_Faults(State : in out Reactor_State) is
   begin
      if State.Core_Temperature > 500.0 then
         State.Alert_Flag := True;
         State.Fault_Message := "Overheat!" & (1..92 => ' ');
      end if;
   end Check_Faults;
end Faults;
