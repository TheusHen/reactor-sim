with Interfaces.C;
use Interfaces.C;
with Reactor;

package body Faults is

   procedure Apply_Faults (State : in out Reactor.Reactor_State) is
      F : constant Integer := Integer(State.Failure);
   begin
      case F is
         when 0 => null; -- None
         when 1 => -- Leak
            State.Flow := State.Flow - Interfaces.C.double(3.0);
            State.Pressure := State.Pressure - Interfaces.C.double(2.0);
            State.Temperature := State.Temperature + Interfaces.C.double(1.5);
         when 2 => -- Pump failure
            State.Pump_On := Interfaces.C.int(0);
         when 3 => -- Stuck valve
            State.Valve_Open := Interfaces.C.int(0);
         when 4 => -- Power loss
            State.Pump_On := Interfaces.C.int(0);
         when 5 => -- Sensor failure
            null;
         when 6 => -- Overheating
            State.Temperature := State.Temperature + Interfaces.C.double(10.0);
         when 7 => -- SCRAM failure
            State.Temperature := State.Temperature + Interfaces.C.double(15.0);
         when 8 => -- Flooding
            State.Flow := State.Flow + Interfaces.C.double(5.0);
            State.Pressure := State.Pressure - Interfaces.C.double(2.0);
         when 9 => -- Pipe blockage
            State.Flow := State.Flow - Interfaces.C.double(4.0);
            State.Pressure := State.Pressure + Interfaces.C.double(2.0);
         when 10 => -- Stuck control rod
            State.Temperature := State.Temperature + Interfaces.C.double(3.0);
         when 11 => -- Loss of cooling
            State.Flow := State.Flow - Interfaces.C.double(8.0);
            State.Temperature := State.Temperature + Interfaces.C.double(4.0);
         when 12 => -- Air ingress
            State.Pressure := State.Pressure - Interfaces.C.double(3.0);
         when others =>
            null;
      end case;
   end Apply_Faults;
end Faults;