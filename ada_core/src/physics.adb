with Interfaces.C;
use Interfaces.C;
with Reactor;

package body Physics is

   procedure Update_Physics (State : in out Reactor.Reactor_State) is
   begin
      if State.Pump_On = Interfaces.C.int(1) and State.Valve_Open = Interfaces.C.int(1) then
         State.Temperature := State.Temperature - Interfaces.C.double(0.3);
         State.Pressure    := State.Pressure - Interfaces.C.double(0.1);
         State.Flow        := State.Flow + Interfaces.C.double(0.3);
      else
         State.Temperature := State.Temperature + Interfaces.C.double(0.5);
         State.Pressure    := State.Pressure + Interfaces.C.double(0.2);
         State.Flow        := State.Flow - Interfaces.C.double(0.4);
      end if;
      if State.Temperature < Interfaces.C.double(250.0) then
         State.Temperature := Interfaces.C.double(250.0);
      elsif State.Temperature > Interfaces.C.double(1200.0) then
         State.Temperature := Interfaces.C.double(1200.0);
      end if;
      if State.Pressure < Interfaces.C.double(50.0) then
         State.Pressure := Interfaces.C.double(50.0);
      elsif State.Pressure > Interfaces.C.double(300.0) then
         State.Pressure := Interfaces.C.double(300.0);
      end if;
      if State.Flow < Interfaces.C.double(0.0) then
         State.Flow := Interfaces.C.double(0.0);
      elsif State.Flow > Interfaces.C.double(200.0) then
         State.Flow := Interfaces.C.double(200.0);
      end if;
   end Update_Physics;

end Physics;