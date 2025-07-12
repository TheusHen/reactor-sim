with Interfaces.C;
use Interfaces.C;
with Physics, Faults;

package body Reactor is

   State : Reactor_State := (
      Temperature => Interfaces.C.double(300.0),
      Pressure    => Interfaces.C.double(150.0),
      Flow        => Interfaces.C.double(100.0),
      Pump_On     => Interfaces.C.int(1),
      Valve_Open  => Interfaces.C.int(1),
      Failure     => Interfaces.C.int(0)
   );

   procedure Init is
   begin
      State := (
         Temperature => Interfaces.C.double(300.0),
         Pressure    => Interfaces.C.double(150.0),
         Flow        => Interfaces.C.double(100.0),
         Pump_On     => Interfaces.C.int(1),
         Valve_Open  => Interfaces.C.int(1),
         Failure     => Interfaces.C.int(0)
      );
   end Init;

   procedure Step is
   begin
      Physics.Update_Physics (State);
      Faults.Apply_Faults (State);
   end Step;

   function Get_State return Reactor_State is
   begin
      return State;
   end Get_State;

   procedure Set_Valve(Open : Interfaces.C.int) is
   begin
      State.Valve_Open := Open;
   end Set_Valve;

   procedure Set_Pump(On : Interfaces.C.int) is
   begin
      State.Pump_On := On;
   end Set_Pump;

   procedure Inject_Failure(F: Interfaces.C.int) is
   begin
      State.Failure := F;
   end Inject_Failure;

end Reactor;