with Interfaces.C;
use Interfaces.C;
with Reactor;
package Faults is
   type Failure_Code is (
      None,             -- 0
      Leak,             -- 1 Mechanical: Leak
      Pump_Fail,        -- 2 Mechanical: Pump failure
      Valve_Stuck,      -- 3 Mechanical: Stuck valve
      Power_Loss,       -- 4 Electrical: Power loss
      Sensor_Fail,      -- 5 Sensor: Sensor failure
      Overheat,         -- 6 Operational: Overheating
      Scram_Fail,       -- 7 Operational: SCRAM failure
      Flooding,         -- 8 Hydraulic: Flooding
      Blocked_Pipe,     -- 9 Hydraulic: Pipe blockage
      Control_Rod_Stuck,-- 10 Mechanical: Stuck control rod
      Cooling_Loss,     -- 11 Hydraulic: Loss of cooling
      Air_Ingress       -- 12 Hydraulic: Air ingress
   );
   pragma Convention (C, Failure_Code);

   procedure Apply_Faults (State : in out Reactor.Reactor_State);
end Faults;