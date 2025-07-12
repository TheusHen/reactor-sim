with Reactor; use Reactor;
package body Physics is
   procedure Update_Physics(State : in out Reactor_State) is
   begin
      -- Simulação simplificada:
      -- Temperatura sobe se barras de controle estão baixas, desce se fluxo do refrigerante alto
      State.Core_Temperature :=
         State.Core_Temperature
         + 10.0 * (1.0 - State.Control_Rods)
         - 5.0 * State.Coolant_Flow * State.Valve_Position;

      if State.Core_Temperature < 50.0 then
         State.Core_Temperature := 50.0;
      end if;

      State.Pressure := 100.0 + (State.Core_Temperature - 300.0) * 0.2;
   end Update_Physics;
end Physics;
