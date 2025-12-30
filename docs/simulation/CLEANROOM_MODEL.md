Cleanroom Physics Model (outline)
- Mechanics: rigid body gears, inertia per wheel, contact friction, backlash; efficiency per stage.
- Steam drive: piston force = pressure * area; torque via crank; Stephenson valve timing (lap/lead/cutoff) maps to admission/exhaust windows over crank angle; RPM setpoint vs load.
- Bearings & lubrication: plain bearings with viscous + Coulomb friction; temperature-insensitive first pass; loss per rev.
- Card feed: discrete-event queue with jam probability; service time per card; blocked engine effect on timing.
- Timing: map opcode cycles to mechanical angle/period using RPM and gear ratios; validate against TIMING_TABLE.
- Stochastic: noise on friction and feed; Monte Carlo for p99 tails.
