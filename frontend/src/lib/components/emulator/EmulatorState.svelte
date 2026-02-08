<script lang="ts">
	import type { MachineState } from '$lib/api/types';
	import { MechanicalPhase } from '$lib/api/types';

	export let state: MachineState | null = null;

	/**
	 * Format column value for display (right-aligned, 31 digits)
	 */
	function formatColumnValue(value: number): string {
		return Math.abs(value).toString().padStart(31, '0');
	}

	/**
	 * Get CSS class for phase indicator
	 */
	function getPhaseClass(phase: MechanicalPhase | string): string {
		switch (phase) {
			case MechanicalPhase.ADDITION:
				return 'phase-addition';
			case MechanicalPhase.CARRY:
				return 'phase-carry';
			case MechanicalPhase.TABLE:
				return 'phase-table';
			case MechanicalPhase.OUTPUT:
				return 'phase-output';
			default:
				return 'phase-idle';
		}
	}

	/**
	 * Get human-readable phase name
	 */
	function getPhaseLabel(phase: MechanicalPhase | string): string {
		switch (phase) {
			case MechanicalPhase.ADDITION:
				return 'Addition';
			case MechanicalPhase.CARRY:
				return 'Carry';
			case MechanicalPhase.TABLE:
				return 'Table';
			case MechanicalPhase.OUTPUT:
				return 'Output';
			default:
				return 'Idle';
		}
	}

	/**
	 * Get angle display in degrees
	 */
	function getAngleDegrees(angle: number): number {
		return Math.round((angle / 65536) * 360 * 100) / 100;
	}
</script>

<div class="emulator-state">
	{#if state}
		<div class="state-header">
			<h3>Machine State</h3>
		</div>

		<div class="state-grid">
			<!-- Cycle and Operation Counters -->
			<div class="state-section">
				<h4>Execution</h4>
				<div class="state-item">
					<span class="label">Cycle Count</span>
					<span class="value mono">{state.cycle}</span>
				</div>
				<div class="state-item">
					<span class="label">Total Operations</span>
					<span class="value mono">{state.totalOperations}</span>
				</div>
			</div>

			<!-- Mechanical Phase -->
			<div class="state-section">
				<h4>Mechanical State</h4>
				<div class="state-item">
					<span class="label">Phase</span>
					<span class={`value phase-indicator ${getPhaseClass(state.phase)}`}>
						{getPhaseLabel(state.phase)}
					</span>
				</div>
				<div class="state-item">
					<span class="label">Shaft Angle</span>
					<span class="value mono">{getAngleDegrees(state.angle)}°</span>
				</div>
			</div>

			<!-- Accumulator -->
			<div class="state-section">
				<h4>Accumulator</h4>
				<div class="state-item full-width">
					<span class="label">Value</span>
					<span class="value mono accumulator-value">{formatColumnValue(state.accumulator)}</span>
				</div>
			</div>
		</div>

		<!-- Column Values -->
		<div class="columns-section">
			<h4>Column Values</h4>
			<div class="columns-grid">
				{#each state.columns as value, index}
					<div class="column">
						<div class="column-header">
							Col {index}
						</div>
						<div class="column-value mono">
							{formatColumnValue(value)}
						</div>
						{#if state.carrySignals[index]}
							<div class="carry-indicator" title="Carry signal active">
								⚡
							</div>
						{/if}
					</div>
				{/each}
			</div>
		</div>

		<!-- Carry Signals Summary -->
		<div class="carry-summary">
			<h4>Carry Signals</h4>
			<div class="carry-display">
				{#each state.carrySignals as carry, index}
					<span class={`carry-bit ${carry ? 'active' : 'inactive'}`} title={`Column ${index}`}>
						{index}
					</span>
				{/each}
			</div>
		</div>
	{:else}
		<div class="empty-state">
			<p>No state available. Execute a polynomial to see machine state.</p>
		</div>
	{/if}
</div>

<style>
	.emulator-state {
		background: #16213e;
		border: 1px solid #0f3460;
		border-radius: 8px;
		padding: 1.5rem;
		box-shadow: 0 4px 6px rgba(0, 0, 0, 0.2);
	}

	.state-header h3 {
		color: #667eea;
		margin: 0 0 1.5rem 0;
		font-size: 1.3rem;
	}

	.state-grid {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
		gap: 1.5rem;
		margin-bottom: 2rem;
	}

	.state-section {
		background: rgba(15, 52, 96, 0.5);
		border: 1px solid #533483;
		border-radius: 6px;
		padding: 1rem;
	}

	.state-section h4 {
		color: #b0b8d4;
		margin: 0 0 1rem 0;
		font-size: 0.95rem;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		font-weight: 600;
	}

	.state-item {
		display: flex;
		justify-content: space-between;
		align-items: center;
		gap: 1rem;
		padding: 0.5rem 0;
		border-bottom: 1px solid #0f3460;
	}

	.state-item:last-child {
		border-bottom: none;
	}

	.state-item.full-width {
		flex-direction: column;
		align-items: flex-start;
	}

	.label {
		color: #b0b8d4;
		font-size: 0.9rem;
		font-weight: 500;
	}

	.value {
		color: #e0e0e0;
		font-size: 0.95rem;
		font-weight: 600;
	}

	.value.mono {
		font-family: 'Courier New', monospace;
		letter-spacing: 0.05em;
	}

	.accumulator-value {
		background: rgba(102, 126, 234, 0.1);
		padding: 0.5rem;
		border-radius: 4px;
		border-left: 3px solid #667eea;
		padding-left: 0.75rem;
		display: block;
		width: 100%;
		word-break: break-all;
	}

	.phase-indicator {
		display: inline-block;
		padding: 0.4rem 0.8rem;
		border-radius: 4px;
		font-size: 0.85rem;
		font-weight: 600;
		text-transform: uppercase;
		letter-spacing: 0.5px;
	}

	.phase-idle {
		background: rgba(90, 90, 90, 0.3);
		color: #b0b8d4;
		border: 1px solid #5a5a5a;
	}

	.phase-addition {
		background: rgba(102, 126, 234, 0.3);
		color: #b0d0f0;
		border: 1px solid #667eea;
	}

	.phase-carry {
		background: rgba(184, 134, 11, 0.3);
		color: #f0d080;
		border: 1px solid #b8860b;
	}

	.phase-table {
		background: rgba(34, 139, 34, 0.3);
		color: #b0f0c0;
		border: 1px solid #228b22;
	}

	.phase-output {
		background: rgba(178, 34, 52, 0.3);
		color: #f0b0c0;
		border: 1px solid #b22234;
	}

	.columns-section {
		margin-bottom: 1.5rem;
		border-top: 1px solid #0f3460;
		padding-top: 1.5rem;
	}

	.columns-section h4 {
		color: #b0b8d4;
		margin: 0 0 1rem 0;
		font-size: 0.95rem;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		font-weight: 600;
	}

	.columns-grid {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(100px, 1fr));
		gap: 1rem;
	}

	.column {
		background: rgba(15, 52, 96, 0.5);
		border: 1px solid #533483;
		border-radius: 6px;
		padding: 0.75rem;
		position: relative;
	}

	.column-header {
		color: #667eea;
		font-size: 0.85rem;
		font-weight: 600;
		margin-bottom: 0.5rem;
		text-align: center;
	}

	.column-value {
		color: #e0e0e0;
		font-size: 0.75rem;
		word-break: break-all;
		line-height: 1.3;
		font-weight: 600;
	}

	.carry-indicator {
		position: absolute;
		top: -8px;
		right: -8px;
		background: #ff6b6b;
		border: 1px solid #ee5a5a;
		border-radius: 50%;
		width: 24px;
		height: 24px;
		display: flex;
		align-items: center;
		justify-content: center;
		font-size: 0.9rem;
		animation: pulse 1s infinite;
	}

	@keyframes pulse {
		0%,
		100% {
			opacity: 1;
		}
		50% {
			opacity: 0.6;
		}
	}

	.carry-summary {
		border-top: 1px solid #0f3460;
		padding-top: 1.5rem;
	}

	.carry-summary h4 {
		color: #b0b8d4;
		margin: 0 0 1rem 0;
		font-size: 0.95rem;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		font-weight: 600;
	}

	.carry-display {
		display: flex;
		gap: 0.5rem;
		flex-wrap: wrap;
	}

	.carry-bit {
		display: inline-flex;
		align-items: center;
		justify-content: center;
		width: 32px;
		height: 32px;
		border: 2px solid #533483;
		border-radius: 4px;
		font-weight: 600;
		font-size: 0.9rem;
		cursor: default;
	}

	.carry-bit.inactive {
		background: rgba(15, 52, 96, 0.5);
		color: #b0b8d4;
	}

	.carry-bit.active {
		background: rgba(255, 107, 107, 0.3);
		border-color: #ff6b6b;
		color: #ff9999;
		animation: pulse 1s infinite;
	}

	.empty-state {
		text-align: center;
		padding: 3rem 1rem;
		color: #b0b8d4;
	}

	.empty-state p {
		margin: 0;
		font-size: 0.95rem;
		opacity: 0.8;
	}

	@media (max-width: 600px) {
		.columns-grid {
			grid-template-columns: repeat(auto-fit, minmax(80px, 1fr));
		}

		.column-value {
			font-size: 0.65rem;
		}

		.state-grid {
			grid-template-columns: 1fr;
		}
	}
</style>
