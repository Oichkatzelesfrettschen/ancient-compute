<script lang="ts">
	import type { Breakpoint, DebuggerVariable, MachineState } from '$lib/api/types';
	import { MechanicalPhase } from '$lib/api/types';
	import {
		setBreakpoint,
		enableBreakpoint,
		disableBreakpoint,
		removeBreakpoint,
		debugStep,
		debugContinue,
		defineVariable,
		setVariable
	} from '$lib/api/emulator';

	export let isPaused: boolean = false;
	export let currentState: MachineState | null = null;
	export let variables: Record<string, DebuggerVariable> = {};

	let breakpoints: Breakpoint[] = [];
	let breakpointType: 'CYCLE' | 'PHASE' | 'VALUE_CHANGE' | 'CONDITION' = 'CYCLE';
	let cycleTarget: number = 1;
	let phaseTarget: MechanicalPhase = MechanicalPhase.ADDITION;
	let variableTarget: string = '';
	let newVarName: string = '';
	let newVarValue: number = 0;
	let errorMessage: string = '';
	let successMessage: string = '';

	/**
	 * Add a new breakpoint
	 */
	async function handleAddBreakpoint() {
		errorMessage = '';
		successMessage = '';

		try {
			const options: Record<string, unknown> = {};

			if (breakpointType === 'CYCLE') {
				if (cycleTarget < 0) throw new Error('Cycle target must be non-negative');
				options.cycle_target = cycleTarget;
			} else if (breakpointType === 'PHASE') {
				options.phase_target = phaseTarget;
			} else if (breakpointType === 'VALUE_CHANGE') {
				if (!variableTarget) throw new Error('Variable name required');
				options.variable_name = variableTarget;
			}

			const response = await setBreakpoint(breakpointType, options);

			if (response.success) {
				successMessage = `Breakpoint ${response.breakpointId} created`;
				// Add to local list
				const newBp: Breakpoint = {
					id: response.breakpointId || breakpoints.length + 1,
					type: breakpointType,
					enabled: true,
					hitCount: 0,
					cycleTarget: options.cycle_target as number | undefined,
					phaseTarget: options.phase_target as MechanicalPhase | undefined,
					variableName: options.variable_name as string | undefined
				};
				breakpoints = [...breakpoints, newBp];
			} else {
				errorMessage = response.error || 'Failed to create breakpoint';
			}
		} catch (error) {
			errorMessage = error instanceof Error ? error.message : 'Error creating breakpoint';
		}
	}

	/**
	 * Toggle breakpoint enabled state
	 */
	async function handleToggleBreakpoint(id: number) {
		const bp = breakpoints.find((b) => b.id === id);
		if (!bp) return;

		try {
			const response = bp.enabled
				? await disableBreakpoint(id)
				: await enableBreakpoint(id);

			if (response.success) {
				// Update local state
				const index = breakpoints.findIndex((b) => b.id === id);
				if (index >= 0) {
					breakpoints[index].enabled = !bp.enabled;
					breakpoints = breakpoints;
				}
			} else {
				errorMessage = response.error || 'Failed to toggle breakpoint';
			}
		} catch (error) {
			errorMessage = error instanceof Error ? error.message : 'Error toggling breakpoint';
		}
	}

	/**
	 * Delete a breakpoint
	 */
	async function handleDeleteBreakpoint(id: number) {
		try {
			const response = await removeBreakpoint(id);

			if (response.success) {
				breakpoints = breakpoints.filter((b) => b.id !== id);
				successMessage = `Breakpoint ${id} deleted`;
			} else {
				errorMessage = response.error || 'Failed to delete breakpoint';
			}
		} catch (error) {
			errorMessage = error instanceof Error ? error.message : 'Error deleting breakpoint';
		}
	}

	/**
	 * Step through execution
	 */
	async function handleStep() {
		errorMessage = '';

		try {
			const response = await debugStep();

			if (response.success) {
				currentState = response.state || null;
				isPaused = response.breakpointsHit && response.breakpointsHit.length > 0;
			} else {
				errorMessage = response.error || 'Step failed';
			}
		} catch (error) {
			errorMessage = error instanceof Error ? error.message : 'Error stepping';
		}
	}

	/**
	 * Continue execution
	 */
	async function handleContinue() {
		errorMessage = '';

		try {
			const response = await debugContinue(100); // Max 100 cycles

			if (response.success) {
				currentState = response.state || null;
				successMessage = `Executed ${response.cyclesRun} cycles`;
				isPaused = response.breakpointHit !== undefined;
			} else {
				errorMessage = response.error || 'Continue failed';
			}
		} catch (error) {
			errorMessage = error instanceof Error ? error.message : 'Error continuing';
		}
	}

	/**
	 * Define a new variable
	 */
	async function handleDefineVariable() {
		errorMessage = '';

		if (!newVarName.trim()) {
			errorMessage = 'Variable name required';
			return;
		}

		try {
			const response = await defineVariable(newVarName, newVarValue);

			if (response.success) {
				successMessage = `Variable '${newVarName}' defined`;
				newVarName = '';
				newVarValue = 0;
			} else {
				errorMessage = response.error || 'Failed to define variable';
			}
		} catch (error) {
			errorMessage = error instanceof Error ? error.message : 'Error defining variable';
		}
	}

	/**
	 * Update a variable value
	 */
	async function handleSetVariable(name: string, value: number) {
		errorMessage = '';

		try {
			const response = await setVariable(name, value);

			if (response.success) {
				// Update local state
				if (variables[name]) {
					variables[name].currentValue = value;
					variables = variables;
				}
			} else {
				errorMessage = response.error || 'Failed to set variable';
			}
		} catch (error) {
			errorMessage = error instanceof Error ? error.message : 'Error setting variable';
		}
	}

	/**
	 * Get breakpoint description
	 */
	function getBreakpointDescription(bp: Breakpoint): string {
		if (bp.type === 'CYCLE') {
			return `Cycle ${bp.cycleTarget}`;
		} else if (bp.type === 'PHASE') {
			return `Phase ${bp.phaseTarget}`;
		} else if (bp.type === 'VALUE_CHANGE') {
			return `Variable change: ${bp.variableName}`;
		}
		return bp.type;
	}
</script>

<div class="debugger-panel">
	<div class="panel-header">
		<h3>Debugger</h3>
		<span class={`status ${isPaused ? 'paused' : 'running'}`}>
			{isPaused ? '⏸ Paused' : '▶ Running'}
		</span>
	</div>

	<!-- Execution Controls -->
	<div class="section">
		<h4>Execution Control</h4>
		<div class="button-group">
			<button on:click={handleStep} class="step-btn">
				⏭ Step Cycle
			</button>
			<button on:click={handleContinue} class="continue-btn">
				▶ Continue
			</button>
		</div>
	</div>

	<!-- Breakpoint Management -->
	<div class="section">
		<h4>Breakpoints ({breakpoints.length})</h4>

		{#if breakpoints.length === 0}
			<p class="empty-message">No breakpoints set</p>
		{:else}
			<div class="breakpoints-list">
				{#each breakpoints as bp (bp.id)}
					<div class="breakpoint-item">
						<label class="breakpoint-checkbox">
							<input
								type="checkbox"
								checked={bp.enabled}
								on:change={() => handleToggleBreakpoint(bp.id)}
							/>
							<span class="breakpoint-description">
								{getBreakpointDescription(bp)}
								<span class="hit-count">(hit {bp.hitCount}x)</span>
							</span>
						</label>
						<button
							on:click={() => handleDeleteBreakpoint(bp.id)}
							class="delete-btn"
							title="Delete breakpoint"
						>
							✕
						</button>
					</div>
				{/each}
			</div>
		{/if}

		<div class="add-breakpoint">
			<h5>Add Breakpoint</h5>
			<select bind:value={breakpointType} class="breakpoint-type-select">
				<option value="CYCLE">At Cycle</option>
				<option value="PHASE">At Phase</option>
				<option value="VALUE_CHANGE">Value Change</option>
			</select>

			{#if breakpointType === 'CYCLE'}
				<input
					type="number"
					bind:value={cycleTarget}
					min="0"
					placeholder="Cycle number"
					class="breakpoint-input"
				/>
			{:else if breakpointType === 'PHASE'}
				<select bind:value={phaseTarget} class="breakpoint-input">
					<option value={MechanicalPhase.ADDITION}>Addition</option>
					<option value={MechanicalPhase.CARRY}>Carry</option>
					<option value={MechanicalPhase.TABLE}>Table</option>
					<option value={MechanicalPhase.OUTPUT}>Output</option>
				</select>
			{:else if breakpointType === 'VALUE_CHANGE'}
				<input
					type="text"
					bind:value={variableTarget}
					placeholder="Variable name"
					class="breakpoint-input"
				/>
			{/if}

			<button on:click={handleAddBreakpoint} class="add-bp-btn">
				+ Add Breakpoint
			</button>
		</div>
	</div>

	<!-- Variable Management -->
	<div class="section">
		<h4>Variables ({Object.keys(variables).length})</h4>

		{#if Object.keys(variables).length === 0}
			<p class="empty-message">No variables defined</p>
		{:else}
			<div class="variables-list">
				{#each Object.entries(variables) as [name, variable] (name)}
					<div class="variable-item">
						<div class="variable-info">
							<span class="variable-name">{name}</span>
							<span class="variable-value">{variable.currentValue}</span>
							<span class="variable-stats">
								R:{variable.readCount} W:{variable.writeCount}
							</span>
						</div>
					</div>
				{/each}
			</div>
		{/if}

		<div class="add-variable">
			<h5>Define Variable</h5>
			<div class="variable-inputs">
				<input
					type="text"
					bind:value={newVarName}
					placeholder="Variable name"
					class="var-name-input"
				/>
				<input
					type="number"
					bind:value={newVarValue}
					placeholder="Initial value"
					class="var-value-input"
				/>
			</div>
			<button on:click={handleDefineVariable} class="add-var-btn">
				+ Define Variable
			</button>
		</div>
	</div>

	<!-- State Snapshot -->
	{#if currentState}
		<div class="section">
			<h4>Current State</h4>
			<div class="state-snapshot">
				<div class="snapshot-item">
					<span class="label">Cycle</span>
					<span class="value">{currentState.cycle}</span>
				</div>
				<div class="snapshot-item">
					<span class="label">Phase</span>
					<span class="value">{currentState.phase}</span>
				</div>
				<div class="snapshot-item">
					<span class="label">Operations</span>
					<span class="value">{currentState.totalOperations}</span>
				</div>
			</div>
		</div>
	{/if}

	<!-- Messages -->
	{#if errorMessage}
		<div class="message error-message">
			<span class="icon">✕</span>
			{errorMessage}
		</div>
	{/if}

	{#if successMessage}
		<div class="message success-message">
			<span class="icon">✓</span>
			{successMessage}
		</div>
	{/if}
</div>

<style>
	.debugger-panel {
		background: #16213e;
		border: 1px solid #0f3460;
		border-radius: 8px;
		padding: 1.5rem;
		box-shadow: 0 4px 6px rgba(0, 0, 0, 0.2);
	}

	.panel-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 1.5rem;
	}

	.panel-header h3 {
		color: #667eea;
		margin: 0;
		font-size: 1.2rem;
	}

	.status {
		display: inline-block;
		padding: 0.4rem 0.8rem;
		border-radius: 4px;
		font-size: 0.85rem;
		font-weight: 600;
	}

	.status.running {
		background: rgba(34, 139, 34, 0.3);
		color: #b0f0c0;
		border: 1px solid #228b22;
	}

	.status.paused {
		background: rgba(184, 134, 11, 0.3);
		color: #f0d080;
		border: 1px solid #b8860b;
	}

	.section {
		margin-bottom: 1.5rem;
		padding-bottom: 1.5rem;
		border-bottom: 1px solid #0f3460;
	}

	.section:last-child {
		border-bottom: none;
		margin-bottom: 0;
		padding-bottom: 0;
	}

	.section h4 {
		color: #b0b8d4;
		margin: 0 0 1rem 0;
		font-size: 0.95rem;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		font-weight: 600;
	}

	.section h5 {
		color: #b0b8d4;
		margin: 1rem 0 0.75rem 0;
		font-size: 0.85rem;
		font-weight: 500;
	}

	.empty-message {
		color: #b0b8d4;
		font-size: 0.9rem;
		margin: 0;
		opacity: 0.8;
	}

	/* Execution Controls */
	.button-group {
		display: grid;
		grid-template-columns: repeat(2, 1fr);
		gap: 0.75rem;
	}

	button {
		padding: 0.75rem;
		border: none;
		border-radius: 4px;
		font-weight: 600;
		cursor: pointer;
		font-size: 0.9rem;
		transition: all 0.2s;
	}

	.step-btn {
		background: #4a7c59;
		color: #d0f0e0;
	}

	.step-btn:hover {
		background: #5e9d72;
	}

	.continue-btn {
		background: #667eea;
		color: white;
	}

	.continue-btn:hover {
		background: #7a8fee;
	}

	/* Breakpoints */
	.breakpoints-list {
		background: rgba(15, 52, 96, 0.3);
		border: 1px solid #533483;
		border-radius: 6px;
		padding: 0.75rem;
		margin-bottom: 1rem;
	}

	.breakpoint-item {
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 0.75rem;
		border-bottom: 1px solid #0f3460;
	}

	.breakpoint-item:last-child {
		border-bottom: none;
	}

	.breakpoint-checkbox {
		display: flex;
		align-items: center;
		gap: 0.75rem;
		cursor: pointer;
		flex: 1;
	}

	.breakpoint-checkbox input[type='checkbox'] {
		cursor: pointer;
	}

	.breakpoint-description {
		color: #b0b8d4;
		font-size: 0.9rem;
	}

	.hit-count {
		color: #667eea;
		margin-left: 0.5rem;
		font-size: 0.85rem;
	}

	.delete-btn {
		background: #8b3a3a;
		color: #e0e0e0;
		padding: 0.4rem 0.6rem;
		font-size: 0.85rem;
	}

	.delete-btn:hover {
		background: #a84e4e;
	}

	.add-breakpoint {
		background: rgba(15, 52, 96, 0.3);
		border: 1px dashed #533483;
		border-radius: 6px;
		padding: 1rem;
	}

	.breakpoint-type-select,
	.breakpoint-input {
		width: 100%;
		padding: 0.5rem;
		background: #0f3460;
		border: 1px solid #533483;
		border-radius: 4px;
		color: #e0e0e0;
		margin-bottom: 0.75rem;
		font-size: 0.9rem;
	}

	.breakpoint-type-select:focus,
	.breakpoint-input:focus {
		outline: none;
		border-color: #667eea;
		box-shadow: 0 0 4px rgba(102, 126, 234, 0.3);
	}

	.add-bp-btn {
		width: 100%;
		background: #2d5a3d;
		color: #b0e0c0;
		border: 1px solid #4a8a5e;
	}

	.add-bp-btn:hover {
		background: #3d7a4d;
	}

	/* Variables */
	.variables-list {
		background: rgba(15, 52, 96, 0.3);
		border: 1px solid #533483;
		border-radius: 6px;
		padding: 0.75rem;
		margin-bottom: 1rem;
	}

	.variable-item {
		padding: 0.75rem;
		border-bottom: 1px solid #0f3460;
	}

	.variable-item:last-child {
		border-bottom: none;
	}

	.variable-info {
		display: flex;
		gap: 1rem;
		align-items: center;
		flex-wrap: wrap;
	}

	.variable-name {
		color: #667eea;
		font-weight: 600;
		min-width: 80px;
	}

	.variable-value {
		color: #b0e0c0;
		font-family: 'Courier New', monospace;
		font-weight: 600;
	}

	.variable-stats {
		color: #b0b8d4;
		font-size: 0.85rem;
		margin-left: auto;
	}

	.add-variable {
		background: rgba(15, 52, 96, 0.3);
		border: 1px dashed #533483;
		border-radius: 6px;
		padding: 1rem;
	}

	.variable-inputs {
		display: flex;
		gap: 0.75rem;
		margin-bottom: 0.75rem;
	}

	.var-name-input,
	.var-value-input {
		flex: 1;
		padding: 0.5rem;
		background: #0f3460;
		border: 1px solid #533483;
		border-radius: 4px;
		color: #e0e0e0;
		font-size: 0.9rem;
	}

	.var-name-input:focus,
	.var-value-input:focus {
		outline: none;
		border-color: #667eea;
		box-shadow: 0 0 4px rgba(102, 126, 234, 0.3);
	}

	.add-var-btn {
		width: 100%;
		background: #2d5a3d;
		color: #b0e0c0;
		border: 1px solid #4a8a5e;
	}

	.add-var-btn:hover {
		background: #3d7a4d;
	}

	/* State Snapshot */
	.state-snapshot {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(120px, 1fr));
		gap: 0.75rem;
	}

	.snapshot-item {
		background: rgba(15, 52, 96, 0.5);
		border: 1px solid #533483;
		border-radius: 4px;
		padding: 0.75rem;
		display: flex;
		justify-content: space-between;
		align-items: center;
		gap: 0.5rem;
	}

	.snapshot-item .label {
		color: #b0b8d4;
		font-size: 0.85rem;
		font-weight: 500;
	}

	.snapshot-item .value {
		color: #667eea;
		font-weight: 600;
		font-family: 'Courier New', monospace;
	}

	/* Messages */
	.message {
		padding: 1rem;
		border-radius: 4px;
		display: flex;
		gap: 0.75rem;
		align-items: flex-start;
		margin-top: 1rem;
	}

	.message .icon {
		font-weight: bold;
		flex-shrink: 0;
	}

	.success-message {
		background: rgba(45, 90, 61, 0.3);
		border-left: 4px solid #4a8a5e;
		color: #b0e0c0;
	}

	.error-message {
		background: rgba(139, 58, 58, 0.3);
		border-left: 4px solid #a84e4e;
		color: #e0b0b0;
	}

	@media (max-width: 600px) {
		.button-group {
			grid-template-columns: 1fr;
		}

		.variable-inputs {
			flex-direction: column;
		}

		.state-snapshot {
			grid-template-columns: 1fr;
		}
	}
</style>
