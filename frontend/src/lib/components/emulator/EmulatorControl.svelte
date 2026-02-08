<script lang="ts">
	import type { ExecutionResult } from '$lib/api/types';
	import { executePolynomial, resetEmulator, stepCycle } from '$lib/api/emulator';

	// Component state
	let coefficients: number[] = [1, 1];
	let xStart: number = 1;
	let xEnd: number = 5;
	let executionSpeed: number = 1.0;
	let isRunning: boolean = false;
	let isPaused: boolean = false;
	let results: ExecutionResult[] = [];
	let errorMessage: string = '';
	let successMessage: string = '';

	// Polynomial degree management
	let maxDegree: number = 5;

	/**
	 * Add a new coefficient to the polynomial
	 */
	function addCoefficient() {
		if (coefficients.length < maxDegree + 1) {
			coefficients = [...coefficients, 0];
		}
	}

	/**
	 * Remove the last coefficient
	 */
	function removeCoefficient() {
		if (coefficients.length > 1) {
			coefficients = coefficients.slice(0, -1);
		}
	}

	/**
	 * Update a specific coefficient value
	 */
	function updateCoefficient(index: number, value: number) {
		coefficients[index] = value;
		coefficients = coefficients; // Trigger reactivity
	}

	/**
	 * Execute polynomial evaluation
	 */
	async function handleExecute() {
		errorMessage = '';
		successMessage = '';
		isRunning = true;

		try {
			// Validate inputs
			if (xStart < 0 || xEnd < 0) {
				throw new Error('X range values must be non-negative');
			}
			if (xStart > xEnd) {
				throw new Error('X start must be less than or equal to X end');
			}
			if (coefficients.some((c) => isNaN(c))) {
				throw new Error('All coefficients must be valid numbers');
			}

			// Execute polynomial evaluation
			const response = await executePolynomial(coefficients, [xStart, xEnd], executionSpeed);

			if (response.success) {
				results = response.results || [];
				successMessage = `Polynomial evaluation complete. ${results.length} values computed.`;
			} else {
				errorMessage = response.error || 'Execution failed';
			}
		} catch (error) {
			errorMessage = error instanceof Error ? error.message : 'Unknown error occurred';
		} finally {
			isRunning = false;
			isPaused = false;
		}
	}

	/**
	 * Reset the emulator to initial state
	 */
	async function handleReset() {
		errorMessage = '';
		successMessage = '';
		isRunning = false;
		isPaused = false;
		results = [];

		try {
			const response = await resetEmulator();
			if (response.success) {
				successMessage = 'Emulator reset successfully';
			} else {
				errorMessage = response.error || 'Reset failed';
			}
		} catch (error) {
			errorMessage = error instanceof Error ? error.message : 'Failed to reset emulator';
		}
	}

	/**
	 * Step through a single mechanical cycle
	 */
	async function handleStep() {
		errorMessage = '';

		try {
			const response = await stepCycle();
			if (response.success) {
				results = response.results || [];
			} else {
				errorMessage = response.error || 'Step failed';
			}
		} catch (error) {
			errorMessage = error instanceof Error ? error.message : 'Step operation failed';
		}
	}

	/**
	 * Clear all state and start fresh
	 */
	function handleClear() {
		coefficients = [1, 1];
		xStart = 1;
		xEnd = 5;
		executionSpeed = 1.0;
		results = [];
		errorMessage = '';
		successMessage = '';
		isRunning = false;
		isPaused = false;
	}

	/**
	 * Format a polynomial expression for display
	 */
	function getPolynomialExpression(): string {
		const terms: string[] = [];
		for (let i = coefficients.length - 1; i >= 0; i--) {
			if (coefficients[i] === 0 && i !== 0) continue;
			if (i === 0) {
				terms.push(`${coefficients[i]}`);
			} else if (i === 1) {
				terms.push(`${coefficients[i]}x`);
			} else {
				terms.push(`${coefficients[i]}x^${i}`);
			}
		}
		return terms.length > 0 ? terms.join(' + ').replace(/\+ -/g, '- ') : '0';
	}
</script>

<div class="emulator-control">
	<div class="control-panel">
		<h2>Difference Engine No. 2 Emulator</h2>

		<form on:submit|preventDefault={handleExecute}>
			<!-- Polynomial Coefficients Section -->
			<div class="section">
				<h3>Polynomial Definition</h3>
				<p class="polynomial-display">f(x) = {getPolynomialExpression()}</p>

				<div class="coefficients-input">
					<label>Coefficients (a₀, a₁, a₂, ..., aₙ)</label>
					<div class="coefficients-grid">
						{#each coefficients as coeff, index (index)}
							<div class="coefficient-input">
								<label>a<sub>{index}</sub></label>
								<input
									type="number"
									bind:value={coefficients[index]}
									on:change={() => updateCoefficient(index, coefficients[index])}
									disabled={isRunning}
								/>
								{#if index === coefficients.length - 1 && coefficients.length > 1}
									<button
										type="button"
										on:click={removeCoefficient}
										disabled={isRunning}
										class="remove-btn"
										title="Remove this coefficient"
									>
										−
									</button>
								{/if}
							</div>
						{/each}
					</div>

					{#if coefficients.length < maxDegree + 1}
						<button
							type="button"
							on:click={addCoefficient}
							disabled={isRunning}
							class="add-coefficient-btn"
						>
							+ Add Coefficient
						</button>
					{/if}
				</div>
			</div>

			<!-- X Range Section -->
			<div class="section">
				<h3>X Range</h3>
				<div class="range-inputs">
					<div class="range-input">
						<label for="x-start">Start (x₀)</label>
						<input
							id="x-start"
							type="number"
							bind:value={xStart}
							disabled={isRunning}
							min="0"
						/>
					</div>
					<div class="range-input">
						<label for="x-end">End (xₙ)</label>
						<input id="x-end" type="number" bind:value={xEnd} disabled={isRunning} min="0" />
					</div>
				</div>
			</div>

			<!-- Execution Speed Section -->
			<div class="section">
				<h3>Execution Control</h3>
				<div class="speed-control">
					<label for="speed">Speed (cycles/second)</label>
					<div class="speed-slider-container">
						<input
							id="speed"
							type="range"
							bind:value={executionSpeed}
							min="0.25"
							max="10"
							step="0.25"
							disabled={isRunning}
						/>
						<span class="speed-value">{executionSpeed.toFixed(2)}x</span>
					</div>
				</div>
			</div>

			<!-- Control Buttons -->
			<div class="button-group">
				<button type="submit" disabled={isRunning} class="execute-btn">
					{isRunning ? 'Executing...' : 'Execute Polynomial'}
				</button>
				<button type="button" on:click={handleStep} disabled={isRunning} class="step-btn">
					Step Cycle
				</button>
				<button type="button" on:click={handleReset} disabled={isRunning} class="reset-btn">
					Reset
				</button>
				<button type="button" on:click={handleClear} class="clear-btn">
					Clear Form
				</button>
			</div>
		</form>

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
</div>

<style>
	.emulator-control {
		width: 100%;
		max-width: 800px;
		margin: 0 auto;
	}

	.control-panel {
		background: #16213e;
		border: 1px solid #0f3460;
		border-radius: 8px;
		padding: 2rem;
		box-shadow: 0 4px 6px rgba(0, 0, 0, 0.2);
	}

	.control-panel h2 {
		color: #667eea;
		margin: 0 0 1.5rem 0;
		font-size: 1.5rem;
		text-align: center;
	}

	form {
		display: flex;
		flex-direction: column;
		gap: 1.5rem;
	}

	.section {
		border-bottom: 1px solid #0f3460;
		padding-bottom: 1.5rem;
	}

	.section:last-of-type {
		border-bottom: none;
	}

	.section h3 {
		color: #e0e0e0;
		margin: 0 0 1rem 0;
		font-size: 1.1rem;
		font-weight: 600;
	}

	.polynomial-display {
		background: rgba(102, 126, 234, 0.1);
		border-left: 3px solid #667eea;
		padding: 1rem;
		border-radius: 4px;
		margin: 0 0 1rem 0;
		color: #b0b8d4;
		font-family: 'Courier New', monospace;
		font-size: 1rem;
	}

	.coefficients-input label {
		display: block;
		color: #b0b8d4;
		font-size: 0.9rem;
		margin-bottom: 0.75rem;
		font-weight: 500;
	}

	.coefficients-grid {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(80px, 1fr));
		gap: 1rem;
		margin-bottom: 1rem;
	}

	.coefficient-input {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}

	.coefficient-input label {
		font-size: 0.85rem;
		margin-bottom: 0;
	}

	.coefficient-input input {
		padding: 0.5rem;
		background: #0f3460;
		border: 1px solid #533483;
		border-radius: 4px;
		color: #e0e0e0;
		font-size: 0.95rem;
	}

	.coefficient-input input:focus {
		outline: none;
		border-color: #667eea;
		box-shadow: 0 0 4px rgba(102, 126, 234, 0.3);
	}

	.coefficient-input input:disabled {
		opacity: 0.6;
		cursor: not-allowed;
	}

	.remove-btn {
		padding: 0.5rem;
		background: #8b3a3a;
		border: 1px solid #a84e4e;
		border-radius: 4px;
		color: #e0e0e0;
		cursor: pointer;
		font-size: 1.2rem;
		transition: all 0.2s;
	}

	.remove-btn:hover {
		background: #a84e4e;
	}

	.remove-btn:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}

	.add-coefficient-btn {
		background: #2d5a3d;
		color: #b0e0c0;
		border: 1px solid #4a8a5e;
		padding: 0.5rem 1rem;
		border-radius: 4px;
		cursor: pointer;
		font-size: 0.9rem;
		transition: all 0.2s;
		width: 100%;
	}

	.add-coefficient-btn:hover {
		background: #3d7a4d;
	}

	.add-coefficient-btn:disabled {
		opacity: 0.5;
		cursor: not-allowed;
	}

	.range-inputs {
		display: grid;
		grid-template-columns: 1fr 1fr;
		gap: 1rem;
	}

	.range-input {
		display: flex;
		flex-direction: column;
		gap: 0.5rem;
	}

	.range-input label {
		color: #b0b8d4;
		font-size: 0.9rem;
		font-weight: 500;
	}

	.range-input input {
		padding: 0.75rem;
		background: #0f3460;
		border: 1px solid #533483;
		border-radius: 4px;
		color: #e0e0e0;
		font-size: 0.95rem;
	}

	.range-input input:focus {
		outline: none;
		border-color: #667eea;
		box-shadow: 0 0 4px rgba(102, 126, 234, 0.3);
	}

	.range-input input:disabled {
		opacity: 0.6;
		cursor: not-allowed;
	}

	.speed-control {
		display: flex;
		flex-direction: column;
		gap: 0.75rem;
	}

	.speed-control > label {
		color: #b0b8d4;
		font-size: 0.9rem;
		font-weight: 500;
	}

	.speed-slider-container {
		display: flex;
		gap: 1rem;
		align-items: center;
	}

	.speed-slider-container input[type='range'] {
		flex: 1;
	}

	.speed-value {
		color: #667eea;
		font-weight: 600;
		min-width: 50px;
		text-align: right;
	}

	.button-group {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(120px, 1fr));
		gap: 0.75rem;
		margin-top: 1rem;
	}

	button {
		padding: 0.75rem 1.25rem;
		border: none;
		border-radius: 4px;
		font-weight: 600;
		cursor: pointer;
		transition: all 0.2s;
		font-size: 0.9rem;
	}

	.execute-btn {
		background: #667eea;
		color: white;
		grid-column: 1 / -1;
	}

	.execute-btn:hover:not(:disabled) {
		background: #7a8fee;
		box-shadow: 0 4px 12px rgba(102, 126, 234, 0.3);
	}

	.execute-btn:disabled {
		opacity: 0.7;
		cursor: not-allowed;
	}

	.step-btn {
		background: #4a7c59;
		color: #d0f0e0;
	}

	.step-btn:hover:not(:disabled) {
		background: #5e9d72;
	}

	.step-btn:disabled {
		opacity: 0.6;
		cursor: not-allowed;
	}

	.reset-btn {
		background: #7c5a4a;
		color: #f0e0d0;
	}

	.reset-btn:hover:not(:disabled) {
		background: #9d7e5e;
	}

	.reset-btn:disabled {
		opacity: 0.6;
		cursor: not-allowed;
	}

	.clear-btn {
		background: #5a5a5a;
		color: #d0d0d0;
	}

	.clear-btn:hover {
		background: #707070;
	}

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

	sub {
		font-size: 0.85em;
		vertical-align: sub;
	}

	@media (max-width: 600px) {
		.control-panel {
			padding: 1.5rem;
		}

		.coefficients-grid {
			grid-template-columns: repeat(auto-fit, minmax(70px, 1fr));
		}

		.range-inputs {
			grid-template-columns: 1fr;
		}

		.button-group {
			grid-template-columns: 1fr;
		}

		.execute-btn {
			grid-column: auto;
		}
	}
</style>
