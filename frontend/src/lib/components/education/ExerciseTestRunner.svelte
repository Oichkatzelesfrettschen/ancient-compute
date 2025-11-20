<script lang="ts">
/**
 * ExerciseTestRunner.svelte - Test case execution and results display
 *
 * Features:
 * - Test case list with status
 * - Real-time test execution
 * - Diff view for failed tests
 * - Performance metrics
 * - Retry failed tests
 */

import type { TestResult } from '../../stores/educationStore';
import type { TestCase } from '../../stores/timelineStore';

export let testCases: TestCase[] = [];
export let testResults: TestResult[] = [];
export let isRunning = false;
export let onRunTests: () => void = () => {};
export let onRetryFailed: () => void = () => {};

$: totalTests = testCases.length;
$: passedTests = testResults.filter(r => r.status === 'passed').length;
$: failedTests = testResults.filter(r => r.status === 'failed').length;
$: pendingTests = testResults.filter(r => r.status === 'pending').length;
$: runningTests = testResults.filter(r => r.status === 'running').length;

function getStatusIcon(status: TestResult['status']): string {
	switch (status) {
		case 'passed': return '✓';
		case 'failed': return '✗';
		case 'running': return '⋯';
		default: return '○';
	}
}

function getStatusColor(status: TestResult['status']): string {
	switch (status) {
		case 'passed': return '#7cb342';
		case 'failed': return '#e53935';
		case 'running': return '#2196f3';
		default: return '#999';
	}
}

function formatTime(ms: number): string {
	if (ms < 1000) return `${ms.toFixed(0)}ms`;
	return `${(ms / 1000).toFixed(2)}s`;
}

function getTestResult(testCaseId: string): TestResult | null {
	return testResults.find(r => r.testCaseId === testCaseId) || null;
}

function getDiff(expected: string, actual: string): { expected: string[]; actual: string[] } {
	const expLines = expected.split('\n');
	const actLines = actual.split('\n');
	return { expected: expLines, actual: actLines };
}
</script>

<div class="test-runner">
	<!-- Test summary -->
	<div class="test-summary">
		<div class="summary-header">
			<h3 class="summary-title">Test Results</h3>
			<div class="summary-actions">
				{#if failedTests > 0 && !isRunning}
					<button class="retry-btn" on:click={onRetryFailed}>
						Retry Failed ({failedTests})
					</button>
				{/if}
				<button class="run-all-btn" on:click={onRunTests} disabled={isRunning}>
					{isRunning ? 'Running...' : 'Run All Tests'}
				</button>
			</div>
		</div>

		<div class="summary-stats">
			<div class="stat passed">
				<span class="stat-icon">✓</span>
				<span class="stat-count">{passedTests}</span>
				<span class="stat-label">Passed</span>
			</div>
			<div class="stat failed">
				<span class="stat-icon">✗</span>
				<span class="stat-count">{failedTests}</span>
				<span class="stat-label">Failed</span>
			</div>
			<div class="stat total">
				<span class="stat-icon">○</span>
				<span class="stat-count">{totalTests}</span>
				<span class="stat-label">Total</span>
			</div>
		</div>
	</div>

	<!-- Test cases -->
	<div class="test-cases">
		{#each testCases as testCase}
			{@const result = getTestResult(testCase.id)}
			<div class="test-case" class:passed={result?.status === 'passed'} class:failed={result?.status === 'failed'}>
				<div class="test-header">
					<div class="test-info">
						<span class="test-icon" style="color: {result ? getStatusColor(result.status) : '#999'}">
							{result ? getStatusIcon(result.status) : '○'}
						</span>
						<span class="test-name">{testCase.name}</span>
					</div>
					{#if result?.executionTime}
						<span class="test-time">{formatTime(result.executionTime)}</span>
					{/if}
				</div>

				{#if testCase.explanation}
					<p class="test-explanation">{testCase.explanation}</p>
				{/if}

				<!-- Test input/output -->
				<div class="test-details">
					<div class="detail-section">
						<h4 class="detail-title">Input</h4>
						<pre class="detail-content">{testCase.input || '(none)'}</pre>
					</div>
					<div class="detail-section">
						<h4 class="detail-title">Expected Output</h4>
						<pre class="detail-content">{testCase.expectedOutput}</pre>
					</div>
					{#if result?.actualOutput}
						<div class="detail-section">
							<h4 class="detail-title">Actual Output</h4>
							<pre class="detail-content" class:error={result.status === 'failed'}>{result.actualOutput}</pre>
						</div>
					{/if}
				</div>

				<!-- Error message -->
				{#if result?.error}
					<div class="error-message">
						<h4 class="error-title">Error</h4>
						<pre class="error-content">{result.error}</pre>
					</div>
				{/if}

				<!-- Diff view for failed tests -->
				{#if result?.status === 'failed' && result.actualOutput}
					{@const diff = getDiff(testCase.expectedOutput, result.actualOutput)}
					<details class="diff-viewer">
						<summary>View Diff</summary>
						<div class="diff-content">
							<div class="diff-column">
								<h5>Expected</h5>
								{#each diff.expected as line, i}
									<div class="diff-line expected">{line}</div>
								{/each}
							</div>
							<div class="diff-column">
								<h5>Actual</h5>
								{#each diff.actual as line, i}
									<div class="diff-line actual">{line}</div>
								{/each}
							</div>
						</div>
					</details>
				{/if}
			</div>
		{/each}
	</div>
</div>

<style>
	.test-runner {
		display: flex;
		flex-direction: column;
		gap: 20px;
	}

	.test-summary {
		display: flex;
		flex-direction: column;
		gap: 16px;
		padding: 20px;
		background: white;
		border: 2px solid #e0e0e0;
		border-radius: 8px;
	}

	.summary-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		gap: 16px;
	}

	.summary-title {
		margin: 0;
		font-size: 18px;
		font-weight: 700;
		color: #2c3e50;
	}

	.summary-actions {
		display: flex;
		gap: 8px;
	}

	.retry-btn,
	.run-all-btn {
		padding: 8px 16px;
		border: none;
		border-radius: 6px;
		cursor: pointer;
		font-size: 13px;
		font-weight: 600;
		transition: all 0.2s ease;
	}

	.retry-btn {
		background: #ff9800;
		color: white;
	}

	.retry-btn:hover {
		background: #f57c00;
	}

	.run-all-btn {
		background: #4a90e2;
		color: white;
	}

	.run-all-btn:hover:not(:disabled) {
		background: #357abd;
	}

	.run-all-btn:disabled {
		opacity: 0.6;
		cursor: not-allowed;
	}

	.summary-stats {
		display: flex;
		gap: 16px;
	}

	.stat {
		display: flex;
		flex-direction: column;
		align-items: center;
		gap: 4px;
		padding: 12px 20px;
		background: #f5f5f5;
		border-radius: 6px;
		flex: 1;
	}

	.stat.passed {
		background: #f0f8f4;
		color: #7cb342;
	}

	.stat.failed {
		background: #ffebee;
		color: #e53935;
	}

	.stat-icon {
		font-size: 24px;
	}

	.stat-count {
		font-size: 24px;
		font-weight: 700;
	}

	.stat-label {
		font-size: 12px;
		text-transform: uppercase;
		letter-spacing: 0.5px;
	}

	.test-cases {
		display: flex;
		flex-direction: column;
		gap: 12px;
	}

	.test-case {
		border: 2px solid #e0e0e0;
		border-radius: 8px;
		overflow: hidden;
	}

	.test-case.passed {
		border-color: #7cb342;
		background: #f0f8f4;
	}

	.test-case.failed {
		border-color: #e53935;
		background: #ffebee;
	}

	.test-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 12px 16px;
		background: white;
	}

	.test-info {
		display: flex;
		align-items: center;
		gap: 10px;
	}

	.test-icon {
		font-size: 18px;
		font-weight: 700;
	}

	.test-name {
		font-size: 14px;
		font-weight: 600;
		color: #2c3e50;
	}

	.test-time {
		font-size: 12px;
		color: #999;
		font-weight: 600;
	}

	.test-explanation {
		margin: 0;
		padding: 12px 16px;
		font-size: 13px;
		line-height: 1.5;
		color: #666;
		background: rgba(255, 255, 255, 0.5);
	}

	.test-details {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
		gap: 12px;
		padding: 16px;
	}

	.detail-section {
		display: flex;
		flex-direction: column;
		gap: 6px;
	}

	.detail-title {
		margin: 0;
		font-size: 11px;
		font-weight: 700;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		color: #999;
	}

	.detail-content {
		margin: 0;
		padding: 10px;
		background: white;
		border: 1px solid #e0e0e0;
		border-radius: 4px;
		font-family: 'Courier New', monospace;
		font-size: 12px;
		line-height: 1.4;
		color: #2c3e50;
		overflow-x: auto;
	}

	.detail-content.error {
		border-color: #e53935;
		background: #ffebee;
	}

	.error-message {
		padding: 16px;
		background: #ffebee;
	}

	.error-title {
		margin: 0 0 8px 0;
		font-size: 12px;
		font-weight: 700;
		text-transform: uppercase;
		color: #e53935;
	}

	.error-content {
		margin: 0;
		padding: 10px;
		background: white;
		border: 1px solid #e53935;
		border-radius: 4px;
		font-family: 'Courier New', monospace;
		font-size: 12px;
		color: #e53935;
		overflow-x: auto;
	}

	.diff-viewer {
		padding: 16px;
		background: white;
	}

	.diff-viewer summary {
		cursor: pointer;
		font-weight: 600;
		font-size: 13px;
		color: #4a90e2;
		margin-bottom: 12px;
	}

	.diff-content {
		display: grid;
		grid-template-columns: 1fr 1fr;
		gap: 12px;
	}

	.diff-column h5 {
		margin: 0 0 8px 0;
		font-size: 12px;
		text-transform: uppercase;
		color: #999;
	}

	.diff-line {
		padding: 4px 8px;
		font-family: 'Courier New', monospace;
		font-size: 12px;
		line-height: 1.4;
	}

	.diff-line.expected {
		background: #f0f8f4;
		border-left: 3px solid #7cb342;
	}

	.diff-line.actual {
		background: #ffebee;
		border-left: 3px solid #e53935;
	}

	@media (max-width: 768px) {
		.summary-header {
			flex-direction: column;
			align-items: flex-start;
		}

		.summary-stats {
			flex-direction: column;
		}

		.test-details {
			grid-template-columns: 1fr;
		}

		.diff-content {
			grid-template-columns: 1fr;
		}
	}
</style>
