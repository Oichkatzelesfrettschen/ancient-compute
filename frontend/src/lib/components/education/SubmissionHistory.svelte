<script lang="ts">
/**
 * SubmissionHistory.svelte - Past submission viewer
 *
 * Features:
 * - List of past submissions
 * - Timestamp, status, score
 * - View submission code
 * - View test results
 * - Compare submissions
 * - Best submission indicator
 */

import type { ExerciseSubmission } from '../../stores/educationStore';

export let submissions: ExerciseSubmission[] = [];
export let onViewSubmission: (submissionId: string) => void = () => {};
export let onLoadSubmission: (submissionId: string) => void = () => {};

$: bestSubmission = submissions.reduce((best, current) =>
	current.score > (best?.score || 0) ? current : best, submissions[0]);

function formatDate(isoString: string): string {
	const date = new Date(isoString);
	const now = new Date();
	const diff = now.getTime() - date.getTime();
	const minutes = Math.floor(diff / 60000);
	const hours = Math.floor(diff / 3600000);
	const days = Math.floor(diff / 86400000);

	if (minutes < 1) return 'Just now';
	if (minutes < 60) return `${minutes}m ago`;
	if (hours < 24) return `${hours}h ago`;
	if (days < 7) return `${days}d ago`;
	return date.toLocaleDateString();
}

function getStatusBadge(status: ExerciseSubmission['status']): { text: string; color: string } {
	switch (status) {
		case 'passed': return { text: 'Passed', color: '#7cb342' };
		case 'failed': return { text: 'Failed', color: '#e53935' };
		case 'running': return { text: 'Running', color: '#2196f3' };
		default: return { text: 'Pending', color: '#ff9800' };
	}
}

function getScoreColor(score: number): string {
	if (score >= 90) return '#7cb342';
	if (score >= 70) return '#ff9800';
	return '#e53935';
}
</script>

<div class="submission-history">
	<div class="history-header">
		<h3 class="history-title">Submission History</h3>
		<span class="submission-count">{submissions.length} submission{submissions.length !== 1 ? 's' : ''}</span>
	</div>

	{#if submissions.length > 0}
		<div class="submissions-list">
			{#each submissions as submission}
				{@const badge = getStatusBadge(submission.status)}
				<div class="submission-item" class:best={submission.id === bestSubmission?.id}>
					{#if submission.id === bestSubmission?.id}
						<div class="best-badge">★ Best</div>
					{/if}

					<div class="submission-header">
						<div class="submission-info">
							<span class="submission-date">{formatDate(submission.submittedAt)}</span>
							<span class="submission-language">{submission.language}</span>
						</div>
						<div class="submission-badges">
							<span class="status-badge" style="background-color: {badge.color}">
								{badge.text}
							</span>
							{#if submission.status === 'passed' || submission.status === 'failed'}
								<span class="score-badge" style="color: {getScoreColor(submission.score)}">
									{submission.score}%
								</span>
							{/if}
						</div>
					</div>

					<div class="submission-stats">
						<div class="stat">
							<span class="stat-label">Tests</span>
							<span class="stat-value">
								{submission.testResults.filter(t => t.status === 'passed').length}/{submission.testResults.length}
							</span>
						</div>
						{#if submission.executionTime}
							<div class="stat">
								<span class="stat-label">Time</span>
								<span class="stat-value">{submission.executionTime}ms</span>
							</div>
						{/if}
						{#if submission.memoryUsed}
							<div class="stat">
								<span class="stat-label">Memory</span>
								<span class="stat-value">{submission.memoryUsed.toFixed(1)}MB</span>
							</div>
						{/if}
					</div>

					<div class="submission-actions">
						<button class="action-btn view" on:click={() => onViewSubmission(submission.id)}>
							View Details
						</button>
						<button class="action-btn load" on:click={() => onLoadSubmission(submission.id)}>
							Load Code
						</button>
					</div>
				</div>
			{/each}
		</div>
	{:else}
		<div class="empty-state">
			<span class="empty-icon">📝</span>
			<p class="empty-text">No submissions yet. Submit your solution to see your history here.</p>
		</div>
	{/if}
</div>

<style>
	.submission-history {
		display: flex;
		flex-direction: column;
		gap: 16px;
	}

	.history-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding-bottom: 12px;
		border-bottom: 2px solid #e0e0e0;
	}

	.history-title {
		margin: 0;
		font-size: 18px;
		font-weight: 700;
		color: #2c3e50;
	}

	.submission-count {
		font-size: 13px;
		color: #999;
	}

	.submissions-list {
		display: flex;
		flex-direction: column;
		gap: 12px;
	}

	.submission-item {
		position: relative;
		display: flex;
		flex-direction: column;
		gap: 12px;
		padding: 16px;
		background: white;
		border: 2px solid #e0e0e0;
		border-radius: 8px;
		transition: all 0.2s ease;
	}

	.submission-item:hover {
		border-color: #4a90e2;
		box-shadow: 0 2px 8px rgba(74, 144, 226, 0.1);
	}

	.submission-item.best {
		border-color: #ffd700;
		background: linear-gradient(to right, #fffbf0 0%, white 100%);
	}

	.best-badge {
		position: absolute;
		top: -10px;
		right: 20px;
		padding: 4px 12px;
		background: #ffd700;
		border-radius: 12px;
		font-size: 11px;
		font-weight: 700;
		text-transform: uppercase;
		color: #2c3e50;
	}

	.submission-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		gap: 12px;
	}

	.submission-info {
		display: flex;
		align-items: center;
		gap: 12px;
	}

	.submission-date {
		font-size: 14px;
		font-weight: 600;
		color: #2c3e50;
	}

	.submission-language {
		padding: 4px 10px;
		background: #e0e0e0;
		border-radius: 4px;
		font-size: 11px;
		font-weight: 700;
		text-transform: uppercase;
		color: #666;
	}

	.submission-badges {
		display: flex;
		align-items: center;
		gap: 8px;
	}

	.status-badge {
		padding: 4px 10px;
		border-radius: 4px;
		font-size: 11px;
		font-weight: 700;
		text-transform: uppercase;
		color: white;
	}

	.score-badge {
		font-size: 18px;
		font-weight: 700;
	}

	.submission-stats {
		display: flex;
		gap: 20px;
		padding: 12px;
		background: #f8f8f8;
		border-radius: 6px;
	}

	.stat {
		display: flex;
		flex-direction: column;
		gap: 4px;
	}

	.stat-label {
		font-size: 11px;
		color: #999;
		text-transform: uppercase;
		letter-spacing: 0.3px;
	}

	.stat-value {
		font-size: 14px;
		font-weight: 700;
		color: #2c3e50;
	}

	.submission-actions {
		display: flex;
		gap: 8px;
	}

	.action-btn {
		flex: 1;
		padding: 8px 16px;
		border: 2px solid #e0e0e0;
		border-radius: 6px;
		cursor: pointer;
		font-size: 13px;
		font-weight: 600;
		transition: all 0.2s ease;
	}

	.action-btn.view {
		background: white;
		color: #4a90e2;
		border-color: #4a90e2;
	}

	.action-btn.view:hover {
		background: #4a90e2;
		color: white;
	}

	.action-btn.load {
		background: white;
		color: #666;
	}

	.action-btn.load:hover {
		background: #f5f5f5;
		border-color: #999;
	}

	.empty-state {
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		gap: 12px;
		padding: 40px 20px;
		text-align: center;
	}

	.empty-icon {
		font-size: 48px;
		opacity: 0.3;
	}

	.empty-text {
		margin: 0;
		font-size: 14px;
		color: #999;
		max-width: 300px;
	}

	@media (max-width: 768px) {
		.submission-header {
			flex-direction: column;
			align-items: flex-start;
		}

		.submission-stats {
			flex-wrap: wrap;
		}

		.submission-actions {
			flex-direction: column;
		}
	}
</style>
