<script lang="ts">
/**
 * ModuleCard.svelte - Individual module card component
 *
 * Features:
 * - Displays module information with visual hierarchy
 * - Progress indicator
 * - Status badges (locked, available, in progress, completed)
 * - Hover effects and animations
 * - Click to navigate to module detail
 */

import { goto } from '$app/navigation';
import type { Module } from '../../stores/timelineStore';
import type { ModuleProgress } from '../../stores/educationStore';

export let module: Module;
export let progress: ModuleProgress | null = null;
export let locked = false;

$: progressPercentage = progress
	? Math.round(((progress.lessonsCompleted + progress.exercisesCompleted) /
	   (progress.lessonsTotal + progress.exercisesTotal)) * 100) || 0
	: 0;

$: status = locked ? 'locked' : progress?.status || 'not_started';

function handleClick(): void {
	if (!locked) {
		goto(`/modules/${module.id}`);
	}
}

function getDifficultyLevel(): string {
	if (module.estimatedTime < 2) return 'Beginner';
	if (module.estimatedTime < 5) return 'Intermediate';
	return 'Advanced';
}

function getDifficultyColor(): string {
	if (module.estimatedTime < 2) return '#7cb342';
	if (module.estimatedTime < 5) return '#ff9800';
	return '#e53935';
}

function getStatusBadge(): { text: string; color: string } {
	switch (status) {
		case 'locked':
			return { text: 'Locked', color: '#999' };
		case 'not_started':
			return { text: 'Not Started', color: '#ff9800' };
		case 'in_progress':
			return { text: 'In Progress', color: '#2196f3' };
		case 'completed':
			return { text: 'Completed', color: '#7cb342' };
		default:
			return { text: 'Available', color: '#666' };
	}
}

function formatTime(hours: number): string {
	if (hours < 1) return `${Math.round(hours * 60)}m`;
	return `${hours}h`;
}
</script>

<button
	class="module-card"
	class:locked
	class:in-progress={status === 'in_progress'}
	class:completed={status === 'completed'}
	on:click={handleClick}
	disabled={locked}
	aria-label={`${module.title} - ${getStatusBadge().text}`}
>
	<!-- Card header with status badge -->
	<div class="card-header">
		<div class="status-badge" style="background-color: {getStatusBadge().color}">
			{getStatusBadge().text}
		</div>
		{#if status === 'completed'}
			<div class="completed-icon">✓</div>
		{/if}
	</div>

	<!-- Module icon/color -->
	<div class="module-icon" style="background-color: {module.color || '#4a90e2'}">
		{#if module.icon}
			<span class="icon">{module.icon}</span>
		{:else}
			<span class="icon-placeholder">📚</span>
		{/if}
	</div>

	<!-- Module title and subtitle -->
	<div class="module-info">
		<h3 class="module-title">{module.title}</h3>
		{#if module.subtitle}
			<p class="module-subtitle">{module.subtitle}</p>
		{/if}
	</div>

	<!-- Module description -->
	<p class="module-description">{module.description}</p>

	<!-- Module stats -->
	<div class="module-stats">
		<div class="stat">
			<span class="stat-icon">📖</span>
			<span class="stat-value">{module.lessons.length}</span>
			<span class="stat-label">Lessons</span>
		</div>
		<div class="stat">
			<span class="stat-icon">⚙️</span>
			<span class="stat-value">{module.exercises.length}</span>
			<span class="stat-label">Exercises</span>
		</div>
		<div class="stat">
			<span class="stat-icon">🕐</span>
			<span class="stat-value">{formatTime(module.estimatedTime)}</span>
			<span class="stat-label">Est. Time</span>
		</div>
	</div>

	<!-- Difficulty indicator -->
	<div class="difficulty">
		<span class="difficulty-label">Difficulty:</span>
		<span class="difficulty-value" style="color: {getDifficultyColor()}">
			{getDifficultyLevel()}
		</span>
	</div>

	<!-- Progress bar -->
	{#if progress && status !== 'locked' && status !== 'not_started'}
		<div class="progress-section">
			<div class="progress-label">
				<span>Progress</span>
				<span class="progress-percentage">{progressPercentage}%</span>
			</div>
			<div class="progress-bar">
				<div class="progress-fill" style="width: {progressPercentage}%"></div>
			</div>
		</div>
	{/if}

	<!-- Locked overlay -->
	{#if locked}
		<div class="locked-overlay">
			<span class="lock-icon">🔒</span>
			<span class="lock-text">Complete prerequisites to unlock</span>
		</div>
	{/if}
</button>

<style>
	.module-card {
		position: relative;
		display: flex;
		flex-direction: column;
		gap: 12px;
		padding: 20px;
		background: white;
		border: 2px solid #e0e0e0;
		border-radius: 12px;
		cursor: pointer;
		transition: all 0.3s ease;
		text-align: left;
		width: 100%;
		min-height: 380px;
	}

	.module-card:hover:not(.locked) {
		border-color: #4a90e2;
		box-shadow: 0 8px 24px rgba(74, 144, 226, 0.15);
		transform: translateY(-4px);
	}

	.module-card:active:not(.locked) {
		transform: translateY(-2px);
	}

	.module-card.locked {
		opacity: 0.6;
		cursor: not-allowed;
		background: #f5f5f5;
	}

	.module-card.completed {
		border-color: #7cb342;
	}

	.module-card.in-progress {
		border-color: #2196f3;
	}

	.card-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		margin-bottom: 4px;
	}

	.status-badge {
		display: inline-block;
		padding: 4px 10px;
		border-radius: 12px;
		font-size: 11px;
		font-weight: 700;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		color: white;
	}

	.completed-icon {
		display: flex;
		align-items: center;
		justify-content: center;
		width: 28px;
		height: 28px;
		background: #7cb342;
		border-radius: 50%;
		color: white;
		font-size: 16px;
		font-weight: 700;
	}

	.module-icon {
		display: flex;
		align-items: center;
		justify-content: center;
		width: 100%;
		height: 100px;
		border-radius: 8px;
		margin-bottom: 4px;
	}

	.icon,
	.icon-placeholder {
		font-size: 48px;
	}

	.module-info {
		display: flex;
		flex-direction: column;
		gap: 4px;
	}

	.module-title {
		margin: 0;
		font-size: 18px;
		font-weight: 700;
		color: #2c3e50;
		line-height: 1.3;
		display: -webkit-box;
		-webkit-line-clamp: 2;
		-webkit-box-orient: vertical;
		overflow: hidden;
	}

	.module-subtitle {
		margin: 0;
		font-size: 13px;
		color: #666;
		font-style: italic;
		display: -webkit-box;
		-webkit-line-clamp: 1;
		-webkit-box-orient: vertical;
		overflow: hidden;
	}

	.module-description {
		margin: 0;
		font-size: 13px;
		line-height: 1.5;
		color: #666;
		display: -webkit-box;
		-webkit-line-clamp: 3;
		-webkit-box-orient: vertical;
		overflow: hidden;
		flex: 1;
	}

	.module-stats {
		display: flex;
		justify-content: space-between;
		gap: 8px;
		padding: 12px;
		background: #f8f8f8;
		border-radius: 8px;
	}

	.stat {
		display: flex;
		flex-direction: column;
		align-items: center;
		gap: 4px;
		flex: 1;
	}

	.stat-icon {
		font-size: 18px;
	}

	.stat-value {
		font-size: 16px;
		font-weight: 700;
		color: #2c3e50;
	}

	.stat-label {
		font-size: 11px;
		color: #999;
		text-transform: uppercase;
		letter-spacing: 0.3px;
	}

	.difficulty {
		display: flex;
		align-items: center;
		gap: 8px;
		font-size: 13px;
	}

	.difficulty-label {
		color: #666;
		font-weight: 600;
	}

	.difficulty-value {
		font-weight: 700;
	}

	.progress-section {
		display: flex;
		flex-direction: column;
		gap: 6px;
	}

	.progress-label {
		display: flex;
		justify-content: space-between;
		align-items: center;
		font-size: 12px;
		font-weight: 600;
		color: #2c3e50;
	}

	.progress-percentage {
		color: #999;
	}

	.progress-bar {
		width: 100%;
		height: 6px;
		background: #e0e0e0;
		border-radius: 3px;
		overflow: hidden;
	}

	.progress-fill {
		height: 100%;
		background: linear-gradient(90deg, #4a90e2 0%, #7b68ee 100%);
		transition: width 0.3s ease;
	}

	.locked-overlay {
		position: absolute;
		top: 0;
		left: 0;
		right: 0;
		bottom: 0;
		display: flex;
		flex-direction: column;
		align-items: center;
		justify-content: center;
		gap: 8px;
		background: rgba(255, 255, 255, 0.95);
		border-radius: 12px;
	}

	.lock-icon {
		font-size: 32px;
	}

	.lock-text {
		font-size: 13px;
		font-weight: 600;
		color: #666;
		text-align: center;
		padding: 0 20px;
	}

	@media (max-width: 768px) {
		.module-card {
			min-height: 350px;
		}

		.module-title {
			font-size: 16px;
		}

		.module-icon {
			height: 80px;
		}

		.icon,
		.icon-placeholder {
			font-size: 36px;
		}
	}
</style>
