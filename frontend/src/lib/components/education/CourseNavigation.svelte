<script lang="ts">
/**
 * CourseNavigation.svelte - Breadcrumb and course navigation
 *
 * Features:
 * - Breadcrumb navigation (Era → Module → Lesson)
 * - Previous/Next buttons
 * - Jump to specific lesson dropdown
 * - Keyboard shortcuts support
 */

import { goto } from '$app/navigation';
import { onMount } from 'svelte';

export let eraName = '';
export let eraId = '';
export let moduleName = '';
export let moduleId = '';
export let lessonName = '';
export let lessonId = '';
export let prevLessonId: string | null = null;
export let nextLessonId: string | null = null;
export let allLessons: Array<{ id: string; title: string }> = [];

function handleKeyboard(event: KeyboardEvent): void {
	if (event.altKey) {
		if (event.key === 'ArrowLeft' && prevLessonId) {
			event.preventDefault();
			goto(`/modules/${moduleId}/lessons/${prevLessonId}`);
		} else if (event.key === 'ArrowRight' && nextLessonId) {
			event.preventDefault();
			goto(`/modules/${moduleId}/lessons/${nextLessonId}`);
		}
	}
}

onMount(() => {
	window.addEventListener('keydown', handleKeyboard);
	return () => {
		window.removeEventListener('keydown', handleKeyboard);
	};
});

function handleLessonSelect(event: Event): void {
	const target = event.target as HTMLSelectElement;
	const selectedLessonId = target.value;
	if (selectedLessonId) {
		goto(`/modules/${moduleId}/lessons/${selectedLessonId}`);
	}
}
</script>

<nav class="course-navigation">
	<!-- Breadcrumb -->
	<div class="breadcrumb">
		<a href="/timeline" class="breadcrumb-item">Timeline</a>
		<span class="breadcrumb-separator">›</span>
		{#if eraName}
			<a href="/timeline?era={eraId}" class="breadcrumb-item">{eraName}</a>
			<span class="breadcrumb-separator">›</span>
		{/if}
		{#if moduleName}
			<a href="/modules/{moduleId}" class="breadcrumb-item">{moduleName}</a>
		{/if}
		{#if lessonName}
			<span class="breadcrumb-separator">›</span>
			<span class="breadcrumb-item current">{lessonName}</span>
		{/if}
	</div>

	<!-- Navigation Controls -->
	<div class="nav-controls">
		<!-- Lesson selector -->
		{#if allLessons.length > 0}
			<select class="lesson-select" value={lessonId} on:change={handleLessonSelect}>
				{#each allLessons as lesson}
					<option value={lesson.id}>{lesson.title}</option>
				{/each}
			</select>
		{/if}

		<!-- Prev/Next buttons -->
		<div class="nav-buttons">
			<button
				class="nav-btn prev"
				disabled={!prevLessonId}
				on:click={() => prevLessonId && goto(`/modules/${moduleId}/lessons/${prevLessonId}`)}
				title="Previous lesson (Alt+Left)"
			>
				←
			</button>
			<button
				class="nav-btn next"
				disabled={!nextLessonId}
				on:click={() => nextLessonId && goto(`/modules/${moduleId}/lessons/${nextLessonId}`)}
				title="Next lesson (Alt+Right)"
			>
				→
			</button>
		</div>
	</div>
</nav>

<style>
	.course-navigation {
		display: flex;
		justify-content: space-between;
		align-items: center;
		gap: 16px;
		padding: 16px 24px;
		background: white;
		border-bottom: 1px solid #e0e0e0;
		position: sticky;
		top: 0;
		z-index: 100;
	}

	.breadcrumb {
		display: flex;
		align-items: center;
		gap: 8px;
		flex: 1;
		min-width: 0;
	}

	.breadcrumb-item {
		font-size: 14px;
		color: #666;
		text-decoration: none;
		white-space: nowrap;
		overflow: hidden;
		text-overflow: ellipsis;
		transition: color 0.2s ease;
	}

	a.breadcrumb-item:hover {
		color: #4a90e2;
	}

	.breadcrumb-item.current {
		color: #2c3e50;
		font-weight: 600;
	}

	.breadcrumb-separator {
		color: #ccc;
		font-size: 14px;
	}

	.nav-controls {
		display: flex;
		align-items: center;
		gap: 12px;
	}

	.lesson-select {
		padding: 8px 12px;
		border: 2px solid #e0e0e0;
		border-radius: 6px;
		font-size: 13px;
		color: #2c3e50;
		background: white;
		cursor: pointer;
		max-width: 200px;
		transition: border-color 0.2s ease;
	}

	.lesson-select:focus {
		outline: none;
		border-color: #4a90e2;
	}

	.nav-buttons {
		display: flex;
		gap: 8px;
	}

	.nav-btn {
		display: flex;
		align-items: center;
		justify-content: center;
		width: 36px;
		height: 36px;
		background: white;
		border: 2px solid #e0e0e0;
		border-radius: 6px;
		cursor: pointer;
		font-size: 16px;
		color: #2c3e50;
		transition: all 0.2s ease;
	}

	.nav-btn:hover:not(:disabled) {
		border-color: #4a90e2;
		color: #4a90e2;
		background: #f0f8ff;
	}

	.nav-btn:disabled {
		opacity: 0.3;
		cursor: not-allowed;
	}

	@media (max-width: 768px) {
		.course-navigation {
			flex-direction: column;
			align-items: flex-start;
			gap: 12px;
		}

		.breadcrumb {
			width: 100%;
		}

		.nav-controls {
			width: 100%;
			justify-content: space-between;
		}

		.lesson-select {
			flex: 1;
			max-width: none;
		}
	}
</style>
