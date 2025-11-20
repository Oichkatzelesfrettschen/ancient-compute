<script lang="ts">
/**
 * LessonSidebar.svelte - Table of contents and lesson navigation
 *
 * Features:
 * - Auto-generated table of contents
 * - Smooth scroll to sections
 * - Active section highlighting
 * - Module lesson list
 * - Progress tracker
 */

import { onMount } from 'svelte';
import type { Lesson } from '../../stores/timelineStore';

export let currentLesson: Lesson;
export let lessons: Lesson[] = [];
export let activeSection = '';
export let onSectionClick: (id: string) => void = () => {};
export let onLessonClick: (lessonId: string) => void = () => {};

interface TOCItem {
	id: string;
	text: string;
	level: number;
}

export let tableOfContents: TOCItem[] = [];

function getLessonStatus(lesson: Lesson): 'completed' | 'current' | 'pending' {
	if (lesson.id === currentLesson.id) return 'current';
	if (lesson.completed) return 'completed';
	return 'pending';
}
</script>

<aside class="lesson-sidebar">
	<!-- Table of Contents -->
	{#if tableOfContents.length > 0}
		<div class="sidebar-section">
			<h3 class="sidebar-title">On This Page</h3>
			<nav class="toc">
				{#each tableOfContents as item}
					<button
						class="toc-item"
						class:active={activeSection === item.id}
						class:level-2={item.level === 2}
						class:level-3={item.level === 3}
						class:level-4={item.level === 4}
						on:click={() => onSectionClick(item.id)}
					>
						{item.text}
					</button>
				{/each}
			</nav>
		</div>
	{/if}

	<!-- Module Lessons -->
	{#if lessons.length > 0}
		<div class="sidebar-section">
			<h3 class="sidebar-title">Module Lessons</h3>
			<nav class="lessons-list">
				{#each lessons as lesson, index}
					<button
						class="lesson-item"
						class:completed={getLessonStatus(lesson) === 'completed'}
						class:current={getLessonStatus(lesson) === 'current'}
						on:click={() => onLessonClick(lesson.id)}
					>
						<span class="lesson-number">
							{#if getLessonStatus(lesson) === 'completed'}
								✓
							{:else}
								{index + 1}
							{/if}
						</span>
						<span class="lesson-name">{lesson.title}</span>
					</button>
				{/each}
			</nav>
		</div>
	{/if}

	<!-- Progress Summary -->
	{#if lessons.length > 0}
		<div class="sidebar-section">
			<h3 class="sidebar-title">Progress</h3>
			<div class="progress-summary">
				<div class="progress-stat">
					<span class="stat-value">{lessons.filter(l => l.completed).length}/{lessons.length}</span>
					<span class="stat-label">Lessons Complete</span>
				</div>
				<div class="progress-bar">
					<div class="progress-fill" style="width: {(lessons.filter(l => l.completed).length / lessons.length) * 100}%"></div>
				</div>
			</div>
		</div>
	{/if}
</aside>

<style>
	.lesson-sidebar {
		display: flex;
		flex-direction: column;
		gap: 32px;
		padding: 24px;
		background: white;
		border-left: 1px solid #e0e0e0;
		position: sticky;
		top: 80px;
		max-height: calc(100vh - 100px);
		overflow-y: auto;
	}

	.sidebar-section {
		display: flex;
		flex-direction: column;
		gap: 12px;
	}

	.sidebar-title {
		margin: 0;
		font-size: 13px;
		font-weight: 700;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		color: #999;
	}

	.toc {
		display: flex;
		flex-direction: column;
		gap: 4px;
	}

	.toc-item {
		width: 100%;
		padding: 8px 12px;
		background: transparent;
		border: none;
		border-left: 2px solid transparent;
		cursor: pointer;
		text-align: left;
		font-size: 13px;
		color: #666;
		transition: all 0.2s ease;
	}

	.toc-item:hover {
		color: #4a90e2;
		background: #f5f5f5;
	}

	.toc-item.active {
		color: #4a90e2;
		border-left-color: #4a90e2;
		background: #f0f8ff;
		font-weight: 600;
	}

	.toc-item.level-2 {
		padding-left: 12px;
	}

	.toc-item.level-3 {
		padding-left: 24px;
		font-size: 12px;
	}

	.toc-item.level-4 {
		padding-left: 36px;
		font-size: 12px;
	}

	.lessons-list {
		display: flex;
		flex-direction: column;
		gap: 6px;
	}

	.lesson-item {
		display: flex;
		align-items: center;
		gap: 10px;
		width: 100%;
		padding: 10px 12px;
		background: transparent;
		border: 1px solid #e0e0e0;
		border-radius: 6px;
		cursor: pointer;
		text-align: left;
		transition: all 0.2s ease;
	}

	.lesson-item:hover {
		border-color: #4a90e2;
		background: #f5f5f5;
	}

	.lesson-item.current {
		background: #f0f8ff;
		border-color: #4a90e2;
	}

	.lesson-item.completed {
		background: #f0f8f4;
		border-color: #7cb342;
	}

	.lesson-number {
		display: flex;
		align-items: center;
		justify-content: center;
		min-width: 24px;
		height: 24px;
		background: #e0e0e0;
		border-radius: 50%;
		font-size: 12px;
		font-weight: 700;
		color: #666;
	}

	.lesson-item.completed .lesson-number {
		background: #7cb342;
		color: white;
	}

	.lesson-item.current .lesson-number {
		background: #4a90e2;
		color: white;
	}

	.lesson-name {
		flex: 1;
		font-size: 13px;
		color: #2c3e50;
		line-height: 1.3;
	}

	.progress-summary {
		display: flex;
		flex-direction: column;
		gap: 12px;
		padding: 16px;
		background: #f5f5f5;
		border-radius: 8px;
	}

	.progress-stat {
		display: flex;
		flex-direction: column;
		gap: 4px;
	}

	.stat-value {
		font-size: 20px;
		font-weight: 700;
		color: #2c3e50;
	}

	.stat-label {
		font-size: 12px;
		color: #999;
	}

	.progress-bar {
		width: 100%;
		height: 8px;
		background: #e0e0e0;
		border-radius: 4px;
		overflow: hidden;
	}

	.progress-fill {
		height: 100%;
		background: linear-gradient(90deg, #4a90e2 0%, #7b68ee 100%);
		transition: width 0.3s ease;
	}

	@media (max-width: 1024px) {
		.lesson-sidebar {
			position: static;
			max-height: none;
			border-left: none;
			border-top: 1px solid #e0e0e0;
		}
	}
</style>
