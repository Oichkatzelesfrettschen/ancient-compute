<script lang="ts">
/**
 * ExerciseView.svelte - Exercise viewer with code editor
 *
 * Features:
 * - Monaco Editor integration
 * - Language selection
 * - Submit button
 * - Real-time validation
 * - Test runner integration
 * - Submission history
 * - Solution hints (expandable)
 * - Success/failure animations
 */

import { onMount } from 'svelte';
import { marked } from 'marked';
import ExerciseTestRunner from './ExerciseTestRunner.svelte';
import SubmissionHistory from './SubmissionHistory.svelte';
import type { Exercise } from '../../stores/timelineStore';
import type { ExerciseSubmission } from '../../stores/educationStore';
import { submitExercise } from '../../stores/educationStore';

export let exercise: Exercise;
export let submissions: ExerciseSubmission[] = [];
export let starterCode: Record<string, string> = {};

let editorElement: HTMLElement;
let editor: any = null;
let selectedLanguage = exercise.languages[0];
let code = '';
let isSubmitting = false;
let showHints = false;
let currentTab: 'editor' | 'history' = 'editor';
let currentSubmission: ExerciseSubmission | null = null;

$: if (selectedLanguage && starterCode[selectedLanguage]) {
	code = starterCode[selectedLanguage];
	if (editor) {
		editor.setValue(code);
	}
}

async function loadMonaco(): Promise<void> {
	try {
		const monaco = await import('monaco-editor');

		if (!editorElement) return;

		editor = monaco.editor.create(editorElement, {
			value: code || starterCode[selectedLanguage] || '',
			language: getMonacoLanguage(selectedLanguage),
			theme: 'vs-dark',
			minimap: { enabled: false },
			fontSize: 14,
			lineNumbers: 'on',
			roundedSelection: true,
			scrollBeyondLastLine: false,
			automaticLayout: true,
		});

		editor.onDidChangeModelContent(() => {
			code = editor.getValue();
		});
	} catch (error) {
		console.error('Failed to load Monaco Editor:', error);
	}
}

function getMonacoLanguage(lang: string): string {
	const languageMap: Record<string, string> = {
		'python': 'python',
		'c': 'c',
		'haskell': 'haskell',
		'java': 'java',
		'lisp': 'lisp',
		'idris2': 'haskell', // Use Haskell syntax highlighting for IDRIS2
		'systemf': 'haskell', // Use Haskell syntax highlighting for System F
		'babbage-assembly': 'assembly',
	};
	return languageMap[lang.toLowerCase()] || 'plaintext';
}

async function handleSubmit(): Promise<void> {
	if (!code.trim()) {
		alert('Please write some code before submitting.');
		return;
	}

	isSubmitting = true;
	try {
		await submitExercise(exercise.id, code, selectedLanguage);
		// The submission will be added to the submissions array via store
	} catch (error) {
		console.error('Submission failed:', error);
		alert('Submission failed. Please try again.');
	} finally {
		isSubmitting = false;
	}
}

function handleLanguageChange(event: Event): void {
	const target = event.target as HTMLSelectElement;
	selectedLanguage = target.value;

	if (editor) {
		const monaco = (window as any).monaco;
		if (monaco) {
			monaco.editor.setModelLanguage(editor.getModel(), getMonacoLanguage(selectedLanguage));
		}
	}

	// Load starter code for new language
	code = starterCode[selectedLanguage] || '';
	if (editor) {
		editor.setValue(code);
	}
}

function handleRunTests(): void {
	// Trigger test execution
	handleSubmit();
}

function handleRetryFailed(): void {
	// Re-run only failed tests
	handleSubmit();
}

function handleViewSubmission(submissionId: string): void {
	const submission = submissions.find(s => s.id === submissionId);
	if (submission) {
		currentSubmission = submission;
		// Could show a modal or expand details
	}
}

function handleLoadSubmission(submissionId: string): void {
	const submission = submissions.find(s => s.id === submissionId);
	if (submission) {
		code = submission.code;
		selectedLanguage = submission.language;
		if (editor) {
			editor.setValue(code);
		}
	}
}

function getDifficultyColor(): string {
	switch (exercise.difficulty) {
		case 'beginner': return '#7cb342';
		case 'intermediate': return '#ff9800';
		case 'advanced': return '#e53935';
		default: return '#999';
	}
}

onMount(() => {
	loadMonaco();
	code = starterCode[selectedLanguage] || '';

	return () => {
		if (editor) {
			editor.dispose();
		}
	};
});
</script>

<div class="exercise-view">
	<!-- Exercise header -->
	<div class="exercise-header">
		<div class="header-content">
			<h1 class="exercise-title">{exercise.title}</h1>
			<div class="exercise-meta">
				<span class="difficulty" style="color: {getDifficultyColor()}">
					{exercise.difficulty}
				</span>
				<span class="separator">•</span>
				<span class="languages">{exercise.languages.join(', ')}</span>
				<span class="separator">•</span>
				<span class="time-limit">{exercise.timeLimit}s limit</span>
			</div>
		</div>
		{#if exercise.completed}
			<div class="completed-badge">
				<span class="badge-icon">✓</span>
				<span class="badge-text">Completed</span>
			</div>
		{/if}
	</div>

	<!-- Exercise description -->
	<div class="exercise-description">
		{@html marked(exercise.problem)}
	</div>

	<!-- Tabs -->
	<div class="tabs">
		<button class="tab" class:active={currentTab === 'editor'} on:click={() => currentTab = 'editor'}>
			💻 Code Editor
		</button>
		<button class="tab" class:active={currentTab === 'history'} on:click={() => currentTab = 'history'}>
			📝 History ({submissions.length})
		</button>
	</div>

	<!-- Tab content -->
	{#if currentTab === 'editor'}
		<!-- Code editor section -->
		<div class="editor-section">
			<div class="editor-controls">
				<div class="language-selector">
					<label for="language">Language:</label>
					<select id="language" bind:value={selectedLanguage} on:change={handleLanguageChange}>
						{#each exercise.languages as lang}
							<option value={lang}>{lang}</option>
						{/each}
					</select>
				</div>

				<button class="submit-btn" on:click={handleSubmit} disabled={isSubmitting}>
					{isSubmitting ? 'Submitting...' : 'Submit Solution'}
				</button>
			</div>

			<!-- Monaco Editor container -->
			<div class="editor-container" bind:this={editorElement}></div>

			<!-- Constraints -->
			<div class="constraints">
				<h3 class="constraints-title">Constraints</h3>
				<ul class="constraints-list">
					<li>Time Limit: {exercise.timeLimit}s</li>
					<li>Memory Limit: {exercise.memoryLimit}MB</li>
					<li>Test Cases: {exercise.testCases.length}</li>
				</ul>
			</div>
		</div>

		<!-- Hints section -->
		{#if exercise.hints.length > 0}
			<div class="hints-section">
				<button class="hints-toggle" on:click={() => showHints = !showHints}>
					💡 Hints ({exercise.hints.length})
					<span class="toggle-icon">{showHints ? '▼' : '▶'}</span>
				</button>
				{#if showHints}
					<div class="hints-content">
						{#each exercise.hints as hint, index}
							<div class="hint">
								<span class="hint-number">Hint {index + 1}:</span>
								<span class="hint-text">{hint}</span>
							</div>
						{/each}
					</div>
				{/if}
			</div>
		{/if}

		<!-- Test runner -->
		{#if currentSubmission}
			<ExerciseTestRunner
				testCases={exercise.testCases}
				testResults={currentSubmission.testResults}
				isRunning={isSubmitting}
				onRunTests={handleRunTests}
				onRetryFailed={handleRetryFailed}
			/>
		{/if}
	{:else if currentTab === 'history'}
		<!-- Submission history -->
		<SubmissionHistory
			{submissions}
			onViewSubmission={handleViewSubmission}
			onLoadSubmission={handleLoadSubmission}
		/>
	{/if}
</div>

<style>
	.exercise-view {
		display: flex;
		flex-direction: column;
		gap: 24px;
		padding: 24px;
		max-width: 1400px;
		margin: 0 auto;
	}

	.exercise-header {
		display: flex;
		justify-content: space-between;
		align-items: flex-start;
		gap: 20px;
	}

	.header-content {
		flex: 1;
	}

	.exercise-title {
		margin: 0 0 12px 0;
		font-size: 28px;
		font-weight: 700;
		color: #2c3e50;
	}

	.exercise-meta {
		display: flex;
		align-items: center;
		gap: 12px;
		font-size: 14px;
	}

	.difficulty {
		font-weight: 700;
		text-transform: uppercase;
	}

	.separator {
		color: #ccc;
	}

	.languages,
	.time-limit {
		color: #666;
	}

	.completed-badge {
		display: flex;
		align-items: center;
		gap: 8px;
		padding: 12px 20px;
		background: #7cb342;
		border-radius: 8px;
		color: white;
	}

	.badge-icon {
		font-size: 20px;
	}

	.badge-text {
		font-size: 14px;
		font-weight: 600;
	}

	.exercise-description {
		padding: 20px;
		background: white;
		border: 2px solid #e0e0e0;
		border-radius: 8px;
		font-size: 15px;
		line-height: 1.7;
		color: #2c3e50;
	}

	.exercise-description :global(h2) {
		margin: 20px 0 12px 0;
		font-size: 20px;
		font-weight: 700;
	}

	.exercise-description :global(h3) {
		margin: 16px 0 10px 0;
		font-size: 18px;
		font-weight: 600;
	}

	.exercise-description :global(p) {
		margin: 12px 0;
	}

	.exercise-description :global(code) {
		padding: 2px 6px;
		background: #f5f5f5;
		border-radius: 3px;
		font-family: 'Courier New', monospace;
		font-size: 13px;
		color: #e53935;
	}

	.exercise-description :global(pre) {
		padding: 12px;
		background: #f5f5f5;
		border-radius: 4px;
		overflow-x: auto;
	}

	.tabs {
		display: flex;
		gap: 0;
		border-bottom: 2px solid #e0e0e0;
	}

	.tab {
		padding: 12px 24px;
		background: transparent;
		border: none;
		border-bottom: 3px solid transparent;
		cursor: pointer;
		font-size: 14px;
		font-weight: 600;
		color: #999;
		transition: all 0.2s ease;
	}

	.tab:hover {
		color: #2c3e50;
	}

	.tab.active {
		color: #4a90e2;
		border-bottom-color: #4a90e2;
	}

	.editor-section {
		display: flex;
		flex-direction: column;
		gap: 16px;
	}

	.editor-controls {
		display: flex;
		justify-content: space-between;
		align-items: center;
		gap: 16px;
		padding: 16px;
		background: white;
		border: 2px solid #e0e0e0;
		border-radius: 8px 8px 0 0;
	}

	.language-selector {
		display: flex;
		align-items: center;
		gap: 12px;
	}

	.language-selector label {
		font-size: 14px;
		font-weight: 600;
		color: #2c3e50;
	}

	.language-selector select {
		padding: 8px 12px;
		border: 2px solid #e0e0e0;
		border-radius: 6px;
		font-size: 14px;
		font-weight: 600;
		color: #2c3e50;
		background: white;
		cursor: pointer;
	}

	.submit-btn {
		padding: 10px 24px;
		background: #4a90e2;
		border: none;
		border-radius: 6px;
		cursor: pointer;
		font-size: 14px;
		font-weight: 600;
		color: white;
		transition: background 0.2s ease;
	}

	.submit-btn:hover:not(:disabled) {
		background: #357abd;
	}

	.submit-btn:disabled {
		opacity: 0.6;
		cursor: not-allowed;
	}

	.editor-container {
		width: 100%;
		height: 500px;
		border: 2px solid #e0e0e0;
		border-top: none;
		border-radius: 0 0 8px 8px;
		overflow: hidden;
	}

	.constraints {
		padding: 16px;
		background: #f5f5f5;
		border-radius: 8px;
	}

	.constraints-title {
		margin: 0 0 12px 0;
		font-size: 14px;
		font-weight: 700;
		text-transform: uppercase;
		letter-spacing: 0.5px;
		color: #666;
	}

	.constraints-list {
		margin: 0;
		padding-left: 20px;
		display: flex;
		flex-direction: column;
		gap: 6px;
	}

	.constraints-list li {
		font-size: 13px;
		color: #666;
	}

	.hints-section {
		border: 2px solid #ff9800;
		border-radius: 8px;
		overflow: hidden;
	}

	.hints-toggle {
		width: 100%;
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 14px 20px;
		background: #fff3e0;
		border: none;
		cursor: pointer;
		font-size: 14px;
		font-weight: 600;
		color: #e65100;
		text-align: left;
		transition: background 0.2s ease;
	}

	.hints-toggle:hover {
		background: #ffe0b2;
	}

	.toggle-icon {
		font-size: 12px;
		color: #ff9800;
	}

	.hints-content {
		display: flex;
		flex-direction: column;
		gap: 12px;
		padding: 20px;
		background: white;
	}

	.hint {
		display: flex;
		gap: 12px;
		padding: 12px;
		background: #f5f5f5;
		border-left: 4px solid #ff9800;
		border-radius: 4px;
	}

	.hint-number {
		font-size: 13px;
		font-weight: 700;
		color: #ff9800;
		white-space: nowrap;
	}

	.hint-text {
		font-size: 13px;
		line-height: 1.5;
		color: #666;
	}

	@media (max-width: 768px) {
		.exercise-view {
			padding: 16px;
		}

		.exercise-header {
			flex-direction: column;
		}

		.editor-controls {
			flex-direction: column;
			align-items: stretch;
		}

		.language-selector {
			flex-direction: column;
			align-items: stretch;
		}

		.submit-btn {
			width: 100%;
		}

		.editor-container {
			height: 400px;
		}
	}
</style>
