<script lang="ts">
	/**
	 * Exercise Page
	 * Displays exercise with code editor and test runner
	 */
	import { page } from '$app/stores';
	import { onMount } from 'svelte';
	import ExerciseView from '$lib/components/education/ExerciseView.svelte';
	import { currentExercise, currentExerciseSubmissions } from '$lib/stores/educationStore';
	import { sampleModules } from '$lib/data/sampleModules';

	let moduleId: string;
	let exerciseId: string;
	let loading = true;
	let exercise: any = null;
	let submissions: any[] = [];

	// Starter code for different languages
	const starterCode: Record<string, string> = {
		Python: '# Write your solution here\ndef egyptian_multiply(a: int, b: int) -> int:\n    pass',
		C: '// Write your solution here\nint egyptian_multiply(int a, int b) {\n    return 0;\n}',
		Haskell: '-- Write your solution here\negyptianMultiply :: Int -> Int -> Int\negyptianMultiply a b = 0',
		Java: '// Write your solution here\npublic class Solution {\n    public int egyptianMultiply(int a, int b) {\n        return 0;\n    }\n}',
	};

	$: moduleId = $page.params.moduleId;
	$: exerciseId = $page.params.exerciseId;

	$: if ($currentExercise && $currentExercise.id === exerciseId) {
		exercise = $currentExercise;
	}

	$: submissions = $currentExerciseSubmissions;

	onMount(() => {
		// Load exercise data
		const module = sampleModules.find(m => m.id === moduleId);
		if (module) {
			exercise = module.exercises.find(e => e.id === exerciseId);
		}
		loading = false;
	});
</script>

<svelte:head>
	<title>{exercise?.title || 'Exercise'} - Ancient Compute</title>
</svelte:head>

<div class="exercise-page">
	{#if loading}
		<div class="loading">Loading exercise...</div>
	{:else if exercise}
		<ExerciseView {exercise} {submissions} {starterCode} />
	{:else}
		<div class="error">Exercise not found</div>
	{/if}
</div>

<style>
	.exercise-page {
		min-height: 100vh;
		background: #f5f5f5;
	}

	.loading,
	.error {
		text-align: center;
		padding: 60px 20px;
		font-size: 16px;
		color: #999;
	}

	.error {
		color: #e53935;
	}
</style>
