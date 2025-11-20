<script lang="ts">
	/**
	 * Lesson Page
	 * Displays lesson content with navigation
	 */
	import { page } from '$app/stores';
	import { onMount } from 'svelte';
	import { goto } from '$app/navigation';
	import LessonViewer from '$lib/components/education/LessonViewer.svelte';
	import LessonSidebar from '$lib/components/education/LessonSidebar.svelte';
	import CourseNavigation from '$lib/components/education/CourseNavigation.svelte';
	import { currentLesson, currentModule, selectLesson } from '$lib/stores/timelineStore';
	import { sampleModules } from '$lib/data/sampleModules';

	let moduleId: string;
	let lessonId: string;
	let loading = true;
	let lesson: any = null;
	let module: any = null;
	let lessons: any[] = [];
	let prevLessonId: string | null = null;
	let nextLessonId: string | null = null;

	$: moduleId = $page.params.moduleId;
	$: lessonId = $page.params.lessonId;

	$: if ($currentLesson && $currentLesson.id === lessonId) {
		lesson = $currentLesson;
	}

	$: if ($currentModule && $currentModule.id === moduleId) {
		module = $currentModule;
		lessons = $currentModule.lessons;

		// Find prev/next lessons
		const currentIndex = lessons.findIndex(l => l.id === lessonId);
		prevLessonId = currentIndex > 0 ? lessons[currentIndex - 1].id : null;
		nextLessonId = currentIndex < lessons.length - 1 ? lessons[currentIndex + 1].id : null;
	}

	function handleNavigate(id: string): void {
		goto(`/modules/${moduleId}/lessons/${id}`);
	}

	function handleSectionClick(sectionId: string): void {
		const element = document.getElementById(sectionId);
		if (element) {
			element.scrollIntoView({ behavior: 'smooth', block: 'start' });
		}
	}

	function handleLessonClick(id: string): void {
		goto(`/modules/${moduleId}/lessons/${id}`);
	}

	onMount(() => {
		selectLesson(lessonId);
		loading = false;
	});
</script>

<svelte:head>
	<title>{lesson?.title || 'Lesson'} - Ancient Compute</title>
</svelte:head>

<div class="lesson-page">
	{#if module && lesson}
		<CourseNavigation
			eraName="Ancient Era"
			eraId={module.eraId}
			moduleName={module.title}
			moduleId={module.id}
			lessonName={lesson.title}
			{lessonId}
			{prevLessonId}
			{nextLessonId}
			allLessons={lessons}
		/>
	{/if}

	{#if loading}
		<div class="loading">Loading lesson...</div>
	{:else if lesson}
		<div class="lesson-layout">
			<main class="lesson-main">
				<LessonViewer
					{lesson}
					{prevLessonId}
					{nextLessonId}
					onNavigate={handleNavigate}
				/>
			</main>

			<aside class="lesson-aside">
				<LessonSidebar
					currentLesson={lesson}
					{lessons}
					onSectionClick={handleSectionClick}
					onLessonClick={handleLessonClick}
				/>
			</aside>
		</div>
	{:else}
		<div class="error">Lesson not found</div>
	{/if}
</div>

<style>
	.lesson-page {
		display: flex;
		flex-direction: column;
		min-height: 100vh;
	}

	.lesson-layout {
		display: grid;
		grid-template-columns: 1fr 320px;
		gap: 0;
		flex: 1;
	}

	.lesson-main {
		overflow-x: hidden;
	}

	.lesson-aside {
		background: white;
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

	@media (max-width: 1024px) {
		.lesson-layout {
			grid-template-columns: 1fr;
		}

		.lesson-aside {
			order: -1;
		}
	}
</style>
