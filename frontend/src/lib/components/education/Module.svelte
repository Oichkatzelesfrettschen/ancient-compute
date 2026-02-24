<script lang="ts">
/**
 * Module.svelte - Historical module container and navigation
 *
 * Features:
 * - Displays module title, description, and estimated time
 * - Tab interface: Lessons vs Exercises
 * - Progress tracking visualization
 * - Bidirectional navigation (previous/next modules)
 * - Responsive layout for mobile/desktop
 */

import {
  currentModule,
  previousModule,
  nextModule,
  selectLesson,
  selectExercise,
} from '../../stores/timelineStore';
import type { Module, Lesson, Exercise } from '../../stores/timelineStore';

let module: Module | null = null;
let selectedTab: 'lessons' | 'exercises' = 'lessons';
let expandedSections: Set<string> = new Set();

currentModule.subscribe((m) => {
  module = m;
  if (m?.lessons.length === 0 && m?.exercises.length > 0) {
    selectedTab = 'exercises';
  } else {
    selectedTab = 'lessons';
  }
});

function handleLessonClick(lesson: Lesson): void {
  selectLesson(lesson.id);
}

function handleExerciseClick(exercise: Exercise): void {
  selectExercise(exercise.id);
}

function getModuleProgressPercentage(): number {
  if (!module) return 0;
  const totalLessons = module.lessons.length;
  const completedLessons = module.lessons.filter((l) => l.completed).length;
  return totalLessons > 0 ? Math.round((completedLessons / totalLessons) * 100) : 0;
}

function getExerciseProgressPercentage(): number {
  if (!module) return 0;
  const totalExercises = module.exercises.length;
  const completedExercises = module.exercises.filter((e) => e.completed).length;
  return totalExercises > 0 ? Math.round((completedExercises / totalExercises) * 100) : 0;
}

function getDifficultyColor(difficulty: 'beginner' | 'intermediate' | 'advanced'): string {
  switch (difficulty) {
    case 'beginner':
      return '#7cb342';
    case 'intermediate':
      return '#ff9800';
    case 'advanced':
      return '#e53935';
    default:
      return '#999';
  }
}

function getDifficultyLabel(difficulty: 'beginner' | 'intermediate' | 'advanced'): string {
  return difficulty.charAt(0).toUpperCase() + difficulty.slice(1);
}
</script>

{#if module}
  <div class="module-container">
    <!-- Module header -->
    <div class="module-header">
      <div class="header-content">
        <h1 class="module-title">{module.title}</h1>
        {#if module.subtitle}
          <p class="module-subtitle">{module.subtitle}</p>
        {/if}
        <p class="module-description">{module.description}</p>

        <!-- Module stats -->
        <div class="module-stats">
          <div class="stat">
            <span class="stat-label">Lessons</span>
            <span class="stat-value">{module.completedLessons}/{module.lessons.length}</span>
          </div>
          <div class="stat">
            <span class="stat-label">Exercises</span>
            <span class="stat-value">{module.completedExercises}/{module.exercises.length}</span>
          </div>
          <div class="stat">
            <span class="stat-label">Est. Time</span>
            <span class="stat-value">{module.estimatedTime}h</span>
          </div>
        </div>
      </div>

      <!-- Module icon placeholder -->
      <div class="header-icon">
        {#if module.color}
          <div class="module-color-swatch" style="background-color: {module.color}"></div>
        {/if}
      </div>
    </div>

    <!-- Progress bars -->
    <div class="progress-section">
      <div class="progress-item">
        <div class="progress-label">
          <span>Lessons</span>
          <span class="progress-percentage">{getModuleProgressPercentage()}%</span>
        </div>
        <div class="progress-bar">
          <div class="progress-fill" style="width: {getModuleProgressPercentage()}%"></div>
        </div>
      </div>

      <div class="progress-item">
        <div class="progress-label">
          <span>Exercises</span>
          <span class="progress-percentage">{getExerciseProgressPercentage()}%</span>
        </div>
        <div class="progress-bar">
          <div class="progress-fill" style="width: {getExerciseProgressPercentage()}%"></div>
        </div>
      </div>
    </div>

    <!-- Tabs -->
    <div class="tabs-container">
      <button
        class="tab"
        class:active={selectedTab === 'lessons'}
        on:click={() => (selectedTab = 'lessons')}
      >
        <span class="tab-icon">📚</span>
        <span class="tab-label">Lessons</span>
        <span class="tab-count">({module.lessons.length})</span>
      </button>

      <button
        class="tab"
        class:active={selectedTab === 'exercises'}
        on:click={() => (selectedTab = 'exercises')}
      >
        <span class="tab-icon">⚙️</span>
        <span class="tab-label">Exercises</span>
        <span class="tab-count">({module.exercises.length})</span>
      </button>
    </div>

    <!-- Content area -->
    <div class="content-area">
      {#if selectedTab === 'lessons'}
        <div class="lessons-list">
          {#each module.lessons as lesson, index (lesson.id)}
            <div class="lesson-item">
              <button
                class="lesson-header"
                class:completed={lesson.completed}
                on:click={() => handleLessonClick(lesson)}
              >
                <!-- Lesson number and title -->
                <div class="lesson-info">
                  <span class="lesson-number">
                    {#if lesson.completed}
                      ✓
                    {:else}
                      {index + 1}
                    {/if}
                  </span>
                  <div class="lesson-text">
                    <h3 class="lesson-title">{lesson.title}</h3>
                    <p class="lesson-time">
                      {lesson.estimatedReadTime} min read
                      {#if lesson.prerequisites.length > 0}
                        • {lesson.prerequisites.length} prerequisite(s)
                      {/if}
                    </p>
                  </div>
                </div>

                <!-- Lesson status badge -->
                <div class="lesson-status">
                  {#if lesson.completed}
                    <span class="badge completed">Completed</span>
                  {:else}
                    <span class="badge pending">Not Started</span>
                  {/if}
                </div>
              </button>

              <!-- Lesson description (expandable) -->
              {#if expandedSections.has(lesson.id)}
                <div class="lesson-details">
                  <p class="lesson-description">{lesson.description}</p>
                  {#if lesson.keyConcepts.length > 0}
                    <div class="key-concepts">
                      <h4>Key Concepts:</h4>
                      <ul>
                        {#each lesson.keyConcepts as concept}
                          <li>
                            <strong>{concept.term}:</strong>
                            {concept.definition}
                          </li>
                        {/each}
                      </ul>
                    </div>
                  {/if}
                  {#if lesson.codeExamples.length > 0}
                    <div class="code-examples">
                      <h4>Code Examples: {lesson.codeExamples.length}</h4>
                      <p class="muted">Examples visible in lesson view</p>
                    </div>
                  {/if}
                </div>
              {/if}
            </div>
          {/each}
        </div>
      {:else if selectedTab === 'exercises'}
        <div class="exercises-list">
          {#each module.exercises as exercise (exercise.id)}
            <div class="exercise-item">
              <button
                class="exercise-header"
                class:completed={exercise.completed}
                on:click={() => handleExerciseClick(exercise)}
              >
                <!-- Exercise info -->
                <div class="exercise-info">
                  <div class="exercise-icon">
                    {#if exercise.completed}
                      ✓
                    {:else}
                      💻
                    {/if}
                  </div>
                  <div class="exercise-text">
                    <h3 class="exercise-title">{exercise.title}</h3>
                    <div class="exercise-meta">
                      <span class="difficulty" style="color: {getDifficultyColor(exercise.difficulty)}">
                        {getDifficultyLabel(exercise.difficulty)}
                      </span>
                      <span class="separator">•</span>
                      <span class="languages">{exercise.languages.join(', ')}</span>
                      <span class="separator">•</span>
                      <span class="time">{exercise.timeLimit}s limit</span>
                    </div>
                  </div>
                </div>

                <!-- Exercise status -->
                <div class="exercise-status">
                  {#if exercise.completed}
                    <span class="badge completed">
                      Completed
                      {#if exercise.bestScore !== undefined}
                        • {exercise.bestScore}%
                      {/if}
                    </span>
                  {:else}
                    <span class="badge pending">Not Started</span>
                  {/if}
                </div>
              </button>

              <!-- Exercise details (expandable) -->
              {#if expandedSections.has(exercise.id)}
                <div class="exercise-details">
                  <p class="exercise-description">{exercise.description}</p>
                  <div class="exercise-info-grid">
                    <div class="info-item">
                      <span class="label">Test Cases:</span>
                      <span class="value">{exercise.testCases.length}</span>
                    </div>
                    <div class="info-item">
                      <span class="label">Memory Limit:</span>
                      <span class="value">{exercise.memoryLimit} MB</span>
                    </div>
                    <div class="info-item">
                      <span class="label">Languages:</span>
                      <span class="value">{exercise.languages.join(', ')}</span>
                    </div>
                  </div>
                  {#if exercise.hints.length > 0}
                    <details class="hints-section">
                      <summary>Hints ({exercise.hints.length})</summary>
                      <ul>
                        {#each exercise.hints as hint}
                          <li>{hint}</li>
                        {/each}
                      </ul>
                    </details>
                  {/if}
                </div>
              {/if}
            </div>
          {/each}

          {#if module.exercises.length === 0}
            <div class="empty-state">
              <p>No exercises for this module yet.</p>
            </div>
          {/if}
        </div>
      {/if}
    </div>

    <!-- Navigation buttons -->
    <div class="module-navigation">
      <button class="nav-button prev" on:click={previousModule}>
        <svg width="20" height="20" viewBox="0 0 20 20" fill="none">
          <path d="M12 5L7 10l5 5" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" />
        </svg>
        <span>Previous Module</span>
      </button>

      <div class="module-indicator">
        <span class="text">Module</span>
      </div>

      <button class="nav-button next" on:click={nextModule}>
        <span>Next Module</span>
        <svg width="20" height="20" viewBox="0 0 20 20" fill="none">
          <path d="M8 5l5 5-5 5" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" />
        </svg>
      </button>
    </div>
  </div>
{:else}
  <div class="loading-state">
    <p>Loading module...</p>
  </div>
{/if}

<style>
  .module-container {
    display: flex;
    flex-direction: column;
    gap: 24px;
    padding: 24px;
    background: white;
    border-radius: 8px;
  }

  .module-header {
    display: flex;
    justify-content: space-between;
    align-items: flex-start;
    gap: 32px;
  }

  .header-content {
    flex: 1;
  }

  .module-title {
    margin: 0;
    font-size: 28px;
    font-weight: 700;
    color: #2c3e50;
    line-height: 1.3;
  }

  .module-subtitle {
    margin: 8px 0 12px 0;
    font-size: 15px;
    color: #666;
    font-style: italic;
  }

  .module-description {
    margin: 0 0 16px 0;
    font-size: 14px;
    line-height: 1.6;
    color: #666;
  }

  .module-stats {
    display: flex;
    gap: 24px;
  }

  .stat {
    display: flex;
    flex-direction: column;
    gap: 4px;
  }

  .stat-label {
    font-size: 12px;
    color: #999;
    text-transform: uppercase;
    letter-spacing: 0.5px;
    font-weight: 600;
  }

  .stat-value {
    font-size: 18px;
    font-weight: 700;
    color: #2c3e50;
  }

  .header-icon {
    flex-shrink: 0;
    width: 120px;
    height: 120px;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .module-color-swatch {
    width: 100%;
    height: 100%;
    border-radius: 12px;
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
  }

  .progress-section {
    display: flex;
    flex-direction: column;
    gap: 16px;
  }

  .progress-item {
    display: flex;
    flex-direction: column;
    gap: 8px;
  }

  .progress-label {
    display: flex;
    justify-content: space-between;
    align-items: center;
    font-size: 13px;
    font-weight: 600;
    color: #2c3e50;
  }

  .progress-percentage {
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

  .tabs-container {
    display: flex;
    gap: 0;
    border-bottom: 1px solid #e0e0e0;
  }

  .tab {
    display: flex;
    align-items: center;
    gap: 8px;
    padding: 12px 20px;
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

  .tab-icon {
    font-size: 16px;
  }

  .tab-count {
    font-size: 12px;
    font-weight: 400;
    color: inherit;
  }

  .content-area {
    display: flex;
    flex-direction: column;
    gap: 12px;
    min-height: 200px;
  }

  .lessons-list,
  .exercises-list {
    display: flex;
    flex-direction: column;
    gap: 12px;
  }

  .lesson-item,
  .exercise-item {
    border: 1px solid #e0e0e0;
    border-radius: 6px;
    overflow: hidden;
    transition: all 0.2s ease;
  }

  .lesson-item:hover,
  .exercise-item:hover {
    border-color: #4a90e2;
    box-shadow: 0 2px 8px rgba(74, 144, 226, 0.1);
  }

  .lesson-header,
  .exercise-header {
    width: 100%;
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 16px;
    background: #fafafa;
    border: none;
    cursor: pointer;
    transition: background 0.2s ease;
    text-align: left;
  }

  .lesson-header:hover,
  .exercise-header:hover {
    background: #f0f0f0;
  }

  .lesson-header.completed,
  .exercise-header.completed {
    background: #f0f8f4;
  }

  .lesson-info,
  .exercise-info {
    display: flex;
    align-items: center;
    gap: 12px;
    flex: 1;
    min-width: 0;
  }

  .lesson-number,
  .exercise-icon {
    flex-shrink: 0;
    display: flex;
    align-items: center;
    justify-content: center;
    width: 32px;
    height: 32px;
    background: #e0e0e0;
    border-radius: 50%;
    font-weight: 700;
    color: #666;
    font-size: 14px;
  }

  .lesson-header.completed .lesson-number {
    background: #7cb342;
    color: white;
  }

  .exercise-header.completed .exercise-icon {
    background: #7cb342;
    color: white;
  }

  .lesson-text,
  .exercise-text {
    flex: 1;
    min-width: 0;
  }

  .lesson-title,
  .exercise-title {
    margin: 0;
    font-size: 14px;
    font-weight: 600;
    color: #2c3e50;
  }

  .lesson-time,
  .exercise-meta {
    margin: 4px 0 0 0;
    font-size: 12px;
    color: #999;
  }

  .difficulty {
    font-weight: 600;
  }

  .separator {
    margin: 0 6px;
  }

  .lesson-status,
  .exercise-status {
    flex-shrink: 0;
    display: flex;
    gap: 12px;
  }

  .badge {
    display: inline-block;
    padding: 4px 12px;
    border-radius: 12px;
    font-size: 11px;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .badge.completed {
    background: #e8f5e9;
    color: #7cb342;
  }

  .badge.pending {
    background: #fff3e0;
    color: #ff9800;
  }

  .lesson-details,
  .exercise-details {
    padding: 16px;
    background: white;
    border-top: 1px solid #e0e0e0;
  }

  .lesson-description,
  .exercise-description {
    margin: 0 0 16px 0;
    font-size: 13px;
    line-height: 1.6;
    color: #666;
  }

  .key-concepts,
  .code-examples {
    margin-bottom: 12px;
  }

  .key-concepts h4,
  .code-examples h4,
  .exercise-info-grid .label {
    margin: 0 0 8px 0;
    font-size: 12px;
    font-weight: 600;
    color: #2c3e50;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .key-concepts ul {
    margin: 0;
    padding-left: 20px;
  }

  .key-concepts li {
    margin: 6px 0;
    font-size: 12px;
    line-height: 1.5;
    color: #666;
  }

  .muted {
    margin: 0;
    font-size: 12px;
    color: #999;
  }

  .exercise-info-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
    gap: 12px;
    margin-bottom: 12px;
  }

  .info-item {
    display: flex;
    flex-direction: column;
    gap: 4px;
  }

  .info-item .value {
    font-size: 13px;
    font-weight: 600;
    color: #2c3e50;
  }

  .hints-section {
    margin-top: 8px;
    padding: 12px;
    background: #f5f5f5;
    border-radius: 4px;
  }

  .hints-section summary {
    cursor: pointer;
    font-weight: 600;
    font-size: 12px;
    color: #2c3e50;
  }

  .hints-section ul {
    margin: 8px 0 0 0;
    padding-left: 20px;
  }

  .hints-section li {
    margin: 6px 0;
    font-size: 12px;
    color: #666;
    line-height: 1.5;
  }

  .empty-state {
    display: flex;
    align-items: center;
    justify-content: center;
    min-height: 200px;
    color: #999;
    font-size: 14px;
  }

  .module-navigation {
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 16px;
    margin-top: 24px;
    padding-top: 24px;
    border-top: 1px solid #e0e0e0;
  }

  .nav-button {
    display: flex;
    align-items: center;
    gap: 8px;
    padding: 10px 16px;
    background: #f5f5f5;
    border: 1px solid #e0e0e0;
    border-radius: 6px;
    cursor: pointer;
    font-size: 13px;
    font-weight: 600;
    color: #2c3e50;
    transition: all 0.2s ease;
  }

  .nav-button:hover {
    background: #e8e8e8;
    border-color: #4a90e2;
    color: #4a90e2;
  }

  .nav-button:active {
    transform: scale(0.98);
  }

  .nav-button svg {
    width: 16px;
    height: 16px;
  }

  .module-indicator {
    flex: 1;
    text-align: center;
    font-size: 12px;
    color: #999;
    text-transform: uppercase;
    letter-spacing: 1px;
    font-weight: 600;
  }

  .loading-state {
    display: flex;
    align-items: center;
    justify-content: center;
    min-height: 300px;
    color: #999;
    font-size: 14px;
  }

  @media (max-width: 768px) {
    .module-header {
      flex-direction: column;
      align-items: flex-start;
    }

    .header-icon {
      width: 80px;
      height: 80px;
    }

    .module-title {
      font-size: 20px;
    }

    .module-stats {
      flex-wrap: wrap;
    }

    .module-navigation {
      flex-direction: column;
    }

    .nav-button {
      width: 100%;
      justify-content: center;
    }
  }
</style>
