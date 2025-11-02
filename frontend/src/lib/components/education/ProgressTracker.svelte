<script lang="ts">
/**
 * ProgressTracker.svelte - Progress visualization dashboard
 *
 * Features:
 * - Central progress ring showing overall completion
 * - Era progress rings arranged in circular layout
 * - Timeline of recent lesson/exercise completions
 * - Mastery level badges and achievements
 * - Statistics dashboard (lessons, exercises, time spent)
 * - Responsive design for all device sizes
 */

import { overallProgress, erasList, timelineStore } from '../../stores/timelineStore';
import type { Lesson, Exercise } from '../../stores/timelineStore';

interface CompletionEvent {
  id: string;
  type: 'lesson' | 'exercise';
  title: string;
  completedAt: string;
  eraLabel: string;
  icon: string;
  score?: number;
}

interface MasteryBadge {
  level: number;
  label: string;
  threshold: number;
  color: string;
  icon: string;
}

// Mastery levels based on completion percentage
const masteryLevels: MasteryBadge[] = [
  { level: 0, label: 'Novice', threshold: 0, color: '#9e9e9e', icon: '○' },
  { level: 1, label: 'Apprentice', threshold: 25, color: '#4caf50', icon: '◐' },
  { level: 2, label: 'Journeyman', threshold: 50, color: '#2196f3', icon: '◑' },
  { level: 3, label: 'Expert', threshold: 75, color: '#ff9800', icon: '◕' },
  { level: 4, label: 'Master', threshold: 100, color: '#e91e63', icon: '●' },
];

// Calculate mastery level from progress percentage
function getMasteryLevel(progress: number): MasteryBadge {
  return (
    masteryLevels.slice().reverse().find((m) => progress >= m.threshold) || masteryLevels[0]
  );
}

// Get recent completion events
function getRecentCompletions(state: any): CompletionEvent[] {
  const events: CompletionEvent[] = [];

  state.eras.forEach((era) => {
    const eraLabel = era.label;
    const eraColor = era.color;

    era.modules.forEach((module) => {
      module.lessons?.forEach((lesson: Lesson) => {
        if (lesson.completed && lesson.completedAt) {
          events.push({
            id: lesson.id,
            type: 'lesson',
            title: lesson.title,
            completedAt: lesson.completedAt,
            eraLabel,
            icon: '📖',
            score: undefined,
          });
        }
      });

      module.exercises?.forEach((exercise: Exercise) => {
        if (exercise.completed && exercise.bestScore !== undefined) {
          events.push({
            id: exercise.id,
            type: 'exercise',
            title: exercise.title,
            completedAt: exercise.completedAt || new Date().toISOString(),
            eraLabel,
            icon: '💻',
            score: exercise.bestScore,
          });
        }
      });
    });
  });

  // Sort by date descending (most recent first)
  return events.sort((a, b) => new Date(b.completedAt).getTime() - new Date(a.completedAt).getTime()).slice(0, 8);
}

// Calculate statistics
function calculateStats(state: any) {
  let totalLessons = 0;
  let completedLessons = 0;
  let totalExercises = 0;
  let completedExercises = 0;
  let totalHours = 0;

  state.eras.forEach((era) => {
    era.modules.forEach((module) => {
      totalLessons += module.lessons?.length || 0;
      completedLessons += module.lessons?.filter((l: Lesson) => l.completed).length || 0;

      totalExercises += module.exercises?.length || 0;
      completedExercises += module.exercises?.filter((e: Exercise) => e.completed).length || 0;

      // Estimate time: lessons (avg 15 min) + exercises (avg 45 min)
      totalHours += (module.lessons?.length || 0) * 0.25 + (module.exercises?.length || 0) * 0.75;
    });
  });

  return {
    totalLessons,
    completedLessons,
    totalExercises,
    completedExercises,
    totalHours: Math.round(totalHours),
  };
}

// Format date for timeline
function formatDate(dateStr: string): string {
  const date = new Date(dateStr);
  const today = new Date();
  const yesterday = new Date(today);
  yesterday.setDate(yesterday.getDate() - 1);

  if (date.toDateString() === today.toDateString()) {
    return `Today ${date.toLocaleTimeString('en-US', { hour: 'numeric', minute: '2-digit' })}`;
  } else if (date.toDateString() === yesterday.toDateString()) {
    return 'Yesterday';
  } else {
    return date.toLocaleDateString('en-US', { month: 'short', day: 'numeric' });
  }
}

// Reactive state
$: state = $timelineStore;
$: recentCompletions = getRecentCompletions(state);
$: stats = calculateStats(state);
$: currentMastery = getMasteryLevel($overallProgress);
$: eraProgresses = state.eras.map((era) => {
  const totalLessons = era.modules.reduce((sum, m) => sum + (m.lessons?.length || 0), 0);
  const completedLessons = era.modules.reduce((sum, m) => sum + (m.lessons?.filter((l: Lesson) => l.completed).length || 0), 0);
  return {
    id: era.id,
    label: era.label,
    color: era.color,
    progress: totalLessons > 0 ? Math.round((completedLessons / totalLessons) * 100) : 0,
  };
});
</script>

<div class="progress-tracker">
  <!-- Header -->
  <div class="tracker-header">
    <h2>Learning Progress</h2>
    <p class="subtitle">Your journey through computational history</p>
  </div>

  <!-- Main Grid -->
  <div class="tracker-grid">
    <!-- Left Column: Progress Visualization -->
    <div class="column left">
      <!-- Overall Progress Ring -->
      <div class="progress-ring-card">
        <div class="ring-container">
          <!-- SVG Circle Progress -->
          <svg class="progress-circle" viewBox="0 0 120 120">
            <!-- Background circle -->
            <circle cx="60" cy="60" r="54" fill="none" stroke="#e0e0e0" stroke-width="4" />

            <!-- Progress circle (animated) -->
            <circle
              cx="60"
              cy="60"
              r="54"
              fill="none"
              stroke={currentMastery.color}
              stroke-width="4"
              stroke-linecap="round"
              stroke-dasharray={`${(Math.PI * 2 * 54 * $overallProgress) / 100} ${Math.PI * 2 * 54}`}
              stroke-dashoffset="0"
              class="progress-stroke"
            />
          </svg>

          <!-- Center text -->
          <div class="ring-content">
            <div class="progress-percent">{Math.round($overallProgress)}%</div>
            <div class="progress-label">Complete</div>
          </div>
        </div>

        <!-- Mastery Badge -->
        <div class="mastery-badge" style="background-color: {currentMastery.color}">
          <div class="badge-icon">{currentMastery.icon}</div>
          <div class="badge-text">
            <div class="badge-level">{currentMastery.label}</div>
            <div class="badge-next">
              {#if currentMastery.level < 4}
                {masteryLevels[currentMastery.level + 1].threshold}% to next
              {:else}
                Maximum mastery!
              {/if}
            </div>
          </div>
        </div>
      </div>

      <!-- Statistics Cards -->
      <div class="stats-grid">
        <div class="stat-card">
          <div class="stat-icon">📖</div>
          <div class="stat-content">
            <div class="stat-value">{stats.completedLessons}/{stats.totalLessons}</div>
            <div class="stat-label">Lessons</div>
          </div>
        </div>

        <div class="stat-card">
          <div class="stat-icon">💻</div>
          <div class="stat-content">
            <div class="stat-value">{stats.completedExercises}/{stats.totalExercises}</div>
            <div class="stat-label">Exercises</div>
          </div>
        </div>

        <div class="stat-card">
          <div class="stat-icon">⏱️</div>
          <div class="stat-content">
            <div class="stat-value">{stats.totalHours}h</div>
            <div class="stat-label">Estimated</div>
          </div>
        </div>

        <div class="stat-card">
          <div class="stat-icon">⭐</div>
          <div class="stat-content">
            <div class="stat-value">{eraProgresses.filter((e) => e.progress === 100).length}/{eraProgresses.length}</div>
            <div class="stat-label">Eras Done</div>
          </div>
        </div>
      </div>
    </div>

    <!-- Right Column: Timeline & Era Progress -->
    <div class="column right">
      <!-- Era Progress Grid -->
      <div class="era-progress-card">
        <h3 class="card-title">Era Progress</h3>
        <div class="era-grid">
          {#each eraProgresses as era (era.id)}
            <div class="era-progress-item">
              <div class="era-header">
                <div class="era-indicator" style="background-color: {era.color}"></div>
                <span class="era-name">{era.label}</span>
              </div>
              <div class="progress-bar">
                <div class="progress-fill" style="width: {era.progress}%; background-color: {era.color}"></div>
              </div>
              <div class="progress-text">{era.progress}%</div>
            </div>
          {/each}
        </div>
      </div>

      <!-- Recent Activity Timeline -->
      <div class="timeline-card">
        <h3 class="card-title">Recent Activity</h3>
        <div class="timeline">
          {#if recentCompletions.length > 0}
            {#each recentCompletions as event (event.id)}
              <div class="timeline-item">
                <div class="timeline-marker" style="color: {eraProgresses.find((e) => e.label === event.eraLabel)?.color || '#999'}">
                  {event.icon}
                </div>
                <div class="timeline-content">
                  <div class="timeline-title">{event.title}</div>
                  <div class="timeline-meta">
                    <span class="era-badge">{event.eraLabel}</span>
                    {#if event.score !== undefined}
                      <span class="score-badge">{event.score}%</span>
                    {/if}
                    <span class="date-badge">{formatDate(event.completedAt)}</span>
                  </div>
                </div>
              </div>
            {/each}
          {:else}
            <div class="empty-state">
              <div class="empty-icon">📚</div>
              <p>Complete lessons and exercises to see your progress!</p>
            </div>
          {/if}
        </div>
      </div>
    </div>
  </div>
</div>

<style>
  .progress-tracker {
    display: flex;
    flex-direction: column;
    gap: 32px;
    padding: 24px;
    background: linear-gradient(135deg, #fafafa 0%, #f5f5f5 100%);
    border-radius: 12px;
    min-height: 600px;
  }

  .tracker-header {
    text-align: center;
  }

  .tracker-header h2 {
    margin: 0;
    font-size: 28px;
    font-weight: 700;
    color: #2c3e50;
  }

  .subtitle {
    margin: 8px 0 0 0;
    font-size: 14px;
    color: #999;
  }

  /* Grid Layout */
  .tracker-grid {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 24px;
  }

  .column {
    display: flex;
    flex-direction: column;
    gap: 24px;
  }

  /* Progress Ring Card */
  .progress-ring-card {
    background: white;
    border-radius: 12px;
    padding: 24px;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 20px;
  }

  .ring-container {
    position: relative;
    width: 160px;
    height: 160px;
  }

  .progress-circle {
    width: 100%;
    height: 100%;
    transform: rotate(-90deg);
  }

  .progress-stroke {
    transition: stroke-dasharray 0.5s ease, stroke 0.3s ease;
  }

  .ring-content {
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    text-align: center;
  }

  .progress-percent {
    font-size: 36px;
    font-weight: 700;
    color: #2c3e50;
  }

  .progress-label {
    font-size: 12px;
    color: #999;
    margin-top: 4px;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  /* Mastery Badge */
  .mastery-badge {
    width: 100%;
    padding: 16px;
    border-radius: 8px;
    display: flex;
    align-items: center;
    gap: 12px;
    color: white;
    font-weight: 600;
    transition: transform 0.2s ease;
  }

  .mastery-badge:hover {
    transform: scale(1.02);
  }

  .badge-icon {
    font-size: 24px;
  }

  .badge-text {
    flex: 1;
  }

  .badge-level {
    font-size: 14px;
    font-weight: 700;
  }

  .badge-next {
    font-size: 11px;
    opacity: 0.9;
    margin-top: 2px;
  }

  /* Statistics Grid */
  .stats-grid {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 12px;
  }

  .stat-card {
    background: white;
    border-radius: 8px;
    padding: 16px;
    display: flex;
    align-items: center;
    gap: 12px;
    box-shadow: 0 1px 4px rgba(0, 0, 0, 0.06);
    transition: transform 0.2s ease;
  }

  .stat-card:hover {
    transform: translateY(-2px);
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
  }

  .stat-icon {
    font-size: 24px;
    flex-shrink: 0;
  }

  .stat-content {
    flex: 1;
  }

  .stat-value {
    font-size: 16px;
    font-weight: 700;
    color: #2c3e50;
  }

  .stat-label {
    font-size: 11px;
    color: #999;
    margin-top: 2px;
    text-transform: uppercase;
    letter-spacing: 0.3px;
  }

  /* Era Progress Card */
  .era-progress-card,
  .timeline-card {
    background: white;
    border-radius: 12px;
    padding: 24px;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.08);
  }

  .card-title {
    margin: 0 0 16px 0;
    font-size: 16px;
    font-weight: 700;
    color: #2c3e50;
  }

  .era-grid {
    display: flex;
    flex-direction: column;
    gap: 12px;
  }

  .era-progress-item {
    padding: 12px;
    background: #fafafa;
    border-radius: 8px;
  }

  .era-header {
    display: flex;
    align-items: center;
    gap: 8px;
    margin-bottom: 8px;
  }

  .era-indicator {
    width: 12px;
    height: 12px;
    border-radius: 3px;
    flex-shrink: 0;
  }

  .era-name {
    font-size: 13px;
    font-weight: 600;
    color: #2c3e50;
  }

  .progress-bar {
    width: 100%;
    height: 6px;
    background: #e0e0e0;
    border-radius: 3px;
    overflow: hidden;
    margin-bottom: 4px;
  }

  .progress-fill {
    height: 100%;
    border-radius: 3px;
    transition: width 0.3s ease;
  }

  .progress-text {
    font-size: 11px;
    color: #999;
    text-align: right;
  }

  /* Timeline */
  .timeline {
    display: flex;
    flex-direction: column;
    gap: 12px;
  }

  .timeline-item {
    display: flex;
    gap: 12px;
    padding: 12px;
    background: #fafafa;
    border-radius: 8px;
    transition: background 0.2s ease;
  }

  .timeline-item:hover {
    background: #f0f0f0;
  }

  .timeline-marker {
    font-size: 20px;
    flex-shrink: 0;
    width: 28px;
    height: 28px;
    display: flex;
    align-items: center;
    justify-content: center;
  }

  .timeline-content {
    flex: 1;
    min-width: 0;
  }

  .timeline-title {
    font-size: 13px;
    font-weight: 600;
    color: #2c3e50;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
  }

  .timeline-meta {
    display: flex;
    gap: 8px;
    margin-top: 4px;
    flex-wrap: wrap;
  }

  .era-badge,
  .score-badge,
  .date-badge {
    font-size: 10px;
    padding: 2px 6px;
    background: white;
    border-radius: 3px;
    color: #666;
    white-space: nowrap;
  }

  .score-badge {
    background: #e8f5e9;
    color: #2e7d32;
    font-weight: 600;
  }

  /* Empty State */
  .empty-state {
    text-align: center;
    padding: 32px 16px;
    color: #999;
  }

  .empty-icon {
    font-size: 48px;
    margin-bottom: 12px;
  }

  .empty-state p {
    margin: 0;
    font-size: 13px;
  }

  /* Responsive Design */
  @media (max-width: 1024px) {
    .tracker-grid {
      grid-template-columns: 1fr;
    }

    .stats-grid {
      grid-template-columns: 1fr 1fr 1fr 1fr;
    }

    .stat-card {
      flex-direction: column;
      text-align: center;
      gap: 8px;
    }

    .stat-content {
      width: 100%;
    }
  }

  @media (max-width: 768px) {
    .progress-tracker {
      padding: 16px;
      gap: 20px;
    }

    .tracker-header h2 {
      font-size: 22px;
    }

    .stats-grid {
      grid-template-columns: 1fr 1fr;
    }

    .stat-card {
      flex-direction: row;
      text-align: left;
    }

    .progress-ring-card {
      padding: 16px;
    }

    .ring-container {
      width: 120px;
      height: 120px;
    }

    .progress-percent {
      font-size: 28px;
    }

    .progress-bar {
      height: 4px;
    }
  }
</style>
