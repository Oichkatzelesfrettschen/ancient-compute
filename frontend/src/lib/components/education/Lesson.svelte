<script lang="ts">
/**
 * Lesson.svelte - Detailed lesson view with historical context and code examples
 *
 * Features:
 * - Full lesson content in markdown
 * - Historical context sidebar
 * - Key concepts reference
 * - Code examples with syntax highlighting
 * - Navigation between lessons
 * - Completion tracking
 */

import { currentLesson, previousLesson, nextLesson, markLessonComplete } from '../../stores/timelineStore';
import type { Lesson } from '../../stores/timelineStore';

let lesson: Lesson | null = null;
let expandedExamples: Set<string> = new Set();
let copyFeedback: string | null = null;
let copyFeedbackType: 'success' | 'error' = 'success';
let copyFeedbackTimeout: ReturnType<typeof setTimeout> | null = null;

currentLesson.subscribe((l) => {
  lesson = l;
  copyFeedback = null;
  if (copyFeedbackTimeout) {
    clearTimeout(copyFeedbackTimeout);
    copyFeedbackTimeout = null;
  }
});

function toggleExample(exampleId: string): void {
  const newExpanded = new Set(expandedExamples);
  if (newExpanded.has(exampleId)) {
    newExpanded.delete(exampleId);
  } else {
    newExpanded.add(exampleId);
  }
  expandedExamples = newExpanded;
}

function completeLesson(): void {
  if (lesson) {
    markLessonComplete(lesson.id);
  }
}

function getLanguageColor(language: string): string {
  const colors: Record<string, string> = {
    python: '#3776ab',
    javascript: '#f7df1e',
    typescript: '#3178c6',
    c: '#a8b9cc',
    haskell: '#5e5086',
    lisp: '#daa520',
    java: '#007396',
    rust: '#ce422b',
    go: '#00add8',
    cpp: '#00599c',
  };
  return colors[language.toLowerCase()] || '#999';
}

async function copyCodeToClipboard(code: string): Promise<void> {
  try {
    await navigator.clipboard.writeText(code);
    copyFeedback = 'Code copied to clipboard.';
    copyFeedbackType = 'success';
  } catch (error) {
    console.error('Clipboard copy failed:', error);
    copyFeedback = 'Unable to copy code in this browser context.';
    copyFeedbackType = 'error';
  } finally {
    if (copyFeedbackTimeout) {
      clearTimeout(copyFeedbackTimeout);
    }
    copyFeedbackTimeout = setTimeout(() => {
      copyFeedback = null;
      copyFeedbackTimeout = null;
    }, 2200);
  }
}
</script>

{#if lesson}
  <div class="lesson-container">
    <div class="lesson-layout">
      <!-- Main content -->
      <div class="lesson-content">
        <!-- Lesson header -->
        <header class="lesson-header">
          <h1 class="lesson-title">{lesson.title}</h1>
          <p class="lesson-meta">
            {lesson.estimatedReadTime} minute read
            {#if lesson.prerequisites.length > 0}
              • {lesson.prerequisites.length} prerequisite(s)
            {/if}
          </p>

          <!-- Lesson status and completion -->
          <div class="lesson-status">
            {#if lesson.completed}
              <div class="completion-badge">
                <span class="completion-icon">✓</span>
                <span class="completion-text">Completed</span>
              </div>
            {:else}
              <button class="complete-button" on:click={completeLesson}>
                Mark as Complete
              </button>
            {/if}
          </div>
        </header>

        <!-- Historical context section -->
        {#if lesson.historicalContext}
          <section class="historical-context">
            <h2 class="section-title">📜 Historical Context</h2>
            <div class="context-content">
              {lesson.historicalContext}
            </div>
          </section>
        {/if}

        <!-- Main content (markdown) -->
        <section class="lesson-body">
          <div class="markdown-content">
            {lesson.content}
          </div>
        </section>

        <!-- Key concepts section -->
        {#if lesson.keyConcepts.length > 0}
          <section class="key-concepts-section">
            <h2 class="section-title">🔑 Key Concepts</h2>
            <div class="concepts-grid">
              {#each lesson.keyConcepts as concept (concept.term)}
                <div class="concept-card">
                  <h3 class="concept-term">{concept.term}</h3>
                  <p class="concept-definition">{concept.definition}</p>
                  {#if concept.historicalContext}
                    <p class="concept-context">{concept.historicalContext}</p>
                  {/if}
                </div>
              {/each}
            </div>
          </section>
        {/if}

        <!-- Code examples section -->
        {#if lesson.codeExamples.length > 0}
          <section class="code-examples-section">
            <h2 class="section-title">💻 Code Examples</h2>
            {#if copyFeedback}
              <p class="copy-feedback" class:error={copyFeedbackType === 'error'} aria-live="polite">
                {copyFeedback}
              </p>
            {/if}
            <div class="examples-list">
              {#each lesson.codeExamples as example (example.id)}
                <div class="code-example">
                  <button
                    class="example-header"
                    on:click={() => toggleExample(example.id)}
                  >
                    <div class="example-header-content">
                      <h3 class="example-title">{example.title}</h3>
                      <div class="example-language">
                        <span
                          class="language-badge"
                          style="background-color: {getLanguageColor(example.language)}"
                        >
                          {example.language}
                        </span>
                      </div>
                    </div>
                    <div class="expand-indicator" class:expanded={expandedExamples.has(example.id)}>
                      <svg width="16" height="16" viewBox="0 0 16 16" fill="none">
                        <path
                          d="M6 12L12 8 6 4"
                          stroke="currentColor"
                          stroke-width="1.5"
                          stroke-linecap="round"
                          stroke-linejoin="round"
                        />
                      </svg>
                    </div>
                  </button>

                  <!-- Code display (expanded) -->
                  {#if expandedExamples.has(example.id)}
                    <div class="example-content">
                      <!-- Code block -->
                      <div class="code-block">
                        <pre><code>{example.code}</code></pre>
                        <button
                          class="copy-button"
                          on:click={() => copyCodeToClipboard(example.code)}
                          title="Copy code"
                        >
                          <svg width="16" height="16" viewBox="0 0 16 16" fill="none">
                            <path
                              d="M2 4h8v8H2V4m2-2h8a2 2 0 012 2v8a2 2 0 01-2 2H4a2 2 0 01-2-2V4a2 2 0 012-2z"
                              stroke="currentColor"
                              stroke-width="1"
                              stroke-linecap="round"
                              stroke-linejoin="round"
                            />
                          </svg>
                        </button>
                      </div>

                      <!-- Explanation -->
                      <div class="example-explanation">
                        <h4>Explanation:</h4>
                        <p>{example.explanation}</p>
                      </div>
                    </div>
                  {/if}
                </div>
              {/each}
            </div>
          </section>
        {/if}

        <!-- Navigation -->
        <nav class="lesson-navigation">
          {#if lesson.prerequisites.length > 0}
            <div class="prerequisite-note">
              <span class="note-icon">ℹ️</span>
              <span>Prerequisites: Check course structure for required lessons</span>
            </div>
          {/if}

          <div class="navigation-buttons">
            <button class="nav-button prev" on:click={previousLesson}>
              <svg width="16" height="16" viewBox="0 0 16 16" fill="none">
                <path
                  d="M10 3L5 8l5 5"
                  stroke="currentColor"
                  stroke-width="1.5"
                  stroke-linecap="round"
                  stroke-linejoin="round"
                />
              </svg>
              <span>Previous Lesson</span>
            </button>

            <button class="nav-button next" on:click={nextLesson}>
              <span>Next Lesson</span>
              <svg width="16" height="16" viewBox="0 0 16 16" fill="none">
                <path
                  d="M6 3l5 5-5 5"
                  stroke="currentColor"
                  stroke-width="1.5"
                  stroke-linecap="round"
                  stroke-linejoin="round"
                />
              </svg>
            </button>
          </div>
        </nav>
      </div>

      <!-- Sidebar -->
      <aside class="lesson-sidebar">
        <!-- Lesson info card -->
        <div class="info-card">
          <h3 class="card-title">Lesson Info</h3>
          <div class="info-items">
            <div class="info-item">
              <span class="label">Reading Time</span>
              <span class="value">{lesson.estimatedReadTime} min</span>
            </div>
            <div class="info-item">
              <span class="label">Status</span>
              <span class="value">
                {#if lesson.completed}
                  <span class="badge completed">Completed</span>
                {:else}
                  <span class="badge pending">In Progress</span>
                {/if}
              </span>
            </div>
          </div>
        </div>

        <!-- Table of contents (sections) -->
        <div class="toc-card">
          <h3 class="card-title">Contents</h3>
          <div class="toc-list">
            <a href="#content" class="toc-item">
              <span class="toc-label">Content</span>
            </a>
            {#if lesson.keyConcepts.length > 0}
              <a href="#concepts" class="toc-item">
                <span class="toc-label">Key Concepts</span>
                <span class="toc-count">({lesson.keyConcepts.length})</span>
              </a>
            {/if}
            {#if lesson.codeExamples.length > 0}
              <a href="#examples" class="toc-item">
                <span class="toc-label">Code Examples</span>
                <span class="toc-count">({lesson.codeExamples.length})</span>
              </a>
            {/if}
          </div>
        </div>

        <!-- Next lessons preview -->
        {#if lesson.nextLessons.length > 0}
          <div class="preview-card">
            <h3 class="card-title">What's Next</h3>
            <p class="preview-text">After completing this lesson, you can explore:</p>
            <ul class="preview-list">
              {#each lesson.nextLessons.slice(0, 3) as lessonId}
                <li>{lessonId}</li>
              {/each}
            </ul>
          </div>
        {/if}

        <!-- Keyboard shortcuts -->
        <div class="shortcuts-card">
          <h3 class="card-title">Shortcuts</h3>
          <div class="shortcuts-list">
            <div class="shortcut">
              <span class="key">←</span>
              <span class="label">Previous</span>
            </div>
            <div class="shortcut">
              <span class="key">→</span>
              <span class="label">Next</span>
            </div>
            <div class="shortcut">
              <span class="key">M</span>
              <span class="label">Mark complete</span>
            </div>
          </div>
        </div>
      </aside>
    </div>
  </div>
{:else}
  <div class="loading-state">
    <p>Loading lesson...</p>
  </div>
{/if}

<style>
  .lesson-container {
    padding: 24px;
    background: white;
    border-radius: 8px;
  }

  .lesson-layout {
    display: grid;
    grid-template-columns: 1fr 280px;
    gap: 40px;
  }

  .lesson-content {
    display: flex;
    flex-direction: column;
    gap: 32px;
  }

  .lesson-header {
    display: flex;
    flex-direction: column;
    gap: 12px;
    padding-bottom: 24px;
    border-bottom: 2px solid #e0e0e0;
  }

  .lesson-title {
    margin: 0;
    font-size: 32px;
    font-weight: 700;
    color: #2c3e50;
    line-height: 1.3;
  }

  .lesson-meta {
    margin: 0;
    font-size: 13px;
    color: #999;
  }

  .lesson-status {
    display: flex;
    gap: 12px;
    align-items: center;
    margin-top: 12px;
  }

  .completion-badge {
    display: flex;
    align-items: center;
    gap: 8px;
    padding: 8px 16px;
    background: #e8f5e9;
    border-radius: 6px;
    color: #7cb342;
    font-weight: 600;
    font-size: 13px;
  }

  .completion-icon {
    font-size: 16px;
  }

  .complete-button {
    padding: 8px 16px;
    background: #4a90e2;
    color: white;
    border: none;
    border-radius: 6px;
    cursor: pointer;
    font-weight: 600;
    font-size: 13px;
    transition: all 0.2s ease;
  }

  .complete-button:hover {
    background: #357ace;
  }

  .historical-context {
    padding: 20px;
    background: #f5f7fa;
    border-left: 4px solid #4a90e2;
    border-radius: 6px;
  }

  .section-title {
    margin: 0 0 12px 0;
    font-size: 16px;
    font-weight: 700;
    color: #2c3e50;
  }

  .context-content {
    margin: 0;
    font-size: 13px;
    line-height: 1.8;
    color: #666;
  }

  .lesson-body {
    display: flex;
    flex-direction: column;
    gap: 16px;
  }

  .markdown-content {
    font-size: 14px;
    line-height: 1.8;
    color: #444;
  }

  .key-concepts-section {
    display: flex;
    flex-direction: column;
    gap: 16px;
  }

  .concepts-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(250px, 1fr));
    gap: 12px;
  }

  .concept-card {
    padding: 16px;
    background: #f9f9f9;
    border: 1px solid #e0e0e0;
    border-radius: 6px;
    transition: all 0.2s ease;
  }

  .concept-card:hover {
    border-color: #4a90e2;
    box-shadow: 0 2px 8px rgba(74, 144, 226, 0.1);
  }

  .concept-term {
    margin: 0 0 8px 0;
    font-size: 13px;
    font-weight: 700;
    color: #2c3e50;
  }

  .concept-definition {
    margin: 0 0 6px 0;
    font-size: 12px;
    line-height: 1.6;
    color: #666;
  }

  .concept-context {
    margin: 0;
    font-size: 11px;
    line-height: 1.5;
    color: #999;
    font-style: italic;
  }

  .code-examples-section {
    display: flex;
    flex-direction: column;
    gap: 16px;
  }

  .copy-feedback {
    margin: 0;
    padding: 8px 12px;
    border-radius: 6px;
    font-size: 12px;
    font-weight: 600;
    background: #e8f5e9;
    color: #2e7d32;
  }

  .copy-feedback.error {
    background: #ffebee;
    color: #b71c1c;
  }

  .examples-list {
    display: flex;
    flex-direction: column;
    gap: 12px;
  }

  .code-example {
    border: 1px solid #e0e0e0;
    border-radius: 6px;
    overflow: hidden;
  }

  .example-header {
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

  .example-header:hover {
    background: #f0f0f0;
  }

  .example-header-content {
    display: flex;
    align-items: center;
    gap: 12px;
    flex: 1;
  }

  .example-title {
    margin: 0;
    font-size: 14px;
    font-weight: 600;
    color: #2c3e50;
  }

  .example-language {
    display: flex;
    gap: 8px;
  }

  .language-badge {
    padding: 2px 8px;
    border-radius: 4px;
    color: white;
    font-size: 11px;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .expand-indicator {
    flex-shrink: 0;
    color: #999;
    transition: transform 0.2s ease;
  }

  .expand-indicator.expanded {
    transform: rotate(90deg);
  }

  .example-content {
    padding: 0;
    background: white;
    border-top: 1px solid #e0e0e0;
    display: flex;
    flex-direction: column;
  }

  .code-block {
    position: relative;
    padding: 16px;
    background: #f5f5f5;
    overflow-x: auto;
  }

  .code-block pre {
    margin: 0;
    font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
    font-size: 12px;
    line-height: 1.5;
    color: #333;
  }

  .code-block code {
    display: block;
  }

  .copy-button {
    position: absolute;
    top: 8px;
    right: 8px;
    width: 32px;
    height: 32px;
    display: flex;
    align-items: center;
    justify-content: center;
    background: white;
    border: 1px solid #e0e0e0;
    border-radius: 4px;
    cursor: pointer;
    color: #999;
    transition: all 0.2s ease;
  }

  .copy-button:hover {
    background: #f0f0f0;
    border-color: #4a90e2;
    color: #4a90e2;
  }

  .example-explanation {
    padding: 16px;
    background: white;
    border-top: 1px solid #e0e0e0;
  }

  .example-explanation h4 {
    margin: 0 0 8px 0;
    font-size: 12px;
    font-weight: 700;
    color: #2c3e50;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .example-explanation p {
    margin: 0;
    font-size: 13px;
    line-height: 1.6;
    color: #666;
  }

  .lesson-navigation {
    display: flex;
    flex-direction: column;
    gap: 16px;
    margin-top: 24px;
    padding-top: 24px;
    border-top: 1px solid #e0e0e0;
  }

  .prerequisite-note {
    display: flex;
    align-items: center;
    gap: 12px;
    padding: 12px;
    background: #fff3e0;
    border-radius: 6px;
    font-size: 12px;
    color: #e65100;
  }

  .note-icon {
    flex-shrink: 0;
    font-size: 16px;
  }

  .navigation-buttons {
    display: flex;
    gap: 12px;
    justify-content: space-between;
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

  .nav-button svg {
    width: 14px;
    height: 14px;
  }

  .lesson-sidebar {
    display: flex;
    flex-direction: column;
    gap: 24px;
    height: fit-content;
    position: sticky;
    top: 24px;
  }

  .info-card,
  .toc-card,
  .preview-card,
  .shortcuts-card {
    padding: 16px;
    background: #f9f9f9;
    border: 1px solid #e0e0e0;
    border-radius: 6px;
  }

  .card-title {
    margin: 0 0 12px 0;
    font-size: 12px;
    font-weight: 700;
    color: #2c3e50;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .info-items {
    display: flex;
    flex-direction: column;
    gap: 12px;
  }

  .info-item {
    display: flex;
    justify-content: space-between;
    align-items: center;
  }

  .info-item .label {
    font-size: 12px;
    color: #999;
  }

  .info-item .value {
    font-size: 13px;
    font-weight: 600;
    color: #2c3e50;
  }

  .badge {
    display: inline-block;
    padding: 2px 8px;
    border-radius: 4px;
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

  .toc-list {
    display: flex;
    flex-direction: column;
    gap: 8px;
  }

  .toc-item {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 8px 0;
    color: #4a90e2;
    text-decoration: none;
    font-size: 12px;
    cursor: pointer;
    transition: color 0.2s ease;
  }

  .toc-item:hover {
    color: #357ace;
  }

  .toc-label {
    font-weight: 500;
  }

  .toc-count {
    font-size: 11px;
    color: #999;
  }

  .preview-text {
    margin: 0 0 8px 0;
    font-size: 12px;
    color: #666;
  }

  .preview-list {
    margin: 0;
    padding-left: 16px;
  }

  .preview-list li {
    font-size: 11px;
    color: #666;
    margin: 4px 0;
  }

  .shortcuts-list {
    display: flex;
    flex-direction: column;
    gap: 8px;
  }

  .shortcut {
    display: flex;
    align-items: center;
    gap: 8px;
    font-size: 12px;
  }

  .key {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 24px;
    height: 24px;
    background: white;
    border: 1px solid #ccc;
    border-radius: 3px;
    font-weight: 600;
    color: #333;
    font-size: 11px;
  }

  .label {
    color: #666;
  }

  .loading-state {
    display: flex;
    align-items: center;
    justify-content: center;
    min-height: 400px;
    color: #999;
  }

  @media (max-width: 1024px) {
    .lesson-layout {
      grid-template-columns: 1fr;
    }

    .lesson-sidebar {
      position: static;
    }
  }

  @media (max-width: 768px) {
    .lesson-container {
      padding: 16px;
    }

    .lesson-title {
      font-size: 24px;
    }

    .concepts-grid {
      grid-template-columns: 1fr;
    }

    .navigation-buttons {
      flex-direction: column;
    }

    .nav-button {
      width: 100%;
      justify-content: center;
    }
  }
</style>
