<script lang="ts">
/**
 * Exercise.svelte - Interactive code exercise with execution and testing
 *
 * Features:
 * - Multi-language code editor support
 * - Code execution with test case validation
 * - Real-time result display
 * - Hint system
 * - Difficulty visualization
 * - Solution comparison
 */

import { markExerciseComplete } from '../../stores/timelineStore';
import type { Exercise } from '../../stores/timelineStore';

interface ExecutionResult {
  testCaseId: string;
  passed: boolean;
  actualOutput: string;
  expectedOutput: string;
  executionTime: number;
  error?: string;
}

interface ExerciseState {
  isRunning: boolean;
  results: ExecutionResult[];
  userCode: string;
  selectedLanguage: string;
  showHints: boolean;
  showSolution: boolean;
  completionScore: number;
}

interface CodeExecutionResponse {
  status: string;
  stdout: string;
  stderr: string;
  compile_output?: string | null;
  execution_time: number;
  memory_used: number;
}

interface ApiErrorResponse {
  detail?: string;
  message?: string;
}

export let exercise: Exercise;

const CODE_EXECUTION_ENDPOINT = '/api/v1/execute/run';
const SUPPORTED_BACKEND_LANGUAGES = new Set([
  'c',
  'python',
  'haskell',
  'idris',
  'lisp',
  'assembly',
  'java',
  'systemf',
]);
const LANGUAGE_ALIASES: Record<string, string> = {
  idris2: 'idris',
  'idris 2': 'idris',
  'system f': 'systemf',
  system_f: 'systemf',
};

const initialLanguage = exercise.languages[0] ?? 'python';

let state: ExerciseState = {
  isRunning: false,
  results: [],
  userCode: getDefaultTemplate(initialLanguage),
  selectedLanguage: initialLanguage,
  showHints: false,
  showSolution: false,
  completionScore: 0,
};

function getDefaultTemplate(language: string): string {
  const templates: Record<string, string> = {
    python: '# Write your solution here\ndef solve():\n    pass\n\n# Test your solution\nprint(solve())',
    javascript:
      '// Write your solution here\nfunction solve() {\n    // TODO\n}\n\n// Test your solution\nconsole.log(solve());',
    typescript:
      '// Write your solution here\nfunction solve(): any {\n    // TODO\n}\n\n// Test your solution\nconsole.log(solve());',
    java: 'public class Solution {\n    public static Object solve() {\n        // TODO\n        return null;\n    }\n}\n',
    c: '#include <stdio.h>\n\nint solve() {\n    // TODO\n    return 0;\n}\n\nint main() {\n    printf("%d\\n", solve());\n    return 0;\n}\n',
    haskell: 'solve :: Int\nsolve = 0  -- TODO\n\nmain :: IO ()\nmain = print solve\n',
    lisp: '(defun solve ()\n  ;; TODO\n  nil)\n\n(print (solve))\n',
    rust: 'fn solve() -> i32 {\n    // TODO\n    0\n}\n\nfn main() {\n    println!("{}", solve());\n}\n',
  };

  return templates[language] || '// Write your solution here\n';
}

function handleLanguageChange(newLanguage: string): void {
  state.selectedLanguage = newLanguage;
  state.userCode = getDefaultTemplate(newLanguage);
}

function normalizeLanguage(language: string): string | null {
  const normalized = language.trim().toLowerCase();
  if (SUPPORTED_BACKEND_LANGUAGES.has(normalized)) {
    return normalized;
  }

  return LANGUAGE_ALIASES[normalized] ?? null;
}

function normalizeOutput(output: string): string {
  return output.replace(/\r\n/g, '\n').trim();
}

function buildExecutionError(response: CodeExecutionResponse): string {
  const compileOutput = response.compile_output?.trim();
  const stderr = response.stderr.trim();

  if (compileOutput) {
    return compileOutput;
  }
  if (stderr) {
    return stderr;
  }
  return `Execution failed (${response.status})`;
}

async function runTests(): Promise<void> {
  state.isRunning = true;
  state.results = [];

  try {
    const language = normalizeLanguage(state.selectedLanguage);
    if (!language) {
      state.results = exercise.testCases.map((testCase) => ({
        testCaseId: testCase.id,
        passed: false,
        actualOutput: '',
        expectedOutput: testCase.expectedOutput,
        executionTime: 0,
        error: `Language "${state.selectedLanguage}" is not supported by backend execution`,
      }));
      return;
    }

    const results: ExecutionResult[] = [];
    for (const testCase of exercise.testCases) {
      const response = await fetch(CODE_EXECUTION_ENDPOINT, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          language,
          code: state.userCode,
          input_data: testCase.input,
        }),
      });

      if (!response.ok) {
        let errorMessage = `HTTP ${response.status}`;
        try {
          const payload = (await response.json()) as ApiErrorResponse;
          errorMessage =
            payload.detail?.toString() ?? payload.message?.toString() ?? errorMessage;
        } catch {
          // Ignore JSON parsing errors and keep generic HTTP message.
        }

        results.push({
          testCaseId: testCase.id,
          passed: false,
          actualOutput: '',
          expectedOutput: testCase.expectedOutput,
          executionTime: 0,
          error: errorMessage,
        });
        continue;
      }

      const execution = (await response.json()) as CodeExecutionResponse;
      const actualOutput = normalizeOutput(execution.stdout);
      const expectedOutput = normalizeOutput(testCase.expectedOutput);
      const passed = execution.status === 'success' && actualOutput === expectedOutput;

      results.push({
        testCaseId: testCase.id,
        passed,
        actualOutput,
        expectedOutput: testCase.expectedOutput,
        executionTime: execution.execution_time * 1000,
        error: passed ? undefined : buildExecutionError(execution),
      });
    }

    state.results = results;

    // Calculate completion score
    const passedTests = results.filter((r) => r.passed).length;
    const totalTests = results.length;
    const completionScore = totalTests > 0 ? Math.round((passedTests / totalTests) * 100) : 0;
    state.completionScore = completionScore;

    // Mark as complete if all tests pass
    if (completionScore === 100 && totalTests > 0) {
      markExerciseComplete(exercise.id, completionScore);
    }
  } catch (error) {
    console.error('Exercise execution error:', error);
    state.results = exercise.testCases.map((testCase) => ({
      testCaseId: testCase.id,
      passed: false,
      actualOutput: '',
      expectedOutput: testCase.expectedOutput,
      executionTime: 0,
      error: error instanceof Error ? error.message : 'Unknown execution error',
    }));
  } finally {
    state.isRunning = false;
  }
}

function getDifficultyColor(difficulty: string): string {
  const colors: Record<string, string> = {
    beginner: '#7cb342',
    intermediate: '#ff9800',
    advanced: '#e53935',
  };
  return colors[difficulty] || '#999';
}

function getTestPassPercentage(): number {
  if (state.results.length === 0) return 0;
  const passed = state.results.filter((r) => r.passed).length;
  return Math.round((passed / state.results.length) * 100);
}
</script>

<div class="exercise-container">
  <!-- Exercise header -->
  <header class="exercise-header">
    <div class="header-content">
      <h1 class="exercise-title">{exercise.title}</h1>
      <p class="exercise-description">{exercise.description}</p>

      <!-- Difficulty and metadata -->
      <div class="exercise-meta">
        <span
          class="difficulty-badge"
          style="color: {getDifficultyColor(exercise.difficulty)};
                 border-color: {getDifficultyColor(exercise.difficulty)}"
        >
          {exercise.difficulty.charAt(0).toUpperCase() + exercise.difficulty.slice(1)}
        </span>
        <span class="separator">•</span>
        <span class="languages">{exercise.languages.join(', ')}</span>
        <span class="separator">•</span>
        <span class="constraints">
          {exercise.timeLimit}s • {exercise.memoryLimit} MB
        </span>
      </div>
    </div>

    <!-- Completion indicator -->
    {#if exercise.completed}
      <div class="completion-badge">
        <span class="icon">✓</span>
        <span class="text">Completed {exercise.bestScore ?? 0}%</span>
      </div>
    {/if}
  </header>

  <!-- Problem statement -->
  <section class="problem-statement">
    <h2>Problem Statement</h2>
    <div class="problem-content">
      {exercise.problem}
    </div>
  </section>

  <!-- Main layout: code editor + test results -->
  <div class="exercise-layout">
    <!-- Code editor area -->
    <div class="editor-section">
      <!-- Language selector -->
      <div class="language-selector">
        <label for="language-select">Language:</label>
        <select
          id="language-select"
          value={state.selectedLanguage}
          on:change={(e) => handleLanguageChange(e.currentTarget.value)}
        >
          {#each exercise.languages as lang}
            <option value={lang}>{lang}</option>
          {/each}
        </select>
      </div>

      <!-- Code editor (textarea for MVP) -->
      <div class="code-editor">
        <textarea
          bind:value={state.userCode}
          placeholder="Write your solution here..."
          spellcheck="false"
        ></textarea>
      </div>

      <!-- Run tests button -->
      <button
        class="run-button"
        on:click={runTests}
        disabled={state.isRunning}
      >
        {#if state.isRunning}
          <span class="spinner"></span>
          Running Tests...
        {:else}
          <span>Run Tests</span>
        {/if}
      </button>
    </div>

    <!-- Test results area -->
    <div class="results-section">
      {#if state.results.length > 0}
        <div class="results-header">
          <h3>Test Results</h3>
          <div class="results-summary">
            <div class="summary-item">
              <span class="summary-label">Passed:</span>
              <span class="summary-value passed">
                {state.results.filter((r) => r.passed).length}/{state.results.length}
              </span>
            </div>
            <div class="summary-item">
              <span class="summary-label">Score:</span>
              <span class="summary-value">{getTestPassPercentage()}%</span>
            </div>
          </div>
        </div>

        <!-- Test case list -->
        <div class="test-cases">
          {#each state.results as result (result.testCaseId)}
            {@const testCase = exercise.testCases.find(
              (t) => t.id === result.testCaseId
            )}
            <div class="test-case" class:passed={result.passed}>
              <div class="test-case-header">
                <span class="test-icon">{result.passed ? '✓' : '✗'}</span>
                <span class="test-name">{testCase?.name || result.testCaseId}</span>
                <span class="test-time">{result.executionTime.toFixed(2)}ms</span>
              </div>

              {#if !result.passed}
                <div class="test-details">
                  <div class="detail-item">
                    <span class="detail-label">Expected:</span>
                    <pre class="detail-value">{result.expectedOutput}</pre>
                  </div>
                  <div class="detail-item">
                    <span class="detail-label">Got:</span>
                    <pre class="detail-value">{result.actualOutput}</pre>
                  </div>
                  {#if result.error}
                    <div class="detail-item">
                      <span class="detail-label">Error:</span>
                      <pre class="detail-value error">{result.error}</pre>
                    </div>
                  {/if}
                </div>
              {/if}
            </div>
          {/each}
        </div>
      {:else}
        <div class="empty-results">
          <p>Run your code to see test results</p>
        </div>
      {/if}
    </div>
  </div>

  <!-- Hints and solution -->
  <div class="hints-solution">
    {#if exercise.hints.length > 0}
      <details class="hints-section" bind:open={state.showHints}>
        <summary class="collapsible-title">
          💡 Hints ({exercise.hints.length})
        </summary>
        <div class="hints-content">
          {#each exercise.hints as hint, index}
            <div class="hint-item">
              <span class="hint-number">Hint {index + 1}:</span>
              <p>{hint}</p>
            </div>
          {/each}
        </div>
      </details>
    {/if}

    <details class="solution-section" bind:open={state.showSolution}>
      <summary class="collapsible-title">📝 Solution</summary>
      <div class="solution-content">
        <p class="warning">
          Try to solve it yourself first! Viewing the solution may reduce your
          learning.
        </p>
        <pre><code>{exercise.solution}</code></pre>
      </div>
    </details>
  </div>

  <!-- Exercise information -->
  <aside class="exercise-info">
    <div class="info-card">
      <h3 class="card-title">Test Cases</h3>
      <p class="info-value">{exercise.testCases.length}</p>
    </div>

    <div class="info-card">
      <h3 class="card-title">Time Limit</h3>
      <p class="info-value">{exercise.timeLimit}s</p>
    </div>

    <div class="info-card">
      <h3 class="card-title">Memory Limit</h3>
      <p class="info-value">{exercise.memoryLimit} MB</p>
    </div>

    <div class="info-card">
      <h3 class="card-title">Supported Languages</h3>
      <div class="languages-list">
        {#each exercise.languages as lang}
          <span class="language-tag">{lang}</span>
        {/each}
      </div>
    </div>
  </aside>
</div>

<style>
  .exercise-container {
    display: flex;
    flex-direction: column;
    gap: 24px;
    padding: 24px;
    background: white;
    border-radius: 8px;
  }

  .exercise-header {
    display: flex;
    justify-content: space-between;
    align-items: flex-start;
    gap: 24px;
    padding-bottom: 24px;
    border-bottom: 2px solid #e0e0e0;
  }

  .header-content {
    flex: 1;
  }

  .exercise-title {
    margin: 0 0 8px 0;
    font-size: 28px;
    font-weight: 700;
    color: #2c3e50;
  }

  .exercise-description {
    margin: 0 0 12px 0;
    font-size: 14px;
    line-height: 1.6;
    color: #666;
  }

  .exercise-meta {
    display: flex;
    align-items: center;
    gap: 12px;
    font-size: 12px;
    color: #999;
  }

  .difficulty-badge {
    padding: 4px 12px;
    border: 2px solid;
    border-radius: 4px;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .separator {
    color: #e0e0e0;
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

  .completion-badge .icon {
    font-size: 16px;
  }

  .problem-statement {
    display: flex;
    flex-direction: column;
    gap: 12px;
    padding: 20px;
    background: #f5f7fa;
    border-radius: 6px;
  }

  .problem-statement h2 {
    margin: 0;
    font-size: 14px;
    font-weight: 700;
    color: #2c3e50;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .problem-content {
    margin: 0;
    font-size: 13px;
    line-height: 1.8;
    color: #666;
  }

  .exercise-layout {
    display: grid;
    grid-template-columns: 1fr 1fr;
    gap: 24px;
  }

  .editor-section {
    display: flex;
    flex-direction: column;
    gap: 12px;
  }

  .language-selector {
    display: flex;
    align-items: center;
    gap: 8px;
  }

  .language-selector label {
    font-size: 12px;
    font-weight: 600;
    color: #2c3e50;
  }

  .language-selector select {
    padding: 6px 12px;
    background: white;
    border: 1px solid #e0e0e0;
    border-radius: 4px;
    font-size: 12px;
    cursor: pointer;
  }

  .code-editor {
    flex: 1;
    display: flex;
    border: 1px solid #e0e0e0;
    border-radius: 6px;
    overflow: hidden;
    background: #f5f5f5;
    min-height: 300px;
  }

  .code-editor textarea {
    flex: 1;
    padding: 12px;
    font-family: 'Monaco', 'Menlo', 'Ubuntu Mono', monospace;
    font-size: 12px;
    line-height: 1.5;
    color: #333;
    background: white;
    border: none;
    resize: none;
    outline: none;
  }

  .run-button {
    padding: 12px 24px;
    background: #4a90e2;
    color: white;
    border: none;
    border-radius: 6px;
    cursor: pointer;
    font-size: 14px;
    font-weight: 600;
    transition: all 0.2s ease;
    display: flex;
    align-items: center;
    justify-content: center;
    gap: 8px;
  }

  .run-button:hover:not(:disabled) {
    background: #357ace;
  }

  .run-button:disabled {
    opacity: 0.7;
    cursor: not-allowed;
  }

  .spinner {
    display: inline-block;
    width: 12px;
    height: 12px;
    border: 2px solid rgba(255, 255, 255, 0.3);
    border-top-color: white;
    border-radius: 50%;
    animation: spin 0.8s linear infinite;
  }

  @keyframes spin {
    to {
      transform: rotate(360deg);
    }
  }

  .results-section {
    display: flex;
    flex-direction: column;
    gap: 12px;
    padding: 16px;
    background: #f9f9f9;
    border: 1px solid #e0e0e0;
    border-radius: 6px;
  }

  .results-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding-bottom: 12px;
    border-bottom: 1px solid #e0e0e0;
  }

  .results-header h3 {
    margin: 0;
    font-size: 13px;
    font-weight: 700;
    color: #2c3e50;
  }

  .results-summary {
    display: flex;
    gap: 16px;
  }

  .summary-item {
    display: flex;
    gap: 6px;
    font-size: 12px;
  }

  .summary-label {
    color: #999;
    font-weight: 600;
  }

  .summary-value {
    font-weight: 700;
    color: #2c3e50;
  }

  .summary-value.passed {
    color: #7cb342;
  }

  .test-cases {
    display: flex;
    flex-direction: column;
    gap: 8px;
    max-height: 400px;
    overflow-y: auto;
  }

  .test-case {
    padding: 12px;
    background: white;
    border: 1px solid #e0e0e0;
    border-radius: 4px;
    border-left: 4px solid #ff9800;
  }

  .test-case.passed {
    border-left-color: #7cb342;
    background: #f0f8f4;
  }

  .test-case-header {
    display: flex;
    align-items: center;
    gap: 8px;
    font-size: 12px;
    font-weight: 600;
  }

  .test-icon {
    font-size: 14px;
  }

  .test-case.passed .test-icon {
    color: #7cb342;
  }

  .test-case:not(.passed) .test-icon {
    color: #ff9800;
  }

  .test-name {
    flex: 1;
    color: #2c3e50;
  }

  .test-time {
    color: #999;
  }

  .test-details {
    margin-top: 12px;
    display: flex;
    flex-direction: column;
    gap: 8px;
  }

  .detail-item {
    display: flex;
    flex-direction: column;
    gap: 4px;
  }

  .detail-label {
    font-size: 11px;
    font-weight: 600;
    color: #666;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .detail-value {
    margin: 0;
    padding: 8px;
    background: white;
    border: 1px solid #e0e0e0;
    border-radius: 3px;
    font-family: 'Monaco', 'Menlo', monospace;
    font-size: 11px;
    line-height: 1.4;
    color: #333;
    overflow-x: auto;
  }

  .detail-value.error {
    color: #e53935;
  }

  .empty-results {
    display: flex;
    align-items: center;
    justify-content: center;
    min-height: 150px;
    color: #999;
    font-size: 13px;
  }

  .hints-solution {
    display: flex;
    flex-direction: column;
    gap: 12px;
  }

  .hints-section,
  .solution-section {
    border: 1px solid #e0e0e0;
    border-radius: 6px;
    overflow: hidden;
  }

  .collapsible-title {
    display: flex;
    align-items: center;
    gap: 8px;
    width: 100%;
    padding: 12px 16px;
    background: #f9f9f9;
    border: none;
    cursor: pointer;
    font-size: 13px;
    font-weight: 600;
    color: #2c3e50;
    transition: background 0.2s ease;
  }

  .collapsible-title:hover {
    background: #f0f0f0;
  }

  .hints-content,
  .solution-content {
    padding: 16px;
    background: white;
    border-top: 1px solid #e0e0e0;
  }

  .hints-content {
    display: flex;
    flex-direction: column;
    gap: 12px;
  }

  .hint-item {
    display: flex;
    flex-direction: column;
    gap: 6px;
    padding: 8px;
    background: #f5f5f5;
    border-radius: 4px;
  }

  .hint-number {
    font-size: 11px;
    font-weight: 700;
    color: #2c3e50;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .hint-item p {
    margin: 0;
    font-size: 12px;
    line-height: 1.6;
    color: #666;
  }

  .warning {
    margin: 0 0 12px 0;
    padding: 12px;
    background: #fff3e0;
    border-radius: 4px;
    font-size: 12px;
    color: #e65100;
    border-left: 4px solid #ff9800;
  }

  .solution-content pre {
    margin: 0;
    padding: 12px;
    background: #f5f5f5;
    border-radius: 4px;
    font-family: 'Monaco', 'Menlo', monospace;
    font-size: 11px;
    line-height: 1.5;
    color: #333;
    overflow-x: auto;
  }

  .exercise-info {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
    gap: 16px;
  }

  .info-card {
    padding: 16px;
    background: #f9f9f9;
    border: 1px solid #e0e0e0;
    border-radius: 6px;
  }

  .card-title {
    margin: 0 0 8px 0;
    font-size: 11px;
    font-weight: 700;
    color: #2c3e50;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .info-value {
    margin: 0;
    font-size: 18px;
    font-weight: 700;
    color: #4a90e2;
  }

  .languages-list {
    display: flex;
    flex-wrap: wrap;
    gap: 6px;
  }

  .language-tag {
    padding: 2px 8px;
    background: #e3f2fd;
    border-radius: 3px;
    font-size: 11px;
    font-weight: 600;
    color: #4a90e2;
  }

  @media (max-width: 1024px) {
    .exercise-layout {
      grid-template-columns: 1fr;
    }

    .results-section {
      max-height: 300px;
    }
  }

  @media (max-width: 768px) {
    .exercise-header {
      flex-direction: column;
    }

    .exercise-layout {
      grid-template-columns: 1fr;
    }

    .exercise-info {
      grid-template-columns: 1fr 1fr;
    }
  }
</style>
