<script lang="ts">
  import { onMount, afterUpdate } from 'svelte';
  import { playgroundStore } from '$lib/stores/playgroundStore';
  import { fade, slide } from 'svelte/transition';
  import Prism from 'prismjs';
  import 'prismjs/components/prism-c';
  import 'prismjs/components/prism-python';
  import 'prismjs/components/prism-haskell';
  import 'prismjs/components/prism-java';
  import 'prismjs/components/prism-lisp';

  type TabType = 'output' | 'errors' | 'compilation' | 'ir' | 'assembly';

  interface Tab {
    id: TabType;
    label: string;
    icon: string;
    count?: number;
  }

  let activeTab: TabType = 'output';
  let autoScroll = true;
  let wordWrap = true;
  let outputElement: HTMLDivElement;
  let copySuccess = false;

  $: output = $playgroundStore.output;
  $: hasOutput = output.stdout || output.stderr || output.compilationLog || output.ir || output.assembly;

  const tabs: Tab[] = [
    { id: 'output', label: 'Output', icon: '📤' },
    { id: 'errors', label: 'Errors', icon: '⚠️' },
    { id: 'compilation', label: 'Compilation', icon: '🔧' },
    { id: 'ir', label: 'IR', icon: '📊' },
    { id: 'assembly', label: 'Assembly', icon: '⚛️' }
  ];

  // Update tab counts
  $: {
    tabs[0].count = output.stdout ? output.stdout.split('\n').length - 1 : 0;
    tabs[1].count = output.stderr ? output.stderr.split('\n').length - 1 : 0;
    tabs[2].count = output.compilationLog ? 1 : 0;
    tabs[3].count = output.ir ? output.ir.split('\n').length - 1 : 0;
    tabs[4].count = output.assembly ? output.assembly.split('\n').length - 1 : 0;
  }

  // ANSI color code mapping
  const ansiColors: Record<string, string> = {
    '30': '#000000', // Black
    '31': '#c72e29', // Red
    '32': '#42c767', // Green
    '33': '#c7c329', // Yellow
    '34': '#2a84d2', // Blue
    '35': '#c72ec7', // Magenta
    '36': '#2ac7c7', // Cyan
    '37': '#d3d3d3', // White
    '90': '#5c5c5c', // Bright Black
    '91': '#ff6666', // Bright Red
    '92': '#66ff66', // Bright Green
    '93': '#ffff66', // Bright Yellow
    '94': '#6666ff', // Bright Blue
    '95': '#ff66ff', // Bright Magenta
    '96': '#66ffff', // Bright Cyan
    '97': '#ffffff', // Bright White
  };

  function processAnsiCodes(text: string): string {
    if (!text) return '';

    // Replace ANSI escape codes with HTML spans
    return text.replace(
      /\u001b\[(\d+)m/g,
      (match, code) => {
        const color = ansiColors[code];
        if (color) {
          return `<span style="color: ${color}">`;
        } else if (code === '0') {
          return '</span>';
        }
        return '';
      }
    );
  }

  function getTabContent(tabId: TabType): string {
    switch (tabId) {
      case 'output':
        return output.stdout || 'No output yet. Run your code to see results.';
      case 'errors':
        return output.stderr || 'No errors.';
      case 'compilation':
        return output.compilationLog || 'No compilation log available.';
      case 'ir':
        return output.ir || 'No intermediate representation generated.';
      case 'assembly':
        return output.assembly || 'No assembly code generated.';
      default:
        return '';
    }
  }

  function getSyntaxHighlightedContent(content: string, tabId: TabType): string {
    if (!content) return content;

    // Apply syntax highlighting for IR and Assembly tabs
    if (tabId === 'ir' || tabId === 'assembly') {
      try {
        // Use a basic highlighting for now
        return content
          .replace(/(\b\d+\b)/g, '<span class="token number">$1</span>')
          .replace(/(\b(mov|add|sub|jmp|call|ret|push|pop|ld|st)\b)/gi, '<span class="token keyword">$1</span>')
          .replace(/(;.*)$/gm, '<span class="token comment">$1</span>')
          .replace(/(%\w+)/g, '<span class="token variable">$1</span>')
          .replace(/([@#]\w+)/g, '<span class="token function">$1</span>');
      } catch (e) {
        console.error('Syntax highlighting failed:', e);
        return content;
      }
    }

    // Process ANSI codes for output and errors
    if (tabId === 'output' || tabId === 'errors') {
      return processAnsiCodes(content);
    }

    return content;
  }

  function clearOutput() {
    playgroundStore.clearOutput();
  }

  async function copyToClipboard() {
    const content = getTabContent(activeTab);
    try {
      await navigator.clipboard.writeText(content);
      copySuccess = true;
      setTimeout(() => {
        copySuccess = false;
      }, 2000);
    } catch (err) {
      console.error('Failed to copy:', err);
    }
  }

  function downloadOutput() {
    const content = getTabContent(activeTab);
    const blob = new Blob([content], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `${activeTab}-${Date.now()}.txt`;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  }

  function scrollToBottom() {
    if (outputElement && autoScroll) {
      outputElement.scrollTop = outputElement.scrollHeight;
    }
  }

  // Auto-scroll when new content arrives
  afterUpdate(() => {
    scrollToBottom();
  });

  onMount(() => {
    // Initialize syntax highlighting
    if (typeof window !== 'undefined') {
      Prism.highlightAll();
    }
  });
</script>

<div class="output-console">
  <div class="console-header">
    <div class="tabs">
      {#each tabs as tab}
        <button
          class="tab"
          class:active={activeTab === tab.id}
          on:click={() => (activeTab = tab.id)}
          title={tab.label}
        >
          <span class="tab-icon">{tab.icon}</span>
          <span class="tab-label">{tab.label}</span>
          {#if tab.count && tab.count > 0}
            <span class="tab-count" class:has-error={tab.id === 'errors'}>
              {tab.count}
            </span>
          {/if}
        </button>
      {/each}
    </div>

    <div class="console-controls">
      <button
        class="control-btn"
        class:active={wordWrap}
        on:click={() => (wordWrap = !wordWrap)}
        title="Toggle word wrap"
      >
        <svg width="16" height="16" viewBox="0 0 16 16">
          <path
            d="M3 2h10v2H5v8h8V8h2v6H3V2zm8 4h2v2h-2V6zm0 4h2v2h-2v-2z"
            fill="currentColor"
          />
        </svg>
      </button>

      <button
        class="control-btn"
        class:active={autoScroll}
        on:click={() => (autoScroll = !autoScroll)}
        title="Toggle auto-scroll"
      >
        <svg width="16" height="16" viewBox="0 0 16 16">
          <path
            d="M8 11l-4-4h8l-4 4zm0-6L4 1h8L8 5z"
            fill="currentColor"
          />
        </svg>
      </button>

      <div class="separator"></div>

      <button
        class="control-btn"
        on:click={copyToClipboard}
        title="Copy to clipboard"
        class:success={copySuccess}
      >
        {#if copySuccess}
          <svg width="16" height="16" viewBox="0 0 16 16">
            <path d="M13 2L5 10l-3-3-1 1 4 4L14 3l-1-1z" fill="currentColor" />
          </svg>
        {:else}
          <svg width="16" height="16" viewBox="0 0 16 16">
            <path
              d="M10 2H4v10h2V4h4V2zm4 2H8v10h6V4zm-1 9H9V5h4v8z"
              fill="currentColor"
            />
          </svg>
        {/if}
      </button>

      <button class="control-btn" on:click={downloadOutput} title="Download output">
        <svg width="16" height="16" viewBox="0 0 16 16">
          <path
            d="M8 12l-4-4h2V2h4v6h2l-4 4zm-6 2h12v2H2v-2z"
            fill="currentColor"
          />
        </svg>
      </button>

      <button class="control-btn" on:click={clearOutput} title="Clear output">
        <svg width="16" height="16" viewBox="0 0 16 16">
          <path
            d="M8 2a6 6 0 1 0 0 12A6 6 0 0 0 8 2zm3 8L9 8l2-2-1-1-2 2-2-2-1 1 2 2-2 2 1 1 2-2 2 2 1-1z"
            fill="currentColor"
          />
        </svg>
      </button>
    </div>
  </div>

  <div
    bind:this={outputElement}
    class="console-output"
    class:word-wrap={wordWrap}
  >
    {#if hasOutput}
      {@const content = getTabContent(activeTab)}
      {@const highlightedContent = getSyntaxHighlightedContent(content, activeTab)}
      <pre class="output-content" class:has-syntax={activeTab === 'ir' || activeTab === 'assembly'}>
        {@html highlightedContent}
      </pre>
    {:else}
      <div class="empty-state" transition:fade>
        <svg class="empty-icon" width="48" height="48" viewBox="0 0 48 48">
          <circle cx="24" cy="24" r="20" fill="none" stroke="currentColor" stroke-width="2" opacity="0.3" />
          <path d="M16 24l4 4 8-8" stroke="currentColor" stroke-width="2" fill="none" opacity="0.3" />
        </svg>
        <p class="empty-message">No output yet</p>
        <p class="empty-hint">Run your code to see results here</p>
      </div>
    {/if}

    {#if $playgroundStore.executionTime > 0}
      <div class="execution-stats" transition:slide>
        <span class="stat">
          <span class="stat-label">Execution time:</span>
          <span class="stat-value">{$playgroundStore.executionTime}ms</span>
        </span>
        {#if output.memoryUsage}
          <span class="stat">
            <span class="stat-label">Memory:</span>
            <span class="stat-value">{output.memoryUsage}KB</span>
          </span>
        {/if}
        {#if output.exitCode !== undefined}
          <span class="stat">
            <span class="stat-label">Exit code:</span>
            <span class="stat-value" class:error={output.exitCode !== 0}>
              {output.exitCode}
            </span>
          </span>
        {/if}
      </div>
    {/if}
  </div>
</div>

<style>
  .output-console {
    display: flex;
    flex-direction: column;
    height: 300px;
    background: var(--color-console-bg, #1e1e1e);
    border-top: 1px solid var(--color-border, #3e3e42);
    font-family: 'Fira Code', 'Monaco', 'Courier New', monospace;
  }

  .console-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0 8px;
    background: var(--color-header-bg, #252526);
    border-bottom: 1px solid var(--color-border, #3e3e42);
    min-height: 35px;
  }

  .tabs {
    display: flex;
    gap: 2px;
    height: 100%;
  }

  .tab {
    display: flex;
    align-items: center;
    gap: 4px;
    padding: 6px 12px;
    background: transparent;
    color: var(--color-text-secondary, #969696);
    border: none;
    border-bottom: 2px solid transparent;
    cursor: pointer;
    font-size: 12px;
    transition: all 0.2s;
  }

  .tab:hover {
    background: var(--color-hover, #2a2d2e);
    color: var(--color-text, #cccccc);
  }

  .tab.active {
    color: var(--color-text, #cccccc);
    background: var(--color-active-bg, #1e1e1e);
    border-bottom-color: var(--color-accent, #007acc);
  }

  .tab-icon {
    font-size: 14px;
  }

  .tab-label {
    font-weight: 500;
  }

  .tab-count {
    padding: 2px 6px;
    background: var(--color-badge-bg, #3e3e42);
    color: var(--color-text, #cccccc);
    border-radius: 10px;
    font-size: 10px;
    font-weight: 600;
    min-width: 18px;
    text-align: center;
  }

  .tab-count.has-error {
    background: var(--color-error-bg, #5a1d1d);
    color: var(--color-error, #f48771);
  }

  .console-controls {
    display: flex;
    align-items: center;
    gap: 4px;
  }

  .control-btn {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 28px;
    height: 28px;
    padding: 0;
    background: transparent;
    color: var(--color-text-secondary, #969696);
    border: none;
    border-radius: 4px;
    cursor: pointer;
    transition: all 0.2s;
  }

  .control-btn:hover {
    background: var(--color-hover, #2a2d2e);
    color: var(--color-text, #cccccc);
  }

  .control-btn.active {
    background: var(--color-active, #094771);
    color: var(--color-accent, #007acc);
  }

  .control-btn.success {
    color: var(--color-success, #42c767);
  }

  .separator {
    width: 1px;
    height: 20px;
    background: var(--color-border, #3e3e42);
    margin: 0 4px;
  }

  .console-output {
    flex: 1;
    overflow: auto;
    padding: 12px;
    background: var(--color-console-bg, #1e1e1e);
    position: relative;
  }

  .console-output.word-wrap pre {
    white-space: pre-wrap;
    word-wrap: break-word;
  }

  .output-content {
    margin: 0;
    font-size: 13px;
    line-height: 1.5;
    color: var(--color-output-text, #d4d4d4);
    white-space: pre;
    font-family: inherit;
  }

  .output-content.has-syntax {
    color: var(--color-syntax, #d4d4d4);
  }

  /* Syntax highlighting colors */
  :global(.output-content .token.keyword) {
    color: #569cd6;
  }

  :global(.output-content .token.number) {
    color: #b5cea8;
  }

  :global(.output-content .token.comment) {
    color: #608b4e;
    font-style: italic;
  }

  :global(.output-content .token.variable) {
    color: #9cdcfe;
  }

  :global(.output-content .token.function) {
    color: #dcdcaa;
  }

  .empty-state {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    height: 100%;
    color: var(--color-text-secondary, #969696);
  }

  .empty-icon {
    margin-bottom: 16px;
    color: var(--color-text-secondary, #969696);
  }

  .empty-message {
    font-size: 16px;
    font-weight: 500;
    margin: 0 0 8px;
  }

  .empty-hint {
    font-size: 13px;
    margin: 0;
    opacity: 0.7;
  }

  .execution-stats {
    margin-top: 12px;
    padding: 8px 12px;
    background: var(--color-stats-bg, #252526);
    border-radius: 4px;
    display: flex;
    gap: 24px;
    font-size: 12px;
  }

  .stat {
    display: flex;
    gap: 6px;
  }

  .stat-label {
    color: var(--color-text-secondary, #969696);
  }

  .stat-value {
    color: var(--color-text, #cccccc);
    font-weight: 500;
  }

  .stat-value.error {
    color: var(--color-error, #f48771);
  }

  /* Scrollbar styling */
  .console-output::-webkit-scrollbar {
    width: 10px;
    height: 10px;
  }

  .console-output::-webkit-scrollbar-track {
    background: var(--color-scrollbar-track, #1e1e1e);
  }

  .console-output::-webkit-scrollbar-thumb {
    background: var(--color-scrollbar-thumb, #4e4e4e);
    border-radius: 4px;
  }

  .console-output::-webkit-scrollbar-thumb:hover {
    background: var(--color-scrollbar-hover, #5e5e5e);
  }

  /* Responsive design */
  @media (max-width: 768px) {
    .tab-label {
      display: none;
    }

    .tab {
      padding: 6px 8px;
    }

    .console-controls {
      gap: 2px;
    }

    .control-btn {
      width: 24px;
      height: 24px;
    }
  }
</style>