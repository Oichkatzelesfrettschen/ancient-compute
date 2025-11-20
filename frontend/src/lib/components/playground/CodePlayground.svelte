<script lang="ts">
  import { onMount, onDestroy } from 'svelte';
  import { playgroundStore } from '$lib/stores/playgroundStore';
  import LanguageSelector from './LanguageSelector.svelte';
  import OutputConsole from './OutputConsole.svelte';
  import ExecutionPanel from './ExecutionPanel.svelte';
  import ExampleLibrary from './ExampleLibrary.svelte';
  import { executeCode } from '$lib/api/execution';
  import type * as Monaco from 'monaco-editor/esm/vs/editor/editor.api';

  export let initialCode = '';
  export let initialLanguage = 'python';
  export let height = '600px';

  let containerElement: HTMLDivElement;
  let editor: Monaco.editor.IStandaloneCodeEditor | null = null;
  let monaco: typeof Monaco | null = null;
  let isLoading = true;
  let error: string | null = null;

  // Reactive state
  $: code = $playgroundStore.code;
  $: language = $playgroundStore.language;
  $: theme = $playgroundStore.theme;
  $: fontSize = $playgroundStore.fontSize;
  $: isExecuting = $playgroundStore.isExecuting;
  $: exampleLibraryOpen = $playgroundStore.exampleLibraryOpen;

  // Theme mapping
  const themeMap = {
    light: 'vs',
    dark: 'vs-dark',
    victorian: 'vs-dark' // Custom theme to be registered
  };

  // Language display names
  const languageNames: Record<string, string> = {
    c: 'C',
    python: 'Python',
    haskell: 'Haskell',
    java: 'Java',
    lisp: 'LISP',
    idris2: 'IDRIS2',
    systemf: 'System F',
    'babbage-asm': 'Babbage Assembly'
  };

  // Initialize Monaco Editor
  async function initializeEditor() {
    try {
      isLoading = true;

      // Lazy load Monaco and worker configuration
      const [monacoModule, { setupMonaco }] = await Promise.all([
        import('monaco-editor'),
        import('$lib/monaco/setup')
      ]);

      monaco = monacoModule.default;
      setupMonaco(monaco);

      // Register custom languages
      await import('$lib/monaco/languages').then(mod => mod.registerLanguages(monaco));

      // Create editor instance
      editor = monaco.editor.create(containerElement, {
        value: initialCode || $playgroundStore.code,
        language: initialLanguage || $playgroundStore.language,
        theme: themeMap[$playgroundStore.theme],
        automaticLayout: true,
        minimap: { enabled: true },
        lineNumbers: 'on',
        fontSize: $playgroundStore.fontSize,
        fontFamily: 'Fira Code, Monaco, Courier New, monospace',
        fontLigatures: true,
        scrollBeyondLastLine: false,
        renderWhitespace: 'selection',
        bracketPairColorization: { enabled: true },
        folding: true,
        wordWrap: 'on',
        suggestOnTriggerCharacters: true,
        quickSuggestions: true,
        contextmenu: true,
        formatOnPaste: true,
        formatOnType: true,
        scrollbar: {
          vertical: 'visible',
          horizontal: 'visible',
          useShadows: false,
          verticalScrollbarSize: 10,
          horizontalScrollbarSize: 10
        }
      });

      // Listen for code changes
      editor.onDidChangeModelContent(() => {
        if (editor) {
          const newCode = editor.getValue();
          playgroundStore.updateCode(newCode);
          saveToLocalStorage();
        }
      });

      // Set up keyboard shortcuts
      setupKeyboardShortcuts();

      // Load from localStorage if available
      loadFromLocalStorage();

      isLoading = false;
    } catch (err) {
      error = `Failed to initialize editor: ${err}`;
      isLoading = false;
    }
  }

  // Setup keyboard shortcuts
  function setupKeyboardShortcuts() {
    if (!editor || !monaco) return;

    // Run code: Ctrl/Cmd + Enter
    editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter, () => {
      handleRun();
    });

    // Format code: Shift + Alt + F
    editor.addCommand(
      monaco.KeyMod.Shift | monaco.KeyMod.Alt | monaco.KeyCode.KeyF,
      () => {
        editor?.getAction('editor.action.formatDocument')?.run();
      }
    );

    // Toggle comment: Ctrl/Cmd + /
    editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.Slash, () => {
      editor?.getAction('editor.action.commentLine')?.run();
    });

    // Increase font size: Ctrl/Cmd + Plus
    editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.Equal, () => {
      playgroundStore.increaseFontSize();
    });

    // Decrease font size: Ctrl/Cmd + Minus
    editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.Minus, () => {
      playgroundStore.decreaseFontSize();
    });

    // Open example library: Ctrl/Cmd + E
    editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KeyE, () => {
      playgroundStore.toggleExampleLibrary();
    });
  }

  // Handle language change
  export function changeLanguage(newLanguage: string) {
    if (!editor || !monaco) return;

    const model = editor.getModel();
    if (model) {
      monaco.editor.setModelLanguage(model, newLanguage);
    }
    playgroundStore.updateLanguage(newLanguage);
    saveToLocalStorage();
  }

  // Handle theme change
  function changeTheme(newTheme: 'light' | 'dark' | 'victorian') {
    if (!editor || !monaco) return;

    // Register Victorian theme if needed
    if (newTheme === 'victorian') {
      registerVictorianTheme();
    }

    monaco.editor.setTheme(themeMap[newTheme]);
    playgroundStore.updateTheme(newTheme);
    saveToLocalStorage();
  }

  // Register Victorian theme
  function registerVictorianTheme() {
    if (!monaco) return;

    monaco.editor.defineTheme('victorian', {
      base: 'vs-dark',
      inherit: true,
      rules: [
        { token: 'comment', foreground: '8B7355', fontStyle: 'italic' },
        { token: 'keyword', foreground: 'CD853F', fontStyle: 'bold' },
        { token: 'string', foreground: 'DEB887' },
        { token: 'number', foreground: 'F4A460' },
        { token: 'function', foreground: 'D2691E' },
        { token: 'variable', foreground: 'FAEBD7' },
        { token: 'type', foreground: 'BC8F8F' }
      ],
      colors: {
        'editor.background': '#2A2520',
        'editor.foreground': '#F5DEB3',
        'editor.lineHighlightBackground': '#3A3530',
        'editorCursor.foreground': '#FFD700',
        'editor.selectionBackground': '#4A4540',
        'editorLineNumber.foreground': '#8B7355'
      }
    });
  }

  // Update font size
  function updateFontSize(size: number) {
    if (!editor) return;
    editor.updateOptions({ fontSize: size });
  }

  // Handle run code
  async function handleRun() {
    if (!editor) return;
    const code = editor.getValue();
    await executeCode(code, $playgroundStore.language);
  }

  // Handle stop execution
  function handleStop() {
    playgroundStore.stopExecution();
  }

  // Handle reset
  function handleReset() {
    if (!editor) return;
    editor.setValue('');
    playgroundStore.reset();
    localStorage.removeItem('ancientCompute_playground');
  }

  // Load example code
  export function loadExample(exampleCode: string, exampleLanguage: string) {
    if (!editor) return;

    editor.setValue(exampleCode);
    changeLanguage(exampleLanguage);
    playgroundStore.updateCode(exampleCode);
    playgroundStore.updateLanguage(exampleLanguage);
    playgroundStore.toggleExampleLibrary();
  }

  // Save to localStorage
  function saveToLocalStorage() {
    const state = {
      code: $playgroundStore.code,
      language: $playgroundStore.language,
      theme: $playgroundStore.theme,
      fontSize: $playgroundStore.fontSize,
      timestamp: new Date().toISOString()
    };
    localStorage.setItem('ancientCompute_playground', JSON.stringify(state));
  }

  // Load from localStorage
  function loadFromLocalStorage() {
    const stored = localStorage.getItem('ancientCompute_playground');
    if (!stored) return;

    try {
      const state = JSON.parse(stored);
      if (editor) {
        editor.setValue(state.code || '');
        changeLanguage(state.language || 'python');
      }
      playgroundStore.updateCode(state.code || '');
      playgroundStore.updateLanguage(state.language || 'python');
      playgroundStore.updateTheme(state.theme || 'dark');
      playgroundStore.updateFontSize(state.fontSize || 14);
    } catch (err) {
      console.error('Failed to load from localStorage:', err);
    }
  }

  // React to store changes
  $: if (editor && $playgroundStore.fontSize !== editor.getOption(43)) {
    updateFontSize($playgroundStore.fontSize);
  }

  onMount(() => {
    initializeEditor();
  });

  onDestroy(() => {
    if (editor) {
      editor.getModel()?.dispose();
      editor.dispose();
    }
  });
</script>

<div class="code-playground">
  <div class="playground-header">
    <div class="header-left">
      <LanguageSelector
        currentLanguage={language}
        on:change={(e) => changeLanguage(e.detail)}
      />
      <div class="editor-info">
        <span class="info-item">
          <i class="icon-lines"></i>
          Lines: {code.split('\n').length}
        </span>
        <span class="info-item">
          <i class="icon-chars"></i>
          Characters: {code.length}
        </span>
      </div>
    </div>

    <div class="header-center">
      <h2 class="playground-title">Ancient Compute Playground</h2>
    </div>

    <div class="header-right">
      <div class="theme-controls">
        <button
          class="theme-btn"
          class:active={theme === 'light'}
          on:click={() => changeTheme('light')}
          title="Light Theme"
        >
          <i class="icon-sun"></i>
        </button>
        <button
          class="theme-btn"
          class:active={theme === 'dark'}
          on:click={() => changeTheme('dark')}
          title="Dark Theme"
        >
          <i class="icon-moon"></i>
        </button>
        <button
          class="theme-btn"
          class:active={theme === 'victorian'}
          on:click={() => changeTheme('victorian')}
          title="Victorian Theme"
        >
          <i class="icon-vintage"></i>
        </button>
      </div>

      <div class="font-controls">
        <button
          class="font-btn"
          on:click={() => playgroundStore.decreaseFontSize()}
          title="Decrease Font Size (Ctrl+-)"
        >
          A-
        </button>
        <span class="font-size">{fontSize}px</span>
        <button
          class="font-btn"
          on:click={() => playgroundStore.increaseFontSize()}
          title="Increase Font Size (Ctrl+=)"
        >
          A+
        </button>
      </div>

      <button
        class="examples-btn"
        class:active={exampleLibraryOpen}
        on:click={() => playgroundStore.toggleExampleLibrary()}
        title="Example Library (Ctrl+E)"
      >
        <i class="icon-book"></i>
        Examples
      </button>
    </div>
  </div>

  <div class="playground-body">
    <div class="editor-container" style="height: {height}">
      {#if isLoading}
        <div class="loading-overlay">
          <div class="spinner"></div>
          <p>Loading Monaco Editor...</p>
        </div>
      {/if}

      {#if error}
        <div class="error-overlay">
          <i class="icon-error"></i>
          <p>{error}</p>
        </div>
      {/if}

      <div bind:this={containerElement} class="monaco-container"></div>
    </div>

    <ExecutionPanel
      on:run={handleRun}
      on:stop={handleStop}
      on:reset={handleReset}
      {isExecuting}
    />

    <OutputConsole />
  </div>

  {#if exampleLibraryOpen}
    <ExampleLibrary
      on:load={(e) => loadExample(e.detail.code, e.detail.language)}
      on:close={() => playgroundStore.toggleExampleLibrary()}
    />
  {/if}
</div>

<style>
  .code-playground {
    display: flex;
    flex-direction: column;
    height: 100%;
    background: var(--color-background, #1e1e1e);
    border-radius: 8px;
    overflow: hidden;
    box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
  }

  .playground-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 12px 20px;
    background: var(--color-header-bg, #252526);
    border-bottom: 1px solid var(--color-border, #3e3e42);
  }

  .header-left,
  .header-center,
  .header-right {
    display: flex;
    align-items: center;
    gap: 16px;
  }

  .playground-title {
    font-size: 18px;
    font-weight: 600;
    color: var(--color-text, #cccccc);
    margin: 0;
  }

  .editor-info {
    display: flex;
    gap: 12px;
    font-size: 12px;
    color: var(--color-text-secondary, #969696);
  }

  .info-item {
    display: flex;
    align-items: center;
    gap: 4px;
  }

  .theme-controls,
  .font-controls {
    display: flex;
    align-items: center;
    gap: 4px;
    padding: 4px;
    background: var(--color-control-bg, #1e1e1e);
    border-radius: 4px;
  }

  .theme-btn,
  .font-btn {
    padding: 6px 10px;
    background: transparent;
    border: none;
    color: var(--color-text-secondary, #969696);
    cursor: pointer;
    border-radius: 4px;
    transition: all 0.2s;
  }

  .theme-btn:hover,
  .font-btn:hover {
    background: var(--color-hover, #2a2d2e);
    color: var(--color-text, #cccccc);
  }

  .theme-btn.active {
    background: var(--color-accent, #007acc);
    color: white;
  }

  .font-size {
    min-width: 40px;
    text-align: center;
    font-size: 12px;
    color: var(--color-text-secondary, #969696);
  }

  .examples-btn {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 8px 16px;
    background: var(--color-button-bg, #0e639c);
    color: white;
    border: none;
    border-radius: 4px;
    cursor: pointer;
    font-size: 14px;
    transition: all 0.2s;
  }

  .examples-btn:hover {
    background: var(--color-button-hover, #1177bb);
  }

  .examples-btn.active {
    background: var(--color-accent, #007acc);
  }

  .playground-body {
    display: flex;
    flex-direction: column;
    flex: 1;
    overflow: hidden;
  }

  .editor-container {
    position: relative;
    flex: 1;
    min-height: 400px;
  }

  .monaco-container {
    width: 100%;
    height: 100%;
  }

  .loading-overlay,
  .error-overlay {
    position: absolute;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    background: rgba(30, 30, 30, 0.9);
    z-index: 1000;
  }

  .spinner {
    width: 40px;
    height: 40px;
    border: 4px solid rgba(255, 255, 255, 0.1);
    border-top-color: var(--color-accent, #007acc);
    border-radius: 50%;
    animation: spin 1s linear infinite;
  }

  @keyframes spin {
    to {
      transform: rotate(360deg);
    }
  }

  .error-overlay {
    color: #f48771;
  }

  .icon-error {
    font-size: 48px;
    margin-bottom: 16px;
  }

  /* Responsive design */
  @media (max-width: 768px) {
    .playground-header {
      flex-direction: column;
      gap: 12px;
      padding: 12px;
    }

    .header-left,
    .header-center,
    .header-right {
      width: 100%;
      justify-content: center;
    }

    .playground-title {
      font-size: 16px;
    }
  }
</style>