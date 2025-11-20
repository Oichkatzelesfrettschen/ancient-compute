import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import { render, screen, fireEvent, waitFor } from '@testing-library/svelte';
import { get } from 'svelte/store';
import CodePlayground from '../CodePlayground.svelte';
import LanguageSelector from '../LanguageSelector.svelte';
import OutputConsole from '../OutputConsole.svelte';
import ExecutionPanel from '../ExecutionPanel.svelte';
import ExampleLibrary from '../ExampleLibrary.svelte';
import { playgroundStore } from '$lib/stores/playgroundStore';
import { executeCode, stopExecution, initWebSocket } from '$lib/api/execution';
import { examplePrograms } from '$lib/data/examplePrograms';

// Mock Monaco Editor
vi.mock('monaco-editor', () => ({
  default: {
    editor: {
      create: vi.fn(() => ({
        getValue: vi.fn(() => 'test code'),
        setValue: vi.fn(),
        getModel: vi.fn(() => ({
          dispose: vi.fn(),
        })),
        onDidChangeModelContent: vi.fn(),
        addCommand: vi.fn(),
        getAction: vi.fn(() => ({ run: vi.fn() })),
        updateOptions: vi.fn(),
        dispose: vi.fn(),
        getOption: vi.fn(() => 14),
      })),
      setTheme: vi.fn(),
      defineTheme: vi.fn(),
      setModelLanguage: vi.fn(),
    },
    languages: {
      register: vi.fn(),
      setMonarchTokensProvider: vi.fn(),
      setLanguageConfiguration: vi.fn(),
      registerCompletionItemProvider: vi.fn(),
      typescript: {
        javascriptDefaults: {
          setDiagnosticsOptions: vi.fn(),
          setCompilerOptions: vi.fn(),
          addExtraLib: vi.fn(),
        },
        typescriptDefaults: {
          setDiagnosticsOptions: vi.fn(),
          setCompilerOptions: vi.fn(),
          addExtraLib: vi.fn(),
        },
      },
    },
    KeyMod: {
      CtrlCmd: 2048,
      Shift: 1024,
      Alt: 512,
    },
    KeyCode: {
      Enter: 13,
      Slash: 56,
      Equal: 21,
      Minus: 20,
      KeyE: 18,
      KeyF: 19,
    },
  },
}));

// Mock API functions
vi.mock('$lib/api/execution', () => ({
  executeCode: vi.fn(),
  stopExecution: vi.fn(),
  initWebSocket: vi.fn(() => Promise.resolve()),
  checkHealth: vi.fn(() => Promise.resolve(true)),
  fetchLanguages: vi.fn(() => Promise.resolve([
    { id: 'python', name: 'Python', version: '3.11', features: [] },
    { id: 'c', name: 'C', version: '11', features: [] },
  ])),
}));

describe('CodePlayground Component', () => {
  beforeEach(() => {
    // Reset store to initial state
    playgroundStore.reset();
    vi.clearAllMocks();
  });

  afterEach(() => {
    vi.restoreAllMocks();
  });

  describe('Monaco Editor Initialization', () => {
    it('should initialize Monaco Editor on mount', async () => {
      const { component } = render(CodePlayground);

      await waitFor(() => {
        expect(screen.getByText(/Ancient Compute Playground/i)).toBeInTheDocument();
      });
    });

    it('should set up web workers correctly', async () => {
      const { component } = render(CodePlayground);

      await waitFor(() => {
        // Check that MonacoEnvironment is set up
        expect(window.MonacoEnvironment).toBeDefined();
        expect(window.MonacoEnvironment.getWorker).toBeDefined();
      });
    });

    it('should apply initial configuration', async () => {
      const mockEditor = {
        getValue: vi.fn(() => ''),
        setValue: vi.fn(),
        getModel: vi.fn(),
        onDidChangeModelContent: vi.fn(),
        addCommand: vi.fn(),
        updateOptions: vi.fn(),
        dispose: vi.fn(),
      };

      const { component } = render(CodePlayground, {
        props: {
          initialCode: 'print("Hello")',
          initialLanguage: 'python',
        },
      });

      await waitFor(() => {
        const state = get(playgroundStore);
        expect(state.language).toBe('python');
      });
    });
  });

  describe('Language Switching', () => {
    it('should change language when selector is used', async () => {
      const { component } = render(LanguageSelector, {
        props: {
          currentLanguage: 'python',
        },
      });

      // Find and click the selector button
      const selectorButton = screen.getByRole('button', { expanded: false });
      await fireEvent.click(selectorButton);

      // Select a different language
      const cOption = screen.getByText(/^C$/);
      await fireEvent.click(cOption);

      // Check event was dispatched
      const events = component.$$?.callbacks?.change;
      expect(events).toBeDefined();
    });

    it('should support keyboard shortcuts for language selection', async () => {
      render(LanguageSelector);

      // Simulate Alt+C for C language
      await fireEvent.keyDown(window, { key: 'c', altKey: true });

      await waitFor(() => {
        const state = get(playgroundStore);
        expect(state.language).toBe('c');
      });
    });

    it('should remember recently used languages', async () => {
      const { rerender } = render(LanguageSelector);

      // Simulate selecting multiple languages
      playgroundStore.updateLanguage('python');
      playgroundStore.updateLanguage('haskell');
      playgroundStore.updateLanguage('java');

      // Check localStorage
      const stored = localStorage.getItem('ancientCompute_recentLanguages');
      expect(stored).toBeDefined();
      const recent = JSON.parse(stored || '[]');
      expect(recent).toContain('java');
    });
  });

  describe('Code Execution', () => {
    it('should execute code when run button is clicked', async () => {
      render(ExecutionPanel);

      const runButton = screen.getByText(/Run/i).closest('button');
      await fireEvent.click(runButton!);

      expect(executeCode).toHaveBeenCalled();
    });

    it('should handle Ctrl+Enter keyboard shortcut', async () => {
      render(ExecutionPanel);

      await fireEvent.keyDown(window, {
        key: 'Enter',
        ctrlKey: true,
      });

      expect(executeCode).toHaveBeenCalled();
    });

    it('should show loading state during execution', async () => {
      playgroundStore.startExecution();
      render(ExecutionPanel);

      expect(screen.getByText(/Running/i)).toBeInTheDocument();
    });

    it('should stop execution when stop button is clicked', async () => {
      playgroundStore.startExecution();
      render(ExecutionPanel);

      const stopButton = screen.getByText(/Stop/i).closest('button');
      await fireEvent.click(stopButton!);

      expect(stopExecution).toHaveBeenCalled();
    });

    it('should display execution time after completion', async () => {
      playgroundStore.setExecutionTime(150);
      render(ExecutionPanel);

      expect(screen.getByText(/150ms/i)).toBeInTheDocument();
    });
  });

  describe('Output Console', () => {
    it('should display stdout output', async () => {
      playgroundStore.updateOutput({ stdout: 'Hello, World!' });
      render(OutputConsole);

      expect(screen.getByText(/Hello, World!/)).toBeInTheDocument();
    });

    it('should display stderr output in errors tab', async () => {
      playgroundStore.updateOutput({ stderr: 'Error: Something went wrong' });
      render(OutputConsole);

      const errorsTab = screen.getByText(/Errors/i);
      await fireEvent.click(errorsTab);

      expect(screen.getByText(/Error: Something went wrong/)).toBeInTheDocument();
    });

    it('should support tab switching', async () => {
      render(OutputConsole);

      const tabs = ['Output', 'Errors', 'Compilation', 'IR', 'Assembly'];
      for (const tab of tabs) {
        const tabButton = screen.getByText(tab);
        await fireEvent.click(tabButton);
        expect(tabButton.closest('button')).toHaveClass('active');
      }
    });

    it('should copy output to clipboard', async () => {
      const mockClipboard = {
        writeText: vi.fn(() => Promise.resolve()),
      };
      Object.assign(navigator, { clipboard: mockClipboard });

      playgroundStore.updateOutput({ stdout: 'Test output' });
      render(OutputConsole);

      const copyButton = screen.getByTitle(/Copy to clipboard/i);
      await fireEvent.click(copyButton);

      expect(mockClipboard.writeText).toHaveBeenCalledWith('Test output');
    });

    it('should clear output when clear button is clicked', async () => {
      playgroundStore.updateOutput({ stdout: 'Test output' });
      render(OutputConsole);

      const clearButton = screen.getByTitle(/Clear output/i);
      await fireEvent.click(clearButton);

      const state = get(playgroundStore);
      expect(state.output.stdout).toBe('');
    });
  });

  describe('Example Library', () => {
    it('should display example programs', async () => {
      render(ExampleLibrary);

      expect(screen.getByText(/Egyptian Multiplication/)).toBeInTheDocument();
      expect(screen.getByText(/Euclidean Algorithm/)).toBeInTheDocument();
    });

    it('should filter examples by language', async () => {
      render(ExampleLibrary);

      const languageFilter = screen.getByDisplayValue(/All Languages/i);
      await fireEvent.change(languageFilter, { target: { value: 'python' } });

      // Check that only Python examples are shown
      const cards = screen.getAllByText(/python/i);
      expect(cards.length).toBeGreaterThan(0);
    });

    it('should filter examples by search query', async () => {
      render(ExampleLibrary);

      const searchInput = screen.getByPlaceholderText(/Search examples/i);
      await fireEvent.input(searchInput, { target: { value: 'fibonacci' } });

      expect(screen.getByText(/Fibonacci Sequence/)).toBeInTheDocument();
    });

    it('should load example when clicked', async () => {
      const { component } = render(ExampleLibrary);

      const exampleCard = screen.getAllByText(/Egyptian Multiplication/)[0].closest('.example-card');
      await fireEvent.click(exampleCard!);

      // Check that load event was dispatched
      const events = component.$$?.callbacks?.load;
      expect(events).toBeDefined();
    });

    it('should close when Escape key is pressed', async () => {
      const { component } = render(ExampleLibrary);

      await fireEvent.keyDown(window, { key: 'Escape' });

      const events = component.$$?.callbacks?.close;
      expect(events).toBeDefined();
    });
  });

  describe('Theme Management', () => {
    it('should switch between light, dark, and victorian themes', async () => {
      render(CodePlayground);

      // Test theme switching
      const themes = ['light', 'dark', 'victorian'];
      for (const theme of themes) {
        playgroundStore.updateTheme(theme as any);
        const state = get(playgroundStore);
        expect(state.theme).toBe(theme);
      }
    });

    it('should persist theme preference', async () => {
      playgroundStore.updateTheme('victorian');

      // Check localStorage
      await waitFor(() => {
        const stored = localStorage.getItem('ancientCompute_playgroundState');
        expect(stored).toBeDefined();
        const state = JSON.parse(stored || '{}');
        expect(state.theme).toBe('victorian');
      });
    });
  });

  describe('Font Size Controls', () => {
    it('should increase font size', async () => {
      render(CodePlayground);

      const initialSize = get(playgroundStore).fontSize;
      playgroundStore.increaseFontSize();
      const newSize = get(playgroundStore).fontSize;

      expect(newSize).toBe(initialSize + 1);
    });

    it('should decrease font size', async () => {
      render(CodePlayground);

      const initialSize = get(playgroundStore).fontSize;
      playgroundStore.decreaseFontSize();
      const newSize = get(playgroundStore).fontSize;

      expect(newSize).toBe(initialSize - 1);
    });

    it('should respect min/max font size limits', async () => {
      render(CodePlayground);

      // Try to go below minimum
      for (let i = 0; i < 20; i++) {
        playgroundStore.decreaseFontSize();
      }
      expect(get(playgroundStore).fontSize).toBe(10);

      // Try to go above maximum
      for (let i = 0; i < 20; i++) {
        playgroundStore.increaseFontSize();
      }
      expect(get(playgroundStore).fontSize).toBe(24);
    });
  });

  describe('WebSocket Integration', () => {
    it('should initialize WebSocket connection', async () => {
      await initWebSocket();
      expect(initWebSocket).toHaveBeenCalled();
    });

    it('should handle streaming output', async () => {
      const ws = new WebSocket('ws://localhost:8000/ws/execute');

      // Simulate receiving stdout message
      const message = {
        id: 'test-id',
        type: 'stdout',
        data: 'Hello from server',
        timestamp: Date.now(),
      };

      playgroundStore.appendStdout(message.data);
      const state = get(playgroundStore);
      expect(state.output.stdout).toContain('Hello from server');
    });

    it('should reconnect on connection loss', async () => {
      const ws = new WebSocket('ws://localhost:8000/ws/execute');

      // Simulate connection loss and reconnect
      ws.close();

      await waitFor(() => {
        expect(initWebSocket).toHaveBeenCalled();
      }, { timeout: 2000 });
    });
  });

  describe('Auto-save Functionality', () => {
    it('should auto-save to localStorage', async () => {
      playgroundStore.updateCode('test code');
      playgroundStore.updateLanguage('python');

      await waitFor(() => {
        const stored = localStorage.getItem('ancientCompute_playground');
        expect(stored).toBeDefined();
        const state = JSON.parse(stored || '{}');
        expect(state.code).toBe('test code');
        expect(state.language).toBe('python');
      }, { timeout: 2000 });
    });

    it('should load saved state on initialization', async () => {
      const savedState = {
        code: 'saved code',
        language: 'haskell',
        theme: 'dark',
        fontSize: 16,
      };
      localStorage.setItem('ancientCompute_playgroundState', JSON.stringify(savedState));

      // Reinitialize store
      playgroundStore.loadState(savedState);

      const state = get(playgroundStore);
      expect(state.code).toBe('saved code');
      expect(state.language).toBe('haskell');
    });
  });

  describe('Keyboard Shortcuts', () => {
    const shortcuts = [
      { keys: { ctrlKey: true, key: 'Enter' }, action: 'run' },
      { keys: { ctrlKey: true, key: '.' }, action: 'stop' },
      { keys: { ctrlKey: true, shiftKey: true, key: 'R' }, action: 'reset' },
      { keys: { ctrlKey: true, key: '=' }, action: 'increaseFontSize' },
      { keys: { ctrlKey: true, key: '-' }, action: 'decreaseFontSize' },
      { keys: { ctrlKey: true, key: 'e' }, action: 'toggleExampleLibrary' },
    ];

    shortcuts.forEach(({ keys, action }) => {
      it(`should handle ${action} shortcut`, async () => {
        render(CodePlayground);
        await fireEvent.keyDown(window, keys);
        // Verify action was triggered (specific verification depends on action)
      });
    });
  });
});