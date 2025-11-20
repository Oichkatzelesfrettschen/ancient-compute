import { writable, derived } from 'svelte/store';

export interface ExecutionOutput {
  stdout: string;
  stderr: string;
  compilationLog: string;
  ir: string;
  assembly: string;
  exitCode?: number;
  memoryUsage?: number;
}

export interface PlaygroundState {
  code: string;
  language: string;
  output: ExecutionOutput;
  isExecuting: boolean;
  executionTime: number;
  theme: 'light' | 'dark' | 'victorian';
  fontSize: number;
  exampleLibraryOpen: boolean;
  debugMode: boolean;
  breakpoints: number[];
  currentLine?: number;
  watchedVariables: string[];
  executionHistory: ExecutionRecord[];
  settings: PlaygroundSettings;
}

export interface ExecutionRecord {
  id: string;
  timestamp: Date;
  code: string;
  language: string;
  output: ExecutionOutput;
  executionTime: number;
  success: boolean;
}

export interface PlaygroundSettings {
  autoSave: boolean;
  autoFormat: boolean;
  showLineNumbers: boolean;
  showMinimap: boolean;
  wordWrap: boolean;
  tabSize: number;
  insertSpaces: boolean;
  fontLigatures: boolean;
  bracketPairColorization: boolean;
  semanticHighlighting: boolean;
  foldingStrategy: 'auto' | 'indentation';
  renderWhitespace: 'none' | 'boundary' | 'selection' | 'trailing' | 'all';
}

// Default settings
const defaultSettings: PlaygroundSettings = {
  autoSave: true,
  autoFormat: false,
  showLineNumbers: true,
  showMinimap: true,
  wordWrap: false,
  tabSize: 2,
  insertSpaces: true,
  fontLigatures: true,
  bracketPairColorization: true,
  semanticHighlighting: true,
  foldingStrategy: 'auto',
  renderWhitespace: 'selection',
};

// Initial state
const initialState: PlaygroundState = {
  code: '',
  language: 'python',
  output: {
    stdout: '',
    stderr: '',
    compilationLog: '',
    ir: '',
    assembly: '',
  },
  isExecuting: false,
  executionTime: 0,
  theme: 'dark',
  fontSize: 14,
  exampleLibraryOpen: false,
  debugMode: false,
  breakpoints: [],
  currentLine: undefined,
  watchedVariables: [],
  executionHistory: [],
  settings: defaultSettings,
};

// Create the store
function createPlaygroundStore() {
  const { subscribe, set, update } = writable<PlaygroundState>(initialState);

  return {
    subscribe,

    // Code management
    updateCode: (code: string) => update(state => ({ ...state, code })),

    updateLanguage: (language: string) => update(state => ({
      ...state,
      language,
      // Clear language-specific output when switching
      output: {
        ...state.output,
        ir: '',
        assembly: '',
      }
    })),

    // Execution management
    startExecution: () => update(state => ({
      ...state,
      isExecuting: true,
      executionTime: 0,
      output: {
        stdout: '',
        stderr: '',
        compilationLog: '',
        ir: '',
        assembly: '',
      }
    })),

    stopExecution: () => update(state => ({
      ...state,
      isExecuting: false,
    })),

    updateOutput: (output: Partial<ExecutionOutput>) => update(state => ({
      ...state,
      output: {
        ...state.output,
        ...output,
      }
    })),

    appendStdout: (text: string) => update(state => ({
      ...state,
      output: {
        ...state.output,
        stdout: state.output.stdout + text,
      }
    })),

    appendStderr: (text: string) => update(state => ({
      ...state,
      output: {
        ...state.output,
        stderr: state.output.stderr + text,
      }
    })),

    setExecutionTime: (time: number) => update(state => ({
      ...state,
      executionTime: time,
    })),

    clearOutput: () => update(state => ({
      ...state,
      output: {
        stdout: '',
        stderr: '',
        compilationLog: '',
        ir: '',
        assembly: '',
      },
      executionTime: 0,
    })),

    // Theme and appearance
    updateTheme: (theme: 'light' | 'dark' | 'victorian') => update(state => ({
      ...state,
      theme,
    })),

    updateFontSize: (fontSize: number) => update(state => ({
      ...state,
      fontSize: Math.max(10, Math.min(24, fontSize)),
    })),

    increaseFontSize: () => update(state => ({
      ...state,
      fontSize: Math.min(24, state.fontSize + 1),
    })),

    decreaseFontSize: () => update(state => ({
      ...state,
      fontSize: Math.max(10, state.fontSize - 1),
    })),

    // Example library
    toggleExampleLibrary: () => update(state => ({
      ...state,
      exampleLibraryOpen: !state.exampleLibraryOpen,
    })),

    // Debug mode
    toggleDebugMode: () => update(state => ({
      ...state,
      debugMode: !state.debugMode,
    })),

    addBreakpoint: (line: number) => update(state => ({
      ...state,
      breakpoints: state.breakpoints.includes(line)
        ? state.breakpoints
        : [...state.breakpoints, line].sort((a, b) => a - b),
    })),

    removeBreakpoint: (line: number) => update(state => ({
      ...state,
      breakpoints: state.breakpoints.filter(bp => bp !== line),
    })),

    clearBreakpoints: () => update(state => ({
      ...state,
      breakpoints: [],
    })),

    setCurrentLine: (line: number | undefined) => update(state => ({
      ...state,
      currentLine: line,
    })),

    // Variable watching
    addWatchedVariable: (variable: string) => update(state => ({
      ...state,
      watchedVariables: state.watchedVariables.includes(variable)
        ? state.watchedVariables
        : [...state.watchedVariables, variable],
    })),

    removeWatchedVariable: (variable: string) => update(state => ({
      ...state,
      watchedVariables: state.watchedVariables.filter(v => v !== variable),
    })),

    // Execution history
    addToHistory: (record: Omit<ExecutionRecord, 'id' | 'timestamp'>) => update(state => {
      const newRecord: ExecutionRecord = {
        ...record,
        id: crypto.randomUUID(),
        timestamp: new Date(),
      };

      // Keep only last 50 records
      const history = [newRecord, ...state.executionHistory].slice(0, 50);

      return {
        ...state,
        executionHistory: history,
      };
    }),

    clearHistory: () => update(state => ({
      ...state,
      executionHistory: [],
    })),

    // Settings management
    updateSettings: (settings: Partial<PlaygroundSettings>) => update(state => ({
      ...state,
      settings: {
        ...state.settings,
        ...settings,
      }
    })),

    resetSettings: () => update(state => ({
      ...state,
      settings: defaultSettings,
    })),

    // Reset everything
    reset: () => set(initialState),

    // Load state from storage
    loadState: (savedState: Partial<PlaygroundState>) => update(state => ({
      ...state,
      ...savedState,
      // Don't restore execution state
      isExecuting: false,
      executionTime: 0,
    })),

    // Export state for saving
    exportState: () => {
      let currentState: PlaygroundState;
      const unsubscribe = subscribe(state => {
        currentState = state;
      });
      unsubscribe();
      return currentState!;
    },
  };
}

// Create and export the store instance
export const playgroundStore = createPlaygroundStore();

// Derived stores for specific parts of the state
export const code = derived(playgroundStore, $store => $store.code);
export const language = derived(playgroundStore, $store => $store.language);
export const output = derived(playgroundStore, $store => $store.output);
export const isExecuting = derived(playgroundStore, $store => $store.isExecuting);
export const theme = derived(playgroundStore, $store => $store.theme);
export const fontSize = derived(playgroundStore, $store => $store.fontSize);
export const breakpoints = derived(playgroundStore, $store => $store.breakpoints);
export const settings = derived(playgroundStore, $store => $store.settings);

// Auto-save to localStorage when state changes (debounced)
let saveTimeout: ReturnType<typeof setTimeout>;
playgroundStore.subscribe(state => {
  if (state.settings.autoSave) {
    clearTimeout(saveTimeout);
    saveTimeout = setTimeout(() => {
      const stateToSave = {
        code: state.code,
        language: state.language,
        theme: state.theme,
        fontSize: state.fontSize,
        settings: state.settings,
      };
      localStorage.setItem('ancientCompute_playgroundState', JSON.stringify(stateToSave));
    }, 1000);
  }
});

// Load saved state on initialization (if available)
if (typeof window !== 'undefined') {
  const savedState = localStorage.getItem('ancientCompute_playgroundState');
  if (savedState) {
    try {
      const parsed = JSON.parse(savedState);
      playgroundStore.loadState(parsed);
    } catch (e) {
      console.error('Failed to load saved playground state:', e);
    }
  }
}