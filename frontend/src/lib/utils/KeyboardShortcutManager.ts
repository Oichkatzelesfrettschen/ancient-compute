/**
 * KeyboardShortcutManager.ts
 * Global keyboard shortcut handling with context awareness
 *
 * Features:
 * - Context-aware shortcuts (emulator, playground, dashboard)
 * - Modifier key support (Ctrl, Shift, Alt)
 * - Prevent conflicts with browser shortcuts
 * - Enable/disable shortcuts dynamically
 * - Help panel integration
 */

export type ShortcutContext = 'global' | 'emulator' | 'playground' | 'dashboard';
export type ModifierKeys = {
  ctrl?: boolean;
  shift?: boolean;
  alt?: boolean;
  meta?: boolean;
};

export interface Shortcut {
  key: string;
  modifiers?: ModifierKeys;
  context?: ShortcutContext;
  description: string;
  handler: () => void;
  enabled?: boolean;
}

export interface ShortcutCategory {
  name: string;
  shortcuts: Shortcut[];
}

/**
 * Global keyboard shortcut manager
 * Singleton instance to manage keyboard events across the application
 */
export class KeyboardShortcutManager {
  private static instance: KeyboardShortcutManager | null = null;
  private shortcuts: Map<string, Shortcut> = new Map();
  private currentContext: ShortcutContext = 'global';
  private enabled: boolean = true;
  private boundHandler: ((event: KeyboardEvent) => void) | null = null;

  private constructor() {
    this.boundHandler = this.handleKeyDown.bind(this);
  }

  /**
   * Get singleton instance
   */
  public static getInstance(): KeyboardShortcutManager {
    if (!KeyboardShortcutManager.instance) {
      KeyboardShortcutManager.instance = new KeyboardShortcutManager();
    }
    return KeyboardShortcutManager.instance;
  }

  /**
   * Initialize keyboard event listener
   */
  public init(): void {
    if (this.boundHandler) {
      window.addEventListener('keydown', this.boundHandler);
    }
  }

  /**
   * Cleanup keyboard event listener
   */
  public destroy(): void {
    if (this.boundHandler) {
      window.removeEventListener('keydown', this.boundHandler);
    }
  }

  /**
   * Register a keyboard shortcut
   */
  public register(
    id: string,
    key: string,
    handler: () => void,
    options: {
      modifiers?: ModifierKeys;
      context?: ShortcutContext;
      description?: string;
      enabled?: boolean;
    } = {}
  ): void {
    const shortcut: Shortcut = {
      key: key.toLowerCase(),
      modifiers: options.modifiers || {},
      context: options.context || 'global',
      description: options.description || '',
      handler,
      enabled: options.enabled !== false,
    };

    this.shortcuts.set(id, shortcut);
  }

  /**
   * Unregister a keyboard shortcut
   */
  public unregister(id: string): void {
    this.shortcuts.delete(id);
  }

  /**
   * Set current context (e.g., when navigating to different pages)
   */
  public setContext(context: ShortcutContext): void {
    this.currentContext = context;
  }

  /**
   * Enable/disable all shortcuts
   */
  public setEnabled(enabled: boolean): void {
    this.enabled = enabled;
  }

  /**
   * Enable/disable specific shortcut
   */
  public setShortcutEnabled(id: string, enabled: boolean): void {
    const shortcut = this.shortcuts.get(id);
    if (shortcut) {
      shortcut.enabled = enabled;
    }
  }

  /**
   * Get all registered shortcuts grouped by category
   */
  public getShortcutsByCategory(): ShortcutCategory[] {
    const categories: Record<string, Shortcut[]> = {
      'Global': [],
      'Emulator': [],
      'Playground': [],
      'Dashboard': [],
    };

    this.shortcuts.forEach((shortcut) => {
      const category = this.contextToCategory(shortcut.context || 'global');
      categories[category].push(shortcut);
    });

    return Object.entries(categories)
      .filter(([_, shortcuts]) => shortcuts.length > 0)
      .map(([name, shortcuts]) => ({ name, shortcuts }));
  }

  /**
   * Handle keyboard event
   */
  private handleKeyDown(event: KeyboardEvent): void {
    if (!this.enabled) return;

    // Don't interfere with input elements
    const target = event.target as HTMLElement;
    if (
      target.tagName === 'INPUT' ||
      target.tagName === 'TEXTAREA' ||
      target.isContentEditable
    ) {
      return;
    }

    const key = event.key.toLowerCase();
    const modifiers: ModifierKeys = {
      ctrl: event.ctrlKey,
      shift: event.shiftKey,
      alt: event.altKey,
      meta: event.metaKey,
    };

    // Find matching shortcut
    for (const [id, shortcut] of this.shortcuts.entries()) {
      if (!shortcut.enabled) continue;

      // Check context (global shortcuts work in all contexts)
      if (
        shortcut.context !== 'global' &&
        shortcut.context !== this.currentContext
      ) {
        continue;
      }

      // Check key match
      if (shortcut.key !== key) continue;

      // Check modifier match
      const modMatch = this.modifiersMatch(modifiers, shortcut.modifiers || {});
      if (!modMatch) continue;

      // Execute handler and prevent default
      event.preventDefault();
      event.stopPropagation();
      shortcut.handler();
      return;
    }
  }

  /**
   * Check if modifier keys match
   */
  private modifiersMatch(
    actual: ModifierKeys,
    expected: ModifierKeys
  ): boolean {
    return (
      (actual.ctrl || false) === (expected.ctrl || false) &&
      (actual.shift || false) === (expected.shift || false) &&
      (actual.alt || false) === (expected.alt || false) &&
      (actual.meta || false) === (expected.meta || false)
    );
  }

  /**
   * Convert context to category name
   */
  private contextToCategory(context: ShortcutContext): string {
    switch (context) {
      case 'global':
        return 'Global';
      case 'emulator':
        return 'Emulator';
      case 'playground':
        return 'Playground';
      case 'dashboard':
        return 'Dashboard';
      default:
        return 'Other';
    }
  }

  /**
   * Format shortcut for display (e.g., "Ctrl+Shift+P")
   */
  public static formatShortcut(shortcut: Shortcut): string {
    const parts: string[] = [];

    if (shortcut.modifiers?.ctrl) parts.push('Ctrl');
    if (shortcut.modifiers?.shift) parts.push('Shift');
    if (shortcut.modifiers?.alt) parts.push('Alt');
    if (shortcut.modifiers?.meta) parts.push('Meta');

    // Special key names
    const keyName = KeyboardShortcutManager.formatKey(shortcut.key);
    parts.push(keyName);

    return parts.join('+');
  }

  /**
   * Format key name for display
   */
  private static formatKey(key: string): string {
    const specialKeys: Record<string, string> = {
      'escape': 'Esc',
      'enter': 'Enter',
      'tab': 'Tab',
      'backspace': 'Backspace',
      'delete': 'Delete',
      'arrowup': '↑',
      'arrowdown': '↓',
      'arrowleft': '←',
      'arrowright': '→',
      ' ': 'Space',
      'f1': 'F1',
      'f2': 'F2',
      'f3': 'F3',
      'f4': 'F4',
      'f5': 'F5',
      'f6': 'F6',
      'f7': 'F7',
      'f8': 'F8',
      'f9': 'F9',
      'f10': 'F10',
      'f11': 'F11',
      'f12': 'F12',
    };

    return specialKeys[key.toLowerCase()] || key.toUpperCase();
  }
}

/**
 * Hook for Svelte components to use keyboard shortcuts
 */
export function useKeyboardShortcuts() {
  const manager = KeyboardShortcutManager.getInstance();
  return manager;
}

/**
 * Default export for convenience
 */
export default KeyboardShortcutManager;
