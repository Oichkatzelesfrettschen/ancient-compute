<!--
  HelpPanel.svelte
  Keyboard shortcuts help panel

  Features:
  - Display all registered shortcuts
  - Organized by category (Global, Emulator, Playground)
  - Modal overlay with Esc to close
  - Triggered by Ctrl+/ or ? key
-->
<script lang="ts">
  import { onMount, onDestroy } from 'svelte';
  import KeyboardShortcutManager, { type ShortcutCategory } from '$lib/utils/KeyboardShortcutManager';

  // Props
  export let visible = false;

  // State
  let categories: ShortcutCategory[] = [];
  let manager: KeyboardShortcutManager;

  onMount(() => {
    manager = KeyboardShortcutManager.getInstance();
    loadShortcuts();
  });

  /**
   * Load shortcuts from manager
   */
  function loadShortcuts() {
    categories = manager.getShortcutsByCategory();
  }

  /**
   * Close panel
   */
  function close() {
    visible = false;
  }

  /**
   * Handle overlay click
   */
  function handleOverlayClick(event: MouseEvent) {
    if (event.target === event.currentTarget) {
      close();
    }
  }

  /**
   * Handle keyboard events
   */
  function handleKeyDown(event: KeyboardEvent) {
    if (event.key === 'Escape') {
      close();
    }
  }
</script>

<svelte:window on:keydown={handleKeyDown} />

<style>
  .help-overlay {
    position: fixed;
    top: 0;
    left: 0;
    width: 100vw;
    height: 100vh;
    background: rgba(0, 0, 0, 0.7);
    backdrop-filter: blur(4px);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 10000;
    animation: fadeIn 0.2s ease-out;
  }

  @keyframes fadeIn {
    from {
      opacity: 0;
    }
    to {
      opacity: 1;
    }
  }

  .help-panel {
    background: var(--panel-bg, #1a1a1a);
    border: 2px solid var(--border-color, #333);
    border-radius: 12px;
    max-width: 800px;
    max-height: 90vh;
    width: 90%;
    overflow-y: auto;
    box-shadow: 0 8px 32px rgba(0, 0, 0, 0.5);
    animation: slideIn 0.3s ease-out;
  }

  @keyframes slideIn {
    from {
      transform: translateY(-30px);
      opacity: 0;
    }
    to {
      transform: translateY(0);
      opacity: 1;
    }
  }

  .help-header {
    padding: 1.5rem;
    border-bottom: 1px solid var(--border-color, #333);
    display: flex;
    justify-content: space-between;
    align-items: center;
    position: sticky;
    top: 0;
    background: var(--panel-bg, #1a1a1a);
    z-index: 1;
  }

  .help-title {
    font-size: 1.5rem;
    font-weight: bold;
    color: var(--text-primary, #e0e0e0);
    font-family: 'Courier New', monospace;
  }

  .close-button {
    padding: 0.5rem 1rem;
    background: var(--button-bg, #2a2a2a);
    border: 1px solid var(--border-color, #444);
    border-radius: 6px;
    color: var(--text-primary, #e0e0e0);
    cursor: pointer;
    font-family: 'Courier New', monospace;
    font-size: 0.9rem;
    transition: all 0.2s;
  }

  .close-button:hover {
    background: var(--button-hover-bg, #3a3a3a);
    border-color: #555;
  }

  .help-content {
    padding: 1.5rem;
  }

  .category {
    margin-bottom: 2rem;
  }

  .category:last-child {
    margin-bottom: 0;
  }

  .category-title {
    font-size: 1.1rem;
    font-weight: bold;
    color: var(--accent-color, #4caf50);
    margin-bottom: 1rem;
    font-family: 'Courier New', monospace;
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }

  .category-icon {
    font-size: 1.3rem;
  }

  .shortcuts-list {
    display: flex;
    flex-direction: column;
    gap: 0.75rem;
  }

  .shortcut-item {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 0.75rem;
    background: var(--card-bg, #0a0a0a);
    border: 1px solid var(--border-color, #333);
    border-radius: 6px;
    transition: all 0.2s;
  }

  .shortcut-item:hover {
    background: var(--card-hover-bg, #1a1a1a);
    border-color: #444;
  }

  .shortcut-description {
    color: var(--text-primary, #e0e0e0);
    font-family: 'Courier New', monospace;
    font-size: 0.9rem;
  }

  .shortcut-keys {
    display: flex;
    gap: 0.25rem;
    align-items: center;
  }

  .key {
    padding: 0.25rem 0.5rem;
    background: var(--key-bg, #2a2a2a);
    border: 1px solid var(--border-color, #444);
    border-radius: 4px;
    color: var(--text-secondary, #999);
    font-family: 'Courier New', monospace;
    font-size: 0.85rem;
    font-weight: bold;
    min-width: 2rem;
    text-align: center;
    box-shadow: 0 2px 4px rgba(0, 0, 0, 0.3);
  }

  .key-separator {
    color: var(--text-secondary, #666);
    font-weight: bold;
  }

  .help-footer {
    padding: 1rem 1.5rem;
    border-top: 1px solid var(--border-color, #333);
    color: var(--text-secondary, #999);
    font-size: 0.85rem;
    text-align: center;
    font-family: 'Courier New', monospace;
    background: var(--panel-bg, #1a1a1a);
  }

  /* Scrollbar styling */
  .help-panel::-webkit-scrollbar {
    width: 8px;
  }

  .help-panel::-webkit-scrollbar-track {
    background: #0a0a0a;
  }

  .help-panel::-webkit-scrollbar-thumb {
    background: #444;
    border-radius: 4px;
  }

  .help-panel::-webkit-scrollbar-thumb:hover {
    background: #555;
  }
</style>

{#if visible}
  <div class="help-overlay" on:click={handleOverlayClick}>
    <div class="help-panel" role="dialog" aria-labelledby="help-title" aria-modal="true">
      <!-- Header -->
      <div class="help-header">
        <h2 id="help-title" class="help-title">⌨️ Keyboard Shortcuts</h2>
        <button class="close-button" on:click={close} aria-label="Close help panel">
          Close (Esc)
        </button>
      </div>

      <!-- Content -->
      <div class="help-content">
        {#each categories as category}
          <div class="category">
            <h3 class="category-title">
              <span class="category-icon">
                {#if category.name === 'Global'}
                  🌐
                {:else if category.name === 'Emulator'}
                  ⚙️
                {:else if category.name === 'Playground'}
                  🎮
                {:else if category.name === 'Dashboard'}
                  📊
                {/if}
              </span>
              {category.name}
            </h3>

            <div class="shortcuts-list">
              {#each category.shortcuts as shortcut}
                <div class="shortcut-item">
                  <span class="shortcut-description">{shortcut.description}</span>
                  <div class="shortcut-keys">
                    {#if shortcut.modifiers?.ctrl}
                      <span class="key">Ctrl</span>
                      <span class="key-separator">+</span>
                    {/if}
                    {#if shortcut.modifiers?.shift}
                      <span class="key">Shift</span>
                      <span class="key-separator">+</span>
                    {/if}
                    {#if shortcut.modifiers?.alt}
                      <span class="key">Alt</span>
                      <span class="key-separator">+</span>
                    {/if}
                    {#if shortcut.modifiers?.meta}
                      <span class="key">Meta</span>
                      <span class="key-separator">+</span>
                    {/if}
                    <span class="key">{KeyboardShortcutManager.formatShortcut(shortcut).split('+').pop()}</span>
                  </div>
                </div>
              {/each}
            </div>
          </div>
        {/each}
      </div>

      <!-- Footer -->
      <div class="help-footer">
        Press <strong>Ctrl+/</strong> to toggle this help panel
      </div>
    </div>
  </div>
{/if}
