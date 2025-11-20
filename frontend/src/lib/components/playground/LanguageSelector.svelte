<script lang="ts">
  import { createEventDispatcher } from 'svelte';
  import { fade } from 'svelte/transition';

  export let currentLanguage: string = 'python';

  const dispatch = createEventDispatcher();

  interface LanguageInfo {
    id: string;
    name: string;
    icon: string;
    paradigm: string;
    typeSystem: string;
    year: string;
    shortcut: string;
    color: string;
    description: string;
  }

  const languages: LanguageInfo[] = [
    {
      id: 'c',
      name: 'C',
      icon: '⚙️',
      paradigm: 'Imperative',
      typeSystem: 'Weak static',
      year: '1972',
      shortcut: 'Alt+C',
      color: '#00599C',
      description: 'Low-level systems programming language'
    },
    {
      id: 'python',
      name: 'Python',
      icon: '🐍',
      paradigm: 'Multi-paradigm',
      typeSystem: 'Dynamic',
      year: '1991',
      shortcut: 'Alt+P',
      color: '#3776AB',
      description: 'High-level interpreted language'
    },
    {
      id: 'haskell',
      name: 'Haskell',
      icon: 'λ',
      paradigm: 'Functional',
      typeSystem: 'Strong static',
      year: '1990',
      shortcut: 'Alt+H',
      color: '#5D4F85',
      description: 'Pure functional programming language'
    },
    {
      id: 'java',
      name: 'Java',
      icon: '☕',
      paradigm: 'Object-oriented',
      typeSystem: 'Strong static',
      year: '1995',
      shortcut: 'Alt+J',
      color: '#007396',
      description: 'Object-oriented platform-independent language'
    },
    {
      id: 'lisp',
      name: 'LISP',
      icon: '()',
      paradigm: 'Functional',
      typeSystem: 'Dynamic',
      year: '1958',
      shortcut: 'Alt+L',
      color: '#3F3F3F',
      description: 'Oldest high-level programming language'
    },
    {
      id: 'idris2',
      name: 'IDRIS2',
      icon: '∀',
      paradigm: 'Functional',
      typeSystem: 'Dependent',
      year: '2020',
      shortcut: 'Alt+I',
      color: '#8B4513',
      description: 'Dependently typed programming language'
    },
    {
      id: 'systemf',
      name: 'System F',
      icon: 'Λ',
      paradigm: 'Functional',
      typeSystem: 'Polymorphic',
      year: '1974',
      shortcut: 'Alt+F',
      color: '#4B0082',
      description: 'Polymorphic lambda calculus'
    },
    {
      id: 'babbage-asm',
      name: 'Babbage Assembly',
      icon: '⚛',
      paradigm: 'Assembly',
      typeSystem: 'Untyped',
      year: '1837',
      shortcut: 'Alt+B',
      color: '#8B4513',
      description: 'Analytical Engine assembly language'
    }
  ];

  let isOpen = false;
  let hoveredLanguage: string | null = null;
  let recentLanguages: string[] = loadRecentLanguages();

  $: selectedLanguage = languages.find((l) => l.id === currentLanguage) || languages[1];

  function toggleDropdown() {
    isOpen = !isOpen;
  }

  function selectLanguage(langId: string) {
    if (langId !== currentLanguage) {
      dispatch('change', langId);
      updateRecentLanguages(langId);
    }
    isOpen = false;
  }

  function updateRecentLanguages(langId: string) {
    // Remove if exists and add to front
    recentLanguages = recentLanguages.filter((id) => id !== langId);
    recentLanguages.unshift(langId);
    // Keep only 3 recent languages
    recentLanguages = recentLanguages.slice(0, 3);
    saveRecentLanguages();
  }

  function saveRecentLanguages() {
    localStorage.setItem('ancientCompute_recentLanguages', JSON.stringify(recentLanguages));
  }

  function loadRecentLanguages(): string[] {
    const stored = localStorage.getItem('ancientCompute_recentLanguages');
    if (stored) {
      try {
        return JSON.parse(stored);
      } catch {
        return [];
      }
    }
    return [];
  }

  // Keyboard shortcuts
  function handleKeydown(event: KeyboardEvent) {
    if (event.altKey) {
      const shortcuts: Record<string, string> = {
        c: 'c',
        p: 'python',
        h: 'haskell',
        j: 'java',
        l: 'lisp',
        i: 'idris2',
        f: 'systemf',
        b: 'babbage-asm'
      };

      const key = event.key.toLowerCase();
      if (shortcuts[key]) {
        event.preventDefault();
        selectLanguage(shortcuts[key]);
      }
    }

    // Escape to close dropdown
    if (event.key === 'Escape' && isOpen) {
      isOpen = false;
    }
  }

  // Close dropdown when clicking outside
  function handleClickOutside(event: MouseEvent) {
    const target = event.target as HTMLElement;
    if (!target.closest('.language-selector')) {
      isOpen = false;
    }
  }
</script>

<svelte:window on:keydown={handleKeydown} on:click={handleClickOutside} />

<div class="language-selector">
  <button class="selector-button" on:click={toggleDropdown} aria-expanded={isOpen}>
    <span class="language-icon" style="color: {selectedLanguage.color}">
      {selectedLanguage.icon}
    </span>
    <span class="language-name">{selectedLanguage.name}</span>
    <span class="language-paradigm">({selectedLanguage.paradigm})</span>
    <svg
      class="dropdown-arrow"
      class:open={isOpen}
      width="12"
      height="12"
      viewBox="0 0 12 12"
    >
      <path d="M2 4l4 4 4-4" stroke="currentColor" fill="none" stroke-width="2" />
    </svg>
  </button>

  {#if isOpen}
    <div class="dropdown-menu" transition:fade={{ duration: 200 }}>
      {#if recentLanguages.length > 0}
        <div class="dropdown-section">
          <div class="section-header">Recently Used</div>
          {#each recentLanguages as langId}
            {@const lang = languages.find((l) => l.id === langId)}
            {#if lang}
              <button
                class="language-item recent"
                class:selected={langId === currentLanguage}
                on:click={() => selectLanguage(langId)}
                on:mouseenter={() => (hoveredLanguage = langId)}
                on:mouseleave={() => (hoveredLanguage = null)}
              >
                <span class="item-icon" style="color: {lang.color}">{lang.icon}</span>
                <span class="item-name">{lang.name}</span>
                <span class="item-shortcut">{lang.shortcut}</span>
              </button>
            {/if}
          {/each}
        </div>
        <div class="dropdown-divider"></div>
      {/if}

      <div class="dropdown-section">
        <div class="section-header">All Languages</div>
        {#each languages as lang}
          <button
            class="language-item"
            class:selected={lang.id === currentLanguage}
            on:click={() => selectLanguage(lang.id)}
            on:mouseenter={() => (hoveredLanguage = lang.id)}
            on:mouseleave={() => (hoveredLanguage = null)}
          >
            <div class="item-main">
              <span class="item-icon" style="color: {lang.color}">{lang.icon}</span>
              <span class="item-name">{lang.name}</span>
              <span class="item-year">{lang.year}</span>
              <span class="item-shortcut">{lang.shortcut}</span>
            </div>
            {#if hoveredLanguage === lang.id}
              <div class="item-tooltip" transition:fade={{ duration: 150 }}>
                <div class="tooltip-header">{lang.name}</div>
                <div class="tooltip-info">
                  <span class="info-label">Paradigm:</span>
                  <span>{lang.paradigm}</span>
                </div>
                <div class="tooltip-info">
                  <span class="info-label">Type System:</span>
                  <span>{lang.typeSystem}</span>
                </div>
                <div class="tooltip-description">{lang.description}</div>
              </div>
            {/if}
          </button>
        {/each}
      </div>

      <div class="dropdown-footer">
        <div class="footer-hint">
          <kbd>Alt</kbd> + <kbd>Letter</kbd> for quick selection
        </div>
      </div>
    </div>
  {/if}
</div>

<style>
  .language-selector {
    position: relative;
    font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  }

  .selector-button {
    display: flex;
    align-items: center;
    gap: 8px;
    padding: 8px 12px;
    background: var(--color-button-bg, #2d2d30);
    color: var(--color-text, #cccccc);
    border: 1px solid var(--color-border, #3e3e42);
    border-radius: 4px;
    cursor: pointer;
    font-size: 14px;
    transition: all 0.2s;
    min-width: 200px;
  }

  .selector-button:hover {
    background: var(--color-button-hover, #3e3e42);
    border-color: var(--color-border-hover, #515151);
  }

  .selector-button:focus {
    outline: none;
    border-color: var(--color-accent, #007acc);
    box-shadow: 0 0 0 1px var(--color-accent, #007acc);
  }

  .language-icon {
    font-size: 16px;
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 20px;
  }

  .language-name {
    font-weight: 500;
    flex: 1;
    text-align: left;
  }

  .language-paradigm {
    font-size: 12px;
    color: var(--color-text-secondary, #969696);
  }

  .dropdown-arrow {
    transition: transform 0.2s;
  }

  .dropdown-arrow.open {
    transform: rotate(180deg);
  }

  .dropdown-menu {
    position: absolute;
    top: calc(100% + 4px);
    left: 0;
    width: 320px;
    max-height: 500px;
    overflow-y: auto;
    background: var(--color-dropdown-bg, #252526);
    border: 1px solid var(--color-border, #3e3e42);
    border-radius: 4px;
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.15);
    z-index: 1000;
  }

  .dropdown-section {
    padding: 4px;
  }

  .section-header {
    padding: 8px 12px 4px;
    font-size: 11px;
    font-weight: 600;
    text-transform: uppercase;
    color: var(--color-text-secondary, #969696);
    letter-spacing: 0.5px;
  }

  .dropdown-divider {
    height: 1px;
    background: var(--color-border, #3e3e42);
    margin: 4px 0;
  }

  .language-item {
    position: relative;
    display: flex;
    align-items: center;
    width: 100%;
    padding: 8px 12px;
    background: transparent;
    color: var(--color-text, #cccccc);
    border: none;
    border-radius: 2px;
    cursor: pointer;
    font-size: 13px;
    transition: background 0.15s;
    text-align: left;
  }

  .language-item:hover {
    background: var(--color-hover, #2a2d2e);
  }

  .language-item.selected {
    background: var(--color-selected, #094771);
    color: white;
  }

  .language-item.recent {
    background: var(--color-recent, #1e1e1e);
  }

  .item-main {
    display: flex;
    align-items: center;
    gap: 8px;
    width: 100%;
  }

  .item-icon {
    font-size: 14px;
    width: 20px;
    display: inline-flex;
    align-items: center;
    justify-content: center;
  }

  .item-name {
    flex: 1;
    font-weight: 500;
  }

  .item-year {
    font-size: 11px;
    color: var(--color-text-secondary, #969696);
    margin-right: 8px;
  }

  .item-shortcut {
    font-size: 11px;
    color: var(--color-text-secondary, #969696);
    font-family: monospace;
  }

  .item-tooltip {
    position: absolute;
    left: calc(100% + 8px);
    top: 0;
    width: 250px;
    padding: 12px;
    background: var(--color-tooltip-bg, #1e1e1e);
    border: 1px solid var(--color-border, #3e3e42);
    border-radius: 4px;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);
    z-index: 1001;
  }

  .tooltip-header {
    font-size: 14px;
    font-weight: 600;
    margin-bottom: 8px;
    color: var(--color-text, #cccccc);
  }

  .tooltip-info {
    display: flex;
    gap: 8px;
    margin-bottom: 4px;
    font-size: 12px;
  }

  .info-label {
    font-weight: 500;
    color: var(--color-text-secondary, #969696);
  }

  .tooltip-description {
    margin-top: 8px;
    padding-top: 8px;
    border-top: 1px solid var(--color-border, #3e3e42);
    font-size: 12px;
    color: var(--color-text-secondary, #969696);
    line-height: 1.4;
  }

  .dropdown-footer {
    padding: 8px 12px;
    border-top: 1px solid var(--color-border, #3e3e42);
    background: var(--color-footer-bg, #1e1e1e);
  }

  .footer-hint {
    font-size: 11px;
    color: var(--color-text-secondary, #969696);
    display: flex;
    align-items: center;
    gap: 4px;
  }

  kbd {
    padding: 2px 4px;
    background: var(--color-kbd-bg, #3e3e42);
    border: 1px solid var(--color-border, #515151);
    border-radius: 2px;
    font-size: 10px;
    font-family: monospace;
  }

  /* Scrollbar styles */
  .dropdown-menu::-webkit-scrollbar {
    width: 8px;
  }

  .dropdown-menu::-webkit-scrollbar-track {
    background: var(--color-scrollbar-track, #1e1e1e);
  }

  .dropdown-menu::-webkit-scrollbar-thumb {
    background: var(--color-scrollbar-thumb, #4e4e4e);
    border-radius: 4px;
  }

  .dropdown-menu::-webkit-scrollbar-thumb:hover {
    background: var(--color-scrollbar-hover, #5e5e5e);
  }
</style>