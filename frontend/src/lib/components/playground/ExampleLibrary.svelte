<script lang="ts">
  import { createEventDispatcher, onMount } from 'svelte';
  import { fade, fly, scale } from 'svelte/transition';
  import { flip } from 'svelte/animate';
  import { examplePrograms, type ExampleProgram } from '$lib/data/examplePrograms';

  const dispatch = createEventDispatcher();

  let searchQuery = '';
  let selectedLanguage = 'all';
  let selectedEra = 'all';
  let selectedDifficulty = 'all';
  let sortBy: 'name' | 'year' | 'difficulty' = 'name';
  let filteredExamples: ExampleProgram[] = [];
  let hoveredExample: string | null = null;
  let viewMode: 'grid' | 'list' = 'grid';

  // Extract unique values for filters
  const languages = ['all', ...new Set(examplePrograms.map((e) => e.language))];
  const eras = ['all', ...new Set(examplePrograms.map((e) => e.era))];
  const difficulties = ['all', 'beginner', 'intermediate', 'advanced'];

  // Filter and sort examples
  $: {
    filteredExamples = examplePrograms.filter((example) => {
      const matchesSearch =
        searchQuery === '' ||
        example.title.toLowerCase().includes(searchQuery.toLowerCase()) ||
        example.description.toLowerCase().includes(searchQuery.toLowerCase()) ||
        example.tags.some((tag) => tag.toLowerCase().includes(searchQuery.toLowerCase()));

      const matchesLanguage =
        selectedLanguage === 'all' || example.language === selectedLanguage;

      const matchesEra = selectedEra === 'all' || example.era === selectedEra;

      const matchesDifficulty =
        selectedDifficulty === 'all' || example.difficulty === selectedDifficulty;

      return matchesSearch && matchesLanguage && matchesEra && matchesDifficulty;
    });

    // Sort examples
    filteredExamples = [...filteredExamples].sort((a, b) => {
      switch (sortBy) {
        case 'name':
          return a.title.localeCompare(b.title);
        case 'year':
          return a.year - b.year;
        case 'difficulty':
          const diffOrder = { beginner: 1, intermediate: 2, advanced: 3 };
          return diffOrder[a.difficulty] - diffOrder[b.difficulty];
        default:
          return 0;
      }
    });
  }

  function loadExample(example: ExampleProgram) {
    dispatch('load', { code: example.code, language: example.language });
  }

  function closeLibrary() {
    dispatch('close');
  }

  function clearFilters() {
    searchQuery = '';
    selectedLanguage = 'all';
    selectedEra = 'all';
    selectedDifficulty = 'all';
  }

  function handleKeydown(event: KeyboardEvent) {
    if (event.key === 'Escape') {
      closeLibrary();
    }
  }

  // Focus search input on mount
  let searchInput: HTMLInputElement;
  onMount(() => {
    searchInput?.focus();
  });
</script>

<svelte:window on:keydown={handleKeydown} />

<div class="example-library-overlay" on:click={closeLibrary} transition:fade>
  <div
    class="example-library"
    on:click|stopPropagation
    transition:fly={{ y: 20, duration: 300 }}
  >
    <div class="library-header">
      <h2 class="library-title">Example Library</h2>
      <button class="close-btn" on:click={closeLibrary} title="Close (Esc)">
        <svg width="20" height="20" viewBox="0 0 20 20">
          <path
            d="M15 5L5 15M5 5l10 10"
            stroke="currentColor"
            stroke-width="2"
            stroke-linecap="round"
          />
        </svg>
      </button>
    </div>

    <div class="library-controls">
      <div class="search-box">
        <svg class="search-icon" width="16" height="16" viewBox="0 0 16 16">
          <circle cx="6" cy="6" r="5" fill="none" stroke="currentColor" stroke-width="2" />
          <path d="M10 10l4 4" stroke="currentColor" stroke-width="2" stroke-linecap="round" />
        </svg>
        <input
          bind:this={searchInput}
          bind:value={searchQuery}
          type="text"
          placeholder="Search examples..."
          class="search-input"
        />
        {#if searchQuery}
          <button
            class="clear-search"
            on:click={() => (searchQuery = '')}
            transition:scale
          >
            <svg width="14" height="14" viewBox="0 0 14 14">
              <path
                d="M10 4L4 10M4 4l6 6"
                stroke="currentColor"
                stroke-width="2"
                stroke-linecap="round"
              />
            </svg>
          </button>
        {/if}
      </div>

      <div class="filters">
        <select bind:value={selectedLanguage} class="filter-select">
          <option value="all">All Languages</option>
          {#each languages.slice(1) as lang}
            <option value={lang}>{lang.charAt(0).toUpperCase() + lang.slice(1)}</option>
          {/each}
        </select>

        <select bind:value={selectedEra} class="filter-select">
          <option value="all">All Eras</option>
          {#each eras.slice(1) as era}
            <option value={era}>{era}</option>
          {/each}
        </select>

        <select bind:value={selectedDifficulty} class="filter-select">
          <option value="all">All Levels</option>
          {#each difficulties.slice(1) as diff}
            <option value={diff}>{diff.charAt(0).toUpperCase() + diff.slice(1)}</option>
          {/each}
        </select>

        <select bind:value={sortBy} class="filter-select">
          <option value="name">Sort by Name</option>
          <option value="year">Sort by Year</option>
          <option value="difficulty">Sort by Difficulty</option>
        </select>

        {#if searchQuery || selectedLanguage !== 'all' || selectedEra !== 'all' || selectedDifficulty !== 'all'}
          <button class="clear-filters" on:click={clearFilters} transition:scale>
            Clear Filters
          </button>
        {/if}
      </div>

      <div class="view-controls">
        <button
          class="view-btn"
          class:active={viewMode === 'grid'}
          on:click={() => (viewMode = 'grid')}
          title="Grid view"
        >
          <svg width="16" height="16" viewBox="0 0 16 16">
            <rect x="2" y="2" width="5" height="5" fill="currentColor" />
            <rect x="9" y="2" width="5" height="5" fill="currentColor" />
            <rect x="2" y="9" width="5" height="5" fill="currentColor" />
            <rect x="9" y="9" width="5" height="5" fill="currentColor" />
          </svg>
        </button>
        <button
          class="view-btn"
          class:active={viewMode === 'list'}
          on:click={() => (viewMode = 'list')}
          title="List view"
        >
          <svg width="16" height="16" viewBox="0 0 16 16">
            <rect x="2" y="3" width="12" height="2" fill="currentColor" />
            <rect x="2" y="7" width="12" height="2" fill="currentColor" />
            <rect x="2" y="11" width="12" height="2" fill="currentColor" />
          </svg>
        </button>
      </div>
    </div>

    <div class="results-info">
      <span class="results-count">
        {filteredExamples.length} example{filteredExamples.length !== 1 ? 's' : ''} found
      </span>
    </div>

    <div class="library-content" class:list-view={viewMode === 'list'}>
      {#each filteredExamples as example (example.id)}
        <div
          class="example-card"
          class:list-item={viewMode === 'list'}
          on:click={() => loadExample(example)}
          on:mouseenter={() => (hoveredExample = example.id)}
          on:mouseleave={() => (hoveredExample = null)}
          animate:flip={{ duration: 200 }}
        >
          <div class="card-header">
            <h3 class="example-title">{example.title}</h3>
            <span
              class="language-badge"
              style="background: {example.languageColor}"
            >
              {example.language}
            </span>
          </div>

          <p class="example-description">{example.description}</p>

          <div class="card-meta">
            <span class="meta-item">
              <svg width="12" height="12" viewBox="0 0 12 12">
                <circle cx="6" cy="6" r="5" fill="none" stroke="currentColor" />
                <path d="M6 3v3l2 2" stroke="currentColor" fill="none" />
              </svg>
              {example.year > 0 ? example.year : Math.abs(example.year) + ' BC'}
            </span>
            <span class="meta-item">
              <svg width="12" height="12" viewBox="0 0 12 12">
                <path d="M6 2l1.5 3L11 6l-2.5 2.5L9 12H3l.5-3.5L1 6l3.5-1L6 2z" fill="currentColor" />
              </svg>
              {example.difficulty}
            </span>
            <span class="meta-item">
              <svg width="12" height="12" viewBox="0 0 12 12">
                <rect x="2" y="3" width="8" height="6" fill="none" stroke="currentColor" />
                <path d="M4 1v2M8 1v2M2 5h8" stroke="currentColor" />
              </svg>
              {example.era}
            </span>
          </div>

          {#if example.tags.length > 0}
            <div class="card-tags">
              {#each example.tags as tag}
                <span class="tag">{tag}</span>
              {/each}
            </div>
          {/if}

          {#if example.author}
            <div class="card-footer">
              <span class="author">By {example.author}</span>
            </div>
          {/if}

          {#if hoveredExample === example.id && viewMode === 'grid'}
            <div class="hover-preview" transition:fade={{ duration: 150 }}>
              <pre class="code-preview">{example.code.slice(0, 200)}...</pre>
            </div>
          {/if}
        </div>
      {/each}

      {#if filteredExamples.length === 0}
        <div class="empty-state">
          <svg class="empty-icon" width="64" height="64" viewBox="0 0 64 64">
            <circle cx="32" cy="32" r="30" fill="none" stroke="currentColor" stroke-width="2" opacity="0.3" />
            <path d="M22 32l8-8m0 0l8 8m-8-8v20" stroke="currentColor" stroke-width="2" opacity="0.3" />
          </svg>
          <p class="empty-message">No examples found</p>
          <p class="empty-hint">Try adjusting your filters or search terms</p>
          <button class="clear-filters-btn" on:click={clearFilters}>
            Clear All Filters
          </button>
        </div>
      {/if}
    </div>
  </div>
</div>

<style>
  .example-library-overlay {
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: rgba(0, 0, 0, 0.7);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 2000;
    padding: 20px;
  }

  .example-library {
    background: var(--color-modal-bg, #252526);
    border-radius: 8px;
    width: 100%;
    max-width: 1200px;
    max-height: 90vh;
    display: flex;
    flex-direction: column;
    box-shadow: 0 8px 32px rgba(0, 0, 0, 0.3);
  }

  .library-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 20px 24px;
    border-bottom: 1px solid var(--color-border, #3e3e42);
  }

  .library-title {
    margin: 0;
    font-size: 20px;
    font-weight: 600;
    color: var(--color-text, #cccccc);
  }

  .close-btn {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 32px;
    height: 32px;
    padding: 0;
    background: transparent;
    color: var(--color-text-secondary, #969696);
    border: none;
    border-radius: 4px;
    cursor: pointer;
    transition: all 0.2s;
  }

  .close-btn:hover {
    background: var(--color-hover, #2a2d2e);
    color: var(--color-text, #cccccc);
  }

  .library-controls {
    display: flex;
    gap: 16px;
    padding: 16px 24px;
    border-bottom: 1px solid var(--color-border, #3e3e42);
    flex-wrap: wrap;
  }

  .search-box {
    position: relative;
    flex: 1;
    min-width: 250px;
  }

  .search-icon {
    position: absolute;
    left: 12px;
    top: 50%;
    transform: translateY(-50%);
    color: var(--color-text-secondary, #969696);
    pointer-events: none;
  }

  .search-input {
    width: 100%;
    padding: 8px 36px 8px 36px;
    background: var(--color-input-bg, #1e1e1e);
    color: var(--color-text, #cccccc);
    border: 1px solid var(--color-border, #3e3e42);
    border-radius: 4px;
    font-size: 14px;
  }

  .search-input:focus {
    outline: none;
    border-color: var(--color-accent, #007acc);
  }

  .clear-search {
    position: absolute;
    right: 8px;
    top: 50%;
    transform: translateY(-50%);
    display: flex;
    align-items: center;
    justify-content: center;
    width: 24px;
    height: 24px;
    padding: 0;
    background: transparent;
    color: var(--color-text-secondary, #969696);
    border: none;
    border-radius: 4px;
    cursor: pointer;
  }

  .clear-search:hover {
    background: var(--color-hover, #2a2d2e);
  }

  .filters {
    display: flex;
    gap: 8px;
    align-items: center;
  }

  .filter-select {
    padding: 8px 12px;
    background: var(--color-input-bg, #1e1e1e);
    color: var(--color-text, #cccccc);
    border: 1px solid var(--color-border, #3e3e42);
    border-radius: 4px;
    font-size: 13px;
    cursor: pointer;
  }

  .filter-select:focus {
    outline: none;
    border-color: var(--color-accent, #007acc);
  }

  .clear-filters {
    padding: 8px 12px;
    background: var(--color-button-bg, #3e3e42);
    color: var(--color-text, #cccccc);
    border: none;
    border-radius: 4px;
    cursor: pointer;
    font-size: 13px;
    transition: all 0.2s;
  }

  .clear-filters:hover {
    background: var(--color-button-hover, #4e4e4e);
  }

  .view-controls {
    display: flex;
    gap: 2px;
    background: var(--color-view-bg, #1e1e1e);
    border-radius: 4px;
    padding: 2px;
  }

  .view-btn {
    display: flex;
    align-items: center;
    justify-content: center;
    width: 28px;
    height: 28px;
    padding: 0;
    background: transparent;
    color: var(--color-text-secondary, #969696);
    border: none;
    border-radius: 2px;
    cursor: pointer;
    transition: all 0.2s;
  }

  .view-btn:hover {
    background: var(--color-hover, #2a2d2e);
    color: var(--color-text, #cccccc);
  }

  .view-btn.active {
    background: var(--color-accent, #007acc);
    color: white;
  }

  .results-info {
    padding: 8px 24px;
    background: var(--color-info-bg, #1e1e1e);
  }

  .results-count {
    font-size: 12px;
    color: var(--color-text-secondary, #969696);
  }

  .library-content {
    flex: 1;
    overflow-y: auto;
    padding: 16px;
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
    gap: 16px;
  }

  .library-content.list-view {
    display: flex;
    flex-direction: column;
    gap: 8px;
  }

  .example-card {
    background: var(--color-card-bg, #1e1e1e);
    border: 1px solid var(--color-border, #3e3e42);
    border-radius: 6px;
    padding: 16px;
    cursor: pointer;
    transition: all 0.2s;
    position: relative;
  }

  .example-card:hover {
    background: var(--color-card-hover, #2a2d2e);
    border-color: var(--color-accent, #007acc);
    transform: translateY(-2px);
  }

  .example-card.list-item {
    display: flex;
    gap: 16px;
    align-items: center;
  }

  .card-header {
    display: flex;
    justify-content: space-between;
    align-items: flex-start;
    margin-bottom: 8px;
  }

  .example-title {
    margin: 0;
    font-size: 16px;
    font-weight: 600;
    color: var(--color-text, #cccccc);
    flex: 1;
  }

  .language-badge {
    padding: 4px 8px;
    color: white;
    font-size: 11px;
    font-weight: 600;
    border-radius: 12px;
    text-transform: uppercase;
  }

  .example-description {
    margin: 0 0 12px;
    font-size: 13px;
    color: var(--color-text-secondary, #969696);
    line-height: 1.5;
  }

  .card-meta {
    display: flex;
    gap: 12px;
    margin-bottom: 8px;
    font-size: 12px;
  }

  .meta-item {
    display: flex;
    align-items: center;
    gap: 4px;
    color: var(--color-text-secondary, #969696);
  }

  .card-tags {
    display: flex;
    flex-wrap: wrap;
    gap: 4px;
    margin-bottom: 8px;
  }

  .tag {
    padding: 2px 6px;
    background: var(--color-tag-bg, #3e3e42);
    color: var(--color-text-secondary, #969696);
    border-radius: 3px;
    font-size: 11px;
  }

  .card-footer {
    padding-top: 8px;
    border-top: 1px solid var(--color-border, #3e3e42);
  }

  .author {
    font-size: 12px;
    color: var(--color-text-secondary, #969696);
    font-style: italic;
  }

  .hover-preview {
    position: absolute;
    top: 100%;
    left: 0;
    right: 0;
    background: var(--color-preview-bg, #1e1e1e);
    border: 1px solid var(--color-accent, #007acc);
    border-radius: 4px;
    padding: 8px;
    margin-top: 4px;
    z-index: 10;
    box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
  }

  .code-preview {
    margin: 0;
    font-size: 11px;
    font-family: 'Fira Code', 'Monaco', monospace;
    color: var(--color-code, #9cdcfe);
    white-space: pre-wrap;
    word-break: break-word;
    max-height: 150px;
    overflow: hidden;
  }

  .empty-state {
    grid-column: 1 / -1;
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    padding: 48px;
    color: var(--color-text-secondary, #969696);
  }

  .empty-icon {
    margin-bottom: 16px;
  }

  .empty-message {
    font-size: 18px;
    font-weight: 500;
    margin: 0 0 8px;
  }

  .empty-hint {
    font-size: 14px;
    margin: 0 0 16px;
  }

  .clear-filters-btn {
    padding: 8px 16px;
    background: var(--color-accent, #007acc);
    color: white;
    border: none;
    border-radius: 4px;
    cursor: pointer;
    font-size: 14px;
    transition: all 0.2s;
  }

  .clear-filters-btn:hover {
    background: var(--color-accent-hover, #1177bb);
  }

  /* Scrollbar styling */
  .library-content::-webkit-scrollbar {
    width: 10px;
  }

  .library-content::-webkit-scrollbar-track {
    background: var(--color-scrollbar-track, #1e1e1e);
  }

  .library-content::-webkit-scrollbar-thumb {
    background: var(--color-scrollbar-thumb, #4e4e4e);
    border-radius: 5px;
  }

  .library-content::-webkit-scrollbar-thumb:hover {
    background: var(--color-scrollbar-hover, #5e5e5e);
  }
</style>