<script lang="ts">
  import { onMount, onDestroy } from 'svelte';
  import { page } from '$app/stores';
  import CodePlayground from '$lib/components/playground/CodePlayground.svelte';
  import { playgroundStore } from '$lib/stores/playgroundStore';
  import { checkHealth, fetchLanguages } from '$lib/api/execution';
  import type { LanguageCapabilities } from '$lib/api/execution';

  let isHealthy = false;
  let availableLanguages: LanguageCapabilities[] = [];
  let isLoading = true;
  let error: string | null = null;
  let playground: CodePlayground;

  // URL parameters for sharing
  $: {
    const code = $page.url.searchParams.get('code');
    const language = $page.url.searchParams.get('lang');

    if (code) {
      // Decode from base64
      try {
        const decoded = atob(code);
        playgroundStore.updateCode(decoded);
      } catch (e) {
        console.error('Failed to decode shared code:', e);
      }
    }

    if (language) {
      playgroundStore.updateLanguage(language);
    }
  }

  // Generate shareable URL
  function generateShareableUrl(): string {
    const state = playgroundStore.exportState();
    const encoded = btoa(state.code);
    const url = new URL(window.location.href);
    url.searchParams.set('code', encoded);
    url.searchParams.set('lang', state.language);
    return url.toString();
  }

  // Copy share URL to clipboard
  async function shareCode() {
    const url = generateShareableUrl();
    try {
      await navigator.clipboard.writeText(url);
      alert('Share URL copied to clipboard!');
    } catch (err) {
      console.error('Failed to copy URL:', err);
    }
  }

  // Keyboard shortcuts
  function handleKeydown(event: KeyboardEvent) {
    // Ctrl/Cmd + S to share
    if ((event.ctrlKey || event.metaKey) && event.key === 's') {
      event.preventDefault();
      shareCode();
    }
  }

  onMount(async () => {
    // Check service health
    isHealthy = await checkHealth();

    if (!isHealthy) {
      error = 'Code execution service is unavailable. Some features may not work.';
    }

    // Fetch available languages
    availableLanguages = await fetchLanguages();

    isLoading = false;
  });

  onDestroy(() => {
    // Clean up any resources if needed
  });
</script>

<svelte:head>
  <title>Code Playground | Ancient Compute</title>
  <meta name="description" content="Interactive code playground supporting 8 programming languages across 12,500 years of computational history" />
</svelte:head>

<svelte:window on:keydown={handleKeydown} />

<div class="playground-page">
  <div class="page-header">
    <div class="header-content">
      <h1 class="page-title">
        <span class="title-icon">🔬</span>
        Ancient Compute Playground
      </h1>
      <p class="page-subtitle">
        Execute code across 8 languages spanning 12,500 years of computational thought
      </p>
    </div>

    <div class="header-actions">
      <button class="share-btn" on:click={shareCode} title="Share code (Ctrl+S)">
        <svg width="16" height="16" viewBox="0 0 16 16">
          <path d="M13 3a2 2 0 11-2 2 2 2 0 012-2zM3 8a2 2 0 11-2 2 2 2 0 012-2zm10 5a2 2 0 11-2 2 2 2 0 012-2z" fill="none" stroke="currentColor" stroke-width="2"/>
          <path d="M11 5L5 8l6 3" stroke="currentColor" stroke-width="1.5" fill="none"/>
        </svg>
        Share
      </button>

      <div class="service-status" class:healthy={isHealthy} class:unhealthy={!isHealthy}>
        <span class="status-dot"></span>
        <span class="status-text">
          {isHealthy ? 'Service Online' : 'Service Offline'}
        </span>
      </div>
    </div>
  </div>

  {#if error}
    <div class="error-banner">
      <svg class="error-icon" width="20" height="20" viewBox="0 0 20 20">
        <circle cx="10" cy="10" r="9" fill="none" stroke="currentColor" stroke-width="2"/>
        <path d="M10 6v5M10 14v0" stroke="currentColor" stroke-width="2" stroke-linecap="round"/>
      </svg>
      <span>{error}</span>
    </div>
  {/if}

  <div class="playground-container">
    {#if isLoading}
      <div class="loading-container">
        <div class="loading-spinner"></div>
        <p>Initializing playground environment...</p>
      </div>
    {:else}
      <CodePlayground bind:this={playground} />
    {/if}
  </div>

  <div class="info-section">
    <div class="info-grid">
      <div class="info-card">
        <h3>🗿 Historical Languages</h3>
        <p>
          From Babbage's Analytical Engine assembly (1843) to modern dependent types,
          experience the evolution of programming paradigms.
        </p>
      </div>

      <div class="info-card">
        <h3>⚡ Real-time Execution</h3>
        <p>
          Code compiles through our unified pipeline to Babbage ISA,
          with WebSocket streaming for immediate feedback.
        </p>
      </div>

      <div class="info-card">
        <h3>🎓 Educational Examples</h3>
        <p>
          50+ historical algorithms from Egyptian multiplication to
          Church numerals, implemented across all 8 languages.
        </p>
      </div>

      <div class="info-card">
        <h3>🔍 Deep Analysis</h3>
        <p>
          View intermediate representation, assembly output, and
          compilation logs to understand the full compilation pipeline.
        </p>
      </div>
    </div>

    {#if availableLanguages.length > 0}
      <div class="languages-section">
        <h2>Supported Languages</h2>
        <div class="languages-grid">
          {#each availableLanguages as lang}
            <div class="language-card">
              <h4>{lang.name}</h4>
              <p class="language-version">v{lang.version}</p>
              <ul class="language-features">
                {#each lang.features.slice(0, 3) as feature}
                  <li>{feature}</li>
                {/each}
              </ul>
            </div>
          {/each}
        </div>
      </div>
    {/if}
  </div>
</div>

<style>
  .playground-page {
    min-height: 100vh;
    background: var(--color-page-bg, #0d1117);
  }

  .page-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    padding: 24px 32px;
    background: var(--color-header-bg, #161b22);
    border-bottom: 1px solid var(--color-border, #30363d);
  }

  .header-content {
    flex: 1;
  }

  .page-title {
    display: flex;
    align-items: center;
    gap: 12px;
    margin: 0 0 8px;
    font-size: 28px;
    font-weight: 700;
    color: var(--color-text, #c9d1d9);
  }

  .title-icon {
    font-size: 32px;
  }

  .page-subtitle {
    margin: 0;
    font-size: 16px;
    color: var(--color-text-secondary, #8b949e);
  }

  .header-actions {
    display: flex;
    align-items: center;
    gap: 16px;
  }

  .share-btn {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 8px 16px;
    background: var(--color-button-bg, #238636);
    color: white;
    border: none;
    border-radius: 6px;
    font-size: 14px;
    font-weight: 500;
    cursor: pointer;
    transition: all 0.2s;
  }

  .share-btn:hover {
    background: var(--color-button-hover, #2ea043);
  }

  .service-status {
    display: flex;
    align-items: center;
    gap: 6px;
    padding: 6px 12px;
    background: var(--color-status-bg, #1e2228);
    border-radius: 20px;
    font-size: 13px;
  }

  .status-dot {
    width: 8px;
    height: 8px;
    border-radius: 50%;
    background: var(--color-status, #8b949e);
  }

  .service-status.healthy .status-dot {
    background: var(--color-success, #3fb950);
    animation: pulse 2s infinite;
  }

  .service-status.unhealthy .status-dot {
    background: var(--color-error, #f85149);
  }

  @keyframes pulse {
    0%, 100% {
      opacity: 1;
    }
    50% {
      opacity: 0.5;
    }
  }

  .status-text {
    color: var(--color-text-secondary, #8b949e);
  }

  .error-banner {
    display: flex;
    align-items: center;
    gap: 12px;
    padding: 12px 32px;
    background: var(--color-error-bg, #3d1519);
    color: var(--color-error-text, #f85149);
    border-bottom: 1px solid var(--color-error-border, #5a1e1e);
  }

  .error-icon {
    flex-shrink: 0;
  }

  .playground-container {
    padding: 32px;
    max-width: 1600px;
    margin: 0 auto;
  }

  .loading-container {
    display: flex;
    flex-direction: column;
    align-items: center;
    justify-content: center;
    min-height: 600px;
    color: var(--color-text-secondary, #8b949e);
  }

  .loading-spinner {
    width: 48px;
    height: 48px;
    border: 3px solid var(--color-border, #30363d);
    border-top-color: var(--color-accent, #58a6ff);
    border-radius: 50%;
    animation: spin 1s linear infinite;
    margin-bottom: 16px;
  }

  @keyframes spin {
    to {
      transform: rotate(360deg);
    }
  }

  .info-section {
    padding: 48px 32px;
    background: var(--color-section-bg, #0d1117);
    border-top: 1px solid var(--color-border, #30363d);
  }

  .info-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    gap: 24px;
    max-width: 1200px;
    margin: 0 auto 48px;
  }

  .info-card {
    padding: 24px;
    background: var(--color-card-bg, #161b22);
    border: 1px solid var(--color-border, #30363d);
    border-radius: 8px;
  }

  .info-card h3 {
    margin: 0 0 12px;
    font-size: 18px;
    font-weight: 600;
    color: var(--color-text, #c9d1d9);
  }

  .info-card p {
    margin: 0;
    font-size: 14px;
    line-height: 1.6;
    color: var(--color-text-secondary, #8b949e);
  }

  .languages-section {
    max-width: 1200px;
    margin: 0 auto;
  }

  .languages-section h2 {
    margin: 0 0 24px;
    font-size: 24px;
    font-weight: 600;
    color: var(--color-text, #c9d1d9);
  }

  .languages-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(200px, 1fr));
    gap: 16px;
  }

  .language-card {
    padding: 16px;
    background: var(--color-card-bg, #161b22);
    border: 1px solid var(--color-border, #30363d);
    border-radius: 6px;
  }

  .language-card h4 {
    margin: 0 0 4px;
    font-size: 16px;
    font-weight: 600;
    color: var(--color-text, #c9d1d9);
  }

  .language-version {
    margin: 0 0 12px;
    font-size: 12px;
    color: var(--color-text-secondary, #8b949e);
  }

  .language-features {
    margin: 0;
    padding-left: 20px;
    list-style: disc;
  }

  .language-features li {
    font-size: 13px;
    color: var(--color-text-secondary, #8b949e);
    margin-bottom: 4px;
  }

  /* Responsive design */
  @media (max-width: 768px) {
    .page-header {
      flex-direction: column;
      align-items: flex-start;
      gap: 16px;
      padding: 20px;
    }

    .header-actions {
      width: 100%;
      justify-content: space-between;
    }

    .page-title {
      font-size: 24px;
    }

    .playground-container {
      padding: 16px;
    }

    .info-grid {
      grid-template-columns: 1fr;
    }

    .languages-grid {
      grid-template-columns: 1fr;
    }
  }
</style>