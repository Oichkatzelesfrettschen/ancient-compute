<!--
  ExecutionTimeline.svelte
  Visual timeline of emulator execution with phase coloring

  Features:
  - D3.js timeline visualization
  - Color-coded execution phases
  - Zoom and pan controls
  - Click to inspect cycle details
  - Export as PNG/SVG
-->
<script lang="ts">
  import { onMount, onDestroy } from 'svelte';
  import * as d3 from 'd3';
  import { machineStateStore } from '$lib/visualization/state/MachineStateStore';

  // Props
  export let height = 200;
  export let showPhases = true;
  export let showMilestones = true;

  // State
  let svg: SVGSVGElement;
  let container: HTMLDivElement;
  let timeline: any = null;
  let zoom: any = null;
  let executionData: any[] = [];
  let selectedCycle: number | null = null;

  // Phase colors
  const phaseColors: Record<string, string> = {
    'fetch': '#3498db',      // Blue
    'decode': '#9b59b6',     // Purple
    'execute': '#e74c3c',    // Red
    'writeback': '#2ecc71',  // Green
    'idle': '#95a5a6',       // Gray
    'memory': '#f39c12',     // Orange
    'arithmetic': '#e67e22', // Dark orange
    'control': '#1abc9c'     // Teal
  };

  // Reactive data
  $: history = machineStateStore.getHistory();
  $: if (history.length > 0) {
    updateTimelineData(history);
  }

  onMount(() => {
    initializeTimeline();
  });

  onDestroy(() => {
    if (timeline) {
      timeline.remove();
    }
  });

  /**
   * Initialize D3 timeline
   */
  function initializeTimeline() {
    const width = container.clientWidth;
    const margin = { top: 20, right: 30, bottom: 30, left: 50 };

    // Create SVG
    const svgSelection = d3.select(svg)
      .attr('width', width)
      .attr('height', height);

    // Clear existing content
    svgSelection.selectAll('*').remove();

    // Create main group
    const g = svgSelection.append('g')
      .attr('transform', `translate(${margin.left},${margin.top})`);

    // Setup zoom behavior
    zoom = d3.zoom()
      .scaleExtent([1, 50])
      .translateExtent([[0, 0], [width, height]])
      .on('zoom', handleZoom);

    svgSelection.call(zoom);

    // Create scales
    const xScale = d3.scaleLinear()
      .domain([0, 100]) // Will be updated with actual data
      .range([0, width - margin.left - margin.right]);

    const yScale = d3.scaleLinear()
      .domain([0, 10])
      .range([height - margin.top - margin.bottom, 0]);

    // Create axes
    const xAxis = d3.axisBottom(xScale)
      .tickFormat(d => `${d}`);

    const yAxis = d3.axisLeft(yScale);

    // Add axes to SVG
    g.append('g')
      .attr('class', 'x-axis')
      .attr('transform', `translate(0,${height - margin.top - margin.bottom})`)
      .call(xAxis);

    g.append('g')
      .attr('class', 'y-axis')
      .call(yAxis);

    // Add axis labels
    g.append('text')
      .attr('class', 'x-axis-label')
      .attr('x', (width - margin.left - margin.right) / 2)
      .attr('y', height - margin.top - margin.bottom + 35)
      .attr('text-anchor', 'middle')
      .text('Cycle Number');

    g.append('text')
      .attr('class', 'y-axis-label')
      .attr('transform', 'rotate(-90)')
      .attr('x', -(height - margin.top - margin.bottom) / 2)
      .attr('y', -35)
      .attr('text-anchor', 'middle')
      .text('Execution Time (ms)');

    // Store timeline reference
    timeline = { svg: svgSelection, g, xScale, yScale, margin, width, height };
  }

  /**
   * Update timeline data from execution history
   */
  function updateTimelineData(history: any[]) {
    executionData = history.map((entry, index) => ({
      cycle: entry.state.cycles,
      timestamp: entry.timestamp,
      phase: entry.state.phase || 'idle',
      pc: entry.state.programCounter,
      accumulator: entry.state.accumulator,
      index: index
    }));

    if (timeline && executionData.length > 0) {
      renderTimeline();
    }
  }

  /**
   * Render timeline visualization
   */
  function renderTimeline() {
    if (!timeline || executionData.length === 0) return;

    const { g, xScale, yScale, width, height, margin } = timeline;

    // Update scales
    const maxCycle = d3.max(executionData, d => d.cycle) || 100;
    const minTime = d3.min(executionData, d => d.timestamp) || 0;
    const maxTime = d3.max(executionData, d => d.timestamp) || 1000;

    xScale.domain([0, maxCycle]);

    // Calculate time deltas
    const timeDiffs = executionData.map((d, i) => {
      if (i === 0) return 0;
      return d.timestamp - executionData[i - 1].timestamp;
    });

    const maxTimeDiff = d3.max(timeDiffs) || 10;
    yScale.domain([0, maxTimeDiff * 1.1]);

    // Update axes
    g.select('.x-axis')
      .transition()
      .duration(500)
      .call(d3.axisBottom(xScale));

    g.select('.y-axis')
      .transition()
      .duration(500)
      .call(d3.axisLeft(yScale));

    // Create phase bars
    const bars = g.selectAll('.cycle-bar')
      .data(executionData);

    // Enter + Update
    bars.enter()
      .append('rect')
      .attr('class', 'cycle-bar')
      .merge(bars)
      .attr('x', d => xScale(d.cycle))
      .attr('y', (d, i) => {
        const timeDiff = i === 0 ? 0 : d.timestamp - executionData[i - 1].timestamp;
        return yScale(timeDiff);
      })
      .attr('width', Math.max(2, (width - margin.left - margin.right) / maxCycle))
      .attr('height', (d, i) => {
        const timeDiff = i === 0 ? 0 : d.timestamp - executionData[i - 1].timestamp;
        return (height - margin.top - margin.bottom) - yScale(timeDiff);
      })
      .attr('fill', d => phaseColors[d.phase] || phaseColors['idle'])
      .attr('opacity', 0.7)
      .on('click', handleBarClick)
      .on('mouseover', handleBarHover)
      .on('mouseout', handleBarOut)
      .style('cursor', 'pointer');

    // Exit
    bars.exit().remove();

    // Add milestones if enabled
    if (showMilestones) {
      renderMilestones();
    }
  }

  /**
   * Render milestone markers
   */
  function renderMilestones() {
    if (!timeline) return;

    const { g, xScale, height, margin } = timeline;

    // Define milestones (every 100 cycles, or significant events)
    const milestones = executionData.filter((d, i) =>
      d.cycle % 100 === 0 || i === 0 || i === executionData.length - 1
    );

    const milestoneLines = g.selectAll('.milestone-line')
      .data(milestones);

    milestoneLines.enter()
      .append('line')
      .attr('class', 'milestone-line')
      .merge(milestoneLines)
      .attr('x1', d => xScale(d.cycle))
      .attr('x2', d => xScale(d.cycle))
      .attr('y1', 0)
      .attr('y2', height - margin.top - margin.bottom)
      .attr('stroke', '#fff')
      .attr('stroke-width', 1)
      .attr('stroke-dasharray', '4,4')
      .attr('opacity', 0.3);

    milestoneLines.exit().remove();

    // Add milestone labels
    const milestoneLabels = g.selectAll('.milestone-label')
      .data(milestones);

    milestoneLabels.enter()
      .append('text')
      .attr('class', 'milestone-label')
      .merge(milestoneLabels)
      .attr('x', d => xScale(d.cycle))
      .attr('y', -5)
      .attr('text-anchor', 'middle')
      .attr('fill', '#fff')
      .attr('font-size', '10px')
      .text(d => `C${d.cycle}`);

    milestoneLabels.exit().remove();
  }

  /**
   * Handle zoom event
   */
  function handleZoom(event: any) {
    if (!timeline) return;

    const { g, xScale, yScale } = timeline;
    const newXScale = event.transform.rescaleX(xScale);
    const newYScale = event.transform.rescaleY(yScale);

    // Update axes
    g.select('.x-axis').call(d3.axisBottom(newXScale));
    g.select('.y-axis').call(d3.axisLeft(newYScale));

    // Update bars
    g.selectAll('.cycle-bar')
      .attr('x', (d: any) => newXScale(d.cycle))
      .attr('y', (d: any, i: number) => {
        const timeDiff = i === 0 ? 0 : d.timestamp - executionData[i - 1].timestamp;
        return newYScale(timeDiff);
      })
      .attr('height', (d: any, i: number) => {
        const timeDiff = i === 0 ? 0 : d.timestamp - executionData[i - 1].timestamp;
        return (timeline.height - timeline.margin.top - timeline.margin.bottom) - newYScale(timeDiff);
      });

    // Update milestones
    g.selectAll('.milestone-line')
      .attr('x1', (d: any) => newXScale(d.cycle))
      .attr('x2', (d: any) => newXScale(d.cycle));

    g.selectAll('.milestone-label')
      .attr('x', (d: any) => newXScale(d.cycle));
  }

  /**
   * Handle bar click
   */
  function handleBarClick(event: any, d: any) {
    selectedCycle = d.cycle;

    // Rewind to this state
    machineStateStore.rewindToHistoryEntry(d.index);

    // Highlight selected bar
    d3.selectAll('.cycle-bar')
      .attr('stroke', 'none')
      .attr('stroke-width', 0);

    d3.select(event.currentTarget)
      .attr('stroke', '#fff')
      .attr('stroke-width', 2);
  }

  /**
   * Handle bar hover
   */
  function handleBarHover(event: any, d: any) {
    // Show tooltip
    const tooltip = d3.select('body').append('div')
      .attr('class', 'timeline-tooltip')
      .style('position', 'absolute')
      .style('background', '#1a1a1a')
      .style('border', '1px solid #333')
      .style('border-radius', '4px')
      .style('padding', '8px')
      .style('color', '#e0e0e0')
      .style('font-family', 'monospace')
      .style('font-size', '12px')
      .style('pointer-events', 'none')
      .style('z-index', 1000);

    const timeDiff = d.index === 0 ? 0 : d.timestamp - executionData[d.index - 1].timestamp;

    tooltip.html(`
      <strong>Cycle ${d.cycle}</strong><br/>
      Phase: ${d.phase}<br/>
      PC: ${d.pc}<br/>
      ACC: ${d.accumulator}<br/>
      Time: ${timeDiff.toFixed(2)}ms
    `)
      .style('left', `${event.pageX + 10}px`)
      .style('top', `${event.pageY - 10}px`);

    // Store tooltip reference
    (event.currentTarget as any).__tooltip = tooltip;

    // Highlight bar
    d3.select(event.currentTarget)
      .attr('opacity', 1);
  }

  /**
   * Handle bar mouse out
   */
  function handleBarOut(event: any) {
    // Remove tooltip
    const tooltip = (event.currentTarget as any).__tooltip;
    if (tooltip) {
      tooltip.remove();
    }

    // Reset opacity
    d3.select(event.currentTarget)
      .attr('opacity', 0.7);
  }

  /**
   * Reset zoom
   */
  function resetZoom() {
    if (!timeline) return;

    d3.select(svg)
      .transition()
      .duration(500)
      .call(zoom.transform, d3.zoomIdentity);
  }

  /**
   * Export timeline as PNG
   */
  function exportAsPNG() {
    if (!svg) return;

    const svgData = new XMLSerializer().serializeToString(svg);
    const canvas = document.createElement('canvas');
    const ctx = canvas.getContext('2d');

    if (!ctx) return;

    const img = new Image();
    img.onload = () => {
      canvas.width = img.width;
      canvas.height = img.height;
      ctx.drawImage(img, 0, 0);

      const pngFile = canvas.toDataURL('image/png');
      const downloadLink = document.createElement('a');
      downloadLink.download = 'execution-timeline.png';
      downloadLink.href = pngFile;
      downloadLink.click();
    };

    img.src = 'data:image/svg+xml;base64,' + btoa(svgData);
  }

  /**
   * Export timeline as SVG
   */
  function exportAsSVG() {
    if (!svg) return;

    const svgData = new XMLSerializer().serializeToString(svg);
    const blob = new Blob([svgData], { type: 'image/svg+xml' });
    const url = URL.createObjectURL(blob);

    const downloadLink = document.createElement('a');
    downloadLink.download = 'execution-timeline.svg';
    downloadLink.href = url;
    downloadLink.click();

    URL.revokeObjectURL(url);
  }
</script>

<style>
  .timeline-container {
    background: var(--timeline-bg, #0a0a0a);
    border: 1px solid var(--border-color, #333);
    border-radius: 8px;
    padding: 1rem;
    font-family: 'Courier New', monospace;
  }

  .timeline-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 1rem;
  }

  .header-title {
    font-size: 1rem;
    font-weight: bold;
    color: var(--text-primary, #e0e0e0);
  }

  .timeline-controls {
    display: flex;
    gap: 0.5rem;
  }

  .control-button {
    padding: 0.3rem 0.6rem;
    background: var(--button-bg, #2a2a2a);
    border: 1px solid var(--border-color, #444);
    border-radius: 4px;
    color: var(--text-primary, #e0e0e0);
    cursor: pointer;
    font-family: inherit;
    font-size: 0.75rem;
    transition: all 0.2s;
  }

  .control-button:hover {
    background: var(--button-hover-bg, #3a3a3a);
  }

  .control-button.active {
    background: #4caf50;
    border-color: #4caf50;
    color: white;
  }

  .timeline-svg-container {
    background: #1a1a1a;
    border: 1px solid var(--border-color, #333);
    border-radius: 4px;
    overflow: hidden;
  }

  :global(.timeline-svg-container svg) {
    display: block;
  }

  :global(.cycle-bar) {
    transition: opacity 0.2s;
  }

  .legend {
    display: flex;
    gap: 1rem;
    margin-top: 1rem;
    padding: 0.75rem;
    background: var(--legend-bg, #0a0a0a);
    border: 1px solid var(--border-color, #333);
    border-radius: 4px;
    font-size: 0.8rem;
    flex-wrap: wrap;
  }

  .legend-item {
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }

  .legend-color {
    width: 16px;
    height: 16px;
    border-radius: 3px;
  }

  .empty-message {
    padding: 3rem;
    text-align: center;
    color: var(--text-secondary, #666);
    font-size: 0.9rem;
  }
</style>

<div class="timeline-container" bind:this={container}>
  <!-- Header -->
  <div class="timeline-header">
    <div class="header-title">📊 Execution Timeline</div>
    <div class="timeline-controls">
      <button
        class="control-button"
        class:active={showPhases}
        on:click={() => showPhases = !showPhases}
      >
        Phases
      </button>
      <button
        class="control-button"
        class:active={showMilestones}
        on:click={() => { showMilestones = !showMilestones; renderTimeline(); }}
      >
        Milestones
      </button>
      <button class="control-button" on:click={resetZoom}>
        Reset Zoom
      </button>
      <button class="control-button" on:click={exportAsPNG}>
        📷 PNG
      </button>
      <button class="control-button" on:click={exportAsSVG}>
        📄 SVG
      </button>
    </div>
  </div>

  <!-- Timeline SVG -->
  <div class="timeline-svg-container">
    {#if executionData.length === 0}
      <div class="empty-message">
        No execution data yet.<br/>
        Run a program to see the timeline.
      </div>
    {:else}
      <svg bind:this={svg}></svg>
    {/if}
  </div>

  <!-- Legend -->
  {#if showPhases && executionData.length > 0}
    <div class="legend">
      {#each Object.entries(phaseColors) as [phase, color]}
        <div class="legend-item">
          <div class="legend-color" style="background: {color}"></div>
          <span>{phase}</span>
        </div>
      {/each}
    </div>
  {/if}
</div>
