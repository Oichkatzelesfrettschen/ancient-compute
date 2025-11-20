<script lang="ts">
  /**
   * TimelineD3.svelte - Main D3.js Timeline Visualization Component
   *
   * Features:
   * - Horizontal scrollable timeline with logarithmic scale
   * - 8 color-coded eras
   * - Zoom and pan support
   * - Milestone markers
   * - Responsive design
   */

  import { onMount, onDestroy } from 'svelte';
  import * as d3 from 'd3';
  import { timelineVizStore, selectEra, centerOnYear, setHoveredMilestone } from '$lib/stores/timelineVisualizationStore';
  import type { TimelineEra, Milestone } from '$lib/api/timeline';

  // Props
  export let width = 1200;
  export let height = 400;

  // Component state
  let svgElement: SVGSVGElement;
  let containerElement: HTMLDivElement;
  let svg: d3.Selection<SVGSVGElement, unknown, null, undefined>;
  let resizeObserver: ResizeObserver | null = null;

  // D3 scales
  let xScale: d3.ScaleLogarithmic<number, number>;
  let yScale: d3.ScaleLinear<number, number>;

  // Margins
  const margin = { top: 40, right: 40, bottom: 60, left: 60 };
  $: innerWidth = width - margin.left - margin.right;
  $: innerHeight = height - margin.top - margin.bottom;

  // Subscribe to store
  $: state = $timelineVizStore;
  $: eras = state.eras;
  $: filteredMilestones = state.filteredMilestones;
  $: viewport = state.viewport;

  /**
   * Initialize D3 visualization
   */
  function initializeVisualization() {
    if (!svgElement) return;

    // Clear existing content
    d3.select(svgElement).selectAll('*').remove();

    // Create SVG
    svg = d3.select(svgElement)
      .attr('width', width)
      .attr('height', height);

    // Create main group with margins
    const g = svg.append('g')
      .attr('transform', `translate(${margin.left},${margin.top})`);

    // Initialize scales
    updateScales();

    // Draw components
    drawEras(g);
    drawAxis(g);
    drawMilestones(g);
    drawLabels(g);

    // Set up zoom behavior
    setupZoom();
  }

  /**
   * Update scales based on viewport
   */
  function updateScales() {
    // Create logarithmic scale for time
    // Add 20000 to all years to handle BCE dates (make all positive)
    const offset = 20000;

    xScale = d3.scaleLog()
      .domain([viewport.startYear + offset + 1, viewport.endYear + offset + 1])
      .range([0, innerWidth])
      .clamp(true);

    yScale = d3.scaleLinear()
      .domain([0, 1])
      .range([innerHeight, 0]);
  }

  /**
   * Draw era backgrounds
   */
  function drawEras(g: d3.Selection<SVGGElement, unknown, null, undefined>) {
    const erasGroup = g.append('g').attr('class', 'eras');
    const offset = 20000;

    erasGroup.selectAll('.era-rect')
      .data(eras)
      .join('rect')
      .attr('class', 'era-rect')
      .attr('x', (d: TimelineEra) => xScale(d.startYear + offset + 1))
      .attr('y', 0)
      .attr('width', (d: TimelineEra) => {
        const x1 = xScale(d.startYear + offset + 1);
        const x2 = xScale(d.endYear + offset + 1);
        return Math.max(0, x2 - x1);
      })
      .attr('height', innerHeight)
      .attr('fill', (d: TimelineEra) => d.color)
      .attr('opacity', 0.15)
      .attr('cursor', 'pointer')
      .on('click', (event: MouseEvent, d: TimelineEra) => {
        selectEra(d.id);
      })
      .on('mouseenter', function(event: MouseEvent, d: TimelineEra) {
        d3.select(this).attr('opacity', 0.25);
      })
      .on('mouseleave', function() {
        d3.select(this).attr('opacity', 0.15);
      });
  }

  /**
   * Draw timeline axis
   */
  function drawAxis(g: d3.Selection<SVGGElement, unknown, null, undefined>) {
    const offset = 20000;

    // Create custom tick values
    const tickValues = generateTickValues(viewport.startYear, viewport.endYear);

    const xAxis = d3.axisBottom(xScale)
      .tickValues(tickValues.map(y => y + offset + 1))
      .tickFormat((d) => {
        const year = (d as number) - offset - 1;
        if (year < 0) {
          return `${Math.abs(year)} BCE`;
        } else if (year === 0) {
          return '0';
        } else {
          return `${year} CE`;
        }
      });

    g.append('g')
      .attr('class', 'axis axis-x')
      .attr('transform', `translate(0,${innerHeight})`)
      .call(xAxis)
      .selectAll('text')
      .attr('transform', 'rotate(-45)')
      .attr('text-anchor', 'end')
      .attr('dx', '-0.5em')
      .attr('dy', '0.5em');

    // Draw center line (year 0)
    const centerX = xScale(offset + 1);
    g.append('line')
      .attr('class', 'center-line')
      .attr('x1', centerX)
      .attr('x2', centerX)
      .attr('y1', 0)
      .attr('y2', innerHeight)
      .attr('stroke', '#999')
      .attr('stroke-width', 2)
      .attr('stroke-dasharray', '5,5')
      .attr('opacity', 0.5);
  }

  /**
   * Draw milestone markers
   */
  function drawMilestones(g: d3.Selection<SVGGElement, unknown, null, undefined>) {
    const offset = 20000;
    const milestonesGroup = g.append('g').attr('class', 'milestones');

    // Category icons
    const categoryIcons: Record<string, string> = {
      invention: '⚙️',
      theory: '📐',
      person: '👤',
      event: '📅',
    };

    const milestoneGroups = milestonesGroup.selectAll('.milestone')
      .data(filteredMilestones)
      .join('g')
      .attr('class', 'milestone')
      .attr('transform', (d: Milestone) => {
        const x = xScale(d.year + offset + 1);
        const y = innerHeight / 2;
        return `translate(${x},${y})`;
      })
      .attr('cursor', 'pointer')
      .on('mouseenter', function(event: MouseEvent, d: Milestone) {
        const rect = (event.currentTarget as Element).getBoundingClientRect();
        setHoveredMilestone(d, { x: rect.left, y: rect.top });
        d3.select(this).select('circle').attr('r', 8);
      })
      .on('mouseleave', function() {
        setHoveredMilestone(null, null);
        d3.select(this).select('circle').attr('r', 6);
      })
      .on('click', (event: MouseEvent, d: Milestone) => {
        centerOnYear(d.year);
      });

    // Milestone circles
    milestoneGroups.append('circle')
      .attr('r', 6)
      .attr('fill', (d: Milestone) => {
        const era = eras.find(e => d.year >= e.startYear && d.year <= e.endYear);
        return era ? era.color : '#667eea';
      })
      .attr('stroke', '#fff')
      .attr('stroke-width', 2);

    // Milestone labels (only show if zoomed in enough)
    if (state.zoomLevel === 'decade' || state.zoomLevel === 'year') {
      milestoneGroups.append('text')
        .attr('x', 0)
        .attr('y', -15)
        .attr('text-anchor', 'middle')
        .attr('font-size', '10px')
        .attr('fill', '#333')
        .text((d: Milestone) => d.title.slice(0, 20));
    }
  }

  /**
   * Draw era labels
   */
  function drawLabels(g: d3.Selection<SVGGElement, unknown, null, undefined>) {
    const offset = 20000;
    const labelsGroup = g.append('g').attr('class', 'labels');

    labelsGroup.selectAll('.era-label')
      .data(eras)
      .join('text')
      .attr('class', 'era-label')
      .attr('x', (d: TimelineEra) => {
        const x1 = xScale(d.startYear + offset + 1);
        const x2 = xScale(d.endYear + offset + 1);
        return (x1 + x2) / 2;
      })
      .attr('y', 20)
      .attr('text-anchor', 'middle')
      .attr('font-size', '14px')
      .attr('font-weight', 'bold')
      .attr('fill', (d: TimelineEra) => d.color)
      .attr('pointer-events', 'none')
      .text((d: TimelineEra) => d.name);
  }

  /**
   * Set up zoom and pan behavior
   */
  function setupZoom() {
    const zoom = d3.zoom<SVGSVGElement, unknown>()
      .scaleExtent([1, 10])
      .on('zoom', (event) => {
        const transform = event.transform;
        svg.select('g').attr('transform', `translate(${margin.left + transform.x},${margin.top}) scale(${transform.k})`);
      });

    svg.call(zoom as any);
  }

  /**
   * Generate appropriate tick values based on viewport range
   */
  function generateTickValues(startYear: number, endYear: number): number[] {
    const range = endYear - startYear;
    let step: number;

    if (range > 10000) {
      step = 2000;
    } else if (range > 5000) {
      step = 1000;
    } else if (range > 1000) {
      step = 500;
    } else if (range > 100) {
      step = 100;
    } else {
      step = 10;
    }

    const ticks: number[] = [];
    const start = Math.floor(startYear / step) * step;
    const end = Math.ceil(endYear / step) * step;

    for (let year = start; year <= end; year += step) {
      ticks.push(year);
    }

    return ticks;
  }

  /**
   * Handle window resize
   */
  function handleResize() {
    if (containerElement) {
      width = containerElement.clientWidth;
      initializeVisualization();
    }
  }

  // Lifecycle
  onMount(() => {
    // Initialize visualization
    initializeVisualization();

    // Set up resize observer
    if (containerElement) {
      resizeObserver = new ResizeObserver(handleResize);
      resizeObserver.observe(containerElement);
    }
  });

  onDestroy(() => {
    if (resizeObserver) {
      resizeObserver.disconnect();
    }
  });

  // Reactive updates
  $: if (svgElement && (viewport || filteredMilestones)) {
    initializeVisualization();
  }
</script>

<div class="timeline-container" bind:this={containerElement}>
  <svg bind:this={svgElement} class="timeline-svg"></svg>
</div>

<style>
  .timeline-container {
    width: 100%;
    height: 100%;
    overflow: hidden;
    background: #fafafa;
    border-radius: 8px;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
  }

  .timeline-svg {
    display: block;
  }

  :global(.timeline-svg .axis path),
  :global(.timeline-svg .axis line) {
    stroke: #999;
  }

  :global(.timeline-svg .axis text) {
    fill: #666;
    font-size: 11px;
  }

  :global(.timeline-svg .era-rect) {
    transition: opacity 0.2s ease;
  }

  :global(.timeline-svg .milestone circle) {
    transition: r 0.2s ease;
  }

  :global(.timeline-svg .milestone:hover circle) {
    filter: drop-shadow(0 0 4px rgba(0, 0, 0, 0.3));
  }
</style>
