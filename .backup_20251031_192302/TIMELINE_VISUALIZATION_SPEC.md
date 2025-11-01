# Timeline Visualization Specification
## D3.js and Three.js Interactive Timeline Component

### Executive Summary
This document specifies a comprehensive timeline visualization system using D3.js for 2D views and Three.js for 3D exploration, spanning 12,500 years of computational history with interactive filtering, zooming, and cross-referencing capabilities.

## Component Architecture

### File Structure
```
frontend/src/lib/components/timeline/
├── Timeline.svelte               # Main timeline container
├── Timeline2D.svelte             # D3.js 2D view
├── Timeline3D.svelte             # Three.js 3D view
├── TimelineControls.svelte       # Filter/zoom controls
├── TimelineTooltip.svelte        # Event detail tooltips
├── TimelineMinimap.svelte        # Overview navigation
├── stores/
│   ├── timelineStore.ts         # Timeline state management
│   └── filterStore.ts           # Filter state
├── utils/
│   ├── scaleHelpers.ts         # Time scale utilities
│   ├── colorSchemes.ts         # Visual encoding schemes
│   └── dataTransforms.ts       # Data processing
└── types/
    └── timeline.ts              # TypeScript definitions
```

## Data Structure

### Timeline Event Interface
```typescript
// frontend/src/lib/components/timeline/types/timeline.ts

export interface TimelineEvent {
    id: number;
    year: number;                    // Negative for BC
    month?: number;                  // 1-12
    day?: number;                    // 1-31
    title: string;
    description: string;
    category: EventCategory;
    importance: 1 | 2 | 3 | 4 | 5;  // Visual prominence
    civilizationId?: number;
    civilization?: Civilization;
    moduleId?: number;
    coordinates?: GeoCoordinates;
    mediaUrls?: string[];
    connections?: EventConnection[]; // Links to related events
}

export type EventCategory =
    | 'invention'      // New device/tool
    | 'discovery'      // Mathematical/scientific finding
    | 'person'         // Birth/death of key figure
    | 'publication'    // Important text/manuscript
    | 'event'          // Historical event
    | 'algorithm'      // Algorithm development
    | 'theory';        // Theoretical advancement

export interface GeoCoordinates {
    lat: number;
    lng: number;
    locationName?: string;
}

export interface EventConnection {
    fromId: number;
    toId: number;
    connectionType: 'influence' | 'predecessor' | 'contemporary' | 'evolution';
    description?: string;
}

export interface Civilization {
    id: number;
    name: string;
    color: string;     // Hex color for visualization
    region: string;
    startYear: number;
    endYear: number;
}

export interface TimeRange {
    start: number;     // Start year
    end: number;       // End year
    label: string;     // Era name
}
```

## 2D Timeline Implementation (D3.js)

### Main Timeline Component
```typescript
<!-- frontend/src/lib/components/timeline/Timeline2D.svelte -->
<script lang="ts">
import { onMount, onDestroy } from 'svelte';
import * as d3 from 'd3';
import type { TimelineEvent, TimeRange } from './types/timeline';
import { timelineStore } from './stores/timelineStore';
import { filterStore } from './stores/filterStore';

export let events: TimelineEvent[] = [];
export let width: number = 1200;
export let height: number = 600;

let container: HTMLElement;
let svg: d3.Selection<SVGGElement, unknown, null, undefined>;
let xScale: d3.ScaleLinear<number, number>;
let yScale: d3.ScaleBand<string>;
let zoom: d3.ZoomBehavior<Element, unknown>;

const margin = { top: 20, right: 30, bottom: 40, left: 100 };
const innerWidth = width - margin.left - margin.right;
const innerHeight = height - margin.top - margin.bottom;

// Color scale for event categories
const categoryColors = d3.scaleOrdinal<string>()
    .domain(['invention', 'discovery', 'person', 'publication', 'event', 'algorithm', 'theory'])
    .range(['#FF6B6B', '#4ECDC4', '#45B7D1', '#96CEB4', '#FECA57', '#9C88FF', '#FD79A8']);

// Size scale for importance
const importanceScale = d3.scaleLinear()
    .domain([1, 5])
    .range([4, 12]);

onMount(() => {
    initializeTimeline();
    drawTimeline();
});

function initializeTimeline() {
    // Create SVG
    const svgElement = d3.select(container)
        .append('svg')
        .attr('width', width)
        .attr('height', height);

    svg = svgElement.append('g')
        .attr('transform', `translate(${margin.left},${margin.top})`);

    // Create scales
    const yearExtent = d3.extent(events, d => d.year) as [number, number];
    xScale = d3.scaleLinear()
        .domain(yearExtent)
        .range([0, innerWidth]);

    // Create swim lanes for categories
    const categories = Array.from(new Set(events.map(e => e.category)));
    yScale = d3.scaleBand()
        .domain(categories)
        .range([0, innerHeight])
        .padding(0.1);

    // Add zoom behavior
    zoom = d3.zoom()
        .scaleExtent([1, 100])
        .translateExtent([[0, 0], [innerWidth, innerHeight]])
        .on('zoom', handleZoom);

    svgElement.call(zoom as any);

    // Add axes
    drawAxes();

    // Add era backgrounds
    drawEras();
}

function drawAxes() {
    // X-axis (years)
    const xAxis = d3.axisBottom(xScale)
        .tickFormat(d => {
            const year = d as number;
            if (year < 0) {
                return `${Math.abs(year)} BC`;
            } else if (year === 0) {
                return '1 AD';
            } else {
                return `${year} AD`;
            }
        });

    svg.append('g')
        .attr('class', 'x-axis')
        .attr('transform', `translate(0, ${innerHeight})`)
        .call(xAxis);

    // Y-axis (categories)
    const yAxis = d3.axisLeft(yScale);
    svg.append('g')
        .attr('class', 'y-axis')
        .call(yAxis);
}

function drawEras() {
    const eras: TimeRange[] = [
        { start: -10500, end: -3500, label: 'Prehistoric' },
        { start: -3500, end: 500, label: 'Ancient' },
        { start: 500, end: 1400, label: 'Medieval' },
        { start: 1400, end: 1650, label: 'Renaissance' },
        { start: 1650, end: 1800, label: 'Enlightenment' },
        { start: 1800, end: 1900, label: 'Industrial' },
        { start: 1900, end: 1945, label: 'Electronic Dawn' },
        { start: 1945, end: 1970, label: 'Digital Revolution' },
        { start: 1970, end: 1990, label: 'Personal Computing' },
        { start: 1990, end: 2010, label: 'Internet Age' },
        { start: 2010, end: 2025, label: 'Contemporary' }
    ];

    const eraGroup = svg.append('g')
        .attr('class', 'eras');

    eraGroup.selectAll('.era')
        .data(eras)
        .enter().append('rect')
        .attr('class', 'era')
        .attr('x', d => xScale(d.start))
        .attr('y', 0)
        .attr('width', d => xScale(d.end) - xScale(d.start))
        .attr('height', innerHeight)
        .attr('fill', (d, i) => d3.schemeSet3[i % 12])
        .attr('opacity', 0.1);

    // Era labels
    eraGroup.selectAll('.era-label')
        .data(eras)
        .enter().append('text')
        .attr('class', 'era-label')
        .attr('x', d => (xScale(d.start) + xScale(d.end)) / 2)
        .attr('y', -5)
        .attr('text-anchor', 'middle')
        .attr('font-size', '12px')
        .attr('fill', '#666')
        .text(d => d.label);
}

function drawTimeline() {
    // Filter events based on current filters
    const filteredEvents = filterEvents(events);

    // Draw connection lines
    drawConnections(filteredEvents);

    // Draw event circles
    const eventGroup = svg.append('g')
        .attr('class', 'events');

    const eventCircles = eventGroup.selectAll('.event')
        .data(filteredEvents)
        .enter().append('circle')
        .attr('class', 'event')
        .attr('cx', d => xScale(d.year))
        .attr('cy', d => yScale(d.category)! + yScale.bandwidth() / 2)
        .attr('r', d => importanceScale(d.importance))
        .attr('fill', d => categoryColors(d.category))
        .attr('stroke', '#fff')
        .attr('stroke-width', 2)
        .style('cursor', 'pointer')
        .on('click', handleEventClick)
        .on('mouseover', handleEventHover)
        .on('mouseout', handleEventOut);

    // Add transitions
    eventCircles
        .attr('r', 0)
        .transition()
        .duration(500)
        .delay((d, i) => i * 5)
        .attr('r', d => importanceScale(d.importance));
}

function drawConnections(events: TimelineEvent[]) {
    const connections = events.flatMap(e => e.connections || []);

    const lineGenerator = d3.line()
        .x(d => d[0])
        .y(d => d[1])
        .curve(d3.curveBasis);

    const connectionGroup = svg.append('g')
        .attr('class', 'connections');

    connections.forEach(conn => {
        const fromEvent = events.find(e => e.id === conn.fromId);
        const toEvent = events.find(e => e.id === conn.toId);

        if (fromEvent && toEvent) {
            const fromX = xScale(fromEvent.year);
            const fromY = yScale(fromEvent.category)! + yScale.bandwidth() / 2;
            const toX = xScale(toEvent.year);
            const toY = yScale(toEvent.category)! + yScale.bandwidth() / 2;

            // Create curved path
            const midX = (fromX + toX) / 2;
            const midY = (fromY + toY) / 2 - 20;

            connectionGroup.append('path')
                .attr('class', 'connection')
                .attr('d', lineGenerator([[fromX, fromY], [midX, midY], [toX, toY]]))
                .attr('fill', 'none')
                .attr('stroke', '#999')
                .attr('stroke-width', 1)
                .attr('stroke-dasharray', conn.connectionType === 'influence' ? '5,5' : 'none')
                .attr('opacity', 0.3);
        }
    });
}

function handleZoom(event: d3.D3ZoomEvent<Element, unknown>) {
    const transform = event.transform;

    // Update x-scale
    const newXScale = transform.rescaleX(xScale);

    // Update axis
    svg.select('.x-axis')
        .call(d3.axisBottom(newXScale) as any);

    // Update event positions
    svg.selectAll('.event')
        .attr('cx', (d: any) => newXScale(d.year));

    // Update connections
    // ... redraw connections with new scale
}

function handleEventClick(event: MouseEvent, d: TimelineEvent) {
    timelineStore.setSelectedEvent(d);
    // Emit event for parent component
    dispatch('eventSelect', d);
}

function handleEventHover(event: MouseEvent, d: TimelineEvent) {
    // Show tooltip
    const tooltip = d3.select('body').append('div')
        .attr('class', 'timeline-tooltip')
        .style('opacity', 0);

    tooltip.transition()
        .duration(200)
        .style('opacity', 0.9);

    tooltip.html(`
        <strong>${d.title}</strong><br/>
        Year: ${formatYear(d.year)}<br/>
        ${d.description.substring(0, 100)}...
    `)
        .style('left', (event.pageX + 10) + 'px')
        .style('top', (event.pageY - 28) + 'px');
}

function handleEventOut() {
    d3.selectAll('.timeline-tooltip').remove();
}

function filterEvents(events: TimelineEvent[]): TimelineEvent[] {
    const filters = $filterStore;

    return events.filter(event => {
        if (filters.yearRange) {
            if (event.year < filters.yearRange[0] || event.year > filters.yearRange[1]) {
                return false;
            }
        }

        if (filters.categories.length > 0 && !filters.categories.includes(event.category)) {
            return false;
        }

        if (filters.minImportance && event.importance < filters.minImportance) {
            return false;
        }

        if (filters.civilizationId && event.civilizationId !== filters.civilizationId) {
            return false;
        }

        return true;
    });
}

function formatYear(year: number): string {
    if (year < 0) {
        return `${Math.abs(year)} BC`;
    } else if (year === 0) {
        return '1 AD';
    } else {
        return `${year} AD`;
    }
}
</script>

<div bind:this={container} class="timeline-container"></div>

<style>
.timeline-container {
    width: 100%;
    height: 100%;
    position: relative;
}

:global(.timeline-tooltip) {
    position: absolute;
    padding: 10px;
    background: rgba(0, 0, 0, 0.8);
    color: white;
    border-radius: 4px;
    font-size: 12px;
    pointer-events: none;
    z-index: 1000;
}

:global(.era-label) {
    font-family: 'Inter', sans-serif;
    font-weight: 600;
}

:global(.event) {
    transition: all 0.3s ease;
}

:global(.event:hover) {
    transform: scale(1.2);
}
</style>
```

## 3D Timeline Implementation (Three.js)

### 3D Spiral Timeline
```typescript
<!-- frontend/src/lib/components/timeline/Timeline3D.svelte -->
<script lang="ts">
import { onMount, onDestroy } from 'svelte';
import * as THREE from 'three';
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls';
import type { TimelineEvent } from './types/timeline';

export let events: TimelineEvent[] = [];
export let width: number = 1200;
export let height: number = 800;

let container: HTMLElement;
let scene: THREE.Scene;
let camera: THREE.PerspectiveCamera;
let renderer: THREE.WebGLRenderer;
let controls: OrbitControls;
let eventMeshes: Map<number, THREE.Mesh> = new Map();

const SPIRAL_RADIUS = 50;
const SPIRAL_HEIGHT = 200;
const TURNS = 10;

onMount(() => {
    initializeScene();
    createSpiral();
    createEvents();
    animate();
});

onDestroy(() => {
    if (renderer) {
        renderer.dispose();
    }
});

function initializeScene() {
    // Scene setup
    scene = new THREE.Scene();
    scene.background = new THREE.Color(0x0a0a0a);
    scene.fog = new THREE.Fog(0x0a0a0a, 100, 500);

    // Camera setup
    camera = new THREE.PerspectiveCamera(
        75,
        width / height,
        0.1,
        1000
    );
    camera.position.set(100, 50, 100);

    // Renderer setup
    renderer = new THREE.WebGLRenderer({ antialias: true });
    renderer.setSize(width, height);
    renderer.setPixelRatio(window.devicePixelRatio);
    container.appendChild(renderer.domElement);

    // Controls
    controls = new OrbitControls(camera, renderer.domElement);
    controls.enableDamping = true;
    controls.dampingFactor = 0.05;
    controls.minDistance = 50;
    controls.maxDistance = 300;

    // Lighting
    const ambientLight = new THREE.AmbientLight(0x404040);
    scene.add(ambientLight);

    const directionalLight = new THREE.DirectionalLight(0xffffff, 1);
    directionalLight.position.set(50, 50, 50);
    scene.add(directionalLight);

    // Add grid helper
    const gridHelper = new THREE.GridHelper(200, 20, 0x333333, 0x222222);
    scene.add(gridHelper);
}

function createSpiral() {
    // Create spiral path for timeline
    const points: THREE.Vector3[] = [];
    const segments = 500;

    for (let i = 0; i <= segments; i++) {
        const t = i / segments;
        const angle = t * Math.PI * 2 * TURNS;
        const y = t * SPIRAL_HEIGHT - SPIRAL_HEIGHT / 2;
        const x = Math.cos(angle) * SPIRAL_RADIUS;
        const z = Math.sin(angle) * SPIRAL_RADIUS;

        points.push(new THREE.Vector3(x, y, z));
    }

    // Create spiral line
    const spiralGeometry = new THREE.BufferGeometry().setFromPoints(points);
    const spiralMaterial = new THREE.LineBasicMaterial({
        color: 0x4a90e2,
        linewidth: 2,
        transparent: true,
        opacity: 0.6
    });

    const spiral = new THREE.Line(spiralGeometry, spiralMaterial);
    scene.add(spiral);

    // Add era markers along spiral
    createEraMarkers();
}

function createEraMarkers() {
    const eras = [
        { year: -10500, label: 'Prehistoric', color: 0xff6b6b },
        { year: -3500, label: 'Ancient', color: 0x4ecdc4 },
        { year: 500, label: 'Medieval', color: 0x45b7d1 },
        { year: 1400, label: 'Renaissance', color: 0x96ceb4 },
        { year: 1650, label: 'Enlightenment', color: 0xfeca57 },
        { year: 1800, label: 'Industrial', color: 0x9c88ff },
        { year: 1945, label: 'Digital', color: 0xfd79a8 },
        { year: 2025, label: 'Contemporary', color: 0x54a0ff }
    ];

    eras.forEach(era => {
        const position = getPositionForYear(era.year);

        // Create era ring
        const ringGeometry = new THREE.TorusGeometry(SPIRAL_RADIUS + 5, 0.5, 8, 32);
        const ringMaterial = new THREE.MeshPhongMaterial({
            color: era.color,
            emissive: era.color,
            emissiveIntensity: 0.3
        });

        const ring = new THREE.Mesh(ringGeometry, ringMaterial);
        ring.position.copy(position);
        ring.rotation.x = Math.PI / 2;
        scene.add(ring);

        // Add text label (using sprite)
        const sprite = createTextSprite(era.label, era.color);
        sprite.position.copy(position);
        sprite.position.x += SPIRAL_RADIUS + 15;
        scene.add(sprite);
    });
}

function createEvents() {
    const categoryColors = {
        invention: 0xff6b6b,
        discovery: 0x4ecdc4,
        person: 0x45b7d1,
        publication: 0x96ceb4,
        event: 0xfeca57,
        algorithm: 0x9c88ff,
        theory: 0xfd79a8
    };

    events.forEach(event => {
        const position = getPositionForYear(event.year);

        // Create event sphere
        const radius = 0.5 + (event.importance * 0.3);
        const geometry = new THREE.SphereGeometry(radius, 16, 16);
        const material = new THREE.MeshPhongMaterial({
            color: categoryColors[event.category],
            emissive: categoryColors[event.category],
            emissiveIntensity: 0.2
        });

        const mesh = new THREE.Mesh(geometry, material);
        mesh.position.copy(position);

        // Add glow effect for important events
        if (event.importance >= 4) {
            const glowGeometry = new THREE.SphereGeometry(radius * 1.5, 16, 16);
            const glowMaterial = new THREE.MeshBasicMaterial({
                color: categoryColors[event.category],
                transparent: true,
                opacity: 0.2
            });

            const glowMesh = new THREE.Mesh(glowGeometry, glowMaterial);
            mesh.add(glowMesh);
        }

        // Store reference for interaction
        mesh.userData = event;
        eventMeshes.set(event.id, mesh);
        scene.add(mesh);
    });

    // Draw connections
    drawConnections3D();
}

function drawConnections3D() {
    events.forEach(event => {
        if (event.connections) {
            event.connections.forEach(conn => {
                const fromMesh = eventMeshes.get(conn.fromId);
                const toMesh = eventMeshes.get(conn.toId);

                if (fromMesh && toMesh) {
                    // Create curved connection line
                    const curve = new THREE.CatmullRomCurve3([
                        fromMesh.position.clone(),
                        new THREE.Vector3(
                            (fromMesh.position.x + toMesh.position.x) / 2,
                            (fromMesh.position.y + toMesh.position.y) / 2 + 10,
                            (fromMesh.position.z + toMesh.position.z) / 2
                        ),
                        toMesh.position.clone()
                    ]);

                    const points = curve.getPoints(50);
                    const geometry = new THREE.BufferGeometry().setFromPoints(points);
                    const material = new THREE.LineBasicMaterial({
                        color: 0x666666,
                        transparent: true,
                        opacity: 0.3
                    });

                    const line = new THREE.Line(geometry, material);
                    scene.add(line);
                }
            });
        }
    });
}

function getPositionForYear(year: number): THREE.Vector3 {
    // Map year to position along spiral
    const minYear = -10500;
    const maxYear = 2025;
    const t = (year - minYear) / (maxYear - minYear);

    const angle = t * Math.PI * 2 * TURNS;
    const y = t * SPIRAL_HEIGHT - SPIRAL_HEIGHT / 2;
    const x = Math.cos(angle) * SPIRAL_RADIUS;
    const z = Math.sin(angle) * SPIRAL_RADIUS;

    return new THREE.Vector3(x, y, z);
}

function createTextSprite(text: string, color: number): THREE.Sprite {
    const canvas = document.createElement('canvas');
    const context = canvas.getContext('2d')!;

    canvas.width = 256;
    canvas.height = 64;

    context.fillStyle = '#' + color.toString(16).padStart(6, '0');
    context.font = 'Bold 24px Arial';
    context.fillText(text, 0, 32);

    const texture = new THREE.CanvasTexture(canvas);
    const material = new THREE.SpriteMaterial({ map: texture });
    const sprite = new THREE.Sprite(material);
    sprite.scale.set(20, 5, 1);

    return sprite;
}

function animate() {
    requestAnimationFrame(animate);

    // Rotate event spheres
    eventMeshes.forEach(mesh => {
        mesh.rotation.y += 0.01;
    });

    controls.update();
    renderer.render(scene, camera);
}

// Raycasting for interaction
function handleClick(event: MouseEvent) {
    const mouse = new THREE.Vector2(
        (event.clientX / width) * 2 - 1,
        -(event.clientY / height) * 2 + 1
    );

    const raycaster = new THREE.Raycaster();
    raycaster.setFromCamera(mouse, camera);

    const intersects = raycaster.intersectObjects(Array.from(eventMeshes.values()));

    if (intersects.length > 0) {
        const selectedEvent = intersects[0].object.userData as TimelineEvent;
        dispatch('eventSelect', selectedEvent);
    }
}
</script>

<div bind:this={container} class="timeline-3d-container" on:click={handleClick}>
</div>

<style>
.timeline-3d-container {
    width: 100%;
    height: 100%;
    position: relative;
    cursor: grab;
}

.timeline-3d-container:active {
    cursor: grabbing;
}
</style>
```

## Timeline Controls Component

```typescript
<!-- frontend/src/lib/components/timeline/TimelineControls.svelte -->
<script lang="ts">
import { filterStore } from './stores/filterStore';
import type { EventCategory } from './types/timeline';

let yearRangeStart = -10500;
let yearRangeEnd = 2025;
let selectedCategories: EventCategory[] = [];
let minImportance = 1;
let searchQuery = '';
let viewMode: '2d' | '3d' = '2d';

const categories: EventCategory[] = [
    'invention', 'discovery', 'person',
    'publication', 'event', 'algorithm', 'theory'
];

function updateFilters() {
    filterStore.update(filters => ({
        ...filters,
        yearRange: [yearRangeStart, yearRangeEnd],
        categories: selectedCategories,
        minImportance,
        searchQuery
    }));
}

function toggleCategory(category: EventCategory) {
    if (selectedCategories.includes(category)) {
        selectedCategories = selectedCategories.filter(c => c !== category);
    } else {
        selectedCategories = [...selectedCategories, category];
    }
    updateFilters();
}

function resetFilters() {
    yearRangeStart = -10500;
    yearRangeEnd = 2025;
    selectedCategories = [];
    minImportance = 1;
    searchQuery = '';
    updateFilters();
}
</script>

<div class="timeline-controls">
    <div class="control-section">
        <h3>View Mode</h3>
        <div class="view-toggle">
            <button
                class:active={viewMode === '2d'}
                on:click={() => dispatch('viewChange', '2d')}
            >
                2D Timeline
            </button>
            <button
                class:active={viewMode === '3d'}
                on:click={() => dispatch('viewChange', '3d')}
            >
                3D Spiral
            </button>
        </div>
    </div>

    <div class="control-section">
        <h3>Year Range</h3>
        <div class="year-inputs">
            <input
                type="number"
                bind:value={yearRangeStart}
                on:change={updateFilters}
                min="-10500"
                max="2025"
            />
            <span>to</span>
            <input
                type="number"
                bind:value={yearRangeEnd}
                on:change={updateFilters}
                min="-10500"
                max="2025"
            />
        </div>
        <input
            type="range"
            min="-10500"
            max="2025"
            bind:value={yearRangeStart}
            on:input={updateFilters}
        />
        <input
            type="range"
            min="-10500"
            max="2025"
            bind:value={yearRangeEnd}
            on:input={updateFilters}
        />
    </div>

    <div class="control-section">
        <h3>Categories</h3>
        <div class="category-filters">
            {#each categories as category}
                <button
                    class="category-btn"
                    class:active={selectedCategories.includes(category)}
                    on:click={() => toggleCategory(category)}
                >
                    {category}
                </button>
            {/each}
        </div>
    </div>

    <div class="control-section">
        <h3>Minimum Importance</h3>
        <div class="importance-selector">
            {#each [1, 2, 3, 4, 5] as level}
                <button
                    class:active={minImportance <= level}
                    on:click={() => { minImportance = level; updateFilters(); }}
                >
                    ★
                </button>
            {/each}
        </div>
    </div>

    <div class="control-section">
        <h3>Search</h3>
        <input
            type="text"
            placeholder="Search events..."
            bind:value={searchQuery}
            on:input={updateFilters}
        />
    </div>

    <button class="reset-btn" on:click={resetFilters}>
        Reset Filters
    </button>
</div>

<style>
.timeline-controls {
    background: var(--color-surface);
    padding: 1rem;
    border-radius: 8px;
    box-shadow: 0 2px 8px rgba(0, 0, 0, 0.1);
}

.control-section {
    margin-bottom: 1.5rem;
}

.control-section h3 {
    font-size: 0.875rem;
    font-weight: 600;
    margin-bottom: 0.5rem;
    text-transform: uppercase;
    letter-spacing: 0.05em;
    color: var(--color-text-secondary);
}

.view-toggle {
    display: flex;
    gap: 0.5rem;
}

.view-toggle button {
    flex: 1;
    padding: 0.5rem 1rem;
    background: var(--color-background);
    border: 1px solid var(--color-border);
    border-radius: 4px;
    cursor: pointer;
    transition: all 0.2s;
}

.view-toggle button.active {
    background: var(--color-primary);
    color: white;
}

.year-inputs {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    margin-bottom: 0.5rem;
}

.year-inputs input {
    width: 100px;
    padding: 0.25rem 0.5rem;
    border: 1px solid var(--color-border);
    border-radius: 4px;
}

.category-filters {
    display: flex;
    flex-wrap: wrap;
    gap: 0.5rem;
}

.category-btn {
    padding: 0.25rem 0.75rem;
    background: var(--color-background);
    border: 1px solid var(--color-border);
    border-radius: 20px;
    font-size: 0.875rem;
    cursor: pointer;
    transition: all 0.2s;
}

.category-btn.active {
    background: var(--color-primary);
    color: white;
    border-color: var(--color-primary);
}

.importance-selector {
    display: flex;
    gap: 0.25rem;
}

.importance-selector button {
    background: none;
    border: none;
    font-size: 1.5rem;
    cursor: pointer;
    color: #ccc;
    transition: color 0.2s;
}

.importance-selector button.active {
    color: #ffd700;
}

.reset-btn {
    width: 100%;
    padding: 0.75rem;
    background: var(--color-error);
    color: white;
    border: none;
    border-radius: 4px;
    font-weight: 600;
    cursor: pointer;
    transition: background 0.2s;
}

.reset-btn:hover {
    background: var(--color-error-dark);
}
</style>
```

## Store Definitions

```typescript
// frontend/src/lib/components/timeline/stores/timelineStore.ts
import { writable, derived } from 'svelte/store';
import type { TimelineEvent, Civilization } from '../types/timeline';

interface TimelineState {
    events: TimelineEvent[];
    selectedEvent: TimelineEvent | null;
    civilizations: Civilization[];
    loading: boolean;
    error: string | null;
}

function createTimelineStore() {
    const { subscribe, set, update } = writable<TimelineState>({
        events: [],
        selectedEvent: null,
        civilizations: [],
        loading: false,
        error: null
    });

    return {
        subscribe,
        loadEvents: async () => {
            update(state => ({ ...state, loading: true }));

            try {
                const response = await fetch('/api/content/timeline');
                const events = await response.json();

                update(state => ({
                    ...state,
                    events,
                    loading: false
                }));
            } catch (error) {
                update(state => ({
                    ...state,
                    error: error.message,
                    loading: false
                }));
            }
        },
        setSelectedEvent: (event: TimelineEvent | null) => {
            update(state => ({ ...state, selectedEvent: event }));
        }
    };
}

export const timelineStore = createTimelineStore();
```

```typescript
// frontend/src/lib/components/timeline/stores/filterStore.ts
import { writable } from 'svelte/store';
import type { EventCategory } from '../types/timeline';

interface FilterState {
    yearRange: [number, number] | null;
    categories: EventCategory[];
    minImportance: number;
    civilizationId: number | null;
    searchQuery: string;
}

function createFilterStore() {
    const { subscribe, set, update } = writable<FilterState>({
        yearRange: null,
        categories: [],
        minImportance: 1,
        civilizationId: null,
        searchQuery: ''
    });

    return {
        subscribe,
        update,
        reset: () => set({
            yearRange: null,
            categories: [],
            minImportance: 1,
            civilizationId: null,
            searchQuery: ''
        })
    };
}

export const filterStore = createFilterStore();
```

## API Integration

```typescript
// frontend/src/lib/api/timeline.ts
import type { TimelineEvent, Civilization } from '$lib/components/timeline/types/timeline';

export class TimelineAPI {
    private baseUrl = '/api/content';

    async getEvents(params?: {
        startYear?: number;
        endYear?: number;
        category?: string;
        civilizationId?: number;
        importanceMin?: number;
    }): Promise<TimelineEvent[]> {
        const queryParams = new URLSearchParams();

        if (params) {
            Object.entries(params).forEach(([key, value]) => {
                if (value !== undefined) {
                    queryParams.append(key, value.toString());
                }
            });
        }

        const response = await fetch(`${this.baseUrl}/timeline?${queryParams}`);

        if (!response.ok) {
            throw new Error('Failed to fetch timeline events');
        }

        return response.json();
    }

    async getCivilizations(): Promise<Civilization[]> {
        const response = await fetch(`${this.baseUrl}/civilizations`);

        if (!response.ok) {
            throw new Error('Failed to fetch civilizations');
        }

        return response.json();
    }

    async getEventConnections(eventId: number): Promise<any[]> {
        const response = await fetch(`${this.baseUrl}/timeline/${eventId}/connections`);

        if (!response.ok) {
            throw new Error('Failed to fetch event connections');
        }

        return response.json();
    }
}

export const timelineAPI = new TimelineAPI();
```

## Performance Optimizations

```typescript
// frontend/src/lib/components/timeline/utils/dataOptimizer.ts
import type { TimelineEvent } from '../types/timeline';

export class TimelineDataOptimizer {
    // Cluster events for better performance
    static clusterEvents(
        events: TimelineEvent[],
        zoomLevel: number
    ): TimelineEvent[] {
        if (zoomLevel > 5) {
            // No clustering at high zoom
            return events;
        }

        const clusters: Map<string, TimelineEvent[]> = new Map();
        const clusterRadius = (10 - zoomLevel) * 10; // years

        events.forEach(event => {
            const clusterKey = Math.floor(event.year / clusterRadius) * clusterRadius;
            const key = `${clusterKey}_${event.category}`;

            if (!clusters.has(key)) {
                clusters.set(key, []);
            }

            clusters.get(key)!.push(event);
        });

        // Convert clusters to single events
        const clusteredEvents: TimelineEvent[] = [];

        clusters.forEach((clusterEvents, key) => {
            if (clusterEvents.length === 1) {
                clusteredEvents.push(clusterEvents[0]);
            } else {
                // Create cluster representation
                const avgYear = Math.round(
                    clusterEvents.reduce((sum, e) => sum + e.year, 0) / clusterEvents.length
                );

                const maxImportance = Math.max(...clusterEvents.map(e => e.importance));

                clusteredEvents.push({
                    id: -1, // Cluster ID
                    year: avgYear,
                    title: `${clusterEvents.length} events`,
                    description: clusterEvents.map(e => e.title).join(', '),
                    category: clusterEvents[0].category,
                    importance: maxImportance as any,
                    // Store original events
                    clusteredEvents
                } as any);
            }
        });

        return clusteredEvents;
    }

    // Level-of-detail optimization
    static getLODEvents(
        events: TimelineEvent[],
        viewBounds: { startYear: number; endYear: number },
        maxEvents: number = 500
    ): TimelineEvent[] {
        // Filter to visible range
        const visibleEvents = events.filter(
            e => e.year >= viewBounds.startYear && e.year <= viewBounds.endYear
        );

        // If within limit, return all
        if (visibleEvents.length <= maxEvents) {
            return visibleEvents;
        }

        // Sort by importance and take top N
        return visibleEvents
            .sort((a, b) => b.importance - a.importance)
            .slice(0, maxEvents);
    }
}
```

## Testing Specifications

```typescript
// frontend/src/lib/components/timeline/tests/Timeline.test.ts
import { render, fireEvent } from '@testing-library/svelte';
import { describe, it, expect } from 'vitest';
import Timeline2D from '../Timeline2D.svelte';
import TimelineControls from '../TimelineControls.svelte';
import type { TimelineEvent } from '../types/timeline';

describe('Timeline2D Component', () => {
    const mockEvents: TimelineEvent[] = [
        {
            id: 1,
            year: -3500,
            title: 'Sumerian Cuneiform',
            description: 'First written numbers',
            category: 'invention',
            importance: 5
        },
        {
            id: 2,
            year: -300,
            title: 'Euclid Elements',
            description: 'Foundation of algorithms',
            category: 'publication',
            importance: 5
        }
    ];

    it('renders timeline with events', () => {
        const { container } = render(Timeline2D, {
            props: { events: mockEvents }
        });

        const eventCircles = container.querySelectorAll('.event');
        expect(eventCircles.length).toBe(2);
    });

    it('handles zoom correctly', async () => {
        const { container } = render(Timeline2D, {
            props: { events: mockEvents }
        });

        // Simulate zoom
        const svg = container.querySelector('svg');
        // ... zoom simulation
    });

    it('filters events based on criteria', () => {
        // Test filtering logic
    });
});

describe('Timeline Controls', () => {
    it('updates filters on user input', async () => {
        const { getByRole, getByText } = render(TimelineControls);

        // Test year range
        const yearInput = getByRole('spinbutton', { name: /start year/i });
        await fireEvent.input(yearInput, { target: { value: '-2000' } });

        // Test category selection
        const inventionBtn = getByText('invention');
        await fireEvent.click(inventionBtn);

        // Verify filter state update
        // ...
    });
});
```
