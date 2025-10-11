# Interactive Timeline Visualization Design

## Overview

The Ancient Compute timeline visualization provides an interactive, multi-layered view of computation history spanning 12,500 years. It connects historical events, mathematical discoveries, and computational innovations across cultures and millennia.

## Technical Architecture

### Frontend Components

#### Main Timeline Component (D3.js + Three.js)
```typescript
// src/lib/components/timeline/Timeline.svelte
interface TimelineEvent {
  id: string;
  date: Date | DateRange;
  title: string;
  description: string;
  category: EventCategory;
  culture: Culture;
  connections: string[]; // IDs of connected events
  artifacts: Artifact[];
  codeExamples: CodeExample[];
  coordinates: GeographicCoordinates;
  importance: number; // 1-10 scale for visual prominence
}

interface DateRange {
  start: Date;
  end: Date;
  precision: 'year' | 'decade' | 'century' | 'millennium';
}

enum EventCategory {
  COUNTING_SYSTEM = 'counting_system',
  ALGORITHM = 'algorithm',
  LOGIC_SYSTEM = 'logic_system',
  MECHANICAL_DEVICE = 'mechanical_device',
  ELECTRONIC_COMPUTER = 'electronic_computer',
  PROGRAMMING_LANGUAGE = 'programming_language',
  TYPE_THEORY = 'type_theory',
  MATHEMATICAL_CRISIS = 'mathematical_crisis'
}

interface Culture {
  name: string;
  region: string;
  period: string;
  color: string; // For visual differentiation
}
```

### Data Structure

#### Hierarchical Timeline Data
```yaml
# content/timeline/manifest.yaml
eras:
  - id: prehistoric
    name: "Prehistoric Computation"
    range:
      start: -20000
      end: -3500
    events:
      - id: ishango-bone
        date: -18000
        title: "Ishango Bone"
        culture: "Central African"
        category: counting_system
        importance: 9
        description: "Earliest known mathematical artifact"
        artifacts:
          - type: image
            path: /assets/ishango-bone.jpg
          - type: 3d-model
            path: /assets/ishango-bone.gltf
        codeExamples:
          - language: python
            title: "Ishango Bone Prime Pattern"
            code: |
              # Possible prime number sequence on Ishango bone
              def ishango_sequence():
                  return [11, 13, 17, 19]  # Column 1 primes
          - language: c
            title: "Tally Mark Counter"
            code: |
              // Simulate tally mark counting
              int tally_count(int marks[], int n) {
                  int total = 0;
                  for (int i = 0; i < n; i++) {
                      total += marks[i];
                  }
                  return total;
              }
```

### Visualization Layers

#### 1. Temporal Axis (Horizontal)
- Logarithmic scale for ancient periods
- Linear scale for recent centuries
- Smooth zooming between scales
- Contextual date formatting (BCE/CE, century, decade)

#### 2. Cultural Tracks (Vertical)
```javascript
const culturalTracks = [
  { id: 'mesopotamian', y: 0, color: '#8B4513' },
  { id: 'egyptian', y: 100, color: '#FFD700' },
  { id: 'greek', y: 200, color: '#4169E1' },
  { id: 'indian', y: 300, color: '#FF6347' },
  { id: 'chinese', y: 400, color: '#DC143C' },
  { id: 'islamic', y: 500, color: '#228B22' },
  { id: 'european', y: 600, color: '#4B0082' },
  { id: 'american', y: 700, color: '#FF4500' }
];
```

#### 3. Connection Visualization
```typescript
// Connection types with different visual styles
enum ConnectionType {
  INFLUENCED = 'influenced',        // Dashed line
  EVOLVED_FROM = 'evolved_from',   // Solid arrow
  CONTEMPORARY = 'contemporary',   // Dotted line
  CONTRADICTED = 'contradicted',   // Red wavy line
  RESOLVED = 'resolved'            // Green double line
}

interface Connection {
  source: string;  // Event ID
  target: string;  // Event ID
  type: ConnectionType;
  description: string;
  strength: number; // 0-1 for visual weight
}
```

### Interactive Features

#### 1. Zoom and Pan Controls
```javascript
// D3.js zoom behavior
const zoom = d3.zoom()
  .scaleExtent([0.1, 100])
  .on('zoom', (event) => {
    // Update timeline scale
    timeScale.range([0, width * event.transform.k]);

    // Show/hide detail levels based on zoom
    if (event.transform.k > 5) {
      showMinorEvents();
    } else if (event.transform.k > 2) {
      showMajorEvents();
    } else {
      showErasOnly();
    }
  });
```

#### 2. Filter System
```svelte
<script lang="ts">
  let activeFilters = {
    categories: new Set<EventCategory>(),
    cultures: new Set<string>(),
    dateRange: { start: -20000, end: 2025 },
    importance: { min: 1, max: 10 },
    hasCode: false,
    hasArtifacts: false
  };

  function applyFilters(events: TimelineEvent[]): TimelineEvent[] {
    return events.filter(event => {
      if (activeFilters.categories.size > 0 &&
          !activeFilters.categories.has(event.category)) {
        return false;
      }

      if (activeFilters.hasCode && !event.codeExamples.length) {
        return false;
      }

      // Additional filter logic...
      return true;
    });
  }
</script>
```

#### 3. Detail Panel
```svelte
<script lang="ts">
  let selectedEvent: TimelineEvent | null = null;

  function showEventDetail(event: TimelineEvent) {
    selectedEvent = event;
    // Fetch additional data if needed
    loadRelatedEvents(event.connections);
    loadCodeExamples(event.id);
    loadArtifacts(event.id);
  }
</script>

{#if selectedEvent}
  <div class="detail-panel">
    <h2>{selectedEvent.title}</h2>
    <p class="date">{formatDate(selectedEvent.date)}</p>
    <p class="culture">{selectedEvent.culture.name}</p>
    <div class="description">{@html selectedEvent.description}</div>

    {#if selectedEvent.codeExamples.length}
      <CodeExampleTabs examples={selectedEvent.codeExamples} />
    {/if}

    {#if selectedEvent.artifacts.length}
      <ArtifactGallery artifacts={selectedEvent.artifacts} />
    {/if}

    <ConnectionGraph eventId={selectedEvent.id} />
  </div>
{/if}
```

### 3D Visualization Mode (Three.js)

#### Spiral Timeline
```javascript
// Three.js spiral timeline for better space utilization
class SpiralTimeline {
  constructor(scene) {
    this.scene = scene;
    this.spiral = new THREE.Curve();
    this.events = [];
  }

  createSpiral(startYear, endYear, turns) {
    const points = [];
    const yearRange = endYear - startYear;

    for (let i = 0; i <= 1000; i++) {
      const t = i / 1000;
      const year = startYear + (t * yearRange);
      const angle = t * turns * Math.PI * 2;
      const radius = 10 + (t * 50); // Expanding spiral

      const x = Math.cos(angle) * radius;
      const y = t * 100; // Height increases with time
      const z = Math.sin(angle) * radius;

      points.push(new THREE.Vector3(x, y, z));
    }

    this.spiral = new THREE.CatmullRomCurve3(points);
  }

  placeEvent(event, position) {
    const geometry = new THREE.SphereGeometry(
      Math.log(event.importance + 1), 16, 16
    );
    const material = new THREE.MeshPhongMaterial({
      color: this.getCategoryColor(event.category)
    });
    const mesh = new THREE.Mesh(geometry, material);
    mesh.position.copy(position);
    mesh.userData = event;

    this.scene.add(mesh);
    this.events.push(mesh);
  }
}
```

### Animation and Transitions

#### Era Transitions
```javascript
class EraTransition {
  constructor(timeline) {
    this.timeline = timeline;
    this.duration = 2000; // 2 seconds
  }

  async transitionToEra(era) {
    // Fade out current events
    await this.fadeOut(this.timeline.currentEvents);

    // Update scale for new era
    const scale = this.getEraScale(era);
    await this.animateScale(scale);

    // Load and fade in new events
    const newEvents = await this.loadEraEvents(era);
    await this.fadeIn(newEvents);

    // Update connections
    this.updateConnections(newEvents);
  }

  animateScale(newScale) {
    return new Promise(resolve => {
      d3.select('.timeline-axis')
        .transition()
        .duration(this.duration)
        .call(d3.axisBottom(newScale))
        .on('end', resolve);
    });
  }
}
```

### Performance Optimization

#### Level-of-Detail (LOD) System
```javascript
class TimelineLOD {
  constructor() {
    this.zoomLevels = [
      { min: 0, max: 0.5, detail: 'era' },
      { min: 0.5, max: 2, detail: 'major' },
      { min: 2, max: 5, detail: 'standard' },
      { min: 5, max: 100, detail: 'full' }
    ];
  }

  getVisibleEvents(zoom, viewport) {
    const level = this.getDetailLevel(zoom);
    const timeRange = this.getTimeRange(viewport);

    return this.events.filter(event => {
      // Check if in viewport
      if (event.date < timeRange.start || event.date > timeRange.end) {
        return false;
      }

      // Check importance threshold for zoom level
      switch(level.detail) {
        case 'era':
          return event.importance >= 9;
        case 'major':
          return event.importance >= 7;
        case 'standard':
          return event.importance >= 5;
        default:
          return true;
      }
    });
  }
}
```

#### Virtual Scrolling
```javascript
class VirtualTimeline {
  constructor(container, events) {
    this.container = container;
    this.events = events;
    this.visibleRange = { start: 0, end: 100 };
    this.rendered = new Map();
  }

  update(scrollPosition) {
    const newRange = this.calculateVisibleRange(scrollPosition);

    // Remove events no longer visible
    for (const [id, element] of this.rendered) {
      if (!this.isInRange(id, newRange)) {
        element.remove();
        this.rendered.delete(id);
      }
    }

    // Add newly visible events
    const toRender = this.events.filter(e =>
      this.isInRange(e.id, newRange) && !this.rendered.has(e.id)
    );

    toRender.forEach(event => {
      const element = this.renderEvent(event);
      this.container.appendChild(element);
      this.rendered.set(event.id, element);
    });
  }
}
```

### Accessibility Features

#### Keyboard Navigation
```javascript
class TimelineKeyboardNav {
  constructor(timeline) {
    this.timeline = timeline;
    this.currentIndex = 0;
    this.setupKeyHandlers();
  }

  setupKeyHandlers() {
    document.addEventListener('keydown', (e) => {
      switch(e.key) {
        case 'ArrowLeft':
          this.navigatePrevious();
          break;
        case 'ArrowRight':
          this.navigateNext();
          break;
        case 'Enter':
          this.selectCurrent();
          break;
        case '+':
          this.zoomIn();
          break;
        case '-':
          this.zoomOut();
          break;
        case 'f':
          this.openFilterMenu();
          break;
      }
    });
  }

  navigateNext() {
    this.currentIndex = Math.min(
      this.currentIndex + 1,
      this.timeline.events.length - 1
    );
    this.focusEvent(this.currentIndex);
  }

  focusEvent(index) {
    const event = this.timeline.events[index];
    event.element.focus();
    this.announceEvent(event);
  }

  announceEvent(event) {
    const announcement = `${event.title}, ${event.culture.name},
                         ${this.formatDate(event.date)}`;
    this.screenReaderAnnounce(announcement);
  }
}
```

#### Screen Reader Support
```html
<!-- Timeline container with ARIA attributes -->
<div
  class="timeline-container"
  role="application"
  aria-label="Interactive timeline of computation history"
  aria-describedby="timeline-instructions"
>
  <div id="timeline-instructions" class="sr-only">
    Use arrow keys to navigate between events.
    Press Enter to view details.
    Press F to open filters.
  </div>

  <div class="timeline-events" role="list">
    {#each visibleEvents as event}
      <div
        class="timeline-event"
        role="listitem"
        tabindex="0"
        aria-label="{event.title}"
        aria-describedby="event-{event.id}-description"
      >
        <span id="event-{event.id}-description" class="sr-only">
          {event.description}
        </span>
      </div>
    {/each}
  </div>
</div>
```

### Integration with Backend

#### WebSocket Updates
```javascript
class TimelineWebSocket {
  constructor(url) {
    this.ws = new WebSocket(url);
    this.setupHandlers();
  }

  setupHandlers() {
    this.ws.onmessage = (event) => {
      const data = JSON.parse(event.data);

      switch(data.type) {
        case 'new_event':
          this.timeline.addEvent(data.event);
          break;
        case 'update_connections':
          this.timeline.updateConnections(data.connections);
          break;
        case 'user_annotation':
          this.timeline.showAnnotation(data.annotation);
          break;
      }
    };
  }

  requestEventDetails(eventId) {
    this.ws.send(JSON.stringify({
      type: 'get_details',
      eventId: eventId
    }));
  }
}
```

### Export and Sharing

#### Generate Static Timeline Image
```javascript
async function exportTimelineImage(format = 'png') {
  const svg = d3.select('.timeline-svg').node();
  const serializer = new XMLSerializer();
  const svgString = serializer.serializeToString(svg);

  if (format === 'svg') {
    return svgString;
  }

  // Convert to PNG using canvas
  const canvas = document.createElement('canvas');
  const ctx = canvas.getContext('2d');
  const img = new Image();

  return new Promise((resolve) => {
    img.onload = () => {
      canvas.width = img.width;
      canvas.height = img.height;
      ctx.drawImage(img, 0, 0);
      canvas.toBlob(blob => resolve(blob), 'image/png');
    };
    img.src = 'data:image/svg+xml;base64,' + btoa(svgString);
  });
}
```

#### Share Timeline View
```javascript
function generateShareableLink() {
  const state = {
    zoom: timeline.currentZoom,
    center: timeline.currentCenter,
    filters: timeline.activeFilters,
    selectedEvent: timeline.selectedEvent?.id
  };

  const encoded = btoa(JSON.stringify(state));
  return `${window.location.origin}/timeline?state=${encoded}`;
}
```

## Content Structure

### Historical Events Database
```yaml
# content/timeline/events/prehistoric.yaml
events:
  - id: lebombo-bone
    date: -43000
    title: "Lebombo Bone"
    description: "Earliest known counting artifact with 29 notches"
    category: counting_system
    culture:
      name: "Southern African"
      region: "Swaziland"
    importance: 8
    connections:
      - target: ishango-bone
        type: evolved_from
        description: "Similar notch-based counting system"

  - id: clay-tokens
    date: -8000
    title: "Clay Tokens"
    description: "Abstract counting system using shaped clay pieces"
    category: counting_system
    culture:
      name: "Mesopotamian"
      region: "Fertile Crescent"
    importance: 10
    codeExamples:
      - language: python
        title: "Token Counting System"
        code: |
          class ClayToken:
              shapes = {
                  'cone': 1,
                  'sphere': 10,
                  'disk': 100,
                  'cylinder': 1000
              }

              @classmethod
              def count_tokens(cls, tokens):
                  return sum(cls.shapes[t] for t in tokens)
```

This comprehensive timeline visualization system provides:
1. Multi-scale temporal navigation
2. Cross-cultural connection visualization
3. Interactive code examples
4. 3D exploration mode
5. Accessibility features
6. Performance optimization
7. Export capabilities
8. Real-time updates via WebSocket

The design ensures users can explore 12,500 years of computational history in an engaging, educational, and accessible manner.