# Frontend Module - Requirements and Architecture

**Module Path**: `frontend/`
**Language**: TypeScript 5.3+
**Framework**: SvelteKit + Vite
**Status**: Phase 1 Complete, Phase 2 In Progress

---

## Overview

The frontend is a SvelteKit-based web application providing an interactive educational platform for learning computation history. It features:

- **Interactive Timeline**: Visual navigation through 12,500 years of computing
- **Code Editor**: Monaco editor with syntax highlighting for multiple languages
- **Visualizations**: D3.js graphs and Three.js 3D scenes for historical context
- **Curriculum Integration**: Links to lessons, exercises, and multimedia content
- **Code Execution**: Real-time code submission and result display

**Architecture**: Client-side SvelteKit app with WebSocket communication to FastAPI backend

---

## Directory Structure

```
frontend/
├── src/
│   ├── routes/
│   │   ├── +page.svelte          # Home page
│   │   ├── timeline/
│   │   │   └── +page.svelte      # Historical timeline view
│   │   ├── modules/
│   │   │   └── [id]/
│   │   │       └── +page.svelte  # Module lessons
│   │   ├── lessons/
│   │   │   └── [id]/
│   │   │       └── +page.svelte  # Individual lesson
│   │   ├── playground/
│   │   │   └── +page.svelte      # Code execution playground
│   │   └── api/
│   │       ├── execute.ts        # Code execution API route
│   │       └── modules.ts        # Curriculum API routes
│   ├── lib/
│   │   ├── components/           # Reusable Svelte components
│   │   │   ├── Timeline.svelte   # Timeline visualization
│   │   │   ├── CodeEditor.svelte # Monaco editor wrapper
│   │   │   ├── ResultsPanel.svelte
│   │   │   └── ...
│   │   ├── stores/               # Svelte stores (state management)
│   │   │   ├── user.ts           # User authentication state
│   │   │   ├── curriculum.ts     # Curriculum data store
│   │   │   └── execution.ts      # Code execution state
│   │   ├── api/                  # Backend API client
│   │   │   ├── client.ts         # HTTP client setup
│   │   │   ├── code.ts           # Code execution API
│   │   │   └── curriculum.ts     # Curriculum API
│   │   ├── types/                # TypeScript type definitions
│   │   │   ├── api.ts            # API response types
│   │   │   ├── curriculum.ts     # Lesson/module types
│   │   │   └── index.ts          # Central export
│   │   └── utils/                # Utility functions
│   │       ├── formatting.ts     # Output formatting
│   │       └── validation.ts     # Form validation
│   ├── app.html                  # HTML shell
│   ├── app.css                   # Global styles
│   └── app.ts                    # App initialization
├── static/                       # Static assets
│   ├── favicon.png
│   ├── logo.svg
│   └── images/
├── tests/                        # Vitest unit tests
│   ├── components/
│   ├── stores/
│   └── utils/
├── vite.config.ts               # Vite configuration
├── svelte.config.js             # SvelteKit configuration
├── tsconfig.json                # TypeScript configuration
├── eslintrc.json                # ESLint configuration
├── prettier.config.js           # Prettier configuration
├── package.json                 # Node.js dependencies
├── pnpm-lock.yaml               # Lock file (checked in)
├── .env.example                 # Environment template
└── README.md                    # Module documentation
```

---

## Node.js Dependencies

### Core Framework

**SvelteKit Ecosystem** (6 packages, 25 MB):
```json
{
  "@sveltejs/kit": "^1.27.6",              // Meta-framework
  "@sveltejs/adapter-node": "^1.3.1",      // Node.js deployment
  "@sveltejs/vite-plugin-svelte": "^2.5.3",
  "svelte": "^4.2.7",                      // UI framework
  "vite": "^4.5.0",                        // Build tool
  "tslib": "^2.6.2"                        // TypeScript helpers
}
```

**Visualization Libraries** (3 packages, 45 MB):
```json
{
  "d3": "^7.8.5",                 // Data visualization (timeline)
  "three": "^0.158.0",            // 3D graphics (historical scenes)
  "monaco-editor": "^0.44.0"      // Code editor with syntax highlighting
}
```

**Type System** (1 package):
```json
{
  "typescript": "^5.3.2"          // Type safety for JavaScript
}
```

### Development Tools

**Linting & Formatting** (6 packages, 50 MB):
```json
{
  "@typescript-eslint/eslint-plugin": "^6.12.0",
  "@typescript-eslint/parser": "^6.12.0",
  "eslint": "^8.54.0",
  "eslint-config-prettier": "^9.0.0",
  "eslint-plugin-svelte": "^2.35.1",
  "prettier": "^3.1.0",
  "prettier-plugin-svelte": "^3.1.2"
}
```

**Type Checking** (1 package):
```json
{
  "svelte-check": "^3.6.2"        // Svelte type checking
}
```

**Testing** (1 package):
```json
{
  "vitest": "^0.34.6"             // Unit testing framework
}
```

**TypeScript Type Definitions** (2 packages):
```json
{
  "@types/d3": "^7.4.3",          // D3.js types
  "@types/three": "^0.158.3"      // Three.js types
}
```

**Total**: 21 packages, ~200 MB installed

---

## Node.js Version Requirements

**Minimum**: 18.0.0
- Supports async/await natively
- Compatible with SvelteKit 1.x

**Recommended**: 20.x LTS
- Stable, long-term support
- Better performance

**Package Manager**: pnpm 8.0+ (required, not npm/yarn)
- Faster installation
- Better disk usage
- Required by SvelteKit defaults

---

## TypeScript Configuration

### TypeScript Version

**Version**: 5.3.2+ (required)

**Features Used**:
- Type inference (not fully explicit types needed everywhere)
- Generics for reusable components
- Discriminated unions for API responses
- Template literal types for string enums

**Type Safety Level**:
- Strict mode enabled: `"strict": true`
- No implicit any: `"noImplicitAny": true`
- Strict null checks: `"strictNullChecks": true`

**Configuration File**: `tsconfig.json`
```json
{
  "compilerOptions": {
    "target": "ES2020",
    "module": "ES2020",
    "lib": ["ES2020", "DOM"],
    "strict": true,
    "noImplicitAny": true,
    "strictNullChecks": true,
    "moduleResolution": "bundler",
    "resolveJsonModule": true,
    "allowSyntheticDefaultImports": true,
    "baseUrl": ".",
    "paths": {
      "$lib/*": ["src/lib/*"],
      "$app/*": [".svelte-kit/generated/*"]
    }
  },
  "include": ["src/**/*.ts", "src/**/*.svelte"]
}
```

---

## Build Configuration

### Vite

**File**: `vite.config.ts`

**Key Settings**:
```typescript
import { defineConfig } from 'vite'
import { svelte } from '@sveltejs/vite-plugin-svelte'

export default defineConfig({
  plugins: [svelte()],
  server: {
    proxy: {
      '/api': 'http://localhost:8000',
      '/ws': {
        target: 'ws://localhost:8000',
        ws: true
      }
    }
  },
  build: {
    target: 'ES2020',
    minify: 'terser'
  }
})
```

**Dev Server**:
- Port: 5173 (default)
- Hot reload: Automatic on file changes
- API proxy: Routes `/api` to backend

### SvelteKit

**File**: `svelte.config.js`

**Key Settings**:
```javascript
import adapter from '@sveltejs/adapter-node'

export default {
  kit: {
    adapter: adapter(),
    alias: {
      $lib: 'src/lib',
      $components: 'src/lib/components'
    }
  }
}
```

---

## Application Architecture

### Page Structure

**Root Layout**: `src/routes/+layout.svelte`
- Navigation bar
- Global styles
- Authentication check
- Store initialization

**Home Page**: `src/routes/+page.svelte`
- Welcome message
- Quick navigation to modules
- Featured content

**Timeline View**: `src/routes/timeline/+page.svelte`
- D3.js timeline visualization (2000 BC - 2025 AD)
- Interactive event selection
- Links to relevant modules

**Module Pages**: `src/routes/modules/[id]/+page.svelte`
- Module overview
- List of lessons
- Navigation between lessons

**Lesson Pages**: `src/routes/lessons/[id]/+page.svelte`
- Lesson content
- Code examples
- Exercise submission

**Playground**: `src/routes/playground/+page.svelte`
- Code editor (Monaco)
- Language selector (C, Python, Haskell, etc.)
- Execute button
- Results panel (output, errors, assembly)

---

## Component Architecture

### Core Components

**Timeline.svelte** (D3.js visualization):
```svelte
<script lang="ts">
  import * as d3 from 'd3'

  let timelineData: HistoricalEvent[]

  // Render timeline with D3
</script>
```

**CodeEditor.svelte** (Monaco wrapper):
```svelte
<script lang="ts">
  import * as monaco from 'monaco-editor'

  let code: string
  let language: string

  // Initialize Monaco editor
</script>
```

**ResultsPanel.svelte** (Execution results):
```svelte
<script lang="ts">
  import type { ExecutionResult } from '$lib/types'

  let result: ExecutionResult

  // Display output, errors, assembly, timing
</script>
```

**LessonCard.svelte** (Reusable lesson preview):
```svelte
<script lang="ts">
  import type { Lesson } from '$lib/types'

  export let lesson: Lesson

  // Display lesson preview
</script>
```

---

## State Management

### Svelte Stores

**User Store** (`src/lib/stores/user.ts`):
```typescript
import { writable } from 'svelte/store'

export const currentUser = writable<User | null>(null)
export const isAuthenticated = writable(false)
```

**Curriculum Store** (`src/lib/stores/curriculum.ts`):
```typescript
export const modules = writable<Module[]>([])
export const currentModule = writable<Module | null>(null)
```

**Execution Store** (`src/lib/stores/execution.ts`):
```typescript
export const isExecuting = writable(false)
export const lastResult = writable<ExecutionResult | null>(null)
```

---

## API Client

### HTTP Client Setup

**File**: `src/lib/api/client.ts`

```typescript
import axios from 'axios'

export const apiClient = axios.create({
  baseURL: '/api',
  headers: {
    'Content-Type': 'application/json'
  }
})

// Request/response interceptors for auth tokens
apiClient.interceptors.request.use((config) => {
  const token = localStorage.getItem('auth_token')
  if (token) {
    config.headers['Authorization'] = `Bearer ${token}`
  }
  return config
})
```

### Code Execution API

**File**: `src/lib/api/code.ts`

```typescript
export async function executeCode(
  code: string,
  language: string,
  timeout: number = 10
): Promise<ExecutionResult> {
  const response = await apiClient.post('/execute', {
    code,
    language,
    timeout_seconds: timeout
  })
  return response.data
}
```

### WebSocket Connection

```typescript
let ws: WebSocket | null = null

export function connectWebSocket() {
  ws = new WebSocket('ws://localhost:8000/ws/execute')

  ws.onmessage = (event) => {
    const result = JSON.parse(event.data)
    // Update execution store
  }
}
```

---

## Type Definitions

### API Types

**File**: `src/lib/types/api.ts`

```typescript
export interface ExecutionRequest {
  code: string
  language: 'c' | 'python' | 'haskell' | 'idris' | 'lisp' | 'systemf' | 'java' | 'assembly'
  timeout_seconds: number
}

export interface ExecutionResult {
  status: 'SUCCESS' | 'COMPILE_ERROR' | 'TIMEOUT' | 'RUNTIME_ERROR'
  output: string
  errors: string[]
  ir: string
  assembly: string
  machine_code: string
  compile_time_ms: number
  codegen_time_ms: number
  assembly_time_ms: number
}
```

### Curriculum Types

**File**: `src/lib/types/curriculum.ts`

```typescript
export interface Module {
  id: number
  title: string
  description: string
  era: string
  lessons_count: number
  order: number
}

export interface Lesson {
  id: number
  module_id: number
  title: string
  content: string
  order: number
  code_examples: CodeExample[]
  exercises: Exercise[]
}

export interface Exercise {
  id: number
  lesson_id: number
  prompt: string
  starter_code: string
  solution_code: string
  test_cases: TestCase[]
}
```

---

## Running the Frontend

### Development Server

```bash
# 1. Install pnpm
npm install -g pnpm

# 2. Install dependencies
pnpm install

# 3. Set up environment
cp .env.example .env
# Edit .env with API_URL=http://localhost:8000

# 4. Start development server
pnpm run dev

# 5. Access application
# Browser: http://localhost:5173
```

### Production Build

```bash
# 1. Build for production
pnpm run build

# 2. Preview build locally
pnpm run preview

# 3. Deploy
# Using Docker: docker build -t ancient-compute-frontend .
# Using Node.js: node build/index.js
```

### Type Checking

```bash
# Check types without running build
pnpm run check

# Watch mode for continuous checking
pnpm run check:watch
```

---

## Code Quality Standards

### TypeScript Type Hints

**Requirement**: Strict mode enabled

**Example**:
```typescript
// Good
function processData(data: string): number[] {
  return data.split(',').map(Number)
}

// Bad (would fail type check)
function processData(data) {
  return data.split(',').map(Number)
}
```

### ESLint Configuration

**File**: `.eslintrc.json`

**Rules**:
- No unused variables
- Consistent naming conventions
- No console.log in production code
- Svelte-specific linting rules

**Command**: `pnpm run lint`

### Prettier Formatting

**Auto-formatting on save** (if IDE configured)

**Command**: `pnpm run format`

**Configuration**: `.prettierrc`

---

## Testing Strategy

### Unit Tests

**Framework**: Vitest
**Location**: `frontend/tests/`
**Command**: `pnpm run test`

**Test Structure**:
```typescript
import { describe, it, expect } from 'vitest'
import { render, screen } from '@testing-library/svelte'
import Component from '$lib/components/Component.svelte'

describe('Component', () => {
  it('renders correctly', () => {
    render(Component)
    expect(screen.getByText('Expected text')).toBeInTheDocument()
  })
})
```

### Component Testing

- Test props and slots
- Test event handlers
- Test store subscriptions
- Test API calls (mocked)

### Integration Testing

- Full page flows
- State persistence
- API integration
- WebSocket communication

---

## Visualization Implementations

### D3.js Timeline

**File**: `src/lib/components/Timeline.svelte`

**Features**:
- 12,500-year timeline (20000 BC - 2025 AD)
- Interactive events (click to navigate)
- Zoom and pan capabilities
- Color-coded by era

**Data Format**:
```typescript
interface TimelineEvent {
  year: number
  title: string
  description: string
  moduleId: number
  era: 'prehistory' | 'ancient' | 'medieval' | 'early-modern' | 'foundations' | 'electronic' | 'type-theory'
}
```

### Three.js 3D Visualizations

**File**: `src/lib/components/HistoricalScene.svelte`

**Scenes**:
1. Ancient computing devices (abacus, astrolabe, Antikythera mechanism)
2. Mechanical calculators (Pascaline, Leibniz wheel, Babbage engine)
3. Electronic computers (ENIAC, Colossus, early transistor machines)
4. Modern architecture (processor, memory hierarchy)

---

## Environment Configuration

### Development Environment

**File**: `.env` (or `.env.example`)

```bash
VITE_API_URL=http://localhost:8000
VITE_WS_URL=ws://localhost:8000
VITE_APP_NAME=AncientCompute
VITE_ENVIRONMENT=development
```

### Production Environment

```bash
VITE_API_URL=https://api.ancient-compute.com
VITE_WS_URL=wss://api.ancient-compute.com
VITE_APP_NAME=AncientCompute
VITE_ENVIRONMENT=production
```

**Note**: `VITE_` prefix makes variables available in client-side code

---

## Performance Considerations

### Code Splitting

- Each route loads its own JavaScript bundle
- SvelteKit automatically code-splits at route boundaries
- Monaco editor loaded on-demand for playground

### Bundle Size Targets

- Initial page load: < 200 KB (gzipped)
- Monaco editor (lazy): 500 KB (gzipped)
- D3.js library: 200 KB (gzipped)
- Three.js library: 250 KB (gzipped)

### Optimization Techniques

1. **Image Optimization**: WebP format with fallbacks
2. **Lazy Loading**: Routes and heavy libraries on-demand
3. **CSS Tree-shaking**: Unused styles removed
4. **Minification**: Terser for JavaScript

---

## Common Issues and Fixes

### Issue: "Cannot find module '@sveltejs/kit'"

**Cause**: Dependencies not installed

**Fix**:
```bash
pnpm install
```

### Issue: "VITE_API_URL is undefined"

**Cause**: Environment variables not loaded or wrong prefix

**Fix**:
```bash
# Ensure .env file exists with VITE_ prefix
VITE_API_URL=http://localhost:8000

# Access in code
const apiUrl = import.meta.env.VITE_API_URL
```

### Issue: "Cannot connect to backend on localhost:8000"

**Cause**: Backend not running or CORS not configured

**Fix**:
1. Start backend: `cd backend && uvicorn src.main:app --reload`
2. Check vite.config.ts proxy configuration
3. Verify backend CORS settings

---

## Integration with Backend

### API Communication Flow

```
User interacts → Component event → Store update
    ↓
API call (axios/fetch) → Backend /execute endpoint
    ↓
WebSocket response ← Backend executes code
    ↓
Update execution store → Component re-renders with results
```

### CORS Headers

**Frontend**: http://localhost:5173
**Backend**: Must allow this origin

**Configuration**:
```python
# backend/src/main.py
app.add_middleware(
    CORSMiddleware,
    allow_origins=["http://localhost:5173"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)
```

---

## References

- **SvelteKit**: https://kit.svelte.dev
- **Svelte**: https://svelte.dev
- **Vite**: https://vitejs.dev
- **TypeScript**: https://www.typescriptlang.org
- **D3.js**: https://d3js.org
- **Three.js**: https://threejs.org
- **Monaco Editor**: https://microsoft.github.io/monaco-editor/

---

**End of Frontend Requirements**
