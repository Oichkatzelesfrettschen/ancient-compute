/**
 * Canvas2DProvider.ts
 * Canvas2D implementation for 2D schematic visualization (fallback)
 */

import {
  BaseVisualizationProvider,
  MachineState,
  VisualizationOptions,
  ColumnState,
  MillState
} from './VisualizationProvider';

interface AnimatedValue {
  current: number;
  target: number;
  velocity: number;
}

export class Canvas2DProvider extends BaseVisualizationProvider {
  private ctx: CanvasRenderingContext2D | null = null;
  private animationId: number | null = null;
  private lastTime: number = 0;

  // Layout constants
  private readonly PADDING = 40;
  private readonly COLUMN_WIDTH = 40;
  private readonly COLUMN_HEIGHT = 120;
  private readonly COLUMN_SPACING = 50;
  private readonly MILL_SIZE = 100;
  private readonly STORE_CELL_SIZE = 20;

  // Color scheme
  private colors = {
    background: '#1a1a1a',
    grid: '#2a2a2a',
    text: '#ffffff',
    primary: '#b5a642',
    secondary: '#8b4513',
    accent: '#ff6600',
    active: '#00ff00',
    inactive: '#666666',
    error: '#ff0000'
  };

  // Animation state
  private animations: Map<string, AnimatedValue> = new Map();
  private particleEffects: Particle[] = [];

  // Performance tracking
  private frameCount = 0;
  private lastFpsUpdate = 0;

  // Interaction state
  private hoveredComponent: string | null = null;
  private mousePosition = { x: 0, y: 0 };

  async initialize(canvas: HTMLCanvasElement, options: VisualizationOptions): Promise<void> {
    this.canvas = canvas;
    this.options = { ...this.options, ...options };

    const ctx = canvas.getContext('2d', {
      alpha: false,
      desynchronized: true
    });

    if (!ctx) {
      throw new Error('Canvas2D context not available');
    }

    this.ctx = ctx;
    this.setupCanvas();
    this.setupEventListeners();
    this.startAnimationLoop();
  }

  private setupCanvas(): void {
    if (!this.canvas || !this.ctx) return;

    // Set canvas size
    const dpr = window.devicePixelRatio || 1;
    const rect = this.canvas.getBoundingClientRect();

    this.canvas.width = rect.width * dpr;
    this.canvas.height = rect.height * dpr;

    this.ctx.scale(dpr, dpr);

    // Set rendering hints
    this.ctx.imageSmoothingEnabled = true;
    this.ctx.imageSmoothingQuality = this.options.quality === 'high' ? 'high' : 'medium';
  }

  private setupEventListeners(): void {
    if (!this.canvas) return;

    this.canvas.addEventListener('mousemove', (e) => {
      const rect = this.canvas!.getBoundingClientRect();
      this.mousePosition.x = e.clientX - rect.left;
      this.mousePosition.y = e.clientY - rect.top;
      this.updateHoveredComponent();
    });

    this.canvas.addEventListener('click', (e) => {
      if (this.hoveredComponent) {
        this.handleComponentClick(this.hoveredComponent);
      }
    });
  }

  private updateHoveredComponent(): void {
    // Simple hit detection for components
    // In production, implement proper hit testing
    this.hoveredComponent = null;
  }

  private handleComponentClick(component: string): void {
    // Create particle effect at component
    this.createParticleEffect(this.mousePosition.x, this.mousePosition.y);
  }

  private createParticleEffect(x: number, y: number): void {
    for (let i = 0; i < 10; i++) {
      this.particleEffects.push(new Particle(x, y));
    }
  }

  private startAnimationLoop(): void {
    const animate = (timestamp: number) => {
      const deltaTime = timestamp - this.lastTime;
      this.lastTime = timestamp;

      this.update(deltaTime);
      this.draw();

      this.updatePerformanceMetrics(timestamp);

      this.animationId = requestAnimationFrame(animate);
    };

    this.animationId = requestAnimationFrame(animate);
  }

  private update(deltaTime: number): void {
    // Update animations
    this.animations.forEach((anim) => {
      const diff = anim.target - anim.current;
      anim.velocity = diff * 0.1;
      anim.current += anim.velocity;

      if (Math.abs(diff) < 0.01) {
        anim.current = anim.target;
      }
    });

    // Update particles
    this.particleEffects = this.particleEffects.filter(particle => {
      particle.update(deltaTime / 1000);
      return particle.life > 0;
    });
  }

  private draw(): void {
    if (!this.ctx || !this.canvas) return;

    const ctx = this.ctx;
    const width = this.canvas.width / (window.devicePixelRatio || 1);
    const height = this.canvas.height / (window.devicePixelRatio || 1);

    // Clear canvas
    ctx.fillStyle = this.colors.background;
    ctx.fillRect(0, 0, width, height);

    // Draw grid
    this.drawGrid(ctx, width, height);

    // Draw components
    if (this.lastState) {
      this.drawColumns(ctx, this.lastState.columns);
      this.drawMill(ctx, this.lastState.mill);
      this.drawStore(ctx, this.lastState.store);
      this.drawBarrels(ctx, this.lastState.barrels);
      this.drawStatusPanel(ctx, this.lastState);
    }

    // Draw particles
    this.drawParticles(ctx);

    // Draw performance overlay
    if (this.options.showAnnotations) {
      this.drawPerformanceOverlay(ctx);
    }
  }

  private drawGrid(ctx: CanvasRenderingContext2D, width: number, height: number): void {
    ctx.strokeStyle = this.colors.grid;
    ctx.lineWidth = 1;
    ctx.setLineDash([5, 5]);

    // Vertical lines
    for (let x = 0; x < width; x += 50) {
      ctx.beginPath();
      ctx.moveTo(x, 0);
      ctx.lineTo(x, height);
      ctx.stroke();
    }

    // Horizontal lines
    for (let y = 0; y < height; y += 50) {
      ctx.beginPath();
      ctx.moveTo(0, y);
      ctx.lineTo(width, y);
      ctx.stroke();
    }

    ctx.setLineDash([]);
  }

  private drawColumns(ctx: CanvasRenderingContext2D, columns: ColumnState[]): void {
    const startX = this.PADDING;
    const startY = this.PADDING;

    columns.forEach((column, index) => {
      const x = startX + index * this.COLUMN_SPACING;
      const y = startY;

      // Get animated value
      const animKey = `column_${index}`;
      if (!this.animations.has(animKey)) {
        this.animations.set(animKey, {
          current: column.value,
          target: column.value,
          velocity: 0
        });
      }

      const anim = this.animations.get(animKey)!;
      anim.target = column.value;

      // Draw column background
      ctx.fillStyle = column.isActive ? this.colors.active : this.colors.inactive;
      ctx.fillRect(x, y, this.COLUMN_WIDTH, this.COLUMN_HEIGHT);

      // Draw column border
      ctx.strokeStyle = this.colors.primary;
      ctx.lineWidth = 2;
      ctx.strokeRect(x, y, this.COLUMN_WIDTH, this.COLUMN_HEIGHT);

      // Draw digit wheel
      const wheelY = y + this.COLUMN_HEIGHT / 2;
      ctx.save();
      ctx.translate(x + this.COLUMN_WIDTH / 2, wheelY);
      ctx.rotate((anim.current / 10) * Math.PI * 2);

      // Draw wheel
      ctx.beginPath();
      ctx.arc(0, 0, 15, 0, Math.PI * 2);
      ctx.fillStyle = this.colors.secondary;
      ctx.fill();
      ctx.strokeStyle = this.colors.primary;
      ctx.stroke();

      // Draw digit
      ctx.fillStyle = this.colors.text;
      ctx.font = '14px monospace';
      ctx.textAlign = 'center';
      ctx.textBaseline = 'middle';
      ctx.fillText(Math.floor(anim.current).toString(), 0, 0);

      ctx.restore();

      // Draw column label
      ctx.fillStyle = this.colors.text;
      ctx.font = '10px monospace';
      ctx.textAlign = 'center';
      ctx.fillText(`C${index}`, x + this.COLUMN_WIDTH / 2, y + this.COLUMN_HEIGHT + 15);
    });
  }

  private drawMill(ctx: CanvasRenderingContext2D, mill: MillState): void {
    const x = this.PADDING + 550;
    const y = this.PADDING;

    // Draw mill housing
    ctx.fillStyle = this.colors.secondary;
    ctx.fillRect(x, y, this.MILL_SIZE, this.MILL_SIZE);

    ctx.strokeStyle = this.colors.primary;
    ctx.lineWidth = 2;
    ctx.strokeRect(x, y, this.MILL_SIZE, this.MILL_SIZE);

    // Draw operation indicator
    ctx.fillStyle = mill.operation !== 'idle' ? this.colors.active : this.colors.inactive;
    ctx.beginPath();
    ctx.arc(x + this.MILL_SIZE / 2, y + this.MILL_SIZE / 2, 20, 0, Math.PI * 2);
    ctx.fill();

    // Draw operation symbol
    ctx.fillStyle = this.colors.text;
    ctx.font = 'bold 20px monospace';
    ctx.textAlign = 'center';
    ctx.textBaseline = 'middle';

    let symbol = '';
    switch (mill.operation) {
      case 'add': symbol = '+'; break;
      case 'subtract': symbol = '-'; break;
      case 'multiply': symbol = '×'; break;
      case 'divide': symbol = '÷'; break;
      default: symbol = '⚙'; break;
    }

    ctx.fillText(symbol, x + this.MILL_SIZE / 2, y + this.MILL_SIZE / 2);

    // Draw progress bar
    if (mill.operation !== 'idle') {
      const barY = y + this.MILL_SIZE + 10;
      ctx.fillStyle = this.colors.inactive;
      ctx.fillRect(x, barY, this.MILL_SIZE, 5);

      ctx.fillStyle = this.colors.accent;
      ctx.fillRect(x, barY, this.MILL_SIZE * mill.progress, 5);
    }

    // Draw operands
    ctx.fillStyle = this.colors.text;
    ctx.font = '12px monospace';
    ctx.textAlign = 'left';
    ctx.fillText(`A: ${mill.operandA}`, x, y - 10);
    ctx.fillText(`B: ${mill.operandB}`, x, y - 25);
    ctx.fillText(`R: ${mill.result}`, x + this.MILL_SIZE - 40, y - 10);
  }

  private drawStore(ctx: CanvasRenderingContext2D, store: any): void {
    const x = this.PADDING;
    const y = this.PADDING + 200;
    const cols = 16;
    const rows = Math.ceil(store.positions.length / cols);

    // Draw store grid
    for (let row = 0; row < rows; row++) {
      for (let col = 0; col < cols; col++) {
        const index = row * cols + col;
        if (index >= store.positions.length) break;

        const cellX = x + col * (this.STORE_CELL_SIZE + 2);
        const cellY = y + row * (this.STORE_CELL_SIZE + 2);

        // Highlight active cells
        const isActive = store.activeIndices.includes(index);
        ctx.fillStyle = isActive ? this.colors.active : this.colors.inactive;
        ctx.fillRect(cellX, cellY, this.STORE_CELL_SIZE, this.STORE_CELL_SIZE);

        // Draw value
        if (store.positions[index] !== 0) {
          ctx.fillStyle = this.colors.text;
          ctx.font = '8px monospace';
          ctx.textAlign = 'center';
          ctx.textBaseline = 'middle';
          ctx.fillText(
            store.positions[index].toString(16).toUpperCase(),
            cellX + this.STORE_CELL_SIZE / 2,
            cellY + this.STORE_CELL_SIZE / 2
          );
        }
      }
    }

    // Draw store label
    ctx.fillStyle = this.colors.text;
    ctx.font = '12px monospace';
    ctx.textAlign = 'left';
    ctx.fillText('STORE (Memory)', x, y - 10);
  }

  private drawBarrels(ctx: CanvasRenderingContext2D, barrels: any[]): void {
    const x = this.PADDING + 400;
    const y = this.PADDING + 200;

    barrels.forEach((barrel, index) => {
      const barrelX = x + index * 80;
      const barrelY = y;

      // Draw barrel cylinder
      ctx.fillStyle = barrel.isActive ? this.colors.active : this.colors.secondary;
      ctx.fillRect(barrelX, barrelY, 60, 80);

      ctx.strokeStyle = this.colors.primary;
      ctx.lineWidth = 2;
      ctx.strokeRect(barrelX, barrelY, 60, 80);

      // Draw program counter
      ctx.fillStyle = this.colors.text;
      ctx.font = '10px monospace';
      ctx.textAlign = 'center';
      ctx.fillText(`PC: ${barrel.programCounter}`, barrelX + 30, barrelY + 40);

      // Draw barrel label
      ctx.fillText(barrel.id, barrelX + 30, barrelY + 95);
    });

    // Draw barrels label
    ctx.fillStyle = this.colors.text;
    ctx.font = '12px monospace';
    ctx.textAlign = 'left';
    ctx.fillText('PROGRAM BARRELS', x, y - 10);
  }

  private drawStatusPanel(ctx: CanvasRenderingContext2D, state: MachineState): void {
    const x = this.PADDING;
    const y = this.PADDING + 350;

    // Draw panel background
    ctx.fillStyle = 'rgba(0, 0, 0, 0.5)';
    ctx.fillRect(x, y, 300, 100);

    ctx.strokeStyle = this.colors.primary;
    ctx.lineWidth = 1;
    ctx.strokeRect(x, y, 300, 100);

    // Draw status information
    ctx.fillStyle = this.colors.text;
    ctx.font = '12px monospace';
    ctx.textAlign = 'left';

    const lines = [
      `PC: ${state.programCounter}`,
      `ACC: ${state.accumulator}`,
      `Carry: ${state.carryFlag ? 'SET' : 'CLEAR'}`,
      `Status: ${state.runningFlag ? 'RUNNING' : 'HALTED'}`,
      `Cycles: ${state.cycles}`
    ];

    lines.forEach((line, index) => {
      ctx.fillText(line, x + 10, y + 20 + index * 15);
    });
  }

  private drawParticles(ctx: CanvasRenderingContext2D): void {
    this.particleEffects.forEach(particle => {
      ctx.save();
      ctx.globalAlpha = particle.life;
      ctx.fillStyle = this.colors.accent;
      ctx.beginPath();
      ctx.arc(particle.x, particle.y, particle.size, 0, Math.PI * 2);
      ctx.fill();
      ctx.restore();
    });
  }

  private drawPerformanceOverlay(ctx: CanvasRenderingContext2D): void {
    const metrics = this.getPerformanceMetrics();

    ctx.fillStyle = 'rgba(0, 0, 0, 0.7)';
    ctx.fillRect(10, 10, 150, 60);

    ctx.fillStyle = this.colors.text;
    ctx.font = '10px monospace';
    ctx.textAlign = 'left';

    ctx.fillText(`FPS: ${metrics.fps}`, 20, 25);
    ctx.fillText(`Frame Time: ${metrics.frameTime.toFixed(2)}ms`, 20, 40);
    ctx.fillText(`Particles: ${this.particleEffects.length}`, 20, 55);
  }

  private updatePerformanceMetrics(timestamp: number): void {
    this.frameCount++;

    if (timestamp - this.lastFpsUpdate >= 1000) {
      this.performanceMetrics.fps = this.frameCount;
      this.performanceMetrics.frameTime = 1000 / this.frameCount;
      this.frameCount = 0;
      this.lastFpsUpdate = timestamp;
    }
  }

  render(state: MachineState): void {
    this.lastState = state;
    this.notifyStateChange(state);
  }

  setQuality(level: 'low' | 'medium' | 'high'): void {
    this.options.quality = level;

    if (this.ctx) {
      this.ctx.imageSmoothingQuality = level === 'high' ? 'high' : 'medium';
    }

    // Adjust particle effects based on quality
    if (level === 'low') {
      this.particleEffects = [];
    }
  }

  resize(width: number, height: number): void {
    if (!this.canvas) return;

    const dpr = window.devicePixelRatio || 1;
    this.canvas.width = width * dpr;
    this.canvas.height = height * dpr;

    if (this.ctx) {
      this.ctx.scale(dpr, dpr);
    }
  }

  setCamera(position: { x: number; y: number; z: number }): void {
    // 2D view doesn't have camera controls
    // Could implement panning/zooming if needed
  }

  resetCamera(): void {
    // Reset any panning/zooming
  }

  async captureScreenshot(): Promise<Blob> {
    return new Promise((resolve) => {
      if (this.canvas) {
        this.canvas.toBlob((blob) => {
          resolve(blob || new Blob());
        });
      } else {
        resolve(new Blob());
      }
    });
  }

  dispose(): void {
    if (this.animationId) {
      cancelAnimationFrame(this.animationId);
      this.animationId = null;
    }

    this.animations.clear();
    this.particleEffects = [];
    this.stateCallbacks.clear();
    this.errorCallbacks.clear();

    if (this.canvas) {
      this.canvas.removeEventListener('mousemove', () => {});
      this.canvas.removeEventListener('click', () => {});
    }

    this.ctx = null;
    this.canvas = null;
  }
}

// Particle effect class
class Particle {
  x: number;
  y: number;
  vx: number;
  vy: number;
  size: number;
  life: number;

  constructor(x: number, y: number) {
    this.x = x;
    this.y = y;
    this.vx = (Math.random() - 0.5) * 100;
    this.vy = (Math.random() - 0.5) * 100;
    this.size = Math.random() * 3 + 1;
    this.life = 1;
  }

  update(deltaTime: number): void {
    this.x += this.vx * deltaTime;
    this.y += this.vy * deltaTime;
    this.life -= deltaTime * 2;

    if (this.life < 0) {
      this.life = 0;
    }
  }
}