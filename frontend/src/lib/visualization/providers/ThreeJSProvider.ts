/**
 * ThreeJSProvider.ts
 * Three.js implementation for 3D Victorian-era Babbage Engine visualization
 */

import * as THREE from 'three';
import { OrbitControls } from 'three/examples/jsm/controls/OrbitControls';
import { EffectComposer } from 'three/examples/jsm/postprocessing/EffectComposer';
import { RenderPass } from 'three/examples/jsm/postprocessing/RenderPass';
import { UnrealBloomPass } from 'three/examples/jsm/postprocessing/UnrealBloomPass';
import { DRACOLoader } from 'three/examples/jsm/loaders/DRACOLoader';
import { GLTFLoader } from 'three/examples/jsm/loaders/GLTFLoader';
import {
  BaseVisualizationProvider,
  MachineState,
  VisualizationOptions,
  ColumnState,
  MillState
} from './VisualizationProvider';

export class ThreeJSProvider extends BaseVisualizationProvider {
  private scene!: THREE.Scene;
  private camera!: THREE.PerspectiveCamera;
  private renderer!: THREE.WebGLRenderer;
  private controls!: OrbitControls;
  private composer?: EffectComposer;
  private clock: THREE.Clock;

  // Machine components
  private columnMeshes: Map<string, THREE.Group> = new Map();
  private millGroup?: THREE.Group;
  private storeGroup?: THREE.Group;
  private barrelGroups: Map<string, THREE.Group> = new Map();
  private gears: THREE.Mesh[] = [];

  // Animation state
  private animationMixer?: THREE.AnimationMixer;
  private activeAnimations: THREE.AnimationAction[] = [];
  private frameCount = 0;
  private lastFrameTime = 0;

  // Materials
  private materials: {
    brass: THREE.MeshStandardMaterial;
    wood: THREE.MeshStandardMaterial;
    leather: THREE.MeshStandardMaterial;
    steel: THREE.MeshStandardMaterial;
    glass: THREE.MeshPhysicalMaterial;
  };

  // LOD management
  private lodGroups: THREE.LOD[] = [];

  constructor() {
    super();
    this.clock = new THREE.Clock();
    this.materials = this.createMaterials();
  }

  async initialize(canvas: HTMLCanvasElement, options: VisualizationOptions): Promise<void> {
    this.canvas = canvas;
    this.options = { ...this.options, ...options };

    try {
      await this.setupRenderer();
      this.setupScene();
      this.setupCamera();
      this.setupLighting();
      this.setupControls();
      await this.loadAssets();
      this.createMachine();

      if (this.options.enableShadows && this.options.quality !== 'low') {
        this.setupPostProcessing();
      }

      this.startRenderLoop();
    } catch (error) {
      this.notifyError(new Error(`Three.js initialization failed: ${error}`));
      throw error;
    }
  }

  private createMaterials(): typeof this.materials {
    // Victorian-era materials
    const brass = new THREE.MeshStandardMaterial({
      color: 0xb5a642,
      metalness: 0.9,
      roughness: 0.3,
      envMapIntensity: 0.8
    });

    const wood = new THREE.MeshStandardMaterial({
      color: 0x8b4513,
      metalness: 0.0,
      roughness: 0.8,
      map: this.createWoodTexture()
    });

    const leather = new THREE.MeshStandardMaterial({
      color: 0x4b2f20,
      metalness: 0.0,
      roughness: 0.9
    });

    const steel = new THREE.MeshStandardMaterial({
      color: 0x444444,
      metalness: 0.95,
      roughness: 0.2
    });

    const glass = new THREE.MeshPhysicalMaterial({
      color: 0xffffff,
      metalness: 0.0,
      roughness: 0.0,
      transparency: 0.95,
      thickness: 0.5,
      clearcoat: 1.0,
      clearcoatRoughness: 0.0
    });

    return { brass, wood, leather, steel, glass };
  }

  private createWoodTexture(): THREE.Texture {
    const canvas = document.createElement('canvas');
    canvas.width = 512;
    canvas.height = 512;
    const ctx = canvas.getContext('2d')!;

    // Generate wood grain pattern
    const gradient = ctx.createLinearGradient(0, 0, 512, 0);
    gradient.addColorStop(0, '#8b4513');
    gradient.addColorStop(0.5, '#a0522d');
    gradient.addColorStop(1, '#8b4513');
    ctx.fillStyle = gradient;
    ctx.fillRect(0, 0, 512, 512);

    // Add grain lines
    ctx.strokeStyle = '#654321';
    ctx.lineWidth = 1;
    for (let i = 0; i < 512; i += 8) {
      ctx.beginPath();
      ctx.moveTo(0, i + Math.random() * 4);
      ctx.lineTo(512, i + Math.random() * 4);
      ctx.stroke();
    }

    const texture = new THREE.CanvasTexture(canvas);
    texture.wrapS = texture.wrapT = THREE.RepeatWrapping;
    texture.repeat.set(2, 2);
    return texture;
  }

  private async setupRenderer(): Promise<void> {
    this.renderer = new THREE.WebGLRenderer({
      canvas: this.canvas!,
      antialias: this.options.antialias && this.options.quality !== 'low',
      powerPreference: this.options.quality === 'high' ? 'high-performance' : 'default'
    });

    this.renderer.setSize(this.canvas!.width, this.canvas!.height);
    this.renderer.setPixelRatio(Math.min(window.devicePixelRatio, this.options.quality === 'high' ? 2 : 1));
    this.renderer.shadowMap.enabled = this.options.enableShadows;
    this.renderer.shadowMap.type = this.options.quality === 'high'
      ? THREE.PCFSoftShadowMap
      : THREE.PCFShadowMap;
    this.renderer.outputEncoding = THREE.sRGBEncoding;
    this.renderer.toneMapping = THREE.ACESFilmicToneMapping;
  }

  private setupScene(): void {
    this.scene = new THREE.Scene();
    this.scene.background = new THREE.Color(0x1a1a1a);
    this.scene.fog = new THREE.Fog(0x1a1a1a, 10, 50);

    // Add environment map for reflections
    const pmremGenerator = new THREE.PMREMGenerator(this.renderer);
    const envTexture = this.createEnvironmentMap();
    this.scene.environment = pmremGenerator.fromEquirectangular(envTexture).texture;
    envTexture.dispose();
    pmremGenerator.dispose();
  }

  private createEnvironmentMap(): THREE.DataTexture {
    const size = 256;
    const data = new Uint8Array(size * size * 4);

    for (let i = 0; i < size * size; i++) {
      const stride = i * 4;
      const intensity = Math.random() * 20 + 10;
      data[stride] = intensity;
      data[stride + 1] = intensity;
      data[stride + 2] = intensity;
      data[stride + 3] = 255;
    }

    const texture = new THREE.DataTexture(data, size, size, THREE.RGBAFormat);
    texture.needsUpdate = true;
    return texture;
  }

  private setupCamera(): void {
    const aspect = this.canvas!.width / this.canvas!.height;
    this.camera = new THREE.PerspectiveCamera(50, aspect, 0.1, 100);
    this.camera.position.set(5, 3, 5);
    this.camera.lookAt(0, 0, 0);
  }

  private setupLighting(): void {
    // Victorian gas lamp ambience
    const ambientLight = new THREE.AmbientLight(0xffd4a3, 0.4);
    this.scene.add(ambientLight);

    // Main directional light (window light)
    const dirLight = new THREE.DirectionalLight(0xffffff, 0.6);
    dirLight.position.set(5, 8, 3);
    dirLight.castShadow = this.options.enableShadows;
    dirLight.shadow.mapSize.width = 2048;
    dirLight.shadow.mapSize.height = 2048;
    dirLight.shadow.camera.near = 0.5;
    dirLight.shadow.camera.far = 20;
    dirLight.shadow.camera.left = -10;
    dirLight.shadow.camera.right = 10;
    dirLight.shadow.camera.top = 10;
    dirLight.shadow.camera.bottom = -10;
    this.scene.add(dirLight);

    // Gas lamp point lights
    const lampPositions = [
      { x: -3, y: 3, z: 0 },
      { x: 3, y: 3, z: 0 },
      { x: 0, y: 3, z: -3 }
    ];

    lampPositions.forEach(pos => {
      const lampLight = new THREE.PointLight(0xffaa55, 0.5, 8);
      lampLight.position.set(pos.x, pos.y, pos.z);
      lampLight.castShadow = this.options.enableShadows && this.options.quality === 'high';
      this.scene.add(lampLight);

      // Lamp mesh
      const lampGeometry = new THREE.SphereGeometry(0.1);
      const lampMaterial = new THREE.MeshBasicMaterial({
        color: 0xffaa55,
        emissive: 0xffaa55
      });
      const lamp = new THREE.Mesh(lampGeometry, lampMaterial);
      lamp.position.copy(lampLight.position);
      this.scene.add(lamp);
    });
  }

  private setupControls(): void {
    this.controls = new OrbitControls(this.camera, this.canvas!);
    this.controls.enableDamping = true;
    this.controls.dampingFactor = 0.05;
    this.controls.minDistance = 2;
    this.controls.maxDistance = 20;
    this.controls.maxPolarAngle = Math.PI * 0.45;

    if (this.options.cameraMode === 'fixed') {
      this.controls.enabled = false;
    }
  }

  private async loadAssets(): Promise<void> {
    // In production, load actual GLTF/Draco models
    // For now, we'll use procedural geometry
    return Promise.resolve();
  }

  private createMachine(): void {
    // Create base platform
    const platformGeometry = new THREE.BoxGeometry(8, 0.5, 6);
    const platform = new THREE.Mesh(platformGeometry, this.materials.wood);
    platform.position.y = -0.25;
    platform.receiveShadow = true;
    this.scene.add(platform);

    // Create columns (digit wheels)
    this.createColumns();

    // Create mill (calculating unit)
    this.createMill();

    // Create store (memory)
    this.createStore();

    // Create barrels (program storage)
    this.createBarrels();

    // Create decorative gears
    this.createGears();
  }

  private createColumns(): void {
    const columnCount = 10;
    const spacing = 0.6;
    const startX = -(columnCount - 1) * spacing / 2;

    for (let i = 0; i < columnCount; i++) {
      const columnGroup = new THREE.Group();

      // Column structure
      const columnGeometry = new THREE.CylinderGeometry(0.15, 0.15, 2);
      const column = new THREE.Mesh(columnGeometry, this.materials.brass);
      column.castShadow = true;
      columnGroup.add(column);

      // Digit wheel
      const wheelGeometry = new THREE.CylinderGeometry(0.25, 0.25, 0.1);
      const wheel = new THREE.Mesh(wheelGeometry, this.materials.steel);
      wheel.position.y = 0.5;
      wheel.castShadow = true;
      columnGroup.add(wheel);

      // Add digits as LOD
      const digitLOD = new THREE.LOD();

      // High detail - actual numbers
      const highDetail = this.createDigitWheel(i);
      digitLOD.addLevel(highDetail, 0);

      // Low detail - simple cylinder
      const lowDetail = wheel.clone();
      digitLOD.addLevel(lowDetail, 10);

      columnGroup.add(digitLOD);
      this.lodGroups.push(digitLOD);

      columnGroup.position.set(startX + i * spacing, 1.5, 0);
      this.columnMeshes.set(`column_${i}`, columnGroup);
      this.scene.add(columnGroup);
    }
  }

  private createDigitWheel(index: number): THREE.Group {
    const group = new THREE.Group();
    const radius = 0.25;

    for (let digit = 0; digit < 10; digit++) {
      const angle = (digit / 10) * Math.PI * 2;
      const digitGeometry = new THREE.PlaneGeometry(0.08, 0.08);
      const canvas = document.createElement('canvas');
      canvas.width = canvas.height = 64;
      const ctx = canvas.getContext('2d')!;
      ctx.fillStyle = '#ffffff';
      ctx.font = '48px serif';
      ctx.textAlign = 'center';
      ctx.textBaseline = 'middle';
      ctx.fillText(digit.toString(), 32, 32);

      const texture = new THREE.CanvasTexture(canvas);
      const material = new THREE.MeshBasicMaterial({ map: texture, transparent: true });
      const digitMesh = new THREE.Mesh(digitGeometry, material);

      digitMesh.position.x = Math.sin(angle) * radius;
      digitMesh.position.z = Math.cos(angle) * radius;
      digitMesh.position.y = 0.5;
      digitMesh.lookAt(new THREE.Vector3(0, digitMesh.position.y, 0));

      group.add(digitMesh);
    }

    return group;
  }

  private createMill(): void {
    this.millGroup = new THREE.Group();

    // Mill housing
    const housingGeometry = new THREE.BoxGeometry(1.5, 1, 1);
    const housing = new THREE.Mesh(housingGeometry, this.materials.brass);
    housing.castShadow = true;
    this.millGroup.add(housing);

    // Operation gears
    for (let i = 0; i < 4; i++) {
      const gearRadius = 0.2 - i * 0.03;
      const gear = this.createGear(gearRadius, 12 + i * 4);
      gear.position.set(0.3 * Math.cos(i * Math.PI / 2), 0, 0.3 * Math.sin(i * Math.PI / 2));
      this.millGroup.add(gear);
      this.gears.push(gear);
    }

    this.millGroup.position.set(0, 1, 2);
    this.scene.add(this.millGroup);
  }

  private createStore(): void {
    this.storeGroup = new THREE.Group();

    // Storage rack
    const rackGeometry = new THREE.BoxGeometry(3, 2, 0.5);
    const rack = new THREE.Mesh(rackGeometry, this.materials.wood);
    rack.castShadow = true;
    this.storeGroup.add(rack);

    // Storage wheels
    for (let row = 0; row < 4; row++) {
      for (let col = 0; col < 8; col++) {
        const wheelGeometry = new THREE.CylinderGeometry(0.1, 0.1, 0.05);
        const wheel = new THREE.Mesh(wheelGeometry, this.materials.steel);
        wheel.rotation.z = Math.PI / 2;
        wheel.position.set(
          -1.3 + col * 0.35,
          -0.7 + row * 0.35,
          0.3
        );
        wheel.castShadow = true;
        this.storeGroup.add(wheel);
      }
    }

    this.storeGroup.position.set(0, 1.5, -2);
    this.scene.add(this.storeGroup);
  }

  private createBarrels(): void {
    // Program barrels (punch card readers)
    for (let i = 0; i < 3; i++) {
      const barrelGroup = new THREE.Group();

      const barrelGeometry = new THREE.CylinderGeometry(0.3, 0.3, 0.8);
      const barrel = new THREE.Mesh(barrelGeometry, this.materials.wood);
      barrel.rotation.x = Math.PI / 2;
      barrel.castShadow = true;
      barrelGroup.add(barrel);

      // Card slot
      const slotGeometry = new THREE.BoxGeometry(0.05, 0.4, 0.2);
      const slot = new THREE.Mesh(slotGeometry, this.materials.steel);
      slot.position.z = 0.35;
      barrelGroup.add(slot);

      barrelGroup.position.set(-3 + i * 1.2, 1, 0);
      this.barrelGroups.set(`barrel_${i}`, barrelGroup);
      this.scene.add(barrelGroup);
    }
  }

  private createGears(): void {
    // Decorative gears
    const gearPositions = [
      { x: -2, y: 2.5, z: 0, r: 0.3, teeth: 20 },
      { x: 2, y: 2.5, z: 0, r: 0.25, teeth: 16 },
      { x: 0, y: 3, z: -1, r: 0.35, teeth: 24 },
      { x: 0, y: 0.5, z: 1, r: 0.2, teeth: 12 }
    ];

    gearPositions.forEach(pos => {
      const gear = this.createGear(pos.r, pos.teeth);
      gear.position.set(pos.x, pos.y, pos.z);
      this.scene.add(gear);
      this.gears.push(gear);
    });
  }

  private createGear(radius: number, teeth: number): THREE.Mesh {
    const shape = new THREE.Shape();
    const toothHeight = radius * 0.2;

    for (let i = 0; i < teeth; i++) {
      const angle1 = (i / teeth) * Math.PI * 2;
      const angle2 = ((i + 0.5) / teeth) * Math.PI * 2;

      if (i === 0) {
        shape.moveTo(
          Math.cos(angle1) * radius,
          Math.sin(angle1) * radius
        );
      }

      shape.lineTo(
        Math.cos(angle1) * (radius + toothHeight),
        Math.sin(angle1) * (radius + toothHeight)
      );
      shape.lineTo(
        Math.cos(angle2) * (radius + toothHeight),
        Math.sin(angle2) * (radius + toothHeight)
      );
      shape.lineTo(
        Math.cos(angle2) * radius,
        Math.sin(angle2) * radius
      );
    }

    const extrudeSettings = {
      depth: radius * 0.3,
      bevelEnabled: true,
      bevelThickness: 0.01,
      bevelSize: 0.01,
      bevelSegments: 1
    };

    const geometry = new THREE.ExtrudeGeometry(shape, extrudeSettings);
    const gear = new THREE.Mesh(geometry, this.materials.brass);
    gear.castShadow = true;
    gear.receiveShadow = true;

    return gear;
  }

  private setupPostProcessing(): void {
    this.composer = new EffectComposer(this.renderer);
    this.composer.addPass(new RenderPass(this.scene, this.camera));

    if (this.options.quality === 'high') {
      const bloomPass = new UnrealBloomPass(
        new THREE.Vector2(this.canvas!.width, this.canvas!.height),
        0.5,  // strength
        0.4,  // radius
        0.85  // threshold
      );
      this.composer.addPass(bloomPass);
    }
  }

  private startRenderLoop(): void {
    const animate = () => {
      requestAnimationFrame(animate);

      const deltaTime = this.clock.getDelta();
      const elapsedTime = this.clock.getElapsedTime();

      // Update FPS metrics
      this.updatePerformanceMetrics(deltaTime);

      // Animate gears
      this.animateGears(elapsedTime);

      // Update controls
      if (this.controls.enabled) {
        this.controls.update();
      }

      // Update LODs
      this.lodGroups.forEach(lod => {
        lod.update(this.camera);
      });

      // Render
      if (this.composer && this.options.quality === 'high') {
        this.composer.render();
      } else {
        this.renderer.render(this.scene, this.camera);
      }
    };

    animate();
  }

  private animateGears(time: number): void {
    this.gears.forEach((gear, index) => {
      const speed = 0.5 + (index * 0.1);
      const direction = index % 2 === 0 ? 1 : -1;
      gear.rotation.z = time * speed * direction * this.options.animationSpeed;
    });
  }

  private updatePerformanceMetrics(deltaTime: number): void {
    this.frameCount++;
    const currentTime = performance.now();

    if (currentTime - this.lastFrameTime >= 1000) {
      this.performanceMetrics.fps = this.frameCount;
      this.performanceMetrics.frameTime = deltaTime * 1000;
      this.performanceMetrics.drawCalls = this.renderer.info.render.calls;
      this.performanceMetrics.triangles = this.renderer.info.render.triangles;
      this.performanceMetrics.memoryUsage = (performance as any).memory?.usedJSHeapSize || 0;

      this.frameCount = 0;
      this.lastFrameTime = currentTime;
    }
  }

  render(state: MachineState): void {
    this.lastState = state;

    // Update column displays
    state.columns.forEach((column, index) => {
      const columnGroup = this.columnMeshes.get(`column_${index}`);
      if (columnGroup) {
        const wheel = columnGroup.children[1];
        if (wheel) {
          // Animate rotation to show digit
          const targetRotation = (column.value / 10) * Math.PI * 2;
          wheel.rotation.y = THREE.MathUtils.lerp(
            wheel.rotation.y,
            targetRotation,
            0.1
          );
        }
      }
    });

    // Update mill animation
    if (this.millGroup && state.mill.operation !== 'idle') {
      const intensity = state.mill.progress;
      this.millGroup.children.forEach((child, index) => {
        if (child instanceof THREE.Mesh && index > 0) {
          child.rotation.z += 0.1 * intensity * this.options.animationSpeed;
        }
      });
    }

    // Update store highlights
    if (this.storeGroup) {
      state.store.activeIndices.forEach(index => {
        if (index < this.storeGroup.children.length - 1) {
          const wheel = this.storeGroup.children[index + 1];
          if (wheel instanceof THREE.Mesh) {
            wheel.material = new THREE.MeshBasicMaterial({
              color: 0xff6600,
              emissive: 0xff6600
            });
          }
        }
      });
    }

    this.notifyStateChange(state);
  }

  setQuality(level: 'low' | 'medium' | 'high'): void {
    this.options.quality = level;

    // Update shadow quality
    this.renderer.shadowMap.enabled = level !== 'low';
    this.renderer.shadowMap.type = level === 'high'
      ? THREE.PCFSoftShadowMap
      : THREE.PCFShadowMap;

    // Update pixel ratio
    this.renderer.setPixelRatio(Math.min(
      window.devicePixelRatio,
      level === 'high' ? 2 : 1
    ));

    // Update antialias
    if (level === 'low' && this.options.antialias) {
      // Recreate renderer without antialias
      this.setupRenderer();
    }

    // Update post-processing
    if (level === 'high' && !this.composer) {
      this.setupPostProcessing();
    } else if (level !== 'high' && this.composer) {
      this.composer = undefined;
    }
  }

  resize(width: number, height: number): void {
    this.camera.aspect = width / height;
    this.camera.updateProjectionMatrix();
    this.renderer.setSize(width, height);

    if (this.composer) {
      this.composer.setSize(width, height);
    }
  }

  setCamera(position: { x: number; y: number; z: number }): void {
    this.camera.position.set(position.x, position.y, position.z);
    this.camera.lookAt(0, 0, 0);
  }

  resetCamera(): void {
    this.camera.position.set(5, 3, 5);
    this.camera.lookAt(0, 0, 0);
    this.controls.reset();
  }

  async captureScreenshot(): Promise<Blob> {
    return new Promise((resolve) => {
      this.renderer.render(this.scene, this.camera);
      this.canvas!.toBlob((blob) => {
        resolve(blob || new Blob());
      });
    });
  }

  dispose(): void {
    // Clean up Three.js resources
    this.scene.traverse((child) => {
      if (child instanceof THREE.Mesh) {
        child.geometry.dispose();
        if (Array.isArray(child.material)) {
          child.material.forEach(m => m.dispose());
        } else {
          child.material.dispose();
        }
      }
    });

    this.renderer.dispose();
    this.controls.dispose();

    if (this.composer) {
      this.composer.dispose();
    }

    // Clear collections
    this.columnMeshes.clear();
    this.barrelGroups.clear();
    this.gears = [];
    this.lodGroups = [];

    // Clear callbacks
    this.stateCallbacks.clear();
    this.errorCallbacks.clear();
  }
}