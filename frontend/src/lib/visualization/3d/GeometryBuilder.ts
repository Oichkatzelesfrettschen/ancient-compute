/**
 * Geometry Builder: 3D Component Creation
 *
 * Builds optimized 3D geometry for:
 * - 248 digit wheels (8 columns × 31 wheels)
 * - 8 shafts
 * - 8 carry levers
 * - Frame and support structure
 * - Engagement indicators
 *
 * Geometry is shared where possible for performance
 */

import * as THREE from 'three';

/**
 * Geometry cache to reuse cylinders, boxes, etc.
 */
const GeometryCache = {
  cylinderGeometries: new Map<string, THREE.CylinderGeometry>(),
  boxGeometries: new Map<string, THREE.BoxGeometry>(),
  torusGeometries: new Map<string, THREE.TorusGeometry>()
};

/**
 * Get or create cylinder geometry
 */
function getCylinderGeometry(
  radiusTop: number,
  radiusBottom: number,
  height: number,
  segments: number = 32
): THREE.CylinderGeometry {
  const key = `cyl-${radiusTop}-${radiusBottom}-${height}-${segments}`;

  if (GeometryCache.cylinderGeometries.has(key)) {
    return GeometryCache.cylinderGeometries.get(key)!;
  }

  const geometry = new THREE.CylinderGeometry(radiusTop, radiusBottom, height, segments);
  geometry.castShadow = true;
  geometry.receiveShadow = true;
  GeometryCache.cylinderGeometries.set(key, geometry);

  return geometry;
}

/**
 * Get or create box geometry
 */
function getBoxGeometry(width: number, height: number, depth: number): THREE.BoxGeometry {
  const key = `box-${width}-${height}-${depth}`;

  if (GeometryCache.boxGeometries.has(key)) {
    return GeometryCache.boxGeometries.get(key)!;
  }

  const geometry = new THREE.BoxGeometry(width, height, depth);
  geometry.castShadow = true;
  geometry.receiveShadow = true;
  GeometryCache.boxGeometries.set(key, geometry);

  return geometry;
}

/**
 * Get or create torus geometry
 */
function getTorusGeometry(radius: number, tube: number, segments: number = 32): THREE.TorusGeometry {
  const key = `torus-${radius}-${tube}-${segments}`;

  if (GeometryCache.torusGeometries.has(key)) {
    return GeometryCache.torusGeometries.get(key)!;
  }

  const geometry = new THREE.TorusGeometry(radius, tube, segments, segments);
  geometry.castShadow = true;
  geometry.receiveShadow = true;
  GeometryCache.torusGeometries.set(key, geometry);

  return geometry;
}

/**
 * Geometry builder
 */
export class GeometryBuilder {
  private digitWheels: THREE.Mesh[] = [];
  private shaftCylinders: THREE.Mesh[] = [];
  private carryLevers: THREE.Mesh[] = [];
  private engagementIndicators: THREE.Mesh[] = [];
  private frameElements: THREE.Group;

  constructor() {
    this.frameElements = new THREE.Group();
    this.frameElements.name = 'frame';
  }

  /**
   * Build all digit wheels
   */
  buildDigitWheels(materialFactory: any): THREE.Group {
    const group = new THREE.Group();
    group.name = 'digit-wheels';

    const columnSpacing = 1.5; // Distance between columns
    const wheelSpacing = 0.15; // Distance between wheels in column
    const wheelRadius = 0.35; // Radius of digit wheel
    const wheelDepth = 0.1; // Thickness of wheel

    // 8 columns
    for (let col = 0; col < 8; col++) {
      const columnGroup = new THREE.Group();
      columnGroup.name = `column-${col}`;

      const columnX = -5.25 + col * columnSpacing; // Spread across X axis

      // 31 wheels per column (decimal places 0-30)
      for (let wheel = 0; wheel < 31; wheel++) {
        const wheelGeometry = getCylinderGeometry(wheelRadius, wheelRadius, wheelDepth, 32);
        const material = materialFactory.createColumnMaterial(col);

        const wheelMesh = new THREE.Mesh(wheelGeometry, material);
        wheelMesh.castShadow = true;
        wheelMesh.receiveShadow = true;

        // Position wheel
        const wheelZ = -4.5 + wheel * wheelSpacing;
        wheelMesh.position.set(columnX, 0, wheelZ);

        // Add wheel number label (0-9 indicator every 3 wheels for visibility)
        if (wheel % 3 === 0) {
          const labelText = (wheel % 10).toString();
          wheelMesh.userData.label = labelText;
        }

        columnGroup.add(wheelMesh);
        this.digitWheels.push(wheelMesh);
      }

      group.add(columnGroup);
    }

    return group;
  }

  /**
   * Build all shafts
   */
  buildShafts(materialFactory: any): THREE.Group {
    const group = new THREE.Group();
    group.name = 'shafts';

    const shaftRadius = 0.2;
    const shaftLength = 12; // Length of shaft
    const shaftPositionY = 0.6; // Height above digit wheels

    // 8 shafts (one per column)
    for (let shaft = 0; shaft < 8; shaft++) {
      const shaftGeometry = getCylinderGeometry(shaftRadius, shaftRadius, shaftLength, 32);
      const material = materialFactory.createShaftMaterial(shaft);

      const shaftMesh = new THREE.Mesh(shaftGeometry, material);
      shaftMesh.castShadow = true;
      shaftMesh.receiveShadow = true;

      // Position shaft (rotates around Z axis, so oriented along Z)
      const shaftX = -5.25 + shaft * 1.5;
      shaftMesh.position.set(shaftX, shaftPositionY, 0);

      // Rotate to align along Z axis (default cylinder is Y-aligned)
      shaftMesh.rotation.x = Math.PI / 2;

      shaftMesh.userData.shaftIndex = shaft;
      group.add(shaftMesh);
      this.shaftCylinders.push(shaftMesh);
    }

    return group;
  }

  /**
   * Build all carry levers
   */
  buildCarryLevers(materialFactory: any): THREE.Group {
    const group = new THREE.Group();
    group.name = 'carry-levers';

    const leverLength = 1.2;
    const leverHeight = 0.15;
    const leverDepth = 0.1;
    const leverPositionY = 1.2; // Above shafts
    const leverSpacing = 1.5;

    // 8 carry levers
    for (let lever = 0; lever < 8; lever++) {
      const leverGroup = new THREE.Group();
      leverGroup.name = `carry-lever-${lever}`;

      const leverX = -5.25 + lever * leverSpacing;

      // Main lever arm (box)
      const leverGeometry = getBoxGeometry(leverLength, leverHeight, leverDepth);
      const leverMaterial = materialFactory.createCarryLeverMaterial(lever);

      const leverMesh = new THREE.Mesh(leverGeometry, leverMaterial);
      leverMesh.castShadow = true;
      leverMesh.receiveShadow = true;
      leverMesh.position.set(leverX, leverPositionY, 0);

      // Engagement point (pivot axis)
      leverMesh.userData.pivotPoint = new THREE.Vector3(0, 0, 0);

      leverGroup.add(leverMesh);

      // Engagement indicator (highlight geometry - scales/glows when engaged)
      const indicatorGeometry = getBoxGeometry(leverLength * 1.1, leverHeight * 1.2, leverDepth * 1.1);
      const indicatorMaterial = materialFactory.createCarryHighlightMaterial(lever);

      const indicatorMesh = new THREE.Mesh(indicatorGeometry, indicatorMaterial);
      indicatorMesh.position.copy(leverMesh.position);
      indicatorMesh.visible = false; // Hidden until engagement

      leverGroup.add(indicatorMesh);
      this.engagementIndicators.push(indicatorMesh);

      group.add(leverGroup);
      this.carryLevers.push(leverMesh);
    }

    return group;
  }

  /**
   * Build frame and support structure
   */
  buildFrame(materialFactory: any): THREE.Group {
    const frameGroup = new THREE.Group();
    frameGroup.name = 'frame';

    const frameMaterial = materialFactory.createFrameMaterial();

    // Base plate
    const baseGeometry = getBoxGeometry(12, 0.5, 10);
    const baseMesh = new THREE.Mesh(baseGeometry, frameMaterial);
    baseMesh.position.y = -1;
    baseMesh.castShadow = true;
    baseMesh.receiveShadow = true;
    frameGroup.add(baseMesh);

    // Support columns (4 corners)
    const columnPositions = [
      [-5.5, 0.5, -5],
      [5.5, 0.5, -5],
      [-5.5, 0.5, 5],
      [5.5, 0.5, 5]
    ];

    const supportGeometry = getBoxGeometry(0.3, 2, 0.3);

    for (const pos of columnPositions) {
      const supportMesh = new THREE.Mesh(supportGeometry, frameMaterial);
      supportMesh.position.set(pos[0], pos[1], pos[2]);
      supportMesh.castShadow = true;
      supportMesh.receiveShadow = true;
      frameGroup.add(supportMesh);
    }

    // Back plate
    const backPlateGeometry = getBoxGeometry(12, 2, 0.3);
    const backPlateMesh = new THREE.Mesh(backPlateGeometry, frameMaterial);
    backPlateMesh.position.set(0, 0.5, -5.2);
    backPlateMesh.castShadow = true;
    backPlateMesh.receiveShadow = true;
    frameGroup.add(backPlateMesh);

    return frameGroup;
  }

  /**
   * Build complete Difference Engine scene
   */
  buildCompleteEngine(materialFactory: any): THREE.Group {
    const engineGroup = new THREE.Group();
    engineGroup.name = 'difference-engine';

    // Add all components in order
    engineGroup.add(this.buildFrame(materialFactory));
    engineGroup.add(this.buildDigitWheels(materialFactory));
    engineGroup.add(this.buildShafts(materialFactory));
    engineGroup.add(this.buildCarryLevers(materialFactory));

    return engineGroup;
  }

  /**
   * Get digit wheel mesh
   */
  getDigitWheel(columnIndex: number, wheelIndex: number): THREE.Mesh | null {
    const index = columnIndex * 31 + wheelIndex;
    return index < this.digitWheels.length ? this.digitWheels[index] : null;
  }

  /**
   * Get shaft mesh
   */
  getShaft(shaftIndex: number): THREE.Mesh | null {
    return shaftIndex < this.shaftCylinders.length ? this.shaftCylinders[shaftIndex] : null;
  }

  /**
   * Get carry lever mesh
   */
  getCarryLever(leverIndex: number): THREE.Mesh | null {
    return leverIndex < this.carryLevers.length ? this.carryLevers[leverIndex] : null;
  }

  /**
   * Get engagement indicator mesh
   */
  getEngagementIndicator(leverIndex: number): THREE.Mesh | null {
    return leverIndex < this.engagementIndicators.length ? this.engagementIndicators[leverIndex] : null;
  }

  /**
   * Get all digit wheels
   */
  getAllDigitWheels(): THREE.Mesh[] {
    return [...this.digitWheels];
  }

  /**
   * Get all shafts
   */
  getAllShafts(): THREE.Mesh[] {
    return [...this.shaftCylinders];
  }

  /**
   * Get all carry levers
   */
  getAllCarryLevers(): THREE.Mesh[] {
    return [...this.carryLevers];
  }

  /**
   * Dispose all geometries
   */
  static disposeCache(): void {
    for (const geo of GeometryCache.cylinderGeometries.values()) {
      geo.dispose();
    }
    for (const geo of GeometryCache.boxGeometries.values()) {
      geo.dispose();
    }
    for (const geo of GeometryCache.torusGeometries.values()) {
      geo.dispose();
    }

    GeometryCache.cylinderGeometries.clear();
    GeometryCache.boxGeometries.clear();
    GeometryCache.torusGeometries.clear();
  }

  /**
   * Get geometry statistics
   */
  static getStats(): {
    cylinderCount: number;
    boxCount: number;
    torusCount: number;
    totalGeometries: number;
  } {
    return {
      cylinderCount: GeometryCache.cylinderGeometries.size,
      boxCount: GeometryCache.boxGeometries.size,
      torusCount: GeometryCache.torusGeometries.size,
      totalGeometries:
        GeometryCache.cylinderGeometries.size +
        GeometryCache.boxGeometries.size +
        GeometryCache.torusGeometries.size
    };
  }

  /**
   * Get component counts
   */
  getComponentCounts(): {
    digitWheels: number;
    shafts: number;
    carryLevers: number;
    engagementIndicators: number;
  } {
    return {
      digitWheels: this.digitWheels.length,
      shafts: this.shaftCylinders.length,
      carryLevers: this.carryLevers.length,
      engagementIndicators: this.engagementIndicators.length
    };
  }
}
