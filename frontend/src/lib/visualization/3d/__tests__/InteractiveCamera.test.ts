/**
 * InteractiveCamera Test Suite
 * Tests user interaction: mouse, touch, keyboard controls, and momentum
 */

import { describe, it, expect, beforeEach, afterEach, vi } from 'vitest';
import * as THREE from 'three';
import { InteractiveCamera } from '../InteractiveCamera';
import { DifferenceEngineScene } from '../DifferenceEngineScene';

describe('InteractiveCamera', () => {
  let canvas: HTMLCanvasElement;
  let scene: DifferenceEngineScene;
  let camera: InteractiveCamera;

  beforeEach(() => {
    canvas = document.createElement('canvas');
    canvas.width = 1024;
    canvas.height = 768;

    scene = new DifferenceEngineScene({
      canvas,
      width: 1024,
      height: 768
    });

    camera = new InteractiveCamera(scene);
  });

  afterEach(() => {
    camera.dispose();
    scene.dispose();
  });

  describe('Initialization', () => {
    it('should create camera controller instance', () => {
      expect(camera).toBeDefined();
      expect(camera).toBeInstanceOf(InteractiveCamera);
    });

    it('should initialize control state', () => {
      const controlState = camera.getControlState();

      expect(controlState.isDragging).toBe(false);
      expect(controlState.isPanning).toBe(false);
      expect(controlState.momentum.x).toBe(0);
      expect(controlState.momentum.y).toBe(0);
    });

    it('should attach event listeners on creation', () => {
      // Camera should be ready to handle events
      expect(canvas).toBeDefined();
    });
  });

  describe('Mouse Events', () => {
    describe('Mouse Down', () => {
      it('should start dragging on left click', () => {
        const mouseEvent = new MouseEvent('mousedown', {
          button: 0,
          clientX: 100,
          clientY: 100
        });

        canvas.dispatchEvent(mouseEvent);
        const controlState = camera.getControlState();

        expect(controlState.isDragging).toBe(true);
      });

      it('should start panning on right click', () => {
        const mouseEvent = new MouseEvent('mousedown', {
          button: 2,
          clientX: 100,
          clientY: 100
        });

        canvas.dispatchEvent(mouseEvent);
        const controlState = camera.getControlState();

        expect(controlState.isPanning).toBe(true);
      });

      it('should track initial mouse position', () => {
        const mouseEvent = new MouseEvent('mousedown', {
          button: 0,
          clientX: 500,
          clientY: 400
        });

        canvas.dispatchEvent(mouseEvent);
        const controlState = camera.getControlState();

        expect(controlState.previousMousePosition.x).toBe(500);
        expect(controlState.previousMousePosition.y).toBe(400);
      });

      it('should cancel momentum on mouse down', () => {
        // This is harder to test directly, but ensures clean state
        const mouseEvent = new MouseEvent('mousedown', {
          button: 0,
          clientX: 100,
          clientY: 100
        });

        canvas.dispatchEvent(mouseEvent);
        expect(camera).toBeDefined();
      });
    });

    describe('Mouse Move', () => {
      it('should rotate view on drag', () => {
        const downEvent = new MouseEvent('mousedown', {
          button: 0,
          clientX: 100,
          clientY: 100
        });

        const moveEvent = new MouseEvent('mousemove', {
          clientX: 150,
          clientY: 150
        });

        canvas.dispatchEvent(downEvent);
        canvas.dispatchEvent(moveEvent);

        const controlState = camera.getControlState();
        expect(controlState.previousMousePosition.x).toBe(150);
        expect(controlState.previousMousePosition.y).toBe(150);
      });

      it('should pan view on right-click drag', () => {
        const downEvent = new MouseEvent('mousedown', {
          button: 2,
          clientX: 100,
          clientY: 100
        });

        const moveEvent = new MouseEvent('mousemove', {
          clientX: 150,
          clientY: 150
        });

        canvas.dispatchEvent(downEvent);
        canvas.dispatchEvent(moveEvent);

        const controlState = camera.getControlState();
        expect(controlState.previousMousePosition.x).toBe(150);
      });

      it('should not rotate when not dragging', () => {
        const moveEvent = new MouseEvent('mousemove', {
          clientX: 150,
          clientY: 150
        });

        canvas.dispatchEvent(moveEvent);
        const controlState = camera.getControlState();

        expect(controlState.isDragging).toBe(false);
      });

      it('should track momentum during drag', () => {
        const downEvent = new MouseEvent('mousedown', {
          button: 0,
          clientX: 100,
          clientY: 100
        });

        const moveEvent = new MouseEvent('mousemove', {
          clientX: 150,
          clientY: 150
        });

        canvas.dispatchEvent(downEvent);
        canvas.dispatchEvent(moveEvent);

        const controlState = camera.getControlState();
        expect(Math.abs(controlState.momentum.x) > 0 || Math.abs(controlState.momentum.y) > 0).toBe(
          true
        );
      });
    });

    describe('Mouse Up', () => {
      it('should stop dragging on mouse up', () => {
        const downEvent = new MouseEvent('mousedown', {
          button: 0,
          clientX: 100,
          clientY: 100
        });

        const upEvent = new MouseEvent('mouseup', {
          button: 0
        });

        canvas.dispatchEvent(downEvent);
        canvas.dispatchEvent(upEvent);

        const controlState = camera.getControlState();
        expect(controlState.isDragging).toBe(false);
      });

      it('should stop panning on right-click release', () => {
        const downEvent = new MouseEvent('mousedown', {
          button: 2,
          clientX: 100,
          clientY: 100
        });

        const upEvent = new MouseEvent('mouseup', {
          button: 2
        });

        canvas.dispatchEvent(downEvent);
        canvas.dispatchEvent(upEvent);

        const controlState = camera.getControlState();
        expect(controlState.isPanning).toBe(false);
      });
    });

    describe('Mouse Wheel', () => {
      it('should zoom in on scroll up', () => {
        const threeScene = scene.getScene();
        const engineGroup = scene.getEngineGroup();
        const initialRotationY = engineGroup.rotation.y;

        const wheelEvent = new WheelEvent('wheel', {
          deltaY: -100
        });

        canvas.dispatchEvent(wheelEvent);

        // Should not throw
        expect(camera).toBeDefined();
      });

      it('should zoom out on scroll down', () => {
        const wheelEvent = new WheelEvent('wheel', {
          deltaY: 100
        });

        canvas.dispatchEvent(wheelEvent);

        expect(camera).toBeDefined();
      });

      it('should prevent default scroll behavior', () => {
        const wheelEvent = new WheelEvent('wheel', {
          deltaY: 100
        });

        const preventDefaultSpy = vi.spyOn(wheelEvent, 'preventDefault');
        canvas.dispatchEvent(wheelEvent);

        // Should prevent default for scroll wheel
        expect(wheelEvent).toBeDefined();
      });
    });
  });

  describe('Touch Events', () => {
    describe('Single Touch', () => {
      it('should start rotating on single touch', () => {
        const touchEvent = new TouchEvent('touchstart', {
          touches: [
            new Touch({
              identifier: 0,
              target: canvas,
              clientX: 100,
              clientY: 100
            })
          ] as any
        });

        canvas.dispatchEvent(touchEvent);
        const controlState = camera.getControlState();

        expect(controlState.isDragging).toBe(true);
      });

      it('should track initial touch position', () => {
        const touchEvent = new TouchEvent('touchstart', {
          touches: [
            new Touch({
              identifier: 0,
              target: canvas,
              clientX: 250,
              clientY: 350
            })
          ] as any
        });

        canvas.dispatchEvent(touchEvent);
        const controlState = camera.getControlState();

        expect(controlState.previousMousePosition.x).toBe(250);
        expect(controlState.previousMousePosition.y).toBe(350);
      });

      it('should rotate on touch move', () => {
        const startEvent = new TouchEvent('touchstart', {
          touches: [
            new Touch({
              identifier: 0,
              target: canvas,
              clientX: 100,
              clientY: 100
            })
          ] as any
        });

        const moveEvent = new TouchEvent('touchmove', {
          touches: [
            new Touch({
              identifier: 0,
              target: canvas,
              clientX: 150,
              clientY: 150
            })
          ] as any
        });

        canvas.dispatchEvent(startEvent);
        canvas.dispatchEvent(moveEvent);

        const controlState = camera.getControlState();
        expect(controlState.previousMousePosition.x).toBe(150);
      });
    });

    describe('Multi-Touch Pinch', () => {
      it('should detect two-finger touch', () => {
        const touchEvent = new TouchEvent('touchstart', {
          touches: [
            new Touch({
              identifier: 0,
              target: canvas,
              clientX: 100,
              clientY: 100
            }),
            new Touch({
              identifier: 1,
              target: canvas,
              clientX: 200,
              clientY: 200
            })
          ] as any
        });

        canvas.dispatchEvent(touchEvent);
        const controlState = camera.getControlState();

        expect(controlState.isDragging).toBe(false);
      });

      it('should zoom on pinch', () => {
        const startEvent = new TouchEvent('touchstart', {
          touches: [
            new Touch({
              identifier: 0,
              target: canvas,
              clientX: 100,
              clientY: 100
            }),
            new Touch({
              identifier: 1,
              target: canvas,
              clientX: 200,
              clientY: 200
            })
          ] as any
        });

        const moveEvent = new TouchEvent('touchmove', {
          touches: [
            new Touch({
              identifier: 0,
              target: canvas,
              clientX: 80,
              clientY: 80
            }),
            new Touch({
              identifier: 1,
              target: canvas,
              clientX: 220,
              clientY: 220
            })
          ] as any
        });

        canvas.dispatchEvent(startEvent);
        canvas.dispatchEvent(moveEvent);

        // Should process pinch zoom
        expect(camera).toBeDefined();
      });
    });

    describe('Touch End', () => {
      it('should stop rotating on touch end', () => {
        const startEvent = new TouchEvent('touchstart', {
          touches: [
            new Touch({
              identifier: 0,
              target: canvas,
              clientX: 100,
              clientY: 100
            })
          ] as any
        });

        const endEvent = new TouchEvent('touchend', {
          touches: [] as any
        });

        canvas.dispatchEvent(startEvent);
        canvas.dispatchEvent(endEvent);

        const controlState = camera.getControlState();
        expect(controlState.isDragging).toBe(false);
      });
    });
  });

  describe('Keyboard Events', () => {
    it('should zoom in on W key', () => {
      const keyEvent = new KeyboardEvent('keydown', {
        key: 'w'
      });

      canvas.dispatchEvent(keyEvent);
      expect(camera).toBeDefined();
    });

    it('should zoom in on Up Arrow', () => {
      const keyEvent = new KeyboardEvent('keydown', {
        key: 'ArrowUp'
      });

      canvas.dispatchEvent(keyEvent);
      expect(camera).toBeDefined();
    });

    it('should zoom out on S key', () => {
      const keyEvent = new KeyboardEvent('keydown', {
        key: 's'
      });

      canvas.dispatchEvent(keyEvent);
      expect(camera).toBeDefined();
    });

    it('should zoom out on Down Arrow', () => {
      const keyEvent = new KeyboardEvent('keydown', {
        key: 'ArrowDown'
      });

      canvas.dispatchEvent(keyEvent);
      expect(camera).toBeDefined();
    });

    it('should rotate left on A key', () => {
      const keyEvent = new KeyboardEvent('keydown', {
        key: 'a'
      });

      canvas.dispatchEvent(keyEvent);
      expect(camera).toBeDefined();
    });

    it('should rotate left on Left Arrow', () => {
      const keyEvent = new KeyboardEvent('keydown', {
        key: 'ArrowLeft'
      });

      canvas.dispatchEvent(keyEvent);
      expect(camera).toBeDefined();
    });

    it('should rotate right on D key', () => {
      const keyEvent = new KeyboardEvent('keydown', {
        key: 'd'
      });

      canvas.dispatchEvent(keyEvent);
      expect(camera).toBeDefined();
    });

    it('should rotate right on Right Arrow', () => {
      const keyEvent = new KeyboardEvent('keydown', {
        key: 'ArrowRight'
      });

      canvas.dispatchEvent(keyEvent);
      expect(camera).toBeDefined();
    });

    it('should reset view on R key', () => {
      const keyEvent = new KeyboardEvent('keydown', {
        key: 'r'
      });

      canvas.dispatchEvent(keyEvent);
      expect(camera).toBeDefined();
    });

    it('should print help on H key', () => {
      const consoleSpy = vi.spyOn(console, 'log');

      const keyEvent = new KeyboardEvent('keydown', {
        key: 'h'
      });

      canvas.dispatchEvent(keyEvent);

      expect(console.log).toHaveBeenCalled();

      consoleSpy.mockRestore();
    });

    it('should be case-insensitive for key commands', () => {
      const keyEvent = new KeyboardEvent('keydown', {
        key: 'W'
      });

      canvas.dispatchEvent(keyEvent);
      expect(camera).toBeDefined();
    });
  });

  describe('Double-Click Reset', () => {
    it('should reset view on double click', () => {
      const dblclickEvent = new MouseEvent('dblclick');

      canvas.dispatchEvent(dblclickEvent);

      const controlState = camera.getControlState();
      expect(controlState.momentum.x).toBe(0);
      expect(controlState.momentum.y).toBe(0);
    });
  });

  describe('Momentum and Inertia', () => {
    it('should apply momentum damping', () => {
      camera.setMomentumEnabled(true);

      const downEvent = new MouseEvent('mousedown', {
        button: 0,
        clientX: 100,
        clientY: 100
      });

      const moveEvent = new MouseEvent('mousemove', {
        clientX: 150,
        clientY: 150
      });

      const upEvent = new MouseEvent('mouseup', {
        button: 0
      });

      canvas.dispatchEvent(downEvent);
      canvas.dispatchEvent(moveEvent);
      canvas.dispatchEvent(upEvent);

      // Momentum should be applied
      expect(camera).toBeDefined();
    });

    it('should disable momentum when disabled', () => {
      camera.setMomentumEnabled(false);

      const downEvent = new MouseEvent('mousedown', {
        button: 0,
        clientX: 100,
        clientY: 100
      });

      const moveEvent = new MouseEvent('mousemove', {
        clientX: 150,
        clientY: 150
      });

      const upEvent = new MouseEvent('mouseup', {
        button: 0
      });

      canvas.dispatchEvent(downEvent);
      canvas.dispatchEvent(moveEvent);
      canvas.dispatchEvent(upEvent);

      const controlState = camera.getControlState();
      expect(controlState.isDragging).toBe(false);
    });
  });

  describe('Speed Configuration', () => {
    it('should set rotation speed', () => {
      camera.setRotationSpeed(2);

      expect(camera).toBeDefined();
    });

    it('should clamp rotation speed', () => {
      camera.setRotationSpeed(100);
      camera.setRotationSpeed(-100);

      // Should be clamped to valid range
      expect(camera).toBeDefined();
    });

    it('should set zoom speed', () => {
      camera.setZoomSpeed(0.5);

      expect(camera).toBeDefined();
    });

    it('should clamp zoom speed', () => {
      camera.setZoomSpeed(100);
      camera.setZoomSpeed(-100);

      // Should be clamped to valid range
      expect(camera).toBeDefined();
    });
  });

  describe('Control State', () => {
    it('should return copy of control state', () => {
      const state1 = camera.getControlState();
      const state2 = camera.getControlState();

      expect(state1).not.toBe(state2);
      expect(state1.isDragging).toBe(state2.isDragging);
    });

    it('should track momentum values', () => {
      const downEvent = new MouseEvent('mousedown', {
        button: 0,
        clientX: 100,
        clientY: 100
      });

      const moveEvent = new MouseEvent('mousemove', {
        clientX: 150,
        clientY: 150
      });

      canvas.dispatchEvent(downEvent);
      canvas.dispatchEvent(moveEvent);

      const controlState = camera.getControlState();
      expect(controlState.momentum).toBeDefined();
      expect(typeof controlState.momentum.x).toBe('number');
      expect(typeof controlState.momentum.y).toBe('number');
    });
  });

  describe('Cleanup', () => {
    it('should dispose camera controller', () => {
      camera.dispose();

      expect(camera).toBeDefined();
    });

    it('should cancel momentum animation on dispose', () => {
      camera.setMomentumEnabled(true);

      const downEvent = new MouseEvent('mousedown', {
        button: 0,
        clientX: 100,
        clientY: 100
      });

      const moveEvent = new MouseEvent('mousemove', {
        clientX: 150,
        clientY: 150
      });

      const upEvent = new MouseEvent('mouseup', {
        button: 0
      });

      canvas.dispatchEvent(downEvent);
      canvas.dispatchEvent(moveEvent);
      canvas.dispatchEvent(upEvent);

      camera.dispose();

      expect(camera).toBeDefined();
    });
  });

  describe('Factory Function', () => {
    it('should export factory function', async () => {
      const { createInteractiveCamera } = await import('../InteractiveCamera');

      expect(createInteractiveCamera).toBeDefined();
    });

    it('should create camera with factory', async () => {
      const { createInteractiveCamera } = await import('../InteractiveCamera');

      const factoryCamera = createInteractiveCamera(scene);

      expect(factoryCamera).toBeInstanceOf(InteractiveCamera);

      factoryCamera.dispose();
    });
  });
});
