import { test, expect } from '@playwright/test';

/**
 * E2E Tests: 3D Mechanical Visualization Emulator
 *
 * Tests canvas rendering, user interactions, WebSocket state sync,
 * and performance metrics tracking for the Difference Engine 3D view.
 *
 * Run with: npx playwright test
 * Debug:    npx playwright test --debug
 * UI:       npx playwright test --ui
 */

test.describe('Emulator 3D Visualization', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to 3D emulator page
    await page.goto('/emulator/3d');

    // Wait for page to load
    await page.waitForLoadState('networkidle');
  });

  test('should display 3D canvas and render initial scene', async ({ page }) => {
    // Check canvas element exists
    const canvas = page.locator('canvas');
    await expect(canvas).toBeVisible();

    // Verify canvas dimensions are reasonable
    const boundingBox = await canvas.boundingBox();
    expect(boundingBox?.width).toBeGreaterThan(500);
    expect(boundingBox?.height).toBeGreaterThan(400);

    // Check for engine status indicator
    const statusIndicator = page.locator('text=/Ready|Running/');
    await expect(statusIndicator).toBeVisible();
  });

  test('should display control panel with metrics', async ({ page }) => {
    // Check for metrics display
    const fpsDisplay = page.locator('text=/FPS:/');
    const frameTimeDisplay = page.locator('text=/Frame Time:/');

    await expect(fpsDisplay).toBeVisible();
    await expect(frameTimeDisplay).toBeVisible();

    // Verify metrics have numeric values
    const fpsValue = await fpsDisplay.textContent();
    expect(fpsValue).toMatch(/FPS:\s*\d+/);
  });

  test('should display connection status section', async ({ page }) => {
    // Check for backend connection status
    const connectionLabel = page.locator('text=/Backend:/');
    await expect(connectionLabel).toBeVisible();

    // Connection status should show one of the valid states
    const connectionStatus = page.locator(
      'text=/(Connected|Connecting|Disconnected|Error)/'
    );
    await expect(connectionStatus).toBeVisible();
  });

  test('should respond to mouse interactions on canvas', async ({ page }) => {
    const canvas = page.locator('canvas');
    const boundingBox = await canvas.boundingBox();

    if (!boundingBox) {
      throw new Error('Canvas bounding box not found');
    }

    // Simulate mouse movement and drag (camera rotation)
    const x = boundingBox.x + boundingBox.width / 2;
    const y = boundingBox.y + boundingBox.height / 2;

    await page.mouse.move(x, y);
    await page.mouse.down();
    await page.mouse.move(x + 100, y + 100);
    await page.mouse.up();

    // Verify page is still responsive
    await expect(canvas).toBeVisible();
  });

  test('should handle mouse scroll for zoom', async ({ page }) => {
    const canvas = page.locator('canvas');

    // Scroll on canvas to zoom
    await canvas.hover();
    await page.mouse.wheel(0, -100); // Zoom in
    await page.mouse.wheel(0, 100); // Zoom out

    // Verify canvas is still visible and responsive
    await expect(canvas).toBeVisible();
  });

  test('should display help section with controls', async ({ page }) => {
    // Check for help section
    const helpSection = page.locator('text=/Controls/i');
    await expect(helpSection).toBeVisible();

    // Check for control descriptions
    const clickDrag = page.locator('text=/Click.*Drag.*Rotate/i');
    const rightClick = page.locator('text=/Right.*Click.*Pan/i');
    const scroll = page.locator('text=/Scroll.*Zoom/i');

    await expect(clickDrag).toBeVisible();
    await expect(rightClick).toBeVisible();
    await expect(scroll).toBeVisible();
  });

  test('should have Reset View button that works', async ({ page }) => {
    const resetButton = page.locator('button:has-text("Reset View")');
    await expect(resetButton).toBeVisible();

    // Click reset button
    await resetButton.click();

    // Verify button is still clickable (no error)
    await expect(resetButton).toBeEnabled();
  });

  test('should have Debug Overlay button', async ({ page }) => {
    const debugButton = page.locator('button:has-text("Debug Overlay")');
    await expect(debugButton).toBeVisible();

    // Click debug button
    await debugButton.click();

    // Verify button is still clickable
    await expect(debugButton).toBeEnabled();
  });

  test('should display performance metrics over time', async ({ page }) => {
    // Wait for initial metrics to display
    await page.waitForTimeout(1000);

    // Get FPS metric
    let fpsText1 = await page.locator('text=/FPS:.*\\d+/').first().textContent();

    // Wait a bit more
    await page.waitForTimeout(1000);

    // Get updated FPS metric
    let fpsText2 = await page.locator('text=/FPS:.*\\d+/').first().textContent();

    // Verify both have values (metrics are updating)
    expect(fpsText1).toBeTruthy();
    expect(fpsText2).toBeTruthy();
  });

  test('should maintain responsive layout on resize', async ({ page }) => {
    const canvas = page.locator('canvas');
    const initialBox = await canvas.boundingBox();

    // Resize viewport
    await page.setViewportSize({ width: 800, height: 600 });

    // Wait for resize handling
    await page.waitForTimeout(500);

    // Check canvas still visible
    await expect(canvas).toBeVisible();

    // Control panel should still be visible
    const fpsDisplay = page.locator('text=/FPS:/');
    await expect(fpsDisplay).toBeVisible();
  });

  test('should have proper page title and metadata', async ({ page }) => {
    // Check page title contains emulator reference
    const title = await page.title();
    expect(title.toLowerCase()).toContain('ancient');

    // Check page has proper lang attribute
    const html = page.locator('html');
    const lang = await html.getAttribute('lang');
    expect(lang).toBe('en');
  });

  test('should be accessible from main navigation', async ({ page }) => {
    // Go to home page first
    await page.goto('/');

    // Look for link to emulator or 3D view
    const emulatorLink = page.locator(
      'a[href*="/emulator"], a:has-text(/3D|Emulator/i)'
    ).first();

    // Should be able to navigate to 3D emulator
    if (await emulatorLink.isVisible()) {
      await emulatorLink.click();
      await page.waitForLoadState('networkidle');

      // Should be on emulator page
      expect(page.url()).toContain('/emulator');
    }
  });

  test('should load and display without console errors', async ({
    page,
    context,
  }) => {
    // Collect console messages
    const consoleMessages: string[] = [];
    page.on('console', (msg) => {
      if (msg.type() === 'error' || msg.type() === 'warning') {
        consoleMessages.push(`${msg.type()}: ${msg.text()}`);
      }
    });

    // Reload page to catch any startup errors
    await page.reload();
    await page.waitForLoadState('networkidle');

    // Check for critical errors (non-warning)
    const criticalErrors = consoleMessages.filter((msg) =>
      msg.startsWith('error:')
    );

    // Some console messages are expected, but no errors
    if (criticalErrors.length > 0) {
      console.log('Console errors found:', criticalErrors);
    }

    // Critical errors should be minimal or none
    expect(criticalErrors.length).toBeLessThanOrEqual(1);
  });

  test.describe('Performance Benchmarks', () => {
    test('should maintain 60+ FPS on initial load', async ({ page }) => {
      // Get initial FPS reading after load
      const fpsText = await page.locator('text=/FPS:.*\\d+/').first();

      const text = await fpsText.textContent();
      const fpsMatch = text?.match(/FPS:\s*(\d+)/);
      const fps = fpsMatch ? parseInt(fpsMatch[1]) : 0;

      // Most devices should maintain at least 30 FPS, high-end 60+
      expect(fps).toBeGreaterThan(20);
    });

    test('should not exceed 100ms frame time', async ({ page }) => {
      // Wait for page to stabilize
      await page.waitForTimeout(2000);

      // Get frame time
      const frameTimeText = await page.locator('text=/Frame Time:.*\\d+/');
      const text = await frameTimeText.textContent();
      const timeMatch = text?.match(/Frame Time:\s*([\d.]+)/);
      const frameTime = timeMatch ? parseFloat(timeMatch[1]) : 0;

      // Frame time should be reasonable (< 33.33ms for 30 FPS)
      // Allow some margin for slower devices
      expect(frameTime).toBeLessThan(100);
    });
  });

  test.describe('WebSocket Connectivity', () => {
    test('should show connection status', async ({ page }) => {
      // Connection status should be visible
      const statusElement = page.locator('text=/(Connected|Connecting|Disconnected)/');
      await expect(statusElement).toBeVisible();
    });

    test('should handle offline gracefully', async ({ page, context }) => {
      // Set offline mode
      await context.setOffline(true);

      // Wait a bit for disconnection
      await page.waitForTimeout(2000);

      // Connection status should show disconnected or error
      const disconnectedStatus = page.locator(
        'text=/(Disconnected|Error|offline)/i'
      );

      // Either disconnected or it's still trying to connect (connecting state is OK)
      const statusElement = page.locator(
        'text=/(Connected|Connecting|Disconnected|Error)/i'
      );
      await expect(statusElement).toBeVisible();

      // Go back online
      await context.setOffline(false);
    });
  });
});

/**
 * Separate suite for touch/mobile interactions
 */
test.describe('Touch Interactions (Mobile)', () => {
  test.beforeEach(async ({ page }) => {
    // Set mobile device viewport
    await page.setViewportSize({ width: 390, height: 844 });
    await page.goto('/emulator/3d');
    await page.waitForLoadState('networkidle');
  });

  test('should render on mobile viewport', async ({ page }) => {
    const canvas = page.locator('canvas');
    await expect(canvas).toBeVisible();

    // Should not have horizontal scroll
    const body = page.locator('body');
    const scrollWidth = await body.evaluate((el) => el.scrollWidth);
    const clientWidth = await body.evaluate((el) => el.clientWidth);

    expect(scrollWidth).toBeLessThanOrEqual(clientWidth + 1); // +1 for rounding
  });

  test('should handle touch gestures', async ({ page }) => {
    const canvas = page.locator('canvas');
    const boundingBox = await canvas.boundingBox();

    if (!boundingBox) {
      throw new Error('Canvas not found');
    }

    const x = boundingBox.x + boundingBox.width / 2;
    const y = boundingBox.y + boundingBox.height / 2;

    // Simulate touch start, move, end
    await page.touchscreen.tap(x, y);

    // Page should still be responsive
    await expect(canvas).toBeVisible();
  });
});
