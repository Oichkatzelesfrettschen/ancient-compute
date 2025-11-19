/**
 * LessonViewer.test.ts - Tests for LessonViewer component
 *
 * Tests:
 * - Markdown rendering
 * - LaTeX math rendering
 * - Code syntax highlighting
 * - Navigation controls
 * - Lesson completion
 */

import { describe, it, expect, vi, beforeEach } from 'vitest';
import { render, fireEvent, screen } from '@testing-library/svelte';
import LessonViewer from '../LessonViewer.svelte';
import type { Lesson } from '../../../stores/timelineStore';

const mockLesson: Lesson = {
	id: 'test-lesson-1',
	moduleId: 'test-module',
	eraId: 'era-1',
	title: 'Test Lesson',
	subtitle: 'A test lesson',
	description: 'This is a test lesson',
	content: `
# Introduction

This is **bold** and this is *italic*.

## Math Example

Inline math: $E = mc^2$

Block math:
$$
\\int_0^\\infty e^{-x^2} dx = \\frac{\\sqrt{\\pi}}{2}
$$

## Code Example

\`\`\`python
def hello():
    print("Hello, World!")
\`\`\`
	`,
	historicalContext: 'This lesson has historical context.',
	codeExamples: [
		{
			id: 'ex-1',
			language: 'Python',
			title: 'Hello World',
			code: 'print("Hello, World!")',
			explanation: 'A simple hello world program',
		},
	],
	keyConcepts: [
		{
			term: 'Test Concept',
			definition: 'A concept for testing',
			historicalContext: 'Historical context for the concept',
		},
	],
	estimatedReadTime: 10,
	prerequisites: [],
	nextLessons: [],
	completed: false,
};

describe('LessonViewer', () => {
	describe('Rendering', () => {
		it('should render lesson title', () => {
			render(LessonViewer, {
				props: {
					lesson: mockLesson,
					prevLessonId: null,
					nextLessonId: null,
				},
			});

			const title = screen.getByText('Test Lesson');
			expect(title).toBeTruthy();
		});

		it('should render lesson subtitle', () => {
			render(LessonViewer, {
				props: {
					lesson: mockLesson,
					prevLessonId: null,
					nextLessonId: null,
				},
			});

			const subtitle = screen.getByText('A test lesson');
			expect(subtitle).toBeTruthy();
		});

		it('should display estimated read time', () => {
			const { container } = render(LessonViewer, {
				props: {
					lesson: mockLesson,
					prevLessonId: null,
					nextLessonId: null,
				},
			});

			const metaItems = container.querySelectorAll('.meta-item');
			const readTime = Array.from(metaItems).find(item =>
				item.textContent?.includes('min read')
			);
			expect(readTime).toBeTruthy();
		});
	});

	describe('Content Rendering', () => {
		it('should render markdown content', () => {
			const { container } = render(LessonViewer, {
				props: {
					lesson: mockLesson,
					prevLessonId: null,
					nextLessonId: null,
				},
			});

			const content = container.querySelector('.lesson-content');
			expect(content).toBeTruthy();
			expect(content?.innerHTML).toContain('<strong>bold</strong>');
			expect(content?.innerHTML).toContain('<em>italic</em>');
		});

		it('should render code blocks with syntax highlighting', () => {
			const { container } = render(LessonViewer, {
				props: {
					lesson: mockLesson,
					prevLessonId: null,
					nextLessonId: null,
				},
			});

			const codeBlocks = container.querySelectorAll('pre code');
			expect(codeBlocks.length).toBeGreaterThan(0);
		});

		it('should show historical context section', () => {
			const { container } = render(LessonViewer, {
				props: {
					lesson: mockLesson,
					prevLessonId: null,
					nextLessonId: null,
				},
			});

			const historicalContext = container.querySelector('.historical-context');
			expect(historicalContext).toBeTruthy();
			expect(historicalContext?.textContent).toContain(mockLesson.historicalContext);
		});

		it('should render key concepts', () => {
			const { container } = render(LessonViewer, {
				props: {
					lesson: mockLesson,
					prevLessonId: null,
					nextLessonId: null,
				},
			});

			const concepts = container.querySelectorAll('.concept-card');
			expect(concepts.length).toBe(mockLesson.keyConcepts.length);
		});

		it('should render code examples', () => {
			const { container } = render(LessonViewer, {
				props: {
					lesson: mockLesson,
					prevLessonId: null,
					nextLessonId: null,
				},
			});

			const examples = container.querySelectorAll('.example-card');
			expect(examples.length).toBe(mockLesson.codeExamples.length);
		});
	});

	describe('Navigation', () => {
		it('should show previous lesson button when available', () => {
			const { container } = render(LessonViewer, {
				props: {
					lesson: mockLesson,
					prevLessonId: 'prev-lesson',
					nextLessonId: null,
				},
			});

			const prevButton = container.querySelector('.nav-btn.prev');
			expect(prevButton).toBeTruthy();
		});

		it('should show next lesson button when available', () => {
			const { container } = render(LessonViewer, {
				props: {
					lesson: mockLesson,
					prevLessonId: null,
					nextLessonId: 'next-lesson',
				},
			});

			const nextButton = container.querySelector('.nav-btn.next');
			expect(nextButton).toBeTruthy();
		});

		it('should call onNavigate when next button clicked', async () => {
			const mockNavigate = vi.fn();
			const { container } = render(LessonViewer, {
				props: {
					lesson: mockLesson,
					prevLessonId: null,
					nextLessonId: 'next-lesson',
					onNavigate: mockNavigate,
				},
			});

			const nextButton = container.querySelector('.nav-btn.next');
			await fireEvent.click(nextButton as Element);

			expect(mockNavigate).toHaveBeenCalledWith('next-lesson');
		});
	});

	describe('Lesson Completion', () => {
		it('should show mark complete button for incomplete lessons', () => {
			const { container } = render(LessonViewer, {
				props: {
					lesson: mockLesson,
					prevLessonId: null,
					nextLessonId: null,
				},
			});

			const completeButton = container.querySelector('.complete-btn');
			expect(completeButton).toBeTruthy();
			expect(completeButton?.textContent).toContain('Mark as Complete');
		});

		it('should not show mark complete button for completed lessons', () => {
			const completedLesson = { ...mockLesson, completed: true };
			const { container } = render(LessonViewer, {
				props: {
					lesson: completedLesson,
					prevLessonId: null,
					nextLessonId: null,
				},
			});

			const completeButton = container.querySelector('.complete-btn');
			expect(completeButton).toBeFalsy();
		});

		it('should show completed badge for completed lessons', () => {
			const completedLesson = { ...mockLesson, completed: true };
			const { container } = render(LessonViewer, {
				props: {
					lesson: completedLesson,
					prevLessonId: null,
					nextLessonId: null,
				},
			});

			const completedMeta = container.querySelector('.meta-item.completed');
			expect(completedMeta).toBeTruthy();
		});
	});

	describe('Interactive Features', () => {
		it('should have copy buttons for code blocks', () => {
			const { container } = render(LessonViewer, {
				props: {
					lesson: mockLesson,
					prevLessonId: null,
					nextLessonId: null,
				},
			});

			const copyButtons = container.querySelectorAll('.copy-code-btn');
			expect(copyButtons.length).toBeGreaterThan(0);
		});

		it('should make images zoomable', () => {
			const lessonWithImage = {
				...mockLesson,
				content: mockLesson.content + '\n\n![Test Image](test.jpg)',
			};

			const { container } = render(LessonViewer, {
				props: {
					lesson: lessonWithImage,
					prevLessonId: null,
					nextLessonId: null,
				},
			});

			const images = container.querySelectorAll('.zoomable-image');
			expect(images.length).toBeGreaterThan(0);
		});
	});

	describe('Accessibility', () => {
		it('should have proper heading structure', () => {
			const { container } = render(LessonViewer, {
				props: {
					lesson: mockLesson,
					prevLessonId: null,
					nextLessonId: null,
				},
			});

			const h1 = container.querySelector('h1');
			expect(h1?.textContent).toBe('Test Lesson');
		});

		it('should have semantic HTML structure', () => {
			const { container } = render(LessonViewer, {
				props: {
					lesson: mockLesson,
					prevLessonId: null,
					nextLessonId: null,
				},
			});

			const article = container.querySelector('article');
			const header = container.querySelector('header');
			expect(article).toBeTruthy();
			expect(header).toBeTruthy();
		});
	});
});
