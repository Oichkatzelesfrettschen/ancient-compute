<script lang="ts">
/**
 * LessonViewer.svelte - Comprehensive lesson viewer with markdown rendering
 *
 * Features:
 * - Markdown rendering with GitHub Flavored Markdown
 * - LaTeX math rendering (KaTeX)
 * - Syntax highlighting (Prism)
 * - Interactive code blocks
 * - Image zoom functionality
 * - Table of contents (auto-generated)
 * - Previous/Next navigation
 * - Progress tracking
 * - Bookmarking
 */

import { onMount, afterUpdate } from 'svelte';
import { marked } from 'marked';
import katex from 'katex';
import Prism from 'prismjs';
import type { Lesson } from '../../stores/timelineStore';
import { markLessonComplete } from '../../stores/educationStore';

export let lesson: Lesson;
export let prevLessonId: string | null = null;
export let nextLessonId: string | null = null;
export let onNavigate: (lessonId: string) => void = () => {};

let contentElement: HTMLElement;
let renderedContent = '';
let tableOfContents: Array<{ id: string; text: string; level: number }> = [];
let activeSection = '';
let isCompleting = false;
let showImageModal = false;
let modalImageSrc = '';

// Configure marked for GFM and math support
marked.setOptions({
	gfm: true,
	breaks: true,
	headerIds: true,
	mangle: false,
});

// Custom renderer for code blocks with syntax highlighting
const renderer = new marked.Renderer();

renderer.code = (code, language) => {
	const lang = language || 'plaintext';
	let highlighted = code;

	try {
		if (Prism.languages[lang]) {
			highlighted = Prism.highlight(code, Prism.languages[lang], lang);
		}
	} catch (e) {
		console.warn('Prism highlighting failed:', e);
	}

	return `
		<div class="code-block-wrapper">
			<div class="code-header">
				<span class="code-language">${lang}</span>
				<button class="copy-code-btn" data-code="${encodeURIComponent(code)}">Copy</button>
			</div>
			<pre class="language-${lang}"><code class="language-${lang}">${highlighted}</code></pre>
		</div>
	`;
};

// Custom renderer for images with zoom capability
renderer.image = (href, title, text) => {
	return `
		<img
			src="${href}"
			alt="${text}"
			title="${title || ''}"
			class="zoomable-image"
			data-zoom-src="${href}"
		/>
	`;
};

marked.use({ renderer });

// Render markdown content
function renderMarkdown(markdown: string): string {
	let html = marked(markdown) as string;

	// Process LaTeX math (inline and block)
	// Inline math: $...$
	html = html.replace(/\$([^\$]+)\$/g, (_, tex) => {
		try {
			return katex.renderToString(tex, { throwOnError: false });
		} catch (e) {
			return `<span class="math-error">$${tex}$</span>`;
		}
	});

	// Block math: $$...$$
	html = html.replace(/\$\$([^\$]+)\$\$/g, (_, tex) => {
		try {
			return `<div class="math-block">${katex.renderToString(tex, {
				displayMode: true,
				throwOnError: false,
			})}</div>`;
		} catch (e) {
			return `<div class="math-error">$$${tex}$$</div>`;
		}
	});

	return html;
}

// Extract table of contents from rendered HTML
function extractTOC(html: string): Array<{ id: string; text: string; level: number }> {
	const toc: Array<{ id: string; text: string; level: number }> = [];
	const headingRegex = /<h([2-4])[^>]*id="([^"]*)"[^>]*>(.*?)<\/h\1>/g;
	let match;

	while ((match = headingRegex.exec(html)) !== null) {
		const level = parseInt(match[1]);
		const id = match[2];
		const text = match[3].replace(/<[^>]*>/g, ''); // Strip HTML tags

		toc.push({ id, text, level });
	}

	return toc;
}

// Handle copy code button clicks
function handleCopyCode(event: Event): void {
	const target = event.target as HTMLElement;
	if (target.classList.contains('copy-code-btn')) {
		const code = decodeURIComponent(target.getAttribute('data-code') || '');
		navigator.clipboard.writeText(code).then(() => {
			target.textContent = 'Copied!';
			setTimeout(() => {
				target.textContent = 'Copy';
			}, 2000);
		});
	}
}

// Handle image zoom
function handleImageClick(event: Event): void {
	const target = event.target as HTMLElement;
	if (target.classList.contains('zoomable-image')) {
		const src = target.getAttribute('data-zoom-src') || '';
		if (src) {
			modalImageSrc = src;
			showImageModal = true;
		}
	}
}

// Handle lesson completion
async function handleMarkComplete(): Promise<void> {
	isCompleting = true;
	try {
		await markLessonComplete(lesson.id);
	} catch (error) {
		console.error('Failed to mark lesson complete:', error);
	} finally {
		isCompleting = false;
	}
}

// Track active section for TOC highlighting
function updateActiveSection(): void {
	if (!contentElement) return;

	const headings = contentElement.querySelectorAll('h2, h3, h4');
	const scrollPosition = window.scrollY + 100;

	for (let i = headings.length - 1; i >= 0; i--) {
		const heading = headings[i] as HTMLElement;
		if (heading.offsetTop <= scrollPosition) {
			activeSection = heading.id;
			return;
		}
	}

	activeSection = '';
}

// Scroll to section
function scrollToSection(id: string): void {
	const element = document.getElementById(id);
	if (element) {
		element.scrollIntoView({ behavior: 'smooth', block: 'start' });
	}
}

onMount(() => {
	// Render markdown
	renderedContent = renderMarkdown(lesson.content);
	tableOfContents = extractTOC(renderedContent);

	// Add scroll listener for active section tracking
	window.addEventListener('scroll', updateActiveSection);

	return () => {
		window.removeEventListener('scroll', updateActiveSection);
	};
});

afterUpdate(() => {
	// Attach event listeners to dynamically rendered content
	if (contentElement) {
		contentElement.addEventListener('click', handleCopyCode);
		contentElement.addEventListener('click', handleImageClick);
	}
});

$: if (lesson) {
	renderedContent = renderMarkdown(lesson.content);
	tableOfContents = extractTOC(renderedContent);
}
</script>

<div class="lesson-viewer">
	<!-- Lesson header -->
	<header class="lesson-header">
		<h1 class="lesson-title">{lesson.title}</h1>
		{#if lesson.subtitle}
			<p class="lesson-subtitle">{lesson.subtitle}</p>
		{/if}

		<div class="lesson-meta">
			<span class="meta-item">
				<span class="meta-icon">🕐</span>
				{lesson.estimatedReadTime} min read
			</span>
			{#if lesson.prerequisites.length > 0}
				<span class="meta-item">
					<span class="meta-icon">📋</span>
					{lesson.prerequisites.length} prerequisite(s)
				</span>
			{/if}
			{#if lesson.completed}
				<span class="meta-item completed">
					<span class="meta-icon">✓</span>
					Completed
				</span>
			{/if}
		</div>
	</header>

	<!-- Historical context -->
	{#if lesson.historicalContext}
		<div class="historical-context">
			<h2 class="context-title">📜 Historical Context</h2>
			<p class="context-content">{lesson.historicalContext}</p>
		</div>
	{/if}

	<!-- Key concepts -->
	{#if lesson.keyConcepts.length > 0}
		<div class="key-concepts">
			<h2 class="concepts-title">🔑 Key Concepts</h2>
			<div class="concepts-grid">
				{#each lesson.keyConcepts as concept}
					<div class="concept-card">
						<h3 class="concept-term">{concept.term}</h3>
						<p class="concept-definition">{concept.definition}</p>
						{#if concept.historicalContext}
							<p class="concept-history">{concept.historicalContext}</p>
						{/if}
					</div>
				{/each}
			</div>
		</div>
	{/if}

	<!-- Main content -->
	<article class="lesson-content" bind:this={contentElement}>
		{@html renderedContent}
	</article>

	<!-- Code examples -->
	{#if lesson.codeExamples.length > 0}
		<div class="code-examples-section">
			<h2 class="examples-title">💻 Code Examples</h2>
			{#each lesson.codeExamples as example}
				<div class="example-card">
					<div class="example-header">
						<h3 class="example-title">{example.title}</h3>
						<span class="example-language">{example.language}</span>
					</div>
					<pre class="language-{example.language}"><code>{example.code}</code></pre>
					{#if example.explanation}
						<p class="example-explanation">{example.explanation}</p>
					{/if}
				</div>
			{/each}
		</div>
	{/if}

	<!-- Lesson actions -->
	<div class="lesson-actions">
		{#if !lesson.completed}
			<button class="complete-btn" on:click={handleMarkComplete} disabled={isCompleting}>
				{isCompleting ? 'Marking complete...' : 'Mark as Complete'}
			</button>
		{/if}

		<div class="navigation-buttons">
			{#if prevLessonId}
				<button class="nav-btn prev" on:click={() => onNavigate(prevLessonId || '')}>
					← Previous Lesson
				</button>
			{/if}
			{#if nextLessonId}
				<button class="nav-btn next" on:click={() => onNavigate(nextLessonId || '')}>
					Next Lesson →
				</button>
			{/if}
		</div>
	</div>
</div>

<!-- Image modal -->
{#if showImageModal}
	<div class="image-modal" on:click={() => (showImageModal = false)}>
		<img src={modalImageSrc} alt="Zoomed image" class="modal-image" />
		<button class="close-modal" on:click={() => (showImageModal = false)}>✕</button>
	</div>
{/if}

<style>
	.lesson-viewer {
		display: flex;
		flex-direction: column;
		gap: 32px;
		max-width: 800px;
		margin: 0 auto;
		padding: 32px 24px;
	}

	.lesson-header {
		display: flex;
		flex-direction: column;
		gap: 12px;
	}

	.lesson-title {
		margin: 0;
		font-size: 32px;
		font-weight: 700;
		color: #2c3e50;
		line-height: 1.2;
	}

	.lesson-subtitle {
		margin: 0;
		font-size: 18px;
		color: #666;
		font-style: italic;
	}

	.lesson-meta {
		display: flex;
		flex-wrap: wrap;
		gap: 16px;
		padding: 12px 0;
		border-bottom: 1px solid #e0e0e0;
	}

	.meta-item {
		display: flex;
		align-items: center;
		gap: 6px;
		font-size: 14px;
		color: #666;
	}

	.meta-item.completed {
		color: #7cb342;
		font-weight: 600;
	}

	.meta-icon {
		font-size: 16px;
	}

	.historical-context {
		padding: 20px;
		background: #f5f9ff;
		border-left: 4px solid #4a90e2;
		border-radius: 4px;
	}

	.context-title {
		margin: 0 0 12px 0;
		font-size: 18px;
		font-weight: 700;
		color: #2c3e50;
	}

	.context-content {
		margin: 0;
		font-size: 14px;
		line-height: 1.6;
		color: #666;
	}

	.key-concepts {
		display: flex;
		flex-direction: column;
		gap: 16px;
	}

	.concepts-title {
		margin: 0;
		font-size: 20px;
		font-weight: 700;
		color: #2c3e50;
	}

	.concepts-grid {
		display: grid;
		grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
		gap: 16px;
	}

	.concept-card {
		padding: 16px;
		background: white;
		border: 2px solid #e0e0e0;
		border-radius: 8px;
	}

	.concept-term {
		margin: 0 0 8px 0;
		font-size: 16px;
		font-weight: 700;
		color: #4a90e2;
	}

	.concept-definition {
		margin: 0 0 8px 0;
		font-size: 14px;
		line-height: 1.5;
		color: #2c3e50;
	}

	.concept-history {
		margin: 0;
		font-size: 13px;
		line-height: 1.4;
		color: #999;
		font-style: italic;
	}

	.lesson-content {
		font-size: 16px;
		line-height: 1.8;
		color: #2c3e50;
	}

	.lesson-content :global(h2) {
		margin: 32px 0 16px 0;
		font-size: 24px;
		font-weight: 700;
		color: #2c3e50;
	}

	.lesson-content :global(h3) {
		margin: 24px 0 12px 0;
		font-size: 20px;
		font-weight: 700;
		color: #2c3e50;
	}

	.lesson-content :global(h4) {
		margin: 20px 0 10px 0;
		font-size: 18px;
		font-weight: 600;
		color: #2c3e50;
	}

	.lesson-content :global(p) {
		margin: 16px 0;
	}

	.lesson-content :global(a) {
		color: #4a90e2;
		text-decoration: none;
		border-bottom: 1px solid transparent;
		transition: border-bottom-color 0.2s ease;
	}

	.lesson-content :global(a:hover) {
		border-bottom-color: #4a90e2;
	}

	.lesson-content :global(ul),
	.lesson-content :global(ol) {
		margin: 16px 0;
		padding-left: 28px;
	}

	.lesson-content :global(li) {
		margin: 8px 0;
	}

	.lesson-content :global(blockquote) {
		margin: 24px 0;
		padding: 16px 20px;
		border-left: 4px solid #4a90e2;
		background: #f5f5f5;
		font-style: italic;
	}

	.lesson-content :global(.code-block-wrapper) {
		margin: 24px 0;
		border-radius: 8px;
		overflow: hidden;
		border: 1px solid #e0e0e0;
	}

	.lesson-content :global(.code-header) {
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 8px 16px;
		background: #2c3e50;
		color: white;
	}

	.lesson-content :global(.code-language) {
		font-size: 12px;
		font-weight: 600;
		text-transform: uppercase;
	}

	.lesson-content :global(.copy-code-btn) {
		padding: 4px 12px;
		background: rgba(255, 255, 255, 0.1);
		border: 1px solid rgba(255, 255, 255, 0.3);
		border-radius: 4px;
		cursor: pointer;
		font-size: 12px;
		color: white;
		transition: background 0.2s ease;
	}

	.lesson-content :global(.copy-code-btn:hover) {
		background: rgba(255, 255, 255, 0.2);
	}

	.lesson-content :global(pre) {
		margin: 0;
		padding: 16px;
		background: #f5f5f5;
		overflow-x: auto;
	}

	.lesson-content :global(code) {
		font-family: 'Courier New', Courier, monospace;
		font-size: 14px;
	}

	.lesson-content :global(.zoomable-image) {
		max-width: 100%;
		height: auto;
		cursor: zoom-in;
		border-radius: 8px;
		transition: transform 0.2s ease;
	}

	.lesson-content :global(.zoomable-image:hover) {
		transform: scale(1.02);
	}

	.lesson-content :global(.math-block) {
		margin: 24px 0;
		padding: 16px;
		text-align: center;
		overflow-x: auto;
	}

	.lesson-content :global(.math-error) {
		color: #e53935;
		font-family: monospace;
	}

	.code-examples-section {
		display: flex;
		flex-direction: column;
		gap: 16px;
	}

	.examples-title {
		margin: 0;
		font-size: 20px;
		font-weight: 700;
		color: #2c3e50;
	}

	.example-card {
		border: 2px solid #e0e0e0;
		border-radius: 8px;
		overflow: hidden;
	}

	.example-header {
		display: flex;
		justify-content: space-between;
		align-items: center;
		padding: 12px 16px;
		background: #f5f5f5;
	}

	.example-title {
		margin: 0;
		font-size: 16px;
		font-weight: 600;
		color: #2c3e50;
	}

	.example-language {
		padding: 4px 10px;
		background: #4a90e2;
		border-radius: 4px;
		font-size: 11px;
		font-weight: 700;
		text-transform: uppercase;
		color: white;
	}

	.example-explanation {
		margin: 0;
		padding: 16px;
		font-size: 14px;
		line-height: 1.6;
		color: #666;
		background: #f9f9f9;
	}

	.lesson-actions {
		display: flex;
		flex-direction: column;
		gap: 16px;
		padding-top: 24px;
		border-top: 1px solid #e0e0e0;
	}

	.complete-btn {
		padding: 14px 24px;
		background: #7cb342;
		border: none;
		border-radius: 8px;
		cursor: pointer;
		font-size: 16px;
		font-weight: 600;
		color: white;
		transition: background 0.2s ease;
	}

	.complete-btn:hover:not(:disabled) {
		background: #689f38;
	}

	.complete-btn:disabled {
		opacity: 0.6;
		cursor: not-allowed;
	}

	.navigation-buttons {
		display: flex;
		justify-content: space-between;
		gap: 16px;
	}

	.nav-btn {
		padding: 12px 20px;
		background: #f5f5f5;
		border: 2px solid #e0e0e0;
		border-radius: 8px;
		cursor: pointer;
		font-size: 14px;
		font-weight: 600;
		color: #2c3e50;
		transition: all 0.2s ease;
	}

	.nav-btn:hover {
		background: #e8e8e8;
		border-color: #4a90e2;
		color: #4a90e2;
	}

	.image-modal {
		position: fixed;
		top: 0;
		left: 0;
		right: 0;
		bottom: 0;
		display: flex;
		align-items: center;
		justify-content: center;
		background: rgba(0, 0, 0, 0.9);
		z-index: 1000;
		cursor: zoom-out;
	}

	.modal-image {
		max-width: 90%;
		max-height: 90%;
		object-fit: contain;
	}

	.close-modal {
		position: absolute;
		top: 20px;
		right: 20px;
		width: 40px;
		height: 40px;
		background: rgba(255, 255, 255, 0.2);
		border: 2px solid white;
		border-radius: 50%;
		cursor: pointer;
		font-size: 24px;
		color: white;
		display: flex;
		align-items: center;
		justify-content: center;
		transition: background 0.2s ease;
	}

	.close-modal:hover {
		background: rgba(255, 255, 255, 0.3);
	}

	@media (max-width: 768px) {
		.lesson-viewer {
			padding: 24px 16px;
		}

		.lesson-title {
			font-size: 24px;
		}

		.concepts-grid {
			grid-template-columns: 1fr;
		}

		.navigation-buttons {
			flex-direction: column;
		}

		.nav-btn {
			width: 100%;
		}
	}
</style>
