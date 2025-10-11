import { sveltekit } from '@sveltejs/kit/vite';
import { defineConfig } from 'vite';

export default defineConfig({
	plugins: [sveltekit()],

	server: {
		host: '0.0.0.0',
		port: 3000,
		proxy: {
			'/api': {
				target: process.env.VITE_API_URL || 'http://localhost:8000',
				changeOrigin: true
			}
		}
	},

	build: {
		target: 'esnext',
		sourcemap: true
	},

	optimizeDeps: {
		include: ['d3', 'three', 'monaco-editor']
	}
});
