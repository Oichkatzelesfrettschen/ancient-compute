import adapter from '@sveltejs/adapter-node';
import { vitePreprocess } from '@sveltejs/vite-plugin-svelte';

/** @type {import('@sveltejs/kit').Config} */
const config = {
	// Consult https://kit.svelte.dev/docs/integrations#preprocessors
	// for more information about preprocessors
	preprocess: vitePreprocess(),

	kit: {
		// adapter-node for Docker deployment
		adapter: adapter({
			out: 'build',
			precompress: false,
			envPrefix: 'PUBLIC_'
		}),

		// Environment variable prefix
		env: {
			publicPrefix: 'PUBLIC_'
		},

		// CSP settings for security
		csp: {
			mode: 'auto',
			directives: {
				'default-src': ['self'],
				'script-src': ['self', 'unsafe-inline'],
				'style-src': ['self', 'unsafe-inline'],
				'img-src': ['self', 'data:', 'https:'],
				'font-src': ['self'],
				'connect-src': ['self', process.env.PUBLIC_API_URL || 'http://localhost:8000']
			}
		}
	}
};

export default config;
