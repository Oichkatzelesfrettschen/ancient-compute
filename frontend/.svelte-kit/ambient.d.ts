
// this file is generated — do not edit it


/// <reference types="@sveltejs/kit" />

/**
 * Environment variables [loaded by Vite](https://vitejs.dev/guide/env-and-mode.html#env-files) from `.env` files and `process.env`. Like [`$env/dynamic/private`](https://kit.svelte.dev/docs/modules#$env-dynamic-private), this module cannot be imported into client-side code. This module only includes variables that _do not_ begin with [`config.kit.env.publicPrefix`](https://kit.svelte.dev/docs/configuration#env) _and do_ start with [`config.kit.env.privatePrefix`](https://kit.svelte.dev/docs/configuration#env) (if configured).
 * 
 * _Unlike_ [`$env/dynamic/private`](https://kit.svelte.dev/docs/modules#$env-dynamic-private), the values exported from this module are statically injected into your bundle at build time, enabling optimisations like dead code elimination.
 * 
 * ```ts
 * import { API_KEY } from '$env/static/private';
 * ```
 * 
 * Note that all environment variables referenced in your code should be declared (for example in an `.env` file), even if they don't have a value until the app is deployed:
 * 
 * ```
 * MY_FEATURE_FLAG=""
 * ```
 * 
 * You can override `.env` values from the command line like so:
 * 
 * ```bash
 * MY_FEATURE_FLAG="enabled" npm run dev
 * ```
 */
declare module '$env/static/private' {
	export const SHELL: string;
	export const PLAN9: string;
	export const npm_command: string;
	export const LSCOLORS: string;
	export const npm_config_save_dev: string;
	export const COREPACK_ENABLE_AUTO_PIN: string;
	export const SESSION_MANAGER: string;
	export const WINDOWID: string;
	export const npm_config_userconfig: string;
	export const COLORTERM: string;
	export const SONAR_SCANNER_HOME: string;
	export const SHELL_CONFIG: string;
	export const CSF_MDTVTexturesDirectory: string;
	export const npm_config_cache: string;
	export const npm_package_dev_optional: string;
	export const LESS: string;
	export const ADS_API_KEY: string;
	export const XDG_SESSION_PATH: string;
	export const NVM_INC: string;
	export const HISTCONTROL: string;
	export const CSF_DrawPluginDefaults: string;
	export const npm_package_integrity: string;
	export const _P9K_TTY: string;
	export const NODE: string;
	export const NODE_OPTIONS: string;
	export const LESS_TERMCAP_se: string;
	export const LESS_TERMCAP_so: string;
	export const LC_ADDRESS: string;
	export const JAVA_HOME: string;
	export const VDPAU_DRIVER: string;
	export const Z80_OZFILES: string;
	export const CSF_LANGUAGE: string;
	export const LC_NAME: string;
	export const CSF_MIGRATION_TYPES: string;
	export const GRADLE_HOME: string;
	export const P9K_TTY: string;
	export const TORCH_NVCC_FLAGS: string;
	export const __GL_SYNC_TO_VBLANK: string;
	export const _POSIX2_VERSION: string;
	export const __GL_YIELD: string;
	export const COLOR: string;
	export const npm_config_local_prefix: string;
	export const LARCH_PATH: string;
	export const LIBVA_DRIVER_NAME: string;
	export const DESKTOP_SESSION: string;
	export const LC_MONETARY: string;
	export const __ETC_PROFILE_NIX_SOURCED: string;
	export const TORCH_CUDA_ARCH_LIST: string;
	export const CSF_OCCTResourcePath: string;
	export const npm_config_globalconfig: string;
	export const XCURSOR_SIZE: string;
	export const CSF_STEPDefaults: string;
	export const EDITOR: string;
	export const GTK_MODULES: string;
	export const ANDROID_NDK: string;
	export const XDG_SEAT: string;
	export const MATE_DESKTOP_SESSION_ID: string;
	export const PWD: string;
	export const NIX_PROFILES: string;
	export const XDG_SESSION_DESKTOP: string;
	export const LOGNAME: string;
	export const XDG_SESSION_TYPE: string;
	export const DRAWHOME: string;
	export const npm_package_dev: string;
	export const npm_config_init_module: string;
	export const CXXFLAGS: string;
	export const _: string;
	export const XAUTHORITY: string;
	export const CSF_StandardLiteDefaults: string;
	export const RAWWAVE_PATH: string;
	export const NoDefaultCurrentDirectoryInExePath: string;
	export const FZF_DEFAULT_COMMAND: string;
	export const XDG_GREETER_DATA_DIR: string;
	export const CLAUDECODE: string;
	export const QT_STYLE_OVERRIDE: string;
	export const MOTD_SHOWN: string;
	export const GDM_LANG: string;
	export const CUDAARCHS: string;
	export const LDFLAGS: string;
	export const BINARYEN_ROOT: string;
	export const HOME: string;
	export const LCLIMPORTDIR: string;
	export const npm_package_peer: string;
	export const SSH_ASKPASS: string;
	export const LC_PAPER: string;
	export const LANG: string;
	export const CINII_APPID: string;
	export const LS_COLORS: string;
	export const XDG_CURRENT_DESKTOP: string;
	export const npm_package_version: string;
	export const DOTFILES_CONFIG: string;
	export const SEMANTIC_SCHOLAR_API_KEY: string;
	export const VTE_VERSION: string;
	export const NIX_SSL_CERT_FILE: string;
	export const npm_package_resolved: string;
	export const XDG_SEAT_PATH: string;
	export const DEEPSTREAM_DIR: string;
	export const GTK_CSD: string;
	export const INIT_CWD: string;
	export const ANDROID_NDK_HOME: string;
	export const CSF_ShadersDirectory: string;
	export const CSF_EXCEPTION_PROMPT: string;
	export const __GL_THREADED_OPTIMIZATIONS: string;
	export const CSF_XmlOcafResource: string;
	export const npm_lifecycle_script: string;
	export const NVM_DIR: string;
	export const CSF_SHMessage: string;
	export const npm_package_optional: string;
	export const XDG_ACTIVATION_TOKEN: string;
	export const npm_config_npm_version: string;
	export const OPCODE6DIR: string;
	export const MAKEFLAGS: string;
	export const XDG_SESSION_CLASS: string;
	export const ANDROID_HOME: string;
	export const TERM: string;
	export const LC_IDENTIFICATION: string;
	export const npm_package_name: string;
	export const LESS_TERMCAP_mb: string;
	export const ZSH: string;
	export const LESS_TERMCAP_me: string;
	export const LESS_TERMCAP_md: string;
	export const GTK_OVERLAY_SCROLLING: string;
	export const npm_config_prefix: string;
	export const USER: string;
	export const SSH_ASKPASS_REQUIRE: string;
	export const CUDA_PATH: string;
	export const CSF_StandardDefaults: string;
	export const CSF_IGESDefaults: string;
	export const VISUAL: string;
	export const CSSTRNGS: string;
	export const SUDO_ASKPASS: string;
	export const CORE_API_KEY: string;
	export const DISPLAY: string;
	export const CSF_XCAFDefaults: string;
	export const npm_lifecycle_event: string;
	export const LESS_TERMCAP_ue: string;
	export const SHLVL: string;
	export const NVM_CD_FLAGS: string;
	export const LESS_TERMCAP_us: string;
	export const GIT_EDITOR: string;
	export const PAGER: string;
	export const LC_TELEPHONE: string;
	export const ANDROID_SDK_ROOT: string;
	export const _P9K_SSH_TTY: string;
	export const LC_MEASUREMENT: string;
	export const XDG_VTNR: string;
	export const CSF_PluginDefaults: string;
	export const CSF_TObjMessage: string;
	export const XDG_SESSION_ID: string;
	export const npm_config_user_agent: string;
	export const CASROOT: string;
	export const DIRHISTORY_SIZE: string;
	export const GST_PLUGIN_PATH: string;
	export const OTEL_EXPORTER_OTLP_METRICS_TEMPORALITY_PREFERENCE: string;
	export const npm_execpath: string;
	export const LD_LIBRARY_PATH: string;
	export const XDG_RUNTIME_DIR: string;
	export const FZF_BASE: string;
	export const CLAUDE_CODE_ENTRYPOINT: string;
	export const NVCC_CCBIN: string;
	export const DEBUGINFOD_URLS: string;
	export const npm_package_json: string;
	export const LC_TIME: string;
	export const P9K_SSH: string;
	export const ANDROID_NDK_ROOT: string;
	export const CUDA_HOME: string;
	export const CSF_XSMessage: string;
	export const UV_THREADPOOL_SIZE: string;
	export const __SHELL_SECRETS_LOADED: string;
	export const XCURSOR_THEME: string;
	export const GTK3_MODULES: string;
	export const MMGT_CLEAR: string;
	export const XDG_DATA_DIRS: string;
	export const npm_config_noproxy: string;
	export const PATH: string;
	export const CSF_TObjDefaults: string;
	export const ZCCCFG: string;
	export const npm_config_node_gyp: string;
	export const HISTIGNORE: string;
	export const GDMSESSION: string;
	export const RAYON_NUM_THREADS: string;
	export const CFLAGS: string;
	export const DBUS_SESSION_BUS_ADDRESS: string;
	export const CMAKE_CUDA_ARCHITECTURES: string;
	export const FZF_DEFAULT_OPTS: string;
	export const npm_config_global_prefix: string;
	export const HG: string;
	export const NVM_BIN: string;
	export const MAIL: string;
	export const CLAUDE_CODE_SESSION_ACCESS_TOKEN: string;
	export const QT_FONT_DPI: string;
	export const QT_SCALE_FACTOR: string;
	export const DRAWDEFAULT: string;
	export const npm_node_execpath: string;
	export const LC_NUMERIC: string;
	export const OLDPWD: string;
	export const CUDAFLAGS: string;
	export const npm_package_engines_node: string;
	export const CUDA_ARCHITECTURES: string;
}

/**
 * Similar to [`$env/static/private`](https://kit.svelte.dev/docs/modules#$env-static-private), except that it only includes environment variables that begin with [`config.kit.env.publicPrefix`](https://kit.svelte.dev/docs/configuration#env) (which defaults to `PUBLIC_`), and can therefore safely be exposed to client-side code.
 * 
 * Values are replaced statically at build time.
 * 
 * ```ts
 * import { PUBLIC_BASE_URL } from '$env/static/public';
 * ```
 */
declare module '$env/static/public' {
	
}

/**
 * This module provides access to runtime environment variables, as defined by the platform you're running on. For example if you're using [`adapter-node`](https://github.com/sveltejs/kit/tree/master/packages/adapter-node) (or running [`vite preview`](https://kit.svelte.dev/docs/cli)), this is equivalent to `process.env`. This module only includes variables that _do not_ begin with [`config.kit.env.publicPrefix`](https://kit.svelte.dev/docs/configuration#env) _and do_ start with [`config.kit.env.privatePrefix`](https://kit.svelte.dev/docs/configuration#env) (if configured).
 * 
 * This module cannot be imported into client-side code.
 * 
 * ```ts
 * import { env } from '$env/dynamic/private';
 * console.log(env.DEPLOYMENT_SPECIFIC_VARIABLE);
 * ```
 * 
 * > In `dev`, `$env/dynamic` always includes environment variables from `.env`. In `prod`, this behavior will depend on your adapter.
 */
declare module '$env/dynamic/private' {
	export const env: {
		SHELL: string;
		PLAN9: string;
		npm_command: string;
		LSCOLORS: string;
		npm_config_save_dev: string;
		COREPACK_ENABLE_AUTO_PIN: string;
		SESSION_MANAGER: string;
		WINDOWID: string;
		npm_config_userconfig: string;
		COLORTERM: string;
		SONAR_SCANNER_HOME: string;
		SHELL_CONFIG: string;
		CSF_MDTVTexturesDirectory: string;
		npm_config_cache: string;
		npm_package_dev_optional: string;
		LESS: string;
		ADS_API_KEY: string;
		XDG_SESSION_PATH: string;
		NVM_INC: string;
		HISTCONTROL: string;
		CSF_DrawPluginDefaults: string;
		npm_package_integrity: string;
		_P9K_TTY: string;
		NODE: string;
		NODE_OPTIONS: string;
		LESS_TERMCAP_se: string;
		LESS_TERMCAP_so: string;
		LC_ADDRESS: string;
		JAVA_HOME: string;
		VDPAU_DRIVER: string;
		Z80_OZFILES: string;
		CSF_LANGUAGE: string;
		LC_NAME: string;
		CSF_MIGRATION_TYPES: string;
		GRADLE_HOME: string;
		P9K_TTY: string;
		TORCH_NVCC_FLAGS: string;
		__GL_SYNC_TO_VBLANK: string;
		_POSIX2_VERSION: string;
		__GL_YIELD: string;
		COLOR: string;
		npm_config_local_prefix: string;
		LARCH_PATH: string;
		LIBVA_DRIVER_NAME: string;
		DESKTOP_SESSION: string;
		LC_MONETARY: string;
		__ETC_PROFILE_NIX_SOURCED: string;
		TORCH_CUDA_ARCH_LIST: string;
		CSF_OCCTResourcePath: string;
		npm_config_globalconfig: string;
		XCURSOR_SIZE: string;
		CSF_STEPDefaults: string;
		EDITOR: string;
		GTK_MODULES: string;
		ANDROID_NDK: string;
		XDG_SEAT: string;
		MATE_DESKTOP_SESSION_ID: string;
		PWD: string;
		NIX_PROFILES: string;
		XDG_SESSION_DESKTOP: string;
		LOGNAME: string;
		XDG_SESSION_TYPE: string;
		DRAWHOME: string;
		npm_package_dev: string;
		npm_config_init_module: string;
		CXXFLAGS: string;
		_: string;
		XAUTHORITY: string;
		CSF_StandardLiteDefaults: string;
		RAWWAVE_PATH: string;
		NoDefaultCurrentDirectoryInExePath: string;
		FZF_DEFAULT_COMMAND: string;
		XDG_GREETER_DATA_DIR: string;
		CLAUDECODE: string;
		QT_STYLE_OVERRIDE: string;
		MOTD_SHOWN: string;
		GDM_LANG: string;
		CUDAARCHS: string;
		LDFLAGS: string;
		BINARYEN_ROOT: string;
		HOME: string;
		LCLIMPORTDIR: string;
		npm_package_peer: string;
		SSH_ASKPASS: string;
		LC_PAPER: string;
		LANG: string;
		CINII_APPID: string;
		LS_COLORS: string;
		XDG_CURRENT_DESKTOP: string;
		npm_package_version: string;
		DOTFILES_CONFIG: string;
		SEMANTIC_SCHOLAR_API_KEY: string;
		VTE_VERSION: string;
		NIX_SSL_CERT_FILE: string;
		npm_package_resolved: string;
		XDG_SEAT_PATH: string;
		DEEPSTREAM_DIR: string;
		GTK_CSD: string;
		INIT_CWD: string;
		ANDROID_NDK_HOME: string;
		CSF_ShadersDirectory: string;
		CSF_EXCEPTION_PROMPT: string;
		__GL_THREADED_OPTIMIZATIONS: string;
		CSF_XmlOcafResource: string;
		npm_lifecycle_script: string;
		NVM_DIR: string;
		CSF_SHMessage: string;
		npm_package_optional: string;
		XDG_ACTIVATION_TOKEN: string;
		npm_config_npm_version: string;
		OPCODE6DIR: string;
		MAKEFLAGS: string;
		XDG_SESSION_CLASS: string;
		ANDROID_HOME: string;
		TERM: string;
		LC_IDENTIFICATION: string;
		npm_package_name: string;
		LESS_TERMCAP_mb: string;
		ZSH: string;
		LESS_TERMCAP_me: string;
		LESS_TERMCAP_md: string;
		GTK_OVERLAY_SCROLLING: string;
		npm_config_prefix: string;
		USER: string;
		SSH_ASKPASS_REQUIRE: string;
		CUDA_PATH: string;
		CSF_StandardDefaults: string;
		CSF_IGESDefaults: string;
		VISUAL: string;
		CSSTRNGS: string;
		SUDO_ASKPASS: string;
		CORE_API_KEY: string;
		DISPLAY: string;
		CSF_XCAFDefaults: string;
		npm_lifecycle_event: string;
		LESS_TERMCAP_ue: string;
		SHLVL: string;
		NVM_CD_FLAGS: string;
		LESS_TERMCAP_us: string;
		GIT_EDITOR: string;
		PAGER: string;
		LC_TELEPHONE: string;
		ANDROID_SDK_ROOT: string;
		_P9K_SSH_TTY: string;
		LC_MEASUREMENT: string;
		XDG_VTNR: string;
		CSF_PluginDefaults: string;
		CSF_TObjMessage: string;
		XDG_SESSION_ID: string;
		npm_config_user_agent: string;
		CASROOT: string;
		DIRHISTORY_SIZE: string;
		GST_PLUGIN_PATH: string;
		OTEL_EXPORTER_OTLP_METRICS_TEMPORALITY_PREFERENCE: string;
		npm_execpath: string;
		LD_LIBRARY_PATH: string;
		XDG_RUNTIME_DIR: string;
		FZF_BASE: string;
		CLAUDE_CODE_ENTRYPOINT: string;
		NVCC_CCBIN: string;
		DEBUGINFOD_URLS: string;
		npm_package_json: string;
		LC_TIME: string;
		P9K_SSH: string;
		ANDROID_NDK_ROOT: string;
		CUDA_HOME: string;
		CSF_XSMessage: string;
		UV_THREADPOOL_SIZE: string;
		__SHELL_SECRETS_LOADED: string;
		XCURSOR_THEME: string;
		GTK3_MODULES: string;
		MMGT_CLEAR: string;
		XDG_DATA_DIRS: string;
		npm_config_noproxy: string;
		PATH: string;
		CSF_TObjDefaults: string;
		ZCCCFG: string;
		npm_config_node_gyp: string;
		HISTIGNORE: string;
		GDMSESSION: string;
		RAYON_NUM_THREADS: string;
		CFLAGS: string;
		DBUS_SESSION_BUS_ADDRESS: string;
		CMAKE_CUDA_ARCHITECTURES: string;
		FZF_DEFAULT_OPTS: string;
		npm_config_global_prefix: string;
		HG: string;
		NVM_BIN: string;
		MAIL: string;
		CLAUDE_CODE_SESSION_ACCESS_TOKEN: string;
		QT_FONT_DPI: string;
		QT_SCALE_FACTOR: string;
		DRAWDEFAULT: string;
		npm_node_execpath: string;
		LC_NUMERIC: string;
		OLDPWD: string;
		CUDAFLAGS: string;
		npm_package_engines_node: string;
		CUDA_ARCHITECTURES: string;
		[key: `PUBLIC_${string}`]: undefined;
		[key: `${string}`]: string | undefined;
	}
}

/**
 * Similar to [`$env/dynamic/private`](https://kit.svelte.dev/docs/modules#$env-dynamic-private), but only includes variables that begin with [`config.kit.env.publicPrefix`](https://kit.svelte.dev/docs/configuration#env) (which defaults to `PUBLIC_`), and can therefore safely be exposed to client-side code.
 * 
 * Note that public dynamic environment variables must all be sent from the server to the client, causing larger network requests — when possible, use `$env/static/public` instead.
 * 
 * ```ts
 * import { env } from '$env/dynamic/public';
 * console.log(env.PUBLIC_DEPLOYMENT_SPECIFIC_VARIABLE);
 * ```
 */
declare module '$env/dynamic/public' {
	export const env: {
		[key: `PUBLIC_${string}`]: string | undefined;
	}
}
