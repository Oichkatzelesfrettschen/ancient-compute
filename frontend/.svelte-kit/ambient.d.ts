
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
	export const SESSION_MANAGER: string;
	export const COLORTERM: string;
	export const GTK_THEME: string;
	export const SHELL_CONFIG: string;
	export const LESS: string;
	export const XDG_SESSION_PATH: string;
	export const NVM_INC: string;
	export const HISTCONTROL: string;
	export const npm_config_npm_globalconfig: string;
	export const MOZ_X11_EGL: string;
	export const _P9K_TTY: string;
	export const NODE: string;
	export const MAKE_TERMOUT: string;
	export const LESS_TERMCAP_se: string;
	export const LESS_TERMCAP_so: string;
	export const LC_ADDRESS: string;
	export const Z80_OZFILES: string;
	export const VDPAU_DRIVER: string;
	export const LC_NAME: string;
	export const npm_config_verify_deps_before_run: string;
	export const P9K_TTY: string;
	export const TORCH_NVCC_FLAGS: string;
	export const npm_config__jsr_registry: string;
	export const _POSIX2_VERSION: string;
	export const LARCH_PATH: string;
	export const LIBVA_DRIVER_NAME: string;
	export const DESKTOP_SESSION: string;
	export const LC_MONETARY: string;
	export const TORCH_CUDA_ARCH_LIST: string;
	export const npm_config_reporter: string;
	export const npm_config_globalconfig: string;
	export const XCURSOR_SIZE: string;
	export const EDITOR: string;
	export const GTK_MODULES: string;
	export const XDG_SEAT: string;
	export const MATE_DESKTOP_SESSION_ID: string;
	export const PWD: string;
	export const LOGNAME: string;
	export const XDG_SESSION_DESKTOP: string;
	export const QT_QPA_PLATFORMTHEME: string;
	export const XDG_SESSION_TYPE: string;
	export const MANPATH: string;
	export const NODE_ENV: string;
	export const CXXFLAGS: string;
	export const _: string;
	export const XAUTHORITY: string;
	export const RAWWAVE_PATH: string;
	export const FZF_DEFAULT_COMMAND: string;
	export const XDG_GREETER_DATA_DIR: string;
	export const QT_STYLE_OVERRIDE: string;
	export const MOTD_SHOWN: string;
	export const LD_PRELOAD: string;
	export const GITHUB_COPILOT_CLI_MODE: string;
	export const GDM_LANG: string;
	export const CUDAARCHS: string;
	export const LDFLAGS: string;
	export const HOME: string;
	export const LCLIMPORTDIR: string;
	export const SSH_ASKPASS: string;
	export const LC_PAPER: string;
	export const LANG: string;
	export const LS_COLORS: string;
	export const XDG_CURRENT_DESKTOP: string;
	export const npm_package_version: string;
	export const DOTFILES_CONFIG: string;
	export const VTE_VERSION: string;
	export const XDG_SEAT_PATH: string;
	export const pnpm_config_verify_deps_before_run: string;
	export const INIT_CWD: string;
	export const MOZ_DISABLE_RDD_SANDBOX: string;
	export const MFLAGS: string;
	export const npm_lifecycle_script: string;
	export const NVM_DIR: string;
	export const XDG_ACTIVATION_TOKEN: string;
	export const OPCODE6DIR: string;
	export const XDG_SESSION_CLASS: string;
	export const MAKEFLAGS: string;
	export const ANDROID_HOME: string;
	export const LC_IDENTIFICATION: string;
	export const TERM: string;
	export const npm_package_name: string;
	export const LESS_TERMCAP_mb: string;
	export const ZSH: string;
	export const LESS_TERMCAP_me: string;
	export const LESS_TERMCAP_md: string;
	export const GTK_OVERLAY_SCROLLING: string;
	export const USER: string;
	export const npm_config_frozen_lockfile: string;
	export const SSH_ASKPASS_REQUIRE: string;
	export const MAKE_TERMERR: string;
	export const CUDA_PATH: string;
	export const VISUAL: string;
	export const CSSTRNGS: string;
	export const SUDO_ASKPASS: string;
	export const DISPLAY: string;
	export const npm_lifecycle_event: string;
	export const SHLVL: string;
	export const LESS_TERMCAP_ue: string;
	export const NVM_CD_FLAGS: string;
	export const LESS_TERMCAP_us: string;
	export const PAGER: string;
	export const MAKELEVEL: string;
	export const LC_TELEPHONE: string;
	export const ANDROID_SDK_ROOT: string;
	export const LC_MEASUREMENT: string;
	export const _P9K_SSH_TTY: string;
	export const XDG_VTNR: string;
	export const XDG_SESSION_ID: string;
	export const npm_config_user_agent: string;
	export const DIRHISTORY_SIZE: string;
	export const PNPM_SCRIPT_SRC_DIR: string;
	export const npm_execpath: string;
	export const TILIX_ID: string;
	export const LD_LIBRARY_PATH: string;
	export const XDG_RUNTIME_DIR: string;
	export const FZF_BASE: string;
	export const MKLROOT: string;
	export const DEBUGINFOD_URLS: string;
	export const NVCC_CCBIN: string;
	export const npm_package_json: string;
	export const DOCKER_HOST: string;
	export const LC_TIME: string;
	export const P9K_SSH: string;
	export const CUDA_HOME: string;
	export const XCURSOR_THEME: string;
	export const GTK3_MODULES: string;
	export const XDG_DATA_DIRS: string;
	export const PATH: string;
	export const ZCCCFG: string;
	export const npm_config_node_gyp: string;
	export const HISTIGNORE: string;
	export const GDMSESSION: string;
	export const CFLAGS: string;
	export const DBUS_SESSION_BUS_ADDRESS: string;
	export const CMAKE_CUDA_ARCHITECTURES: string;
	export const FZF_DEFAULT_OPTS: string;
	export const HG: string;
	export const npm_package_engines_pnpm: string;
	export const NVM_BIN: string;
	export const MAIL: string;
	export const npm_config_registry: string;
	export const QT_FONT_DPI: string;
	export const QT_SCALE_FACTOR: string;
	export const GIO_LAUNCHED_DESKTOP_FILE_PID: string;
	export const npm_node_execpath: string;
	export const LC_NUMERIC: string;
	export const OLDPWD: string;
	export const CUDAFLAGS: string;
	export const npm_package_engines_node: string;
	export const CUDA_ARCHITECTURES: string;
	export const TEST: string;
	export const VITEST: string;
	export const PROD: string;
	export const DEV: string;
	export const BASE_URL: string;
	export const MODE: string;
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
		SESSION_MANAGER: string;
		COLORTERM: string;
		GTK_THEME: string;
		SHELL_CONFIG: string;
		LESS: string;
		XDG_SESSION_PATH: string;
		NVM_INC: string;
		HISTCONTROL: string;
		npm_config_npm_globalconfig: string;
		MOZ_X11_EGL: string;
		_P9K_TTY: string;
		NODE: string;
		MAKE_TERMOUT: string;
		LESS_TERMCAP_se: string;
		LESS_TERMCAP_so: string;
		LC_ADDRESS: string;
		Z80_OZFILES: string;
		VDPAU_DRIVER: string;
		LC_NAME: string;
		npm_config_verify_deps_before_run: string;
		P9K_TTY: string;
		TORCH_NVCC_FLAGS: string;
		npm_config__jsr_registry: string;
		_POSIX2_VERSION: string;
		LARCH_PATH: string;
		LIBVA_DRIVER_NAME: string;
		DESKTOP_SESSION: string;
		LC_MONETARY: string;
		TORCH_CUDA_ARCH_LIST: string;
		npm_config_reporter: string;
		npm_config_globalconfig: string;
		XCURSOR_SIZE: string;
		EDITOR: string;
		GTK_MODULES: string;
		XDG_SEAT: string;
		MATE_DESKTOP_SESSION_ID: string;
		PWD: string;
		LOGNAME: string;
		XDG_SESSION_DESKTOP: string;
		QT_QPA_PLATFORMTHEME: string;
		XDG_SESSION_TYPE: string;
		MANPATH: string;
		NODE_ENV: string;
		CXXFLAGS: string;
		_: string;
		XAUTHORITY: string;
		RAWWAVE_PATH: string;
		FZF_DEFAULT_COMMAND: string;
		XDG_GREETER_DATA_DIR: string;
		QT_STYLE_OVERRIDE: string;
		MOTD_SHOWN: string;
		LD_PRELOAD: string;
		GITHUB_COPILOT_CLI_MODE: string;
		GDM_LANG: string;
		CUDAARCHS: string;
		LDFLAGS: string;
		HOME: string;
		LCLIMPORTDIR: string;
		SSH_ASKPASS: string;
		LC_PAPER: string;
		LANG: string;
		LS_COLORS: string;
		XDG_CURRENT_DESKTOP: string;
		npm_package_version: string;
		DOTFILES_CONFIG: string;
		VTE_VERSION: string;
		XDG_SEAT_PATH: string;
		pnpm_config_verify_deps_before_run: string;
		INIT_CWD: string;
		MOZ_DISABLE_RDD_SANDBOX: string;
		MFLAGS: string;
		npm_lifecycle_script: string;
		NVM_DIR: string;
		XDG_ACTIVATION_TOKEN: string;
		OPCODE6DIR: string;
		XDG_SESSION_CLASS: string;
		MAKEFLAGS: string;
		ANDROID_HOME: string;
		LC_IDENTIFICATION: string;
		TERM: string;
		npm_package_name: string;
		LESS_TERMCAP_mb: string;
		ZSH: string;
		LESS_TERMCAP_me: string;
		LESS_TERMCAP_md: string;
		GTK_OVERLAY_SCROLLING: string;
		USER: string;
		npm_config_frozen_lockfile: string;
		SSH_ASKPASS_REQUIRE: string;
		MAKE_TERMERR: string;
		CUDA_PATH: string;
		VISUAL: string;
		CSSTRNGS: string;
		SUDO_ASKPASS: string;
		DISPLAY: string;
		npm_lifecycle_event: string;
		SHLVL: string;
		LESS_TERMCAP_ue: string;
		NVM_CD_FLAGS: string;
		LESS_TERMCAP_us: string;
		PAGER: string;
		MAKELEVEL: string;
		LC_TELEPHONE: string;
		ANDROID_SDK_ROOT: string;
		LC_MEASUREMENT: string;
		_P9K_SSH_TTY: string;
		XDG_VTNR: string;
		XDG_SESSION_ID: string;
		npm_config_user_agent: string;
		DIRHISTORY_SIZE: string;
		PNPM_SCRIPT_SRC_DIR: string;
		npm_execpath: string;
		TILIX_ID: string;
		LD_LIBRARY_PATH: string;
		XDG_RUNTIME_DIR: string;
		FZF_BASE: string;
		MKLROOT: string;
		DEBUGINFOD_URLS: string;
		NVCC_CCBIN: string;
		npm_package_json: string;
		DOCKER_HOST: string;
		LC_TIME: string;
		P9K_SSH: string;
		CUDA_HOME: string;
		XCURSOR_THEME: string;
		GTK3_MODULES: string;
		XDG_DATA_DIRS: string;
		PATH: string;
		ZCCCFG: string;
		npm_config_node_gyp: string;
		HISTIGNORE: string;
		GDMSESSION: string;
		CFLAGS: string;
		DBUS_SESSION_BUS_ADDRESS: string;
		CMAKE_CUDA_ARCHITECTURES: string;
		FZF_DEFAULT_OPTS: string;
		HG: string;
		npm_package_engines_pnpm: string;
		NVM_BIN: string;
		MAIL: string;
		npm_config_registry: string;
		QT_FONT_DPI: string;
		QT_SCALE_FACTOR: string;
		GIO_LAUNCHED_DESKTOP_FILE_PID: string;
		npm_node_execpath: string;
		LC_NUMERIC: string;
		OLDPWD: string;
		CUDAFLAGS: string;
		npm_package_engines_node: string;
		CUDA_ARCHITECTURES: string;
		TEST: string;
		VITEST: string;
		PROD: string;
		DEV: string;
		BASE_URL: string;
		MODE: string;
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
