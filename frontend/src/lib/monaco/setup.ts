// Monaco Editor setup with Web Workers for SvelteKit
import type * as Monaco from 'monaco-editor';

// Import workers with Vite's ?worker suffix
import editorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker';
import jsonWorker from 'monaco-editor/esm/vs/language/json/json.worker?worker';
import cssWorker from 'monaco-editor/esm/vs/language/css/css.worker?worker';
import htmlWorker from 'monaco-editor/esm/vs/language/html/html.worker?worker';
import tsWorker from 'monaco-editor/esm/vs/language/typescript/ts.worker?worker';

export function setupMonaco(monaco: typeof Monaco) {
  // Configure Monaco Environment for Web Workers
  if (typeof window !== 'undefined') {
    (self as any).MonacoEnvironment = {
      getWorker: function (_: any, label: string) {
        switch (label) {
          case 'json':
            return new jsonWorker();
          case 'css':
          case 'scss':
          case 'less':
            return new cssWorker();
          case 'html':
          case 'handlebars':
          case 'razor':
            return new htmlWorker();
          case 'typescript':
          case 'javascript':
            return new tsWorker();
          default:
            return new editorWorker();
        }
      }
    };
  }

  // Configure default editor options
  monaco.editor.defineTheme('ancient-dark', {
    base: 'vs-dark',
    inherit: true,
    rules: [
      { token: 'comment', foreground: '608b4e', fontStyle: 'italic' },
      { token: 'keyword', foreground: '569cd6' },
      { token: 'string', foreground: 'ce9178' },
      { token: 'number', foreground: 'b5cea8' },
      { token: 'regexp', foreground: 'd16969' },
      { token: 'operator', foreground: 'd4d4d4' },
      { token: 'namespace', foreground: '4ec9b0' },
      { token: 'type', foreground: '4ec9b0' },
      { token: 'struct', foreground: '4ec9b0' },
      { token: 'class', foreground: '4ec9b0' },
      { token: 'interface', foreground: 'b8d7a3' },
      { token: 'enum', foreground: 'b8d7a3' },
      { token: 'typeParameter', foreground: 'b8d7a3' },
      { token: 'function', foreground: 'dcdcaa' },
      { token: 'member', foreground: '9cdcfe' },
      { token: 'macro', foreground: '569cd6' },
      { token: 'variable', foreground: '9cdcfe' },
      { token: 'parameter', foreground: '9cdcfe' },
      { token: 'property', foreground: '9cdcfe' },
      { token: 'label', foreground: 'c586c0' },
      { token: 'constant', foreground: '4fc1ff' },
      { token: 'punctuation', foreground: 'd4d4d4' },
      { token: 'punctuation.tag', foreground: '808080' },
      { token: 'tag', foreground: '569cd6' },
      { token: 'attribute.name', foreground: '9cdcfe' },
      { token: 'attribute.value', foreground: 'ce9178' },
      { token: 'meta', foreground: '9b9b9b' },
    ],
    colors: {
      'editor.background': '#1e1e1e',
      'editor.foreground': '#d4d4d4',
      'editor.lineHighlightBackground': '#2a2a2a',
      'editor.selectionBackground': '#264f78',
      'editor.inactiveSelectionBackground': '#3a3d41',
      'editorLineNumber.foreground': '#858585',
      'editorCursor.foreground': '#aeafad',
      'editorWhitespace.foreground': '#3b3b3b',
      'editorIndentGuide.background': '#404040',
      'editorIndentGuide.activeBackground': '#707070',
    }
  });

  // Configure language defaults for better IntelliSense
  monaco.languages.typescript.javascriptDefaults.setDiagnosticsOptions({
    noSemanticValidation: false,
    noSyntaxValidation: false,
  });

  monaco.languages.typescript.javascriptDefaults.setCompilerOptions({
    target: monaco.languages.typescript.ScriptTarget.ES2020,
    allowNonTsExtensions: true,
    moduleResolution: monaco.languages.typescript.ModuleResolutionKind.NodeJs,
    module: monaco.languages.typescript.ModuleKind.ESNext,
    allowJs: true,
    checkJs: true,
  });

  monaco.languages.typescript.typescriptDefaults.setDiagnosticsOptions({
    noSemanticValidation: false,
    noSyntaxValidation: false,
  });

  monaco.languages.typescript.typescriptDefaults.setCompilerOptions({
    target: monaco.languages.typescript.ScriptTarget.ES2020,
    allowNonTsExtensions: true,
    moduleResolution: monaco.languages.typescript.ModuleResolutionKind.NodeJs,
    module: monaco.languages.typescript.ModuleKind.ESNext,
    noEmit: true,
    esModuleInterop: true,
    jsx: monaco.languages.typescript.JsxEmit.React,
    allowSyntheticDefaultImports: true,
    typeRoots: ['node_modules/@types'],
  });

  // Add some common library types
  const libSource = `
    declare class Console {
      log(...args: any[]): void;
      error(...args: any[]): void;
      warn(...args: any[]): void;
    }
    declare const console: Console;
    declare function setTimeout(handler: Function, timeout?: number): number;
    declare function clearTimeout(handle: number): void;
    declare function setInterval(handler: Function, timeout?: number): number;
    declare function clearInterval(handle: number): void;
  `;

  monaco.languages.typescript.javascriptDefaults.addExtraLib(libSource, 'ts:global.d.ts');
  monaco.languages.typescript.typescriptDefaults.addExtraLib(libSource, 'ts:global.d.ts');

  return monaco;
}