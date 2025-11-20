// Custom language definitions for Monaco Editor
import type * as Monaco from 'monaco-editor';

export function registerLanguages(monaco: typeof Monaco) {
  // Register LISP language
  monaco.languages.register({ id: 'lisp' });
  monaco.languages.setMonarchTokensProvider('lisp', {
    defaultToken: '',
    brackets: [
      { open: '(', close: ')', token: 'delimiter.parenthesis' },
      { open: '[', close: ']', token: 'delimiter.bracket' },
    ],
    keywords: [
      'define', 'lambda', 'if', 'cond', 'case', 'let', 'let*', 'letrec',
      'begin', 'do', 'quote', 'unquote', 'quasiquote', 'unquote-splicing',
      'defun', 'defmacro', 'defvar', 'defparameter', 'defconstant',
      'setq', 'setf', 'progn', 'prog1', 'prog2', 'loop', 'return',
      'go', 'throw', 'catch', 'unwind-protect', 'declare', 'function',
      'apply', 'funcall', 'eval', 'compile', 'load'
    ],
    builtins: [
      'car', 'cdr', 'cons', 'list', 'append', 'reverse', 'length',
      'nth', 'first', 'rest', 'last', 'butlast', 'member', 'assoc',
      'map', 'mapcar', 'filter', 'reduce', 'fold', 'fold-left', 'fold-right',
      'null', 'null?', 'atom', 'atom?', 'listp', 'list?', 'numberp', 'number?',
      'zerop', 'zero?', 'plusp', 'minusp', 'evenp', 'oddp',
      'eq', 'eql', 'equal', 'equalp', '=', '/=', '<', '>', '<=', '>=',
      '+', '-', '*', '/', 'mod', 'rem', 'abs', 'min', 'max',
      'sin', 'cos', 'tan', 'asin', 'acos', 'atan', 'exp', 'log', 'sqrt',
      'print', 'prin1', 'princ', 'format', 'read', 'read-line',
      'open', 'close', 'with-open-file'
    ],
    operators: ['+', '-', '*', '/', '=', '<', '>', '<=', '>=', '/='],
    symbols: /[=><!~?:&|+\-*\/\^%]+/,
    escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,
    tokenizer: {
      root: [
        [/[a-zA-Z_$][\w$]*/, {
          cases: {
            '@keywords': 'keyword',
            '@builtins': 'support.function',
            '@default': 'identifier'
          }
        }],
        [/;.*$/, 'comment'],
        [/"([^"\\]|\\.)*$/, 'string.invalid'],
        [/"/, 'string', '@string'],
        [/'[^']*'/, 'string'],
        [/\d+/, 'number'],
        [/\d*\.\d+([eE][\-+]?\d+)?/, 'number.float'],
        [/[()]/, '@brackets'],
        [/@symbols/, {
          cases: {
            '@operators': 'operator',
            '@default': ''
          }
        }],
      ],
      string: [
        [/[^\\"]+/, 'string'],
        [/@escapes/, 'string.escape'],
        [/\\./, 'string.escape.invalid'],
        [/"/, 'string', '@pop']
      ],
    }
  });

  // Register IDRIS2 language
  monaco.languages.register({ id: 'idris2' });
  monaco.languages.setMonarchTokensProvider('idris2', {
    defaultToken: '',
    keywords: [
      'data', 'where', 'module', 'import', 'export', 'public', 'private',
      'infixl', 'infixr', 'infix', 'total', 'partial', 'covering',
      'if', 'then', 'else', 'case', 'of', 'let', 'in', 'do', 'with',
      'namespace', 'parameters', 'mutual', 'interface', 'implementation',
      'using', 'rewrite', 'auto', 'impossible', 'implicit', 'default',
      'forall', 'record', 'constructor', 'assert_total', 'assert_smaller',
      'believe_me', 'really_believe_me', 'syntax', 'dsl', 'proof', 'tactics',
      'intros', 'intro', 'exact', 'refine', 'trivial', 'solve', 'attack',
      'try', 'compute', 'unfold', 'search', 'instance'
    ],
    typeKeywords: [
      'Type', 'Int', 'Integer', 'Nat', 'Double', 'Char', 'String', 'Bool',
      'Maybe', 'Either', 'List', 'Vect', 'Stream', 'IO', 'Fin', 'Pair',
      'DPair', 'Sigma', 'Dec', 'So', 'Lazy', 'Inf', 'Force', 'Delay'
    ],
    operators: [
      '=', '<', '>', ':', '::', '\\', '|', '=>', '->', '<-',
      '@', '~', '+', '-', '*', '/', '%', '**', '++', '&&', '||',
      '==', '/=', '<=', '>=', '<$>', '<*>', '>>=', '>>', '<<<', '>>>',
      '.', '$', '!', '?', ';'
    ],
    symbols: /[=><!~?:&|+\-*\/\^%]+/,
    escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,
    tokenizer: {
      root: [
        [/[a-z_$][\w$]*/, {
          cases: {
            '@typeKeywords': 'type',
            '@keywords': 'keyword',
            '@default': 'identifier'
          }
        }],
        [/[A-Z][\w\$]*/, 'type.identifier'],
        [/--.*$/, 'comment'],
        [/\{-/, 'comment', '@comment'],
        [/"([^"\\]|\\.)*$/, 'string.invalid'],
        [/"/, 'string', '@string'],
        [/'[^\\']'/, 'string'],
        [/'/, 'string', '@litstring'],
        [/0[xX][0-9a-fA-F]+/, 'number.hex'],
        [/\d*\.\d+([eE][\-+]?\d+)?/, 'number.float'],
        [/\d+/, 'number'],
        [/[{}()\[\]]/, '@brackets'],
        [/@symbols/, {
          cases: {
            '@operators': 'operator',
            '@default': ''
          }
        }],
      ],
      comment: [
        [/[^\{\-]+/, 'comment'],
        [/\{-/, 'comment', '@push'],
        [/-\}/, 'comment', '@pop'],
        [/[\{\-]/, 'comment']
      ],
      string: [
        [/[^\\"]+/, 'string'],
        [/@escapes/, 'string.escape'],
        [/\\./, 'string.escape.invalid'],
        [/"/, 'string', '@pop']
      ],
      litstring: [
        [/[^\\']+/, 'string'],
        [/@escapes/, 'string.escape'],
        [/\\./, 'string.escape.invalid'],
        [/'/, 'string', '@pop']
      ],
    }
  });

  // Register System F language
  monaco.languages.register({ id: 'systemf' });
  monaco.languages.setMonarchTokensProvider('systemf', {
    defaultToken: '',
    keywords: [
      'forall', 'lambda', 'Lambda', 'let', 'in', 'if', 'then', 'else',
      'fix', 'case', 'of', 'type', 'data', 'where', 'rec', 'as'
    ],
    typeKeywords: [
      'Type', 'Kind', 'Int', 'Bool', 'Unit', 'Void', 'String', 'Char',
      'Float', 'Double', 'List', 'Array', 'Option', 'Result', 'Function'
    ],
    operators: [
      '=', '<', '>', ':', '::', '\\', '|', '=>', '->', '<-', '.',
      '@', '~', '+', '-', '*', '/', '%', '&&', '||', '!', '?',
      '∀', 'λ', 'Λ', '→', '⇒', '∧', '∨', '¬', '∃', '⊤', '⊥'
    ],
    symbols: /[=><!~?:&|+\-*\/\^%]+/,
    escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4}|u[0-9A-Fa-f]{4}|U[0-9A-Fa-f]{8})/,
    unicode: /[∀λΛ→⇒∧∨¬∃⊤⊥]/,
    tokenizer: {
      root: [
        [/[a-z_$][\w$]*/, {
          cases: {
            '@typeKeywords': 'type',
            '@keywords': 'keyword',
            '@default': 'identifier'
          }
        }],
        [/[A-Z][\w\$]*/, 'type.identifier'],
        [/--.*$/, 'comment'],
        [/\/\*/, 'comment', '@comment'],
        [/"([^"\\]|\\.)*$/, 'string.invalid'],
        [/"/, 'string', '@string'],
        [/\d*\.\d+([eE][\-+]?\d+)?/, 'number.float'],
        [/0[xX][0-9a-fA-F]+/, 'number.hex'],
        [/\d+/, 'number'],
        [/[{}()\[\]]/, '@brackets'],
        [/@unicode/, 'operator.unicode'],
        [/@symbols/, {
          cases: {
            '@operators': 'operator',
            '@default': ''
          }
        }],
      ],
      comment: [
        [/[^\/*]+/, 'comment'],
        [/\/\*/, 'comment', '@push'],
        [/\*\//, 'comment', '@pop'],
        [/[\/*]/, 'comment']
      ],
      string: [
        [/[^\\"]+/, 'string'],
        [/@escapes/, 'string.escape'],
        [/\\./, 'string.escape.invalid'],
        [/"/, 'string', '@pop']
      ],
    }
  });

  // Register Babbage Assembly language
  monaco.languages.register({ id: 'babbage-asm' });
  monaco.languages.setMonarchTokensProvider('babbage-asm', {
    defaultToken: '',
    ignoreCase: true,
    keywords: [
      'section', 'global', 'extern', 'segment', 'proc', 'endp',
      'macro', 'endm', 'include', 'org', 'align', 'db', 'dw', 'dd', 'dq',
      'resb', 'resw', 'resd', 'resq', 'equ', 'times', 'struc', 'endstruc'
    ],
    instructions: [
      // Babbage ISA Instructions
      'mov', 'add', 'sub', 'mul', 'div', 'mod', 'and', 'or', 'xor', 'not',
      'shl', 'shr', 'rol', 'ror', 'cmp', 'test', 'jmp', 'je', 'jne', 'jg',
      'jge', 'jl', 'jle', 'jz', 'jnz', 'js', 'jns', 'jo', 'jno', 'jc', 'jnc',
      'call', 'ret', 'push', 'pop', 'ld', 'st', 'ldi', 'sti', 'in', 'out',
      'hlt', 'nop', 'int', 'iret', 'enter', 'leave', 'pusha', 'popa',
      // Analytical Engine specific
      'store', 'load', 'loadc', 'storec', 'cf', 'cb', 'cond', 'ncond',
      'advance', 'back', 'bell', 'print', 'punch', 'readcard',
      'mill', 'store_to_mill', 'load_from_mill', 'barrel', 'rack',
      'variable_card', 'number_card', 'operation_card', 'combinatorial_card'
    ],
    registers: [
      // Babbage Engine registers
      'ingress', 'egress', 'mill', 'store', 'rack', 'barrel',
      'axis', 'wheel', 'sector', 'cage', 'lever', 'cam',
      'v0', 'v1', 'v2', 'v3', 'v4', 'v5', 'v6', 'v7', 'v8', 'v9',
      'v10', 'v11', 'v12', 'v13', 'v14', 'v15', 'v16', 'v17', 'v18', 'v19',
      // Control registers
      'pc', 'sp', 'bp', 'flags', 'cycle', 'state'
    ],
    operators: [
      '+', '-', '*', '/', '%', '<<', '>>', '&', '|', '^', '~',
      '=', '!=', '<', '>', '<=', '>=', '[', ']', ',', ':'
    ],
    symbols: /[=><!~?:&|+\-*\/\^%]+/,
    escapes: /\\(?:[abfnrtv\\"']|x[0-9A-Fa-f]{1,4})/,
    tokenizer: {
      root: [
        [/[a-z_$][\w$]*/, {
          cases: {
            '@keywords': 'keyword',
            '@instructions': 'keyword.control',
            '@registers': 'variable.predefined',
            '@default': 'identifier'
          }
        }],
        [/[A-Z_$][\w$]*/, 'constant'],
        [/;.*$/, 'comment'],
        [/#.*$/, 'comment.doc'],
        [/"([^"\\]|\\.)*$/, 'string.invalid'],
        [/"/, 'string', '@string'],
        [/'[^\\']'/, 'string.char'],
        [/'/, 'string.char', '@char'],
        [/0[xX][0-9a-fA-F]+/, 'number.hex'],
        [/0[bB][01]+/, 'number.binary'],
        [/0[oO][0-7]+/, 'number.octal'],
        [/\d+/, 'number'],
        [/\$[0-9a-fA-F]+/, 'number.hex'],
        [/@/, 'operator'],
        [/[{}()\[\]]/, '@brackets'],
        [/@symbols/, {
          cases: {
            '@operators': 'operator',
            '@default': ''
          }
        }],
      ],
      string: [
        [/[^\\"]+/, 'string'],
        [/@escapes/, 'string.escape'],
        [/\\./, 'string.escape.invalid'],
        [/"/, 'string', '@pop']
      ],
      char: [
        [/[^\\']+/, 'string.char'],
        [/@escapes/, 'string.escape'],
        [/\\./, 'string.escape.invalid'],
        [/'/, 'string.char', '@pop']
      ],
    }
  });

  // Configure language features for better IntelliSense
  const commonLanguageConfig: Monaco.languages.LanguageConfiguration = {
    comments: {
      lineComment: '//',
      blockComment: ['/*', '*/']
    },
    brackets: [
      ['{', '}'],
      ['[', ']'],
      ['(', ')']
    ],
    autoClosingPairs: [
      { open: '{', close: '}' },
      { open: '[', close: ']' },
      { open: '(', close: ')' },
      { open: '"', close: '"' },
      { open: "'", close: "'" },
    ],
    surroundingPairs: [
      { open: '{', close: '}' },
      { open: '[', close: ']' },
      { open: '(', close: ')' },
      { open: '"', close: '"' },
      { open: "'", close: "'" },
    ],
  };

  // LISP specific configuration
  monaco.languages.setLanguageConfiguration('lisp', {
    comments: {
      lineComment: ';',
    },
    brackets: [
      ['(', ')'],
      ['[', ']'],
    ],
    autoClosingPairs: [
      { open: '(', close: ')' },
      { open: '[', close: ']' },
      { open: '"', close: '"' },
    ],
    surroundingPairs: [
      { open: '(', close: ')' },
      { open: '[', close: ']' },
      { open: '"', close: '"' },
    ],
  });

  // IDRIS2 specific configuration
  monaco.languages.setLanguageConfiguration('idris2', {
    comments: {
      lineComment: '--',
      blockComment: ['{-', '-}']
    },
    brackets: [
      ['{', '}'],
      ['[', ']'],
      ['(', ')']
    ],
    autoClosingPairs: [
      { open: '{', close: '}' },
      { open: '[', close: ']' },
      { open: '(', close: ')' },
      { open: '{-', close: '-}' },
      { open: '"', close: '"' },
      { open: "'", close: "'" },
    ],
    surroundingPairs: [
      { open: '{', close: '}' },
      { open: '[', close: ']' },
      { open: '(', close: ')' },
      { open: '"', close: '"' },
      { open: "'", close: "'" },
    ],
    indentationRules: {
      increaseIndentPattern: /(\bwhere\b|\blet\b|\bdo\b|\bof\b|\bthen\b|\belse\b|\{)$/,
      decreaseIndentPattern: /^\s*(\}|\]|\)|where|in|else)/
    }
  });

  // System F configuration
  monaco.languages.setLanguageConfiguration('systemf', commonLanguageConfig);

  // Babbage Assembly configuration
  monaco.languages.setLanguageConfiguration('babbage-asm', {
    comments: {
      lineComment: ';',
    },
    brackets: [
      ['[', ']'],
    ],
    autoClosingPairs: [
      { open: '[', close: ']' },
      { open: '"', close: '"' },
      { open: "'", close: "'" },
    ],
    surroundingPairs: [
      { open: '[', close: ']' },
      { open: '"', close: '"' },
      { open: "'", close: "'" },
    ],
  });

  // Register completion providers for better autocomplete
  registerCompletionProviders(monaco);
}

function registerCompletionProviders(monaco: typeof Monaco) {
  // LISP completion provider
  monaco.languages.registerCompletionItemProvider('lisp', {
    provideCompletionItems: (model, position) => {
      const suggestions: Monaco.languages.CompletionItem[] = [
        {
          label: 'defun',
          kind: monaco.languages.CompletionItemKind.Snippet,
          insertText: 'defun ${1:name} (${2:args})\n  ${3:body}',
          insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
          documentation: 'Define a function'
        },
        {
          label: 'lambda',
          kind: monaco.languages.CompletionItemKind.Snippet,
          insertText: 'lambda (${1:args}) ${2:body}',
          insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
          documentation: 'Create an anonymous function'
        },
        {
          label: 'if',
          kind: monaco.languages.CompletionItemKind.Snippet,
          insertText: 'if ${1:condition}\n    ${2:then}\n    ${3:else}',
          insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
          documentation: 'Conditional expression'
        },
      ];
      return { suggestions };
    }
  });

  // IDRIS2 completion provider
  monaco.languages.registerCompletionItemProvider('idris2', {
    provideCompletionItems: (model, position) => {
      const suggestions: Monaco.languages.CompletionItem[] = [
        {
          label: 'data',
          kind: monaco.languages.CompletionItemKind.Snippet,
          insertText: 'data ${1:TypeName} : ${2:Type} where\n  ${3:Constructor} : ${4:Type}',
          insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
          documentation: 'Define a data type'
        },
        {
          label: 'interface',
          kind: monaco.languages.CompletionItemKind.Snippet,
          insertText: 'interface ${1:InterfaceName} ${2:a} where\n  ${3:method} : ${4:Type}',
          insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
          documentation: 'Define an interface'
        },
        {
          label: 'implementation',
          kind: monaco.languages.CompletionItemKind.Snippet,
          insertText: 'implementation ${1:InterfaceName} ${2:Type} where\n  ${3:method} = ${4:definition}',
          insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet,
          documentation: 'Implement an interface'
        },
      ];
      return { suggestions };
    }
  });
}