# Option B: Complete Phase 2 Implementation Roadmap

**Status**: Strategic Plan
**Scope**: Phase 2 completion (70% → 85%)
**Duration**: 6-7 weeks (Weeks 9-12)
**Target Lines of Code**: 10,300-12,700 lines
**Test Coverage Target**: 100% (no warnings as errors)
**Completion Date**: Early December 2025

---

## Executive Summary

Option B completes Phase 2 by implementing four additional language services (LISP, IDRIS2, System F, Java) plus comprehensive integration testing. This validates the universal IR architecture across 7 fundamentally different programming paradigms and establishes a robust foundation for Phase 3 (Emulator, I/O, Debugger).

### Key Achievements
- **7 Language Services**: C, Python, Haskell, LISP, IDRIS2, System F, Java
- **Universal IR Validation**: All paradigms compile to identical Babbage IR
- **25,000+ Lines of Production Code**: Complete, tested, documented
- **300+ Tests**: 100% pass rate across all services
- **3 Days of Integration Testing**: Validates cross-language interoperability
- **Phase 2 Completion**: Ready for Phase 3 architecture implementation

---

## Week 9.1: LISP Language Service (2-3 Days)

### Overview
LISP introduces meta-programming capabilities: code-as-data, homoiconicity, macros (simplified), and functional programming with imperative side effects. This validates the IR's ability to handle functional paradigms with mutable state.

### Component Breakdown

#### 1. LISP Lexer (400-500 lines)
**File**: `backend/src/compilers/lisp_lexer.py`

**Tokenization Rules**:
- Parentheses: `(`, `)` as primary syntax
- Atoms: symbols, numbers, strings, keywords
- Quote shorthand: `'expr` → `(quote expr)`
- Comments: `;` for line comments
- Numbers: integers and floats (both positive/negative)
- Strings: double-quoted with escape sequences
- Special keywords: `if`, `let`, `defun`, `quote`, `lambda`

**Token Types** (20+):
```
LPAREN, RPAREN
SYMBOL, NUMBER, STRING, KEYWORD
QUOTE, QUASIQUOTE, UNQUOTE
COMMENT, WHITESPACE
EOF
```

**Key Methods**:
```python
def tokenize(self) -> List[Token]
def _scan_atom(self) -> Token
def _scan_string(self) -> Token
def _scan_number(self) -> float
```

**Test Coverage** (12+ tests):
- Empty source, single atoms
- Lists and nested lists
- Strings with escapes
- Comments (line)
- Numbers (integers and floats)
- Quote/quasiquote/unquote syntax

#### 2. LISP AST (150-200 lines)
**File**: `backend/src/compilers/lisp_ast.py`

**Expression Nodes**:
```python
class Expr:
    pass

class Atom(Expr):  # numbers, strings, symbols
    value: Union[float, str]

class Symbol(Expr):
    name: str

class List(Expr):  # (func args...)
    elements: List[Expr]

class Quote(Expr):
    expr: Expr

class Lambda(Expr):
    parameters: List[str]
    body: Expr

class LetBinding(Expr):
    bindings: List[Tuple[str, Expr]]
    body: Expr

class IfExpr(Expr):
    condition: Expr
    then_expr: Expr
    else_expr: Expr

class BuiltinFunction(Expr):
    name: str  # '+', '-', '*', '/', 'if', 'let', etc.
    args: List[Expr]
```

**Built-in Functions** (20+):
- Arithmetic: `+`, `-`, `*`, `/`, `%`, `^`, `abs`, `sqrt`
- Comparison: `=`, `/=`, `<`, `<=`, `>`, `>=`
- Logic: `and`, `or`, `not`
- List: `cons`, `car`, `cdr`, `list`, `length`
- I/O: `print`, `read`
- Control: `if`, `cond`, `let`, `defun`

#### 3. LISP Parser (550-650 lines)
**File**: `backend/src/compilers/lisp_parser.py`

**Parsing Strategy**: Recursive descent with S-expression handling

**Grammar** (simplified):
```
expr := atom | symbol | list | quote | lambda | let | if
atom := number | string
list := '(' expr* ')'
quote := ''' expr
lambda := '(lambda' '(' symbol* ')' expr ')'
let := '(let' '(' binding* ')' expr ')'
binding := '(' symbol expr ')'
if := '(if' expr expr expr ')'
```

**Key Methods**:
```python
def parse(self) -> List[Expr]
def _parse_expr(self) -> Expr
def _parse_list(self) -> List[Expr]
def _parse_atom(self) -> Atom
def _parse_lambda(self) -> Lambda
def _parse_let(self) -> LetBinding
def _parse_if(self) -> IfExpr
```

**Test Coverage** (12+ tests):
- Atoms and symbols
- Lists (nested)
- Lambda expressions
- Let bindings (single and multiple)
- If expressions
- Quote expressions
- Function calls with arguments

#### 4. LISP Type System (200-250 lines)
**File**: `backend/src/compilers/lisp_types.py`

**Type Representation**:
- Any (untyped - LISP is dynamically typed at compilation)
- Int, Float, String, Bool
- List[T] (homogeneous)
- Function (args → return)

**Type Inference**:
- From literals: `3` → Int, `3.14` → Float, `"hello"` → String
- From operations: `(+ 1 2)` → Int
- From function signature (if declared)
- Default: Any for unknown symbols

**Babbage Mapping**:
- All types → i64 or f64 (no polymorphism at runtime)
- Lists → pointer to memory (simplified)
- Functions → function reference

#### 5. LISP Compiler (600-700 lines)
**File**: `backend/src/compilers/lisp_compiler.py`

**4-Phase Pipeline**:
1. **Lexing**: Source → Tokens
2. **Parsing**: Tokens → AST (List of expressions)
3. **Semantic Analysis**: Macro expansion, function registration, type inference
4. **IR Generation**: AST → Babbage IR

**Key Transformations**:
- `(+ a b)` → BinaryOp(add, a, b)
- `(if cond then else)` → BranchTerminator with true/false labels
- `(let ((x 1)) (+ x 2))` → Sequential assignments
- `(lambda (x) (+ x 1))` → Function reference (MVP: cannot capture free variables)
- `(defun f (x) (+ x 1))` → Function definition with registration

**Compilation Strategy**:
- Single-pass: lex, parse, analyze, generate
- Built-in functions compiled directly (no function calls)
- User-defined functions: create separate IR functions
- Tail recursion: simple recursive calls (no optimization)

**Special Handling**:
- Symbols: resolve in current environment
- Numbers: compile to constants
- Lists: identify as function calls or special forms
- Macros (simplified): quote/unquote only

#### 6. LISP Service (250-300 lines)
**File**: `backend/src/services/languages/lisp_service.py`

**Architecture**: Async FastAPI with thread pool execution

**Endpoints**:
```python
async execute(source: str, timeout_seconds: float = 10.0) -> CompilationResult
async validate(source: str) -> CompilationResult
async get_capabilities() -> dict
```

**Execution Pipeline**:
1. Compilation: LISP → IR (0.5ms target)
2. Code generation: IR → Babbage assembly
3. Assembly: Babbage → machine code
4. Execution: Run with resource limits

**Features**:
- Non-blocking compilation (thread pool)
- Full pipeline error reporting
- Timing breakdown (compile_time_ms, codegen_time_ms, assembly_time_ms)
- Hex dump of machine code

#### 7. Test Suite (700-800 lines)
**File**: `backend/src/compilers/test_lisp_compiler.py`

**Test Classes** (65+ tests):

1. **TestLISPLexer** (12 tests)
   - Empty source, atoms, symbols, numbers
   - Strings with escapes
   - Lists and nested structures
   - Comments
   - Quote/quasiquote syntax

2. **TestLISPParser** (12 tests)
   - Atoms and symbols
   - Lists and function calls
   - Lambda expressions
   - Let bindings
   - If expressions
   - Quote expressions

3. **TestLISPTypeSystem** (15 tests)
   - Type inference from literals
   - Type inference from operations
   - Function types
   - List types

4. **TestLISPCompilerSimple** (10 tests)
   - Arithmetic operations
   - Comparison operations
   - Lambda expressions
   - Let bindings
   - If expressions

5. **TestLISPCompilerIntegration** (10 tests)
   - Recursive functions (factorial)
   - Multiple definitions
   - List operations (car, cdr)
   - Nested let bindings

6. **TestLISPCompilerErrorHandling** (5 tests)
   - Unmatched parentheses
   - Undefined symbols
   - Type errors

7. **TestLISPCompilerEdgeCases** (5 tests)
   - Empty lists
   - Single-element lists
   - Deep nesting

**Target**: 65+ tests, 100% pass rate, < 0.15 seconds total

### Integration Points

1. **Service Registration** (`services/languages/__init__.py`):
```python
from .lisp_service import LISPService

executors = {
    "lisp": LISPService,
    ...
}
```

2. **API Endpoint** (`api/code_execution.py`):
- Add "lisp" to ExecutionRequest language literal
- Update health check: 3 → 4 languages
- Add LISP metadata to `/languages` endpoint

3. **Database** (models/):
- CodeSubmission model already supports LISP

### Development Checklist
- [ ] Implement lisp_lexer.py (400-500 lines, 3-4 hours)
- [ ] Implement lisp_ast.py (150-200 lines, 1-2 hours)
- [ ] Implement lisp_parser.py (550-650 lines, 5-6 hours)
- [ ] Implement lisp_types.py (200-250 lines, 2-3 hours)
- [ ] Implement lisp_compiler.py (600-700 lines, 6-8 hours)
- [ ] Implement lisp_service.py (250-300 lines, 2-3 hours)
- [ ] Implement test_lisp_compiler.py (700-800 lines, 4-5 hours)
- [ ] Integrate with services/languages/__init__.py (30 minutes)
- [ ] Update api/code_execution.py (1 hour)
- [ ] Run full test suite (30 minutes)
- [ ] Code review and cleanup (1-2 hours)

### Estimated Effort
- **Development**: 25-30 hours
- **Testing**: 5-8 hours
- **Integration**: 2-3 hours
- **Total**: 32-41 hours (2-3 days with breaks)

---

## Week 10.1: IDRIS2 Language Service (3-4 Days)

### Overview
IDRIS2 introduces dependent types and compile-time proofs. This is the most sophisticated type system so far, validating the IR's ability to handle type-level computation and compile-time verification.

### Component Breakdown

#### 1. IDRIS2 Lexer (500-600 lines)
**File**: `backend/src/compilers/idris_lexer.py`

**Tokenization** (50+ token types):
- Keywords: `data`, `type`, `where`, `let`, `in`, `case`, `if`, `then`, `else`
- Type annotations: `:`, `->`, `←`, `∀`, `∃`
- Operators: `+`, `-`, `*`, `/`, `=`, `/=`, `<`, `<=`, `>`, `>=`, `@`, `{}`
- Identifiers: lowercase (variables), UPPERCASE (types/constructors)
- Literals: integers, floats, strings, characters
- Brackets: `()`, `[]`, `{}`
- Comments: `--` (line), `{- -}` (block)

**Key Methods**:
```python
def tokenize(self) -> List[Token]
def _scan_identifier(self) -> Token
def _scan_type_annotation(self) -> Token
def _is_type_constructor(self, name: str) -> bool
```

**Test Coverage** (15+ tests):
- Type annotations and quantifiers
- Dependent type syntax
- Pattern matching syntax
- Module imports
- Operator precedence markers

#### 2. IDRIS2 AST (250-300 lines)
**File**: `backend/src/compilers/idris_ast.py`

**Type Nodes**:
```python
class Type:
    pass

class BaseType(Type):  # Int, String, Nat, etc.
    name: str

class DependentType(Type):  # Nat → Type
    parameter: str
    body: Type

class FunctionType(Type):
    arg_type: Type
    return_type: Type

class DataConstructor(Type):
    name: str
    parameter_types: List[Type]
```

**Expression Nodes**:
```python
class Expr:
    pass

class Var(Expr):
    name: str

class Lambda(Expr):
    parameters: List[Tuple[str, Type]]  # Type annotations required
    body: Expr

class Application(Expr):
    func: Expr
    args: List[Expr]

class LetExpr(Expr):
    bindings: List[Tuple[str, Type, Expr]]
    body: Expr

class CaseExpr(Expr):
    scrutinee: Expr
    branches: List[Tuple[Pattern, Expr]]

class ProofExpr(Expr):  # Proof by construction
    type_: Type
    proof: Expr
```

#### 3. IDRIS2 Parser (800-900 lines)
**File**: `backend/src/compilers/idris_parser.py`

**Parsing Strategy**: Recursive descent with dependent type handling

**Grammar** (core):
```
expr := application | lambda | let | case | proof
application := primary expr*
lambda := '\\' parameter+ '=>' expr
let := 'let' binding+ 'in' expr
case := 'case' expr 'of' pattern '->' expr
type := base_type | dependent_type | function_type
dependent_type := '(n : Nat) →' type
```

**Key Challenges**:
- Type annotations required for lambda parameters
- Proof constructions: user provides evidence of type property
- Pattern matching on dependent types (e.g., Vect (1+n) a)
- Implicit arguments: {n : Nat} (inferred from context)

#### 4. IDRIS2 Type System (400-500 lines)
**File**: `backend/src/compilers/idris_types.py`

**Type Checking**:
- Dependent function types: `(n : Nat) → Vec n a → a`
- Refinement types: `(x : Nat { x > 0 })`
- Implicit parameters: `{a : Type}` (inferred)
- Type families: `Vect : Nat → Type → Type`

**Type Inference**:
- Full Hindley-Milner for non-dependent parts
- Constraint solving for dependent parameters
- Unification with occurs check

**Key Algorithms**:
- Unification: Standard Robinson with occurs check
- Type checking: Bidirectional type checking (infer/check mode)
- Constraint generation: Collect type constraints from terms

#### 5. IDRIS2 Compiler (800-900 lines)
**File**: `backend/src/compilers/idris_compiler.py`

**Compilation Strategy**:
1. **Parse**: IDRIS2 → AST
2. **Type Check**: Verify dependent type constraints
3. **Lower to Core**: Remove type information (erasure)
4. **Generate IR**: Core → Babbage IR

**Dependent Type Handling**:
- Compile-time parameters: values known at compile time
- Runtime parameters: values known at execution time
- Type erasure: Remove type information before IR generation
- Evidence: Keep proofs of type properties if needed

**Example**:
```idris
append : Vect n a → Vect m a → Vect (n + m) a
```

Compiles to:
```ir
function append:
  parameter n (compile-time)
  parameter m (compile-time)
  parameter xs (runtime)
  parameter ys (runtime)
  # Append operation same as non-dependent version
```

#### 6. IDRIS2 Service (300-350 lines)
**File**: `backend/src/services/languages/idris_service.py`

**Architecture**: Async FastAPI with thread pool execution

**Unique Features**:
- Type checking errors reported before compilation
- Proof construction validation
- Dependent type constraint display in errors

#### 7. Test Suite (900-1000 lines)
**File**: `backend/src/compilers/test_idris_compiler.py`

**Test Coverage** (70+ tests):

1. **TestIDRISLexer** (15 tests)
   - Type annotations and quantifiers
   - Dependent type syntax

2. **TestIDRISParser** (15 tests)
   - Type annotations
   - Lambda with type parameters
   - Proof expressions

3. **TestIDRISTypeSystem** (25 tests)
   - Dependent function types
   - Type inference with constraints
   - Unification of dependent types
   - Type erasure verification

4. **TestIDRISCompilerSimple** (10 tests)
   - Simple typed functions
   - Type annotations
   - Basic dependent types

5. **TestIDRISCompilerIntegration** (5 tests)
   - Vectors with length-dependent operations
   - Type-safe list operations
   - Proof by construction

**Target**: 70+ tests, 100% pass rate

### Development Effort
- **Development**: 40-50 hours
- **Testing**: 8-10 hours
- **Integration**: 2-3 hours
- **Total**: 50-63 hours (3-4 days)

---

## Week 10.2: System F Language Service (2-3 Days)

### Overview
System F (polymorphic lambda calculus) adds universal type quantification: `∀α. α → α`. This validates the IR's handling of higher-ranked types and parametric polymorphism.

### Component Breakdown

#### 1. System F Lexer (350-400 lines)
**File**: `backend/src/compilers/systemf_lexer.py`

**Tokenization** (30+ token types):
- Keywords: `lambda`, `let`, `in`, `type`
- Type abstractions: `Λ`, `∀`
- Type application: `@`
- Operators: arrows `->`, `=>`, `:`
- Identifiers: lowercase (terms), UPPERCASE (type variables)

#### 2. System F AST (200-250 lines)
**File**: `backend/src/compilers/systemf_ast.py`

**Expression Nodes**:
```python
class Expr:
    pass

class Var(Expr):
    name: str

class LambdaTerm(Expr):
    param: str
    param_type: Type
    body: Expr

class TypeLambda(Expr):  # Λα. expr
    type_var: str
    body: Expr

class TypeApplication(Expr):  # expr @ type
    expr: Expr
    type_arg: Type

class LetExpr(Expr):
    bindings: List[Tuple[str, Type, Expr]]
    body: Expr
```

**Type Nodes**:
```python
class Type:
    pass

class TypeVar(Type):
    name: str

class BaseType(Type):
    name: str  # Int, String, etc.

class FunctionType(Type):
    arg: Type
    result: Type

class UniversalType(Type):  # ∀α. type
    type_var: str
    body: Type
```

#### 3. System F Parser (600-700 lines)
**File**: `backend/src/compilers/systemf_parser.py`

**Grammar**:
```
expr := var | lambda_term | type_lambda | type_application | let
lambda_term := 'λ' var ':' type '.' expr
type_lambda := 'Λ' var '.' expr
type_application := expr '@' type
type := type_var | base_type | function_type | universal_type
universal_type := '∀' var '.' type
```

#### 4. System F Type System (300-400 lines)
**File**: `backend/src/compilers/systemf_types.py`

**Type Checking**:
- Rank-1 types: Standard polymorphism (ML-style)
- Rank-2 types: Forall on left of function arrow
- Higher-rank types: Forall anywhere (complex inference)

**Type Inference**:
- Hindley-Milner algorithm
- Constraint generation and solving
- Type variable renaming for hygiene

**Key Features**:
- Polymorphic function types: `∀α. α → α`
- Instantiation: Replace type variables with concrete types
- Specialization: `(∀α. α → α) applied to Int` becomes `Int → Int`

#### 5. System F Compiler (700-800 lines)
**File**: `backend/src/compilers/systemf_compiler.py`

**Compilation Strategy**:
1. **Parse**: System F → AST
2. **Type Check**: Verify rank, quantification, instantiation
3. **Specialize**: Replace type lambdas with concrete implementations (monomorphization)
4. **Generate IR**: Monomorphic AST → Babbage IR

**Type Application Handling**:
- `(∀α. α → α) @ Int` → Create specialized version with α = Int
- `(∀α. List α) @ String` → Create List[String] version
- At IR level, all polymorphism resolved to concrete types

#### 6. System F Service (250-300 lines)
**File**: `backend/src/services/languages/systemf_service.py`

#### 7. Test Suite (600-700 lines)
**File**: `backend/src/compilers/test_systemf_compiler.py`

**Test Coverage** (60+ tests):
- Rank-1 polymorphism
- Type application and instantiation
- Higher-rank function types
- Type inference constraints

### Development Effort
- **Development**: 30-40 hours
- **Testing**: 5-7 hours
- **Integration**: 2-3 hours
- **Total**: 37-50 hours (2-3 days)

---

## Week 11.1: Java Language Service (3-4 Days)

### Overview
Java introduces object-oriented programming: classes, inheritance, method dispatch, and imperative state management. This validates the IR's ability to handle complex OOP semantics.

### Component Breakdown

#### 1. Java Lexer (500-600 lines)
**File**: `backend/src/compilers/java_lexer.py`

**Tokenization** (50+ token types):
- Keywords: `class`, `public`, `private`, `static`, `void`, `return`, `if`, `while`, `for`, `new`, `this`, `super`
- Types: `int`, `float`, `String`, `boolean`, class names
- Operators: `+`, `-`, `*`, `/`, `=`, `==`, `!=`, `<`, `>`, `.`, `;`, `,`
- Literals: integers, floats, strings, booleans
- Brackets: `{}`, `()`, `[]`

#### 2. Java AST (300-400 lines)
**File**: `backend/src/compilers/java_ast.py`

**Class Nodes**:
```python
class ClassDecl:
    name: str
    parent: Optional[str]  # superclass
    fields: List[FieldDecl]
    methods: List[MethodDecl]
    constructors: List[ConstructorDecl]

class FieldDecl:
    name: str
    type: str
    access_modifier: str  # public, private, static

class MethodDecl:
    name: str
    return_type: str
    parameters: List[Tuple[str, str]]  # name, type
    body: List[Stmt]
    access_modifier: str

class ConstructorDecl:
    parameters: List[Tuple[str, str]]
    body: List[Stmt]
```

**Statement Nodes**:
- Variable declarations with types
- Assignments (fields, local vars)
- Method calls with receiver
- Control flow: if, while, for
- Return statements

**Expression Nodes**:
- Field access: `obj.field`
- Method calls: `obj.method(args)`
- Constructor calls: `new ClassName(args)`
- Operators: arithmetic, comparison, logical
- Literals: numbers, strings, booleans

#### 3. Java Parser (1000-1200 lines)
**File**: `backend/src/compilers/java_parser.py`

**Grammar** (simplified):
```
program := class_decl*
class_decl := 'class' ID [extends ID] '{' (field_decl | method_decl | constructor_decl)* '}'
field_decl := type ID ';'
method_decl := type ID '(' parameters ')' '{' stmt* '}'
constructor_decl := ID '(' parameters ')' '{' stmt* '}'
```

**Key Challenges**:
- Type checking (Java is statically typed)
- Method resolution: find method on object type
- Constructor invocation: verify parameter types
- Field access: verify field exists on type

#### 4. Java Type System (350-450 lines)
**File**: `backend/src/compilers/java_types.py`

**Type Representation**:
- Primitive: int, float, boolean, void
- Reference: class types
- Arrays: Type[]
- Generic (simplified): List<T> (as reference type)

**Type Checking**:
- Field resolution: class → field lookup
- Method resolution: class → method lookup
- Type compatibility: subtyping with inheritance
- Variable scope: class fields, method parameters, local vars

**Key Features**:
- Class hierarchy: Object is root
- Method overriding: subclass replaces parent method
- Polymorphic method calls: resolved at runtime (in IR simulation)

#### 5. Java Compiler (800-900 lines)
**File**: `backend/src/compilers/java_compiler.py`

**Compilation Strategy**:
1. **Parse**: Java → AST
2. **Type Check**: Verify types, method resolution, field access
3. **Flatten OOP**: Convert classes to records, methods to functions
4. **Generate IR**: Flattened AST → Babbage IR

**OOP Flattening**:
- Class: converted to record (named tuple) with fields
- Methods: converted to functions taking implicit `this` parameter
- Constructors: converted to factory functions
- Inheritance: fields/methods flattened into subclass

**Example**:
```java
class Counter {
    int value;
    void increment() { value = value + 1; }
}
Counter c = new Counter();
c.increment();
```

Compiles to:
```ir
function Counter_new:  # constructor
  parameter (none)
  # Initialize fields
  return {value: 0}

function Counter_increment:  # method
  parameter this (Counter record)
  # this.value = this.value + 1
  return this (modified)
```

#### 6. Java Service (300-350 lines)
**File**: `backend/src/services/languages/java_service.py`

#### 7. Test Suite (800-900 lines)
**File**: `backend/src/compilers/test_java_compiler.py`

**Test Coverage** (70+ tests):
- Class definitions
- Field declarations
- Method definitions
- Constructor invocation
- Method calls with receivers
- Inheritance and method overriding
- Type checking errors

### Development Effort
- **Development**: 45-55 hours
- **Testing**: 8-10 hours
- **Integration**: 2-3 hours
- **Total**: 55-68 hours (3-4 days)

---

## Week 12: Phase 2 Integration Testing (3-4 Days)

### Overview
Complete Phase 2 by validating all 7 language services work together, resolving all technical debt from Section 1, and establishing robust testing infrastructure.

### Component Breakdown

#### 1. Integration Tests (1,500-2,000 lines)
**File**: `backend/tests/integration/test_multi_language.py`

**Test Suite** (40+ tests):

1. **TestServiceFactory** (8 tests)
   - Get executor for each language
   - Verify all 7 languages registered
   - Error handling for unknown language

2. **TestAPIEndpoints** (12 tests)
   - POST /run for each language
   - GET /languages returns all 7
   - GET /health returns correct language count (7)
   - Input validation (max code size, etc.)
   - Error handling and reporting

3. **TestMultiLanguageCompilation** (15 tests)
   - Same algorithm in multiple languages (fibonacci)
   - Same output for same algorithm
   - Cross-language validation

4. **TestConcurrentExecution** (5 tests)
   - Multiple languages executing concurrently
   - Resource isolation
   - Thread safety

#### 2. End-to-End Tests (800-1,000 lines)
**File**: `backend/tests/e2e/test_complete_workflow.py`

**Scenarios**:
1. Submit code in Language A
2. Verify compilation output
3. Submit code in Language B
4. Verify same functionality
5. Compare IR representations
6. Validate Babbage assembly differences

#### 3. Technical Debt Resolution

**Item 1.1: User Authentication** (2-3 hours)
```python
# backend/src/auth/auth.py (NEW)
class JWTAuthHandler:
    def create_token(self, user_id: int) -> str
    def verify_token(self, token: str) -> Optional[int]

# backend/src/api/code_execution.py (MODIFIED)
@router.post("/run", response_model=ExecutionResponse)
async def execute_code(
    request: ExecutionRequest,
    current_user = Depends(get_current_user),
    db: Session = Depends(get_db)
):
    # Now can save submission with user_id
```

**Item 1.2: Database Connection Checks** (1-2 hours)
```python
# backend/src/main.py (MODIFIED)
@app.get("/ready")
async def readiness_check(db: Session = Depends(get_db)):
    try:
        # Test database
        db.execute("SELECT 1")
        
        # Test Redis
        redis_client.ping()
        
        return {"status": "ready"}
    except Exception as e:
        raise HTTPException(status_code=503, detail=str(e))
```

**Item 1.3: Prometheus Metrics** (2-3 hours)
```python
# backend/src/metrics.py (NEW)
from prometheus_client import Counter, Gauge, Histogram

requests_total = Counter('requests_total', 'Total requests')
active_users = Gauge('active_users', 'Active users')
execution_time = Histogram('execution_time_seconds', 'Execution time')

# backend/src/main.py (MODIFIED)
@app.middleware("http")
async def metrics_middleware(request, call_next):
    requests_total.inc()
    start = time.time()
    response = await call_next(request)
    execution_time.observe(time.time() - start)
    return response

@app.get("/metrics")
async def prometheus_metrics():
    return Response(prometheus_metrics(), media_type="text/plain")
```

**Items 1.4-1.7: Health Check Updates** (1 hour)
```python
# backend/src/main.py (MODIFIED)
@app.get("/metrics")
async def service_metrics(db: Session = Depends(get_db)):
    return {
        "uptime_seconds": time.time() - APP_START_TIME,
        "requests_total": requests_total._value.get(),
        "active_users": db.query(User).filter(
            User.updated_at > datetime.utcnow() - timedelta(hours=1)
        ).count(),
        "modules_count": db.query(Module).count(),
        "lessons_count": db.query(Lesson).count(),
    }
```

**Services Factory Update** (1 hour)
```python
# backend/src/services/languages/__init__.py (MODIFIED)
from .c_service import CService
from .python_service import PythonService
from .haskell_service import HaskellService
from .lisp_service import LISPService
from .idris_service import IDRISService
from .systemf_service import SystemFService
from .java_service import JavaService
from .babbage_assembly_service import BabbageAssemblyService

def get_executor(language: str):
    executors = {
        "c": CService,
        "python": PythonService,
        "haskell": HaskellService,
        "lisp": LISPService,
        "idris": IDRISService,
        "systemf": SystemFService,
        "java": JavaService,
        "babbage-assembly": BabbageAssemblyService,
    }
    executor_class = executors.get(language.lower())
    if executor_class:
        return executor_class()
    return None
```

#### 4. Documentation Updates (6-8 hours)

**CLAUDE.md Updates** (2-3 hours):
```markdown
## Phase 2 Completion (Weeks 9-12)

### Option B Implementation: 4 Additional Language Services

**Week 9**: LISP Language Service
- Meta-programming and homoiconicity
- Functional programming with mutable state
- 1,800-2,200 lines, 65+ tests

**Week 10**: IDRIS2 and System F Services
- IDRIS2: Dependent types and compile-time proofs
- System F: Polymorphic lambda calculus
- 4,500-5,500 lines, 130+ tests

**Week 11**: Java Language Service
- Object-oriented programming
- Class hierarchies and method dispatch
- 2,200-2,800 lines, 70+ tests

**Week 12**: Integration Testing and Technical Debt Resolution
- 40+ integration tests
- 20+ end-to-end tests
- User authentication implementation
- Database/Redis health checks
- Prometheus metrics implementation

### Architecture Validation

All 7 language services successfully compile to identical Babbage IR:
- C (imperative, static typing)
- Python (dynamic, functional + imperative)
- Haskell (functional, polymorphic)
- LISP (meta-programming, homoiconicity)
- IDRIS2 (dependent types, proofs)
- System F (higher-ranked polymorphism)
- Java (object-oriented, inheritance)

### Phase 2 Metrics

- **Total Lines of Code**: 25,000+ (7,000 from Phase 1, 18,000+ new)
- **Test Coverage**: 300+ tests, 100% pass rate
- **Language Services**: 7 complete
- **Integration Tests**: 40+ scenarios
- **Test Execution Time**: < 5 seconds total
- **Warnings as Errors**: 0 violations
```

**README.md Updates** (2-3 hours):
- Project overview and motivation
- Architecture diagrams (text-based)
- How to run each service
- Contributing guidelines

**AGENTS.md Updates** (1-2 hours):
- Multi-agent coordination strategy for Weeks 9-12
- Each agent's role and responsibilities
- Knowledge sharing mechanisms

#### 5. Final Repository Audit (4-6 hours)

**Validation Checklist**:
- [ ] All 7 language services implemented
- [ ] All 300+ tests passing (100% pass rate)
- [ ] No compiler warnings in any module
- [ ] All TODO items resolved (Section 1 of TECHNICAL_DEBT.md)
- [ ] Service factory returns executors for all 7 languages
- [ ] API endpoints test all 7 languages
- [ ] Database tables created and working
- [ ] Redis caching working
- [ ] Prometheus metrics exposed
- [ ] User authentication working
- [ ] Integration tests passing (40+ tests)
- [ ] End-to-end tests passing (20+ tests)
- [ ] Documentation complete (CLAUDE.md, README.md, AGENTS.md)
- [ ] Code quality gates passed (type hints, docstrings, comments)
- [ ] Git history clean and well-commented

### Development Effort
- **Testing**: 30-40 hours
- **Technical Debt**: 8-12 hours
- **Documentation**: 8-10 hours
- **Final Audit**: 5-8 hours
- **Total**: 51-70 hours (3-4 days)

---

## Overall Option B Summary

### Timeline
```
Week 9.1   (2-3 days): LISP Language Service
Week 10.1  (3-4 days): IDRIS2 Language Service
Week 10.2  (2-3 days): System F Language Service
Week 11.1  (3-4 days): Java Language Service
Week 12    (3-4 days): Integration Testing + Technical Debt
─────────────────────────────────────────────
TOTAL: 13-18 days (6-7 weeks with weekends)
```

### Code Production
```
Language Services:
  LISP:    1,800-2,200 lines
  IDRIS2:  2,500-3,000 lines
  System F: 2,000-2,500 lines
  Java:    2,200-2,800 lines
  ─────────────────────────
  Subtotal: 8,500-10,500 lines

Testing & Integration:
  Integration tests: 1,500-2,000 lines
  End-to-end tests: 800-1,000 lines
  Service registration: 200 lines
  API integration: 300 lines
  ─────────────────────────
  Subtotal: 2,800-3,500 lines

Documentation & Technical Debt:
  Code + documentation updates: ~1,500 lines
  
TOTAL OPTION B: 10,300-12,700 lines
```

### Quality Metrics
```
Testing:
  Total Tests: 300+ (from 68 currently)
  Pass Rate: 100% (or find and fix failures)
  Test Execution: < 5 seconds
  Code Coverage: 90%+ on all services

Code Quality:
  Type Hints: 100% on public APIs
  Docstrings: 100% on all classes/functions
  Comments: Explain "why", not "what"
  Warnings: 0 (all treated as errors)
  PEP 8: Full compliance

Integration:
  API Endpoints: All 7 languages supported
  Database: User auth + code submissions working
  Cache: Redis health checked
  Monitoring: Prometheus metrics exposed
```

### Success Criteria
1. ✓ All 7 language services implemented and tested
2. ✓ 300+ tests passing (100% pass rate)
3. ✓ Zero compiler warnings
4. ✓ User authentication working
5. ✓ Database persistence working
6. ✓ Prometheus metrics exposed
7. ✓ Integration tests validating all services
8. ✓ Documentation complete (CLAUDE.md, README.md, AGENTS.md)
9. ✓ Technical debt resolved (Section 1 of TECHNICAL_DEBT.md)
10. ✓ Phase 2 completion at 85% (from 70%)

---

## Next Phase: Option C (Phase 3)

Upon Option B completion, Phase 3 will focus on:
1. **Babbage Emulator** (2,000-2,500 lines): Execute generated machine code
2. **I/O System** (1,500-2,000 lines): Input/output, file operations
3. **Debugger** (1,500-2,000 lines): Breakpoints, step execution, inspection
4. **Performance Analysis** (1,000-1,500 lines): Profiling, optimization suggestions

See OPTION_C_VISION.md for detailed Phase 3 planning.

---

## Resources and References

- **Compiler Architecture**: See WEEK_8_PHASE_3_COMPLETION_SUMMARY.md for proven patterns
- **Language Service Template**: Study haskell_service.py as reference implementation
- **IR Specification**: backend/src/ir_types.py for Babbage IR semantics
- **Test Strategy**: test_haskell_compiler.py shows comprehensive testing approach
- **Code Generation**: backend/src/codegen/codegen.py documents 4-phase pipeline

---

*End of Option B Implementation Roadmap*
