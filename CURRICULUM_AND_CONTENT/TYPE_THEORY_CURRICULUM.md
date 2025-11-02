# Ancient Compute - Type Theory Curriculum

**Document Version**: 1.0
**Date**: November 2, 2025  
**Status**: Comprehensive Type System Documentation
**Agent**: Category-Theory-Expert
**Purpose**: Provide mathematical rigor to type systems and formal verification across 12,500 years of computational history

---

## Executive Summary

This document traces the evolution of type systems from untyped prehistoric computation through modern dependent type theory, demonstrating how mathematical rigor transformed informal algorithmic thinking into formally verifiable programs.

**Historical Arc** (12,500 years compressed):
- **20,000 BC**: Untyped tally marks (Ishango bone)
- **3000 BC**: Babylonian algorithms (implicit types via context)
- **300 BC**: Aristotelian logic (categorical syllogisms)
- **1920s**: Russell's type theory (avoiding paradoxes)
- **1930s**: Church's lambda calculus (untyped → simply-typed)
- **1970s**: Hindley-Milner (type inference)
- **1980s**: System F (parametric polymorphism)
- **1990s**: Martin-Löf (dependent types)
- **2010s**: IDRIS2, Agda, Lean (practical dependent types)

**Educational Goal**: Show students how type systems evolved to catch errors at compile-time that would have required runtime testing or mathematical proofs in earlier eras.

---

## Table of Contents

1. [Type System Progression](#type-system-progression)
2. [Lambda Calculus Foundation](#lambda-calculus-foundation)
3. [Simply-Typed Lambda Calculus](#simply-typed-lambda-calculus)
4. [System F (Polymorphic Lambda Calculus)](#system-f-polymorphic-lambda-calculus)
5. [Dependent Types](#dependent-types)
6. [Category Theory Foundations](#category-theory-foundations)
7. [Formal Verification Examples](#formal-verification-examples)
8. [Type Safety Proofs](#type-safety-proofs)

---

## Type System Progression

### Level 0: Untyped Computation (Prehistory - 1930s)

**Examples**: Tally marks, abacus, Babbage Analytical Engine, untyped lambda calculus

**Characteristics**:
- No static guarantees
- All operations permitted (type errors discovered at runtime)
- Maximal flexibility, minimal safety

**Untyped Lambda Calculus** (Church, 1932):
```
M, N ::= x                 (variable)
       | λx. M             (abstraction)
       | M N               (application)
```

**Example** (factorial):
```
Y = λf. (λx. f (x x)) (λx. f (x x))      (Y combinator, fixed point)
factorial = Y (λf. λn. if (n = 0) then 1 else n * f (n-1))
```

**Problem**: Type errors arise at runtime:
```
(λx. x x) (λx. x x)        (Ω combinator, infinite loop)
(+ 5 "hello")              (nonsensical operation, runtime error)
```

### Level 1: Implicitly Typed (1950s - Present)

**Examples**: Assembly, Python, JavaScript, early LISP

**Characteristics**:
- Types exist but are not written
- Runtime type checking
- Dynamic dispatch based on runtime type tags

**Python Example**:
```python
def add(x, y):
    return x + y

add(5, 3)        # OK: 8
add("a", "b")    # OK: "ab"  (polymorphism via duck typing)
add(5, "b")      # Runtime error: unsupported operand types
```

**Type System** (informal):
- `x : int | str | list | ...` (union of all possible types)
- Runtime checks before each operation

**Advantages**: Flexibility, rapid prototyping
**Disadvantages**: Runtime errors, performance overhead (type checks), no compile-time guarantees

### Level 2: Explicitly Typed (1950s - Present)

**Examples**: C, Java, Go, early Pascal

**Characteristics**:
- Types written explicitly by programmer
- Static type checking at compile-time
- No runtime type tags (types erased after compilation)

**C Example**:
```c
int add(int x, int y) {
    return x + y;
}

add(5, 3);        // OK: 8
add("a", "b");    // Compile error: incompatible types
```

**Type System** (explicit):
```
Γ ⊢ x : int    Γ ⊢ y : int
────────────────────────────
Γ ⊢ add(x, y) : int
```

**Advantages**: Compile-time error detection, no runtime overhead
**Disadvantages**: Verbose, no polymorphism (must write `addInt`, `addFloat`, etc.)

### Level 3: Parametrically Polymorphic (1970s - Present)

**Examples**: ML, Haskell, OCaml, Rust

**Characteristics**:
- Type variables allow generic functions
- Hindley-Milner type inference (types inferred, not written)
- Parametric polymorphism (abstraction over types)

**Haskell Example**:
```haskell
-- Type inferred: id :: a -> a
id x = x

-- Type inferred: map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

-- Type inferred: compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)
```

**Type System** (Hindley-Milner):
```
                    Γ ⊢ e : τ
────────────────────────────────  (∀-Intro)
Γ ⊢ e : ∀α. τ   (α ∉ FV(Γ))

Γ ⊢ e : ∀α. τ
─────────────────────  (∀-Elim)
Γ ⊢ e : τ[τ'/α]
```

**Key Innovation**: Type inference algorithm W (Damas-Milner, 1982)
- Programmer writes no types (or minimal annotations)
- Compiler infers most general type
- Guaranteed to find principal type if one exists

**Advantages**: Conciseness + safety, generic programming
**Disadvantages**: Type errors can be cryptic, no dependent types

### Level 4: System F (Polymorphic Lambda Calculus, 1970s)

**Examples**: System F, Fω, higher-kinded types in Haskell

**Characteristics**:
- Universal quantification (∀α. τ)
- Type abstraction (Λα. M) and type application (M [τ])
- Rank-N polymorphism

**System F Syntax**:
```
Types:  τ ::= α | τ₁ → τ₂ | ∀α. τ
Terms:  M ::= x | λx:τ. M | M N | Λα. M | M [τ]
```

**Example** (polymorphic identity):
```
id = Λα. λx:α. x              (type abstraction)
id [Int] 5                    (type application, result: 5)
id [Bool] true                (type application, result: true)
```

**Example** (Church numerals with types):
```
Nat = ∀α. (α → α) → α → α
zero = Λα. λf:α→α. λx:α. x
succ = λn:Nat. Λα. λf:α→α. λx:α. f (n [α] f x)
```

**Type System** (System F):
```
Γ, α type ⊢ M : τ
────────────────────  (∀-Intro)
Γ ⊢ Λα. M : ∀α. τ

Γ ⊢ M : ∀α. τ
────────────────────  (∀-Elim)
Γ ⊢ M [τ'] : τ[τ'/α]
```

**Key Properties**:
- **Parametricity** (Reynold's theorem): Polymorphic functions behave uniformly across all types
- **Strong normalization**: All well-typed programs terminate
- **Decidable type checking**: Given M and τ, check if M : τ

**Advantages**: Expressive polymorphism, strong theoretical foundation
**Disadvantages**: Type inference undecidable (requires annotations), less practical than Hindley-Milner

### Level 5: Dependent Types (1970s - Present)

**Examples**: IDRIS2, Agda, Coq, Lean

**Characteristics**:
- Types can depend on **values**
- Propositions as types, proofs as programs (Curry-Howard)
- Compile-time verification of program correctness

**IDRIS2 Syntax**:
```idris
-- Vector: length-indexed list
data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect n a -> Vect (S n) a

-- Safe head: requires proof that vector is non-empty
head : Vect (S n) a -> a
head (x :: xs) = x

-- Safe vector append: length statically verified
append : Vect n a -> Vect m a -> Vect (n + m) a
append Nil       ys = ys
append (x :: xs) ys = x :: append xs ys
```

**Example** (dependent pair):
```idris
-- Dependent pair (Σ type): pair where second component's type depends on first
DPair : (A : Type) -> (P : A -> Type) -> Type
DPair A P = (x : A ** P x)

-- Example: natural number with proof that it's even
EvenNat : Type
EvenNat = (n : Nat ** IsEven n)

-- IsEven proof
data IsEven : Nat -> Type where
  ZeroEven : IsEven Z
  SSEven   : IsEven n -> IsEven (S (S n))

-- Safe division by 2 (requires proof of evenness)
half : (n : Nat) -> IsEven n -> Nat
half Z ZeroEven = Z
half (S (S n)) (SSEven pf) = S (half n pf)
```

**Type System** (Martin-Löf Type Theory):
```
Γ ⊢ A : Type    Γ, x : A ⊢ B : Type
────────────────────────────────────  (Π-Form)
Γ ⊢ (x : A) -> B : Type

Γ, x : A ⊢ b : B
────────────────────────  (Π-Intro)
Γ ⊢ λx. b : (x : A) -> B

Γ ⊢ f : (x : A) -> B    Γ ⊢ a : A
────────────────────────────────────  (Π-Elim)
Γ ⊢ f a : B[a/x]
```

**Key Properties**:
- **Curry-Howard correspondence**: Types ≈ Propositions, Programs ≈ Proofs
- **Type erasure**: Dependent types compile to efficient code (proofs erased)
- **Totality checking**: Compiler verifies all functions terminate

**Advantages**: Compile-time correctness guarantees, expressive specifications
**Disadvantages**: Complex type signatures, proof burden, steep learning curve

---

## Lambda Calculus Foundation

### Untyped Lambda Calculus (Church, 1932)

**Syntax**:
```
M, N ::= x                 (variable)
       | λx. M             (abstraction)
       | M N               (application)
```

**Beta reduction** (computation):
```
(λx. M) N  →β  M[N/x]      (substitute N for x in M)
```

**Example** (Church numerals):
```
0 = λf. λx. x
1 = λf. λx. f x
2 = λf. λx. f (f x)
3 = λf. λx. f (f (f x))

succ = λn. λf. λx. f (n f x)
add = λm. λn. λf. λx. m f (n f x)
mult = λm. λn. λf. m (n f)
```

**Problems**:
- No types → nonsensical terms like `(λx. x x) (λx. x x)` (Ω)
- No termination guarantee
- No static error detection

### Simply-Typed Lambda Calculus (Church, 1940)

**Syntax**:
```
Types:  τ ::= ι | τ₁ → τ₂
Terms:  M ::= x | λx:τ. M | M N
```

**Typing Rules**:
```
─────────────  (Var)
Γ, x:τ ⊢ x : τ

Γ, x:τ ⊢ M : σ
──────────────────────  (Abs)
Γ ⊢ λx:τ. M : τ → σ

Γ ⊢ M : τ → σ    Γ ⊢ N : τ
─────────────────────────────  (App)
Γ ⊢ M N : σ
```

**Example** (typed identity):
```
id : τ → τ
id = λx:τ. x
```

**Key Theorem**: **Strong Normalization**
- All well-typed simply-typed lambda calculus terms terminate
- Proof: By induction on typing derivation
- Consequence: Cannot express general recursion (need fixed-point combinator Y, which is **not** typeable)

**Trade-off**: Safety (all programs terminate) vs Expressiveness (cannot express all computable functions)

---

## Simply-Typed Lambda Calculus

### Type System

**Syntax**:
```
Types:  τ ::= Bool | Nat | τ₁ → τ₂
Terms:  M ::= x | λx:τ. M | M N
         | true | false | if M then N₁ else N₂
         | 0 | succ M | pred M | iszero M
```

**Typing Rules** (selected):
```
───────────────  (T-True)
Γ ⊢ true : Bool

───────────────  (T-False)
Γ ⊢ false : Bool

Γ ⊢ M : Bool    Γ ⊢ N₁ : τ    Γ ⊢ N₂ : τ
─────────────────────────────────────────  (T-If)
Γ ⊢ if M then N₁ else N₂ : τ

──────────  (T-Zero)
Γ ⊢ 0 : Nat

Γ ⊢ M : Nat
─────────────────  (T-Succ)
Γ ⊢ succ M : Nat
```

### Type Safety Theorem

**Theorem** (Type Safety):
If `⊢ M : τ`, then either:
1. M is a value, or
2. There exists M' such that M →β M' and `⊢ M' : τ`

**Proof** (sketch):
- **Progress**: Well-typed terms are either values or can take a step
- **Preservation**: If M : τ and M →β M', then M' : τ

**Consequence**: Well-typed programs don't "go wrong" (no runtime type errors)

### Limitations

**Cannot express**:
- Polymorphic identity: `id : ∀α. α → α`
- Self-application: `(λx. x x)`
- General recursion: Y combinator

**Solution**: Extend to System F (parametric polymorphism) or add explicit recursion construct

---

## System F (Polymorphic Lambda Calculus)

### Syntax

**Types**:
```
τ ::= α                    (type variable)
    | τ₁ → τ₂              (function type)
    | ∀α. τ                (universal quantification)
```

**Terms**:
```
M ::= x                    (variable)
    | λx:τ. M              (term abstraction)
    | M N                  (term application)
    | Λα. M                (type abstraction)
    | M [τ]                (type application)
```

### Typing Rules

```
─────────────  (T-Var)
Γ, x:τ ⊢ x : τ

Γ, x:τ ⊢ M : σ
──────────────────────  (T-Abs)
Γ ⊢ λx:τ. M : τ → σ

Γ ⊢ M : τ → σ    Γ ⊢ N : τ
─────────────────────────────  (T-App)
Γ ⊢ M N : σ

Γ, α type ⊢ M : τ
────────────────────  (T-TAbs)
Γ ⊢ Λα. M : ∀α. τ

Γ ⊢ M : ∀α. τ
────────────────────  (T-TApp)
Γ ⊢ M [σ] : τ[σ/α]
```

### Examples

**Polymorphic Identity**:
```
id : ∀α. α → α
id = Λα. λx:α. x

-- Type application
id [Nat] : Nat → Nat
id [Bool] : Bool → Bool
```

**Polymorphic Composition**:
```
compose : ∀α. ∀β. ∀γ. (β → γ) → (α → β) → α → γ
compose = Λα. Λβ. Λγ. λf:β→γ. λg:α→β. λx:α. f (g x)
```

**Church Numerals (Typed)**:
```
Nat = ∀α. (α → α) → α → α

zero : Nat
zero = Λα. λf:α→α. λx:α. x

succ : Nat → Nat
succ = λn:Nat. Λα. λf:α→α. λx:α. f (n [α] f x)

add : Nat → Nat → Nat
add = λm:Nat. λn:Nat. Λα. λf:α→α. λx:α. m [α] f (n [α] f x)
```

**Church Booleans (Typed)**:
```
Bool = ∀α. α → α → α

true : Bool
true = Λα. λt:α. λf:α. t

false : Bool
false = Λα. λt:α. λf:α. f

if : ∀α. Bool → α → α → α
if = Λα. λb:Bool. λt:α. λf:α. b [α] t f
```

### Parametricity (Reynolds' Theorem)

**Theorem** (Parametricity):
For any polymorphic function `f : ∀α. τ`, and any relation R ⊆ A × B, if (a, b) ∈ R, then:
```
(f [A] a, f [B] b) ∈ τ_R
```

**Intuition**: Polymorphic functions must behave uniformly across all types

**Example Consequences**:
- `id : ∀α. α → α` must be the identity function (cannot inspect or modify value)
- `head : ∀α. List α → α` must return first element (cannot construct new value)
- `map : ∀α. ∀β. (α → β) → List α → List β` must apply function to each element

**Free Theorems** (Wadler, 1989):
```
id [A] ∘ f = f ∘ id [B]                    (for any f : A → B)
head [B] ∘ map f = f ∘ head [A]            (for any f : A → B)
```

### Properties

**Decidable Type Checking**: Given M and τ, checking M : τ is decidable
**Undecidable Type Inference**: Given M, finding τ such that M : τ is undecidable
**Strong Normalization**: All well-typed System F terms terminate

---

## Dependent Types

### Martin-Löf Type Theory (MLTT)

**Syntax**:
```
Sorts:     s ::= Type | Kind
Types:     τ ::= x | (x : τ₁) → τ₂ | ...
Terms:     M ::= x | λx:τ. M | M N | ...
```

**Key Innovation**: Types can depend on **terms**

**Π-types** (dependent function types):
```
(x : A) → B      where B may mention x
```

**Examples**:
```
-- Non-dependent (ordinary function)
Nat → Bool       ≡ (x : Nat) → Bool   (B doesn't mention x)

-- Dependent (type depends on value)
(n : Nat) → Vect n Int                 (vector length depends on n)
```

**Σ-types** (dependent pair types):
```
(x : A) ** B     where B may mention x
```

**Example**:
```
-- Dependent pair: natural number with proof it's even
(n : Nat ** IsEven n)
```

### Curry-Howard Correspondence

**Propositions as Types, Proofs as Programs**:

| Logic | Type Theory |
|-------|-------------|
| Proposition P | Type τ |
| Proof of P | Term M : τ |
| P → Q | τ₁ → τ₂ |
| P ∧ Q | τ₁ × τ₂ (product) |
| P ∨ Q | τ₁ + τ₂ (sum) |
| ¬P | τ → ⊥ (empty type) |
| ∀x. P(x) | (x : A) → τ |
| ∃x. P(x) | (x : A ** τ) |

**Example** (proof of modus ponens):
```idris
-- Proposition: (P → Q) → P → Q
modusPonens : (P -> Q) -> P -> Q
modusPonens f p = f p

-- Proof: Apply function f to argument p
```

**Example** (proof of commutativity of addition):
```idris
-- Proposition: ∀m, n. m + n = n + m
plusCommutative : (m : Nat) -> (n : Nat) -> m + n = n + m
plusCommutative Z     n = sym (plusZeroRightNeutral n)
plusCommutative (S k) n = 
  rewrite plusCommutative k n in
  rewrite plusSuccRightSucc n k in
  Refl
```

### IDRIS2 Examples

**Length-Indexed Vectors**:
```idris
data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : a -> Vect n a -> Vect (S n) a

-- Safe head: type guarantees non-empty vector
head : Vect (S n) a -> a
head (x :: xs) = x

-- Safe tail: type guarantees non-empty vector
tail : Vect (S n) a -> Vect n a
tail (x :: xs) = xs

-- Append: length statically verified
(++) : Vect n a -> Vect m a -> Vect (n + m) a
(++) Nil       ys = ys
(++) (x :: xs) ys = x :: (xs ++ ys)

-- Zip: requires equal-length vectors
zip : Vect n a -> Vect n b -> Vect n (a, b)
zip Nil       Nil       = Nil
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys
```

**Verified Sorting**:
```idris
data IsSorted : List Nat -> Type where
  NilSorted  : IsSorted []
  OneSorted  : IsSorted [x]
  ConsSorted : (x <= y) -> IsSorted (y :: ys) -> IsSorted (x :: y :: ys)

-- Sort function with proof that result is sorted
sort : (xs : List Nat) -> (ys : List Nat ** IsSorted ys)
sort xs = -- implementation + proof
```

**Verified Compiler**:
```idris
data Expr : Type where
  Val : Nat -> Expr
  Add : Expr -> Expr -> Expr

-- Evaluator
eval : Expr -> Nat
eval (Val n)   = n
eval (Add x y) = eval x + eval y

-- Stack machine instructions
data Instr = PUSH Nat | ADD

-- Compiler
compile : Expr -> List Instr
compile (Val n)   = [PUSH n]
compile (Add x y) = compile y ++ compile x ++ [ADD]

-- Compiler correctness theorem
compileCorrect : (e : Expr) -> exec (compile e) [] = [eval e]
compileCorrect e = -- proof
```

---

## Category Theory Foundations

### Categories

**Definition**: A category C consists of:
- Objects: Ob(C)
- Morphisms: Hom(A, B) for each pair of objects A, B
- Composition: ∘ : Hom(B, C) × Hom(A, B) → Hom(A, C)
- Identity: id_A : Hom(A, A) for each object A

**Laws**:
```
f ∘ id_A = f                    (left identity)
id_B ∘ f = f                    (right identity)
(f ∘ g) ∘ h = f ∘ (g ∘ h)      (associativity)
```

**Examples**:
- **Set**: Objects = sets, Morphisms = functions
- **Type**: Objects = types, Morphisms = programs
- **Poset**: Objects = elements, Morphisms = ≤ relation

### Functors

**Definition**: Functor F : C → D maps:
- Objects: F(A) ∈ Ob(D) for each A ∈ Ob(C)
- Morphisms: F(f) : F(A) → F(B) for each f : A → B

**Laws**:
```
F(id_A) = id_F(A)              (preserves identity)
F(f ∘ g) = F(f) ∘ F(g)        (preserves composition)
```

**Example** (List functor):
```haskell
-- F(A) = [A]
-- F(f) = map f

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

-- Functor laws:
-- map id = id
-- map (f . g) = map f . map g
```

### Natural Transformations

**Definition**: Natural transformation η : F ⇒ G (for functors F, G : C → D) assigns:
- Component: η_A : F(A) → G(A) for each A ∈ Ob(C)

**Naturality Square** (commutes for all f : A → B):
```
F(A) --η_A--> G(A)
 |             |
F(f)         G(f)
 |             |
 v             v
F(B) --η_B--> G(B)
```

**Example** (reverse : List → List):
```haskell
reverse :: [a] -> [a]

-- Naturality: map f . reverse = reverse . map f
```

### Monads

**Definition**: Monad (T, η, μ) consists of:
- Endofunctor T : C → C
- Unit: η : Id ⇒ T (natural transformation)
- Join: μ : T ∘ T ⇒ T (natural transformation)

**Monad Laws**:
```
μ ∘ T(μ) = μ ∘ μ(T)            (associativity)
μ ∘ η(T) = id = μ ∘ T(η)      (left/right unit)
```

**Haskell Definition**:
```haskell
class Monad m where
  return :: a -> m a              -- η
  (>>=)  :: m a -> (a -> m b) -> m b  -- bind (derived from μ)

-- Monad laws (in terms of >>=):
-- return x >>= f  =  f x                  (left identity)
-- m >>= return    =  m                    (right identity)
-- (m >>= f) >>= g =  m >>= (\x -> f x >>= g)  (associativity)
```

**Examples**:

**Maybe Monad** (partiality):
```haskell
instance Monad Maybe where
  return x = Just x
  Nothing  >>= f = Nothing
  (Just x) >>= f = f x
```

**List Monad** (non-determinism):
```haskell
instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs)
```

**IO Monad** (effects):
```haskell
-- Cannot implement in pure Haskell, built into runtime
```

### Adjunctions

**Definition**: Adjunction F ⊣ G (F : C → D, G : D → C) consists of:
- Natural isomorphism: Hom_D(F(A), B) ≅ Hom_C(A, G(B))

**Example** (free-forgetful adjunction):
- F : Set → Mon (free monoid)
- G : Mon → Set (forgetful functor)
- F(A) = A* (lists over A)
- G(M) = underlying set of monoid M

---

## Formal Verification Examples

### Example 1: List Reversal

**Specification**: Reversing a list twice gives back the original list

**Property**:
```haskell
reverse (reverse xs) = xs
```

**Proof** (by induction on xs):

**Base case**: xs = []
```
reverse (reverse [])
= reverse []          (def. reverse)
= []                  (def. reverse)
```

**Inductive step**: xs = x : xs'
Assume: `reverse (reverse xs') = xs'`  (IH)

```
reverse (reverse (x : xs'))
= reverse (reverse xs' ++ [x])          (def. reverse)
= reverse [x] ++ reverse (reverse xs')  (lemma: reverse distributes)
= [x] ++ xs'                            (IH)
= x : xs'                               (def. ++)
```

**Lemma**: `reverse (xs ++ ys) = reverse ys ++ reverse xs`
Proof by induction on xs (exercise).

### Example 2: Verified Merge Sort

**Specification**: Merge sort produces a sorted list

**IDRIS2 Implementation**:
```idris
-- Sortedness predicate
data Sorted : List Nat -> Type where
  NilSorted  : Sorted []
  OneSorted  : Sorted [x]
  ConsSorted : (x <= y) -> Sorted (y :: ys) -> Sorted (x :: y :: ys)

-- Merge function with proof
merge : (xs : List Nat) -> Sorted xs ->
        (ys : List Nat) -> Sorted ys ->
        (zs : List Nat ** Sorted zs)
merge [] _ ys pf_ys = (ys ** pf_ys)
merge xs pf_xs [] _ = (xs ** pf_xs)
merge (x :: xs') pf_xs (y :: ys') pf_ys = case x <= y of
  True  -> let (zs ** pf_zs) = merge xs' (tail_sorted pf_xs) (y :: ys') pf_ys
           in (x :: zs ** cons_sorted x zs pf_zs)
  False -> let (zs ** pf_zs) = merge (x :: xs') pf_xs ys' (tail_sorted pf_ys)
           in (y :: zs ** cons_sorted y zs pf_zs)

-- Merge sort with proof
mergeSort : (xs : List Nat) -> (ys : List Nat ** Sorted ys)
mergeSort xs = -- implementation + proof
```

### Example 3: Type-Safe Printf

**Problem**: Traditional printf is type-unsafe:
```c
printf("%d %s", "hello", 42);  // Runtime error!
```

**Solution**: Dependent types encode format string in type:
```idris
-- Format descriptor
data Format : Type where
  FInt    : Format -> Format      -- %d
  FStr    : Format -> Format      -- %s
  FChar   : Format -> Format      -- %c
  FEnd    : Format

-- Interpret format as function type
formatType : Format -> Type
formatType (FInt fmt)  = Int -> formatType fmt
formatType (FStr fmt)  = String -> formatType fmt
formatType (FChar fmt) = Char -> formatType fmt
formatType FEnd        = String

-- Type-safe printf
printf : (fmt : Format) -> formatType fmt
printf (FInt rest)  = \i => printf rest   -- partially applied
printf (FStr rest)  = \s => printf rest
printf (FChar rest) = \c => printf rest
printf FEnd         = ""

-- Usage (compile-time type checked!)
printf (FInt (FStr FEnd)) 42 "hello"  -- OK: "%d %s"
printf (FInt (FStr FEnd)) "hello" 42  -- Type error!
```

---

## Type Safety Proofs

### Progress and Preservation

**Theorem** (Type Safety):
If `⊢ M : τ`, then either:
1. M is a value (Progress), or
2. There exists M' such that M →β M' and `⊢ M' : τ` (Preservation)

### Progress Theorem

**Theorem**: If `⊢ M : τ`, then either M is a value or there exists M' such that M → M'.

**Proof** (by induction on typing derivation):

**Case**: M = x
- Cannot occur (no typing derivation for free variables)

**Case**: M = λx:τ. M'
- M is a value (done)

**Case**: M = M₁ M₂ where `⊢ M₁ : τ → σ` and `⊢ M₂ : τ`
- By IH, either M₁ is a value or M₁ → M₁'
  - If M₁ → M₁', then M → M₁' M₂ (E-App1)
  - If M₁ is a value:
    - By canonical forms, M₁ = λx:τ. M'
    - By IH, either M₂ is a value or M₂ → M₂'
      - If M₂ → M₂', then M → M₁ M₂' (E-App2)
      - If M₂ is a value, then M → M'[M₂/x] (E-AppAbs)

### Preservation Theorem

**Theorem**: If `⊢ M : τ` and M → M', then `⊢ M' : τ`.

**Proof** (by induction on typing derivation):

**Case**: M = (λx:σ. M₁) M₂ → M₁[M₂/x] = M'
- By inversion, `⊢ λx:σ. M₁ : σ → τ` and `⊢ M₂ : σ`
- By inversion, `x:σ ⊢ M₁ : τ`
- By Substitution Lemma, `⊢ M₁[M₂/x] : τ`
- Therefore, `⊢ M' : τ`

**Substitution Lemma**: If `Γ, x:σ ⊢ M : τ` and `Γ ⊢ N : σ`, then `Γ ⊢ M[N/x] : τ`.

---

## Appendix A: Type System Comparison Table

| Feature | Untyped | Simple Types | System F | Dependent Types |
|---------|---------|--------------|----------|-----------------|
| **Static Checking** | No | Yes | Yes | Yes |
| **Type Inference** | N/A | Yes (limited) | Decidable checking | Partial |
| **Polymorphism** | No | No | Parametric | Full |
| **Dependent Types** | No | No | No | Yes |
| **Termination** | No | Yes | Yes | Optional |
| **Expressiveness** | Maximum | Limited | High | Maximum |
| **Safety** | Minimum | High | Very High | Maximum |
| **Examples** | Assembly, LISP | C, Java | Haskell, ML | IDRIS2, Coq |

---

## Appendix B: Further Reading

### Historical Sources
- Church, A. (1932). "A Set of Postulates for the Foundation of Logic"
- Curry, H. (1934). "Functionality in Combinatory Logic"
- Girard, J.-Y. (1972). "Interprétation fonctionnelle et élimination des coupures"
- Reynolds, J. (1974). "Towards a Theory of Type Structure"
- Martin-Löf, P. (1975). "An Intuitionistic Theory of Types"

### Modern Textbooks
- Pierce, B. (2002). *Types and Programming Languages*
- Sørensen, M., Urzyczyn, P. (2006). *Lectures on the Curry-Howard Isomorphism*
- Bertot, Y., Castéran, P. (2004). *Interactive Theorem Proving and Program Development*

### Online Resources
- IDRIS2 documentation: https://idris2.readthedocs.io
- Agda tutorial: https://agda.readthedocs.io
- Coq tutorial: https://coq.inria.fr/tutorial

---

**Document Revision**: 1.0
**Last Updated**: November 2, 2025
**Agent**: Category-Theory-Expert
**Next Review**: After IDRIS2 language service implementation (Week 10.1)

**End of Type Theory Curriculum**
