# Content Schema Design
## Ancient Compute Educational Platform - Historical Computational Journey

### Executive Summary
This document defines the comprehensive content schema for 12,500 years of computational history, from prehistoric tally systems (10,500 BC) to contemporary quantum computing (2025 AD). The schema supports multiple lesson types, cross-civilization connections, and progressive difficulty scaling.

## Database Schema Extensions

### Core Content Tables

```sql
-- Historical eras and periods
CREATE TABLE eras (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    start_year INTEGER NOT NULL, -- Negative for BC
    end_year INTEGER NOT NULL,
    description TEXT,
    key_innovations TEXT[],
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Civilizations and cultures
CREATE TABLE civilizations (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    region VARCHAR(100),
    start_year INTEGER,
    end_year INTEGER,
    computational_contributions TEXT[],
    notable_figures TEXT[],
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Cross-civilization connections
CREATE TABLE cultural_exchanges (
    id SERIAL PRIMARY KEY,
    from_civilization_id INTEGER REFERENCES civilizations(id),
    to_civilization_id INTEGER REFERENCES civilizations(id),
    year_approximate INTEGER,
    knowledge_transferred TEXT,
    impact_description TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Timeline events for visualization
CREATE TABLE timeline_events (
    id SERIAL PRIMARY KEY,
    year INTEGER NOT NULL,
    month INTEGER,
    day INTEGER,
    title VARCHAR(200) NOT NULL,
    description TEXT,
    category VARCHAR(50), -- 'invention', 'discovery', 'person', 'publication', 'event'
    importance INTEGER CHECK (importance BETWEEN 1 AND 5),
    civilization_id INTEGER REFERENCES civilizations(id),
    module_id INTEGER REFERENCES modules(id),
    coordinates JSONB, -- {"lat": 0.0, "lng": 0.0} for map visualization
    media_urls TEXT[],
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Extended lesson content
CREATE TABLE lesson_content (
    id SERIAL PRIMARY KEY,
    lesson_id INTEGER REFERENCES lessons(id) ON DELETE CASCADE,
    content_type VARCHAR(50), -- 'markdown', 'interactive', 'code', 'quiz', 'video'
    content_order INTEGER NOT NULL,
    title VARCHAR(200),
    content TEXT, -- Markdown or structured content
    code_template TEXT, -- For coding exercises
    expected_output TEXT,
    hints TEXT[],
    solution TEXT,
    metadata JSONB, -- Flexible additional data
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(lesson_id, content_order)
);

-- Quiz questions
CREATE TABLE quiz_questions (
    id SERIAL PRIMARY KEY,
    lesson_id INTEGER REFERENCES lessons(id),
    question_text TEXT NOT NULL,
    question_type VARCHAR(20), -- 'multiple_choice', 'true_false', 'code', 'numeric'
    options JSONB, -- For multiple choice: [{"id": 1, "text": "...", "correct": true}]
    correct_answer TEXT,
    explanation TEXT,
    points INTEGER DEFAULT 1,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Historical figures
CREATE TABLE historical_figures (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    birth_year INTEGER,
    death_year INTEGER,
    civilization_id INTEGER REFERENCES civilizations(id),
    contributions TEXT[],
    biography TEXT,
    image_url TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Computational artifacts
CREATE TABLE artifacts (
    id SERIAL PRIMARY KEY,
    name VARCHAR(200) NOT NULL,
    year_created INTEGER,
    civilization_id INTEGER REFERENCES civilizations(id),
    artifact_type VARCHAR(50), -- 'device', 'algorithm', 'notation', 'theory'
    description TEXT,
    modern_equivalent TEXT,
    interactive_demo_url TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Algorithm implementations across history
CREATE TABLE historical_algorithms (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    original_source VARCHAR(200),
    year_discovered INTEGER,
    civilization_id INTEGER REFERENCES civilizations(id),
    mathematical_notation TEXT,
    pseudocode TEXT,
    implementations JSONB, -- {"c": "...", "python": "...", "lisp": "..."}
    complexity_analysis TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

### SQLAlchemy Models

```python
# backend/src/models/content.py
from sqlalchemy import Column, Integer, String, Text, DateTime, ForeignKey, ARRAY, JSON
from sqlalchemy.orm import relationship
from datetime import datetime
from ..database import Base

class Era(Base):
    __tablename__ = "eras"

    id = Column(Integer, primary_key=True, index=True)
    name = Column(String(100), nullable=False)
    start_year = Column(Integer, nullable=False)
    end_year = Column(Integer, nullable=False)
    description = Column(Text)
    key_innovations = Column(ARRAY(Text))
    created_at = Column(DateTime, default=datetime.utcnow)

    modules = relationship("Module", back_populates="era")

class Civilization(Base):
    __tablename__ = "civilizations"

    id = Column(Integer, primary_key=True, index=True)
    name = Column(String(100), nullable=False)
    region = Column(String(100))
    start_year = Column(Integer)
    end_year = Column(Integer)
    computational_contributions = Column(ARRAY(Text))
    notable_figures = Column(ARRAY(Text))
    created_at = Column(DateTime, default=datetime.utcnow)

    timeline_events = relationship("TimelineEvent", back_populates="civilization")
    artifacts = relationship("Artifact", back_populates="civilization")
    algorithms = relationship("HistoricalAlgorithm", back_populates="civilization")

class CulturalExchange(Base):
    __tablename__ = "cultural_exchanges"

    id = Column(Integer, primary_key=True, index=True)
    from_civilization_id = Column(Integer, ForeignKey("civilizations.id"))
    to_civilization_id = Column(Integer, ForeignKey("civilizations.id"))
    year_approximate = Column(Integer)
    knowledge_transferred = Column(Text)
    impact_description = Column(Text)
    created_at = Column(DateTime, default=datetime.utcnow)

    from_civilization = relationship("Civilization", foreign_keys=[from_civilization_id])
    to_civilization = relationship("Civilization", foreign_keys=[to_civilization_id])

class TimelineEvent(Base):
    __tablename__ = "timeline_events"

    id = Column(Integer, primary_key=True, index=True)
    year = Column(Integer, nullable=False, index=True)
    month = Column(Integer)
    day = Column(Integer)
    title = Column(String(200), nullable=False)
    description = Column(Text)
    category = Column(String(50))
    importance = Column(Integer)
    civilization_id = Column(Integer, ForeignKey("civilizations.id"))
    module_id = Column(Integer, ForeignKey("modules.id"))
    coordinates = Column(JSON)  # {"lat": 0.0, "lng": 0.0}
    media_urls = Column(ARRAY(Text))
    created_at = Column(DateTime, default=datetime.utcnow)

    civilization = relationship("Civilization", back_populates="timeline_events")
    module = relationship("Module", back_populates="timeline_events")

class LessonContent(Base):
    __tablename__ = "lesson_content"

    id = Column(Integer, primary_key=True, index=True)
    lesson_id = Column(Integer, ForeignKey("lessons.id", ondelete="CASCADE"))
    content_type = Column(String(50))
    content_order = Column(Integer, nullable=False)
    title = Column(String(200))
    content = Column(Text)
    code_template = Column(Text)
    expected_output = Column(Text)
    hints = Column(ARRAY(Text))
    solution = Column(Text)
    metadata = Column(JSON)
    created_at = Column(DateTime, default=datetime.utcnow)

    lesson = relationship("Lesson", back_populates="contents")

class QuizQuestion(Base):
    __tablename__ = "quiz_questions"

    id = Column(Integer, primary_key=True, index=True)
    lesson_id = Column(Integer, ForeignKey("lessons.id"))
    question_text = Column(Text, nullable=False)
    question_type = Column(String(20))
    options = Column(JSON)
    correct_answer = Column(Text)
    explanation = Column(Text)
    points = Column(Integer, default=1)
    created_at = Column(DateTime, default=datetime.utcnow)

    lesson = relationship("Lesson", back_populates="quiz_questions")

class HistoricalFigure(Base):
    __tablename__ = "historical_figures"

    id = Column(Integer, primary_key=True, index=True)
    name = Column(String(100), nullable=False)
    birth_year = Column(Integer)
    death_year = Column(Integer)
    civilization_id = Column(Integer, ForeignKey("civilizations.id"))
    contributions = Column(ARRAY(Text))
    biography = Column(Text)
    image_url = Column(Text)
    created_at = Column(DateTime, default=datetime.utcnow)

    civilization = relationship("Civilization")

class Artifact(Base):
    __tablename__ = "artifacts"

    id = Column(Integer, primary_key=True, index=True)
    name = Column(String(200), nullable=False)
    year_created = Column(Integer)
    civilization_id = Column(Integer, ForeignKey("civilizations.id"))
    artifact_type = Column(String(50))
    description = Column(Text)
    modern_equivalent = Column(Text)
    interactive_demo_url = Column(Text)
    created_at = Column(DateTime, default=datetime.utcnow)

    civilization = relationship("Civilization", back_populates="artifacts")

class HistoricalAlgorithm(Base):
    __tablename__ = "historical_algorithms"

    id = Column(Integer, primary_key=True, index=True)
    name = Column(String(100), nullable=False)
    original_source = Column(String(200))
    year_discovered = Column(Integer)
    civilization_id = Column(Integer, ForeignKey("civilizations.id"))
    mathematical_notation = Column(Text)
    pseudocode = Column(Text)
    implementations = Column(JSON)  # Language implementations
    complexity_analysis = Column(Text)
    created_at = Column(DateTime, default=datetime.utcnow)

    civilization = relationship("Civilization", back_populates="algorithms")
```

## Module Structure and Progression

### Module 0: Prehistory (10,500 BC - 3,500 BC)
**The Dawn of Counting**

```python
# backend/src/content/modules/module_0_prehistory.py

MODULE_0_CONTENT = {
    "title": "Prehistory: The Dawn of Counting",
    "era": "Prehistoric",
    "years": "-10500 to -3500",
    "difficulty": 1,
    "lessons": [
        {
            "title": "Before Numbers: The Tally System",
            "type": "reading",
            "content": """
# Before Numbers: The Tally System

Around 10,500 BC, humans began using tally marks to track quantities.
The Lebombo bone, discovered in Southern Africa, contains 29 distinct notches
and represents one of humanity's earliest computational tools.

## Learning Objectives
- Understand pre-numeric counting systems
- Implement a tally counter in multiple languages
- Explore the cognitive leap from concrete to abstract counting
            """,
            "code_exercises": [
                {
                    "title": "Implement a Tally Counter",
                    "languages": ["c", "python", "lisp"],
                    "template": {
                        "c": """
#include <stdio.h>

// Implement a tally counter that groups marks in fives
// Input: number to tally
// Output: tally representation (|||| for 4, ||||-| for 5)

void print_tally(int count) {
    // Your code here
}

int main() {
    int number;
    scanf("%d", &number);
    print_tally(number);
    return 0;
}
                        """,
                        "python": """
# Implement a tally counter that groups marks in fives
def print_tally(count):
    # Your code here
    pass

number = int(input())
print_tally(number)
                        """
                    },
                    "test_cases": [
                        {"input": "7", "output": "||||-| ||"},
                        {"input": "14", "output": "||||-| ||||-| ||||"},
                        {"input": "23", "output": "||||-| ||||-| ||||-| ||||-| |||"}
                    ]
                }
            ]
        },
        {
            "title": "Token Systems and Clay Counting",
            "type": "interactive",
            "content": """
# Token Systems and Clay Counting

Around 8,000 BC in Mesopotamia, clay tokens emerged as the first
abstract computational system. Different shapes represented different
commodities and quantities.

## Interactive Simulation
Drag and drop clay tokens to represent trades and transactions.
            """,
            "interactive_component": "ClayTokenSimulator"
        },
        {
            "title": "The Ishango Bone: Early Arithmetic?",
            "type": "coding",
            "content": """
# The Ishango Bone: Early Arithmetic?

The Ishango bone (20,000 BC) from Congo shows grouped notches
that might represent:
- Prime numbers (11, 13, 17, 19)
- Lunar calendar (groups of 29-30)
- Multiplication by 2

Implement pattern recognition to analyze the groupings.
            """,
            "code_challenge": {
                "title": "Ishango Pattern Analyzer",
                "description": "Analyze number sequences for patterns",
                "languages": ["python", "haskell"],
                "starter_code": {
                    "python": """
# The Ishango bone has three columns with these notch counts:
column_a = [9, 19, 21, 11]  # Total: 60
column_b = [19, 17, 13, 11]  # Total: 60
column_c = [7, 5, 5, 10, 8, 4, 6, 3]  # Total: 48

def analyze_patterns(numbers):
    # Check for prime numbers
    # Check for arithmetic sequences
    # Check for lunar month patterns (29-30 day cycles)
    pass

# Analyze each column
for column_name, column in [('A', column_a), ('B', column_b), ('C', column_c)]:
    print(f"Column {column_name}: {analyze_patterns(column)}")
                    """
                }
            }
        }
    ],
    "timeline_events": [
        {
            "year": -20000,
            "title": "Ishango Bone Created",
            "description": "Earliest known mathematical artifact with grouped notches",
            "category": "artifact",
            "importance": 5,
            "coordinates": {"lat": -1.0, "lng": 29.5}
        },
        {
            "year": -10500,
            "title": "Lebombo Bone",
            "description": "29 notches possibly tracking lunar cycles",
            "category": "artifact",
            "importance": 4,
            "coordinates": {"lat": -26.0, "lng": 32.0}
        },
        {
            "year": -8000,
            "title": "Clay Tokens in Mesopotamia",
            "description": "Abstract representation of quantities and commodities",
            "category": "invention",
            "importance": 5,
            "coordinates": {"lat": 33.0, "lng": 44.0}
        }
    ]
}
```

### Module 1: Ancient Foundations (3,500 BC - 500 AD)
**Birth of Mathematics and Algorithms**

```python
# backend/src/content/modules/module_1_ancient.py

MODULE_1_CONTENT = {
    "title": "Ancient Foundations: Birth of Mathematics",
    "era": "Ancient",
    "years": "-3500 to 500",
    "difficulty": 2,
    "lessons": [
        {
            "title": "Sumerian Cuneiform: The First Written Numbers",
            "type": "reading",
            "content": """
# Sumerian Cuneiform: The First Written Numbers

Around 3,500 BC, the Sumerians developed cuneiform writing,
including the first written number system using base 60.

## Why Base 60?
- Highly divisible (factors: 1,2,3,4,5,6,10,12,15,20,30)
- Natural for astronomy (360 degrees, 60 minutes)
- Still used today in time and angles

## Cuneiform Number Symbols
- Vertical wedge = 1
- Horizontal wedge = 10
- Large space = multiplication by 60
            """,
            "code_exercises": [
                {
                    "title": "Base 60 Converter",
                    "description": "Convert between decimal and sexagesimal",
                    "languages": ["c", "python", "java"],
                    "template": {
                        "python": """
def decimal_to_base60(n):
    # Convert decimal to sexagesimal (base 60)
    # Return as list of digits
    # Example: 3661 = 1*60^2 + 1*60 + 1 = [1, 1, 1]
    pass

def base60_to_decimal(digits):
    # Convert sexagesimal to decimal
    # Example: [1, 1, 1] = 3661
    pass

# Test your implementation
test_number = 3725  # 1 hour, 2 minutes, 5 seconds
print(f"{test_number} in base 60: {decimal_to_base60(test_number)}")
                        """
                    }
                }
            ]
        },
        {
            "title": "Egyptian Multiplication: Doubling and Halving",
            "type": "coding",
            "content": """
# Egyptian Multiplication Algorithm

The ancient Egyptians (3000 BC) developed a multiplication
algorithm using only doubling, halving, and addition.

## The Algorithm
To multiply A × B:
1. Create two columns starting with 1 and B
2. Double the left column, double the right column
3. Continue until the left column would exceed A
4. Select rows where left column numbers sum to A
5. Sum the corresponding right column values

Example: 13 × 21
```
1    21  ✓
2    42
4    84  ✓
8   168  ✓
---------
13  273
```
            """,
            "code_challenge": {
                "title": "Implement Egyptian Multiplication",
                "languages": ["c", "python", "haskell"],
                "template": {
                    "c": """
#include <stdio.h>

int egyptian_multiply(int a, int b) {
    // Implement Egyptian multiplication
    // Only use addition, doubling (×2), and halving (÷2)
    int result = 0;
    int power = 1;

    // Your code here

    return result;
}

int main() {
    int a, b;
    scanf("%d %d", &a, &b);
    printf("%d\\n", egyptian_multiply(a, b));
    return 0;
}
                    """
                }
            }
        },
        {
            "title": "Babylonian Square Roots: The First Iterative Algorithm",
            "type": "interactive",
            "content": """
# Babylonian Method for Square Roots

Around 1800 BC, Babylonian mathematicians developed an iterative
algorithm for computing square roots - possibly the world's first
iterative numerical method.

## The Algorithm (in modern notation)
To find √S:
1. Start with a guess x₀
2. Improve the guess: x_{n+1} = (x_n + S/x_n) / 2
3. Repeat until convergence

This is actually Newton's method, discovered 3500 years early!
            """,
            "code_exercises": [
                {
                    "title": "Babylonian Square Root",
                    "languages": ["python", "c", "lisp"],
                    "template": {
                        "python": """
def babylonian_sqrt(S, tolerance=0.00001):
    # Implement the Babylonian method
    # Start with guess = S/2
    # Iterate until |guess² - S| < tolerance

    guess = S / 2.0

    # Your code here

    return guess

# Test with perfect and non-perfect squares
test_values = [16, 2, 100, 50]
for val in test_values:
    result = babylonian_sqrt(val)
    print(f"√{val} = {result:.6f}")
    print(f"Verification: {result}² = {result**2:.6f}")
                        """,
                        "lisp": """
(defun babylonian-sqrt (s &optional (tolerance 0.00001))
  ; Implement Babylonian square root method
  ; Use iterative refinement

  (let ((guess (/ s 2.0)))
    ; Your code here

    guess))

; Test the function
(format t "√16 = ~,6f~%" (babylonian-sqrt 16))
(format t "√2 = ~,6f~%" (babylonian-sqrt 2))
                        """
                    }
                }
            ]
        },
        {
            "title": "Euclidean Algorithm: Greatest Common Divisor",
            "type": "coding",
            "content": """
# Euclidean Algorithm (300 BC)

Euclid's algorithm for finding the GCD is one of the oldest
algorithms still in use today. It appears in Book VII of
Euclid's Elements.

## Original Geometric Form
"The lesser of two unequal numbers being continually subtracted
from the greater, if a unit is left, the original numbers are
relatively prime."

## Modern Implementation
GCD(a, b) = GCD(b, a mod b) when b ≠ 0
GCD(a, 0) = a
            """,
            "implementations": {
                "c": """
int gcd(int a, int b) {
    while (b != 0) {
        int temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}
                """,
                "haskell": """
gcd' :: Integer -> Integer -> Integer
gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)
                """,
                "assembly": """
; x86-64 Assembly implementation of GCD
; Inputs: rdi = a, rsi = b
; Output: rax = gcd(a, b)

gcd:
    mov rax, rdi    ; rax = a
.loop:
    test rsi, rsi   ; check if b == 0
    jz .done        ; if yes, we're done

    xor rdx, rdx    ; clear rdx for division
    div rsi         ; rax/rsi, remainder in rdx

    mov rax, rsi    ; a = b
    mov rsi, rdx    ; b = remainder
    jmp .loop

.done:
    ret             ; result is in rax
                """
            }
        },
        {
            "title": "The Antikythera Mechanism: Ancient Analog Computer",
            "type": "reading",
            "content": """
# The Antikythera Mechanism (100 BC)

The Antikythera mechanism is an ancient Greek analog computer
used to predict astronomical positions and eclipses decades
in advance.

## Technical Specifications
- 37+ meshing bronze gears
- Differential gearing (not reinvented until 1500s)
- Computed lunar and solar eclipses
- Tracked Olympic Games timing

## Computational Significance
- First known mechanical computer
- Implemented complex astronomical cycles
- Used fixed gear ratios for mathematical constants
            """,
            "interactive_demo": "AntikytheraSimulator"
        }
    ],
    "timeline_events": [
        {
            "year": -3500,
            "title": "Sumerian Cuneiform Numbers",
            "description": "First written number system, base 60",
            "category": "invention",
            "importance": 5
        },
        {
            "year": -3000,
            "title": "Egyptian Hieroglyphic Numbers",
            "description": "Decimal system with symbols for powers of 10",
            "category": "invention",
            "importance": 4
        },
        {
            "year": -1800,
            "title": "Babylonian Square Root Algorithm",
            "description": "First known iterative algorithm",
            "category": "discovery",
            "importance": 5
        },
        {
            "year": -300,
            "title": "Euclid's Elements Published",
            "description": "Foundation of algorithmic thinking",
            "category": "publication",
            "importance": 5
        },
        {
            "year": -250,
            "title": "Sieve of Eratosthenes",
            "description": "Algorithm for finding prime numbers",
            "category": "discovery",
            "importance": 4
        },
        {
            "year": -100,
            "title": "Antikythera Mechanism Built",
            "description": "First analog computer",
            "category": "invention",
            "importance": 5
        }
    ],
    "cross_references": [
        {
            "from": "Babylonian Mathematics",
            "to": "Islamic Golden Age",
            "connection": "Preservation and expansion of mathematical methods"
        },
        {
            "from": "Egyptian Multiplication",
            "to": "Binary Arithmetic",
            "connection": "Doubling/halving as precursor to binary operations"
        }
    ]
}
```

## Content Authoring Guidelines

### Markdown Format Standards

```markdown
# Lesson Title

## Historical Context
Brief introduction placing the concept in historical context.

## Key Concepts
- Bullet points for main ideas
- Mathematical formulas in LaTeX: $x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}$

## Code Examples
```language
// Code with clear comments
// Emphasize historical significance
```

## Exercises
1. Implement the historical algorithm
2. Compare with modern approaches
3. Analyze computational complexity

## Connections
- Link to previous concepts
- Preview future developments
- Cross-cultural influences

## Further Reading
- Primary sources (translated)
- Modern interpretations
- Interactive demonstrations
```

### Code Exercise Template

```python
{
    "title": "Exercise Title",
    "difficulty": 1-5,
    "languages": ["c", "python", "haskell"],
    "description": "Clear problem statement",
    "historical_context": "Why this was important",
    "template": {
        "language": "starter_code_here"
    },
    "test_cases": [
        {"input": "...", "output": "...", "explanation": "..."}
    ],
    "hints": [
        "Hint 1: Think about...",
        "Hint 2: Consider using..."
    ],
    "solution": "full_solution_code",
    "solution_explanation": "Detailed explanation"
}
```

### Quiz Question Format

```python
{
    "question": "What base did the Sumerians use?",
    "type": "multiple_choice",
    "options": [
        {"id": "a", "text": "Base 10", "correct": false},
        {"id": "b", "text": "Base 60", "correct": true},
        {"id": "c", "text": "Base 12", "correct": false},
        {"id": "d", "text": "Base 20", "correct": false}
    ],
    "explanation": "The Sumerians used base 60 (sexagesimal)...",
    "difficulty": 2,
    "topics": ["sumerian", "number-systems"]
}
```

## Module Progression Tree

```
Module 0: Prehistory (10,500 BC - 3,500 BC)
    ├── Tally Systems
    ├── Token Counting
    └── Pattern Recognition

Module 1: Ancient Foundations (3,500 BC - 500 AD)
    ├── Number Systems (Sumerian, Egyptian, Babylonian)
    ├── Algorithms (Multiplication, Square Roots, GCD)
    └── Mechanical Computation (Antikythera)

Module 2: Medieval Synthesis (500 - 1400 AD)
    ├── Islamic Golden Age
    ├── Indian Mathematics (Zero, Decimal System)
    └── Chinese Computational Methods

Module 3: Renaissance Rebirth (1400 - 1650)
    ├── Mechanical Calculators
    ├── Logarithms and Slide Rules
    └── Binary Notation

Module 4: Enlightenment Formalism (1650 - 1800)
    ├── Calculus and Analysis
    ├── Probability Theory
    └── Early Automata

Module 5: Industrial Revolution (1800 - 1900)
    ├── Babbage's Engines
    ├── Boolean Algebra
    └── Hollerith Cards

Module 6: Electronic Dawn (1900 - 1945)
    ├── Turing Machines
    ├── Lambda Calculus
    └── First Electronic Computers

Module 7: Digital Revolution (1945 - 1970)
    ├── Von Neumann Architecture
    ├── High-Level Languages
    └── Operating Systems

Module 8: Personal Computing (1970 - 1990)
    ├── Microprocessors
    ├── Object-Oriented Programming
    └── Networking Foundations

Module 9: Internet Age (1990 - 2010)
    ├── World Wide Web
    ├── Mobile Computing
    └── Cloud Infrastructure

Module 10: Contemporary (2010 - 2025)
    ├── Machine Learning
    ├── Quantum Computing
    └── Distributed Systems

Module 11: Synthesis
    ├── Historical Patterns
    ├── Future Directions
    └── Philosophical Implications
```

## Content Migration Plan

### Phase 1: Schema Deployment
```sql
-- Run migrations in order
-- 1. Create new tables
CREATE TABLE eras (...);
CREATE TABLE civilizations (...);
CREATE TABLE cultural_exchanges (...);
CREATE TABLE timeline_events (...);
CREATE TABLE lesson_content (...);
CREATE TABLE quiz_questions (...);
CREATE TABLE historical_figures (...);
CREATE TABLE artifacts (...);
CREATE TABLE historical_algorithms (...);

-- 2. Add foreign key relationships
ALTER TABLE modules ADD COLUMN era_id INTEGER REFERENCES eras(id);
ALTER TABLE lessons ADD COLUMN civilization_id INTEGER REFERENCES civilizations(id);

-- 3. Create indexes for performance
CREATE INDEX idx_timeline_year ON timeline_events(year);
CREATE INDEX idx_lesson_content_order ON lesson_content(lesson_id, content_order);
```

### Phase 2: Initial Data Load
```python
# backend/src/content/seed_data.py
from ..database import SessionLocal
from ..models.content import Era, Civilization, TimelineEvent, LessonContent

def seed_historical_data():
    db = SessionLocal()

    # Create eras
    eras = [
        Era(name="Prehistoric", start_year=-10500, end_year=-3500,
            key_innovations=["Tally marks", "Clay tokens", "Pattern recognition"]),
        Era(name="Ancient", start_year=-3500, end_year=500,
            key_innovations=["Written numbers", "Algorithms", "Mechanical devices"]),
        Era(name="Medieval", start_year=500, end_year=1400,
            key_innovations=["Zero", "Algebra", "Decimal system"]),
        # ... more eras
    ]

    # Create civilizations
    civilizations = [
        Civilization(name="Sumerian", region="Mesopotamia", start_year=-4500, end_year=-1900,
                    computational_contributions=["Cuneiform numbers", "Base 60", "Astronomy"]),
        Civilization(name="Egyptian", region="North Africa", start_year=-3100, end_year=-30,
                    computational_contributions=["Hieroglyphic numbers", "Doubling method", "Geometry"]),
        # ... more civilizations
    ]

    # Add to database
    for era in eras:
        db.add(era)
    for civ in civilizations:
        db.add(civ)

    db.commit()
    db.close()

if __name__ == "__main__":
    seed_historical_data()
```

### Phase 3: Content API Endpoints

```python
# backend/src/api/content.py
from fastapi import APIRouter, Depends, HTTPException
from typing import List, Optional
from sqlalchemy.orm import Session
from ..database import get_db
from ..models.content import TimelineEvent, Civilization, Era
from ..schemas.content import TimelineEventResponse, CivilizationResponse

router = APIRouter(prefix="/api/content", tags=["content"])

@router.get("/timeline", response_model=List[TimelineEventResponse])
async def get_timeline_events(
    start_year: Optional[int] = None,
    end_year: Optional[int] = None,
    category: Optional[str] = None,
    civilization_id: Optional[int] = None,
    importance_min: Optional[int] = None,
    db: Session = Depends(get_db)
):
    query = db.query(TimelineEvent)

    if start_year:
        query = query.filter(TimelineEvent.year >= start_year)
    if end_year:
        query = query.filter(TimelineEvent.year <= end_year)
    if category:
        query = query.filter(TimelineEvent.category == category)
    if civilization_id:
        query = query.filter(TimelineEvent.civilization_id == civilization_id)
    if importance_min:
        query = query.filter(TimelineEvent.importance >= importance_min)

    return query.order_by(TimelineEvent.year).all()

@router.get("/civilizations", response_model=List[CivilizationResponse])
async def get_civilizations(db: Session = Depends(get_db)):
    return db.query(Civilization).all()

@router.get("/cultural-exchanges/{civilization_id}")
async def get_cultural_exchanges(
    civilization_id: int,
    db: Session = Depends(get_db)
):
    exchanges_from = db.query(CulturalExchange).filter(
        CulturalExchange.from_civilization_id == civilization_id
    ).all()

    exchanges_to = db.query(CulturalExchange).filter(
        CulturalExchange.to_civilization_id == civilization_id
    ).all()

    return {
        "influenced": exchanges_from,
        "influenced_by": exchanges_to
    }

@router.get("/lesson/{lesson_id}/content")
async def get_lesson_content(
    lesson_id: int,
    db: Session = Depends(get_db)
):
    contents = db.query(LessonContent).filter(
        LessonContent.lesson_id == lesson_id
    ).order_by(LessonContent.content_order).all()

    return contents

@router.get("/algorithms/historical")
async def get_historical_algorithms(
    civilization_id: Optional[int] = None,
    db: Session = Depends(get_db)
):
    query = db.query(HistoricalAlgorithm)

    if civilization_id:
        query = query.filter(HistoricalAlgorithm.civilization_id == civilization_id)

    return query.order_by(HistoricalAlgorithm.year_discovered).all()
```

## Content Validation and Quality Assurance

```python
# backend/src/content/validators.py
from typing import Dict, List, Any
import re

class ContentValidator:
    @staticmethod
    def validate_lesson_content(content: Dict[str, Any]) -> List[str]:
        errors = []

        # Check required fields
        required = ["title", "type", "content"]
        for field in required:
            if field not in content:
                errors.append(f"Missing required field: {field}")

        # Validate content type
        valid_types = ["reading", "interactive", "coding", "quiz", "synthesis"]
        if content.get("type") not in valid_types:
            errors.append(f"Invalid lesson type: {content.get('type')}")

        # Check code exercises
        if "code_exercises" in content:
            for exercise in content["code_exercises"]:
                if "template" not in exercise:
                    errors.append("Code exercise missing template")
                if "test_cases" not in exercise:
                    errors.append("Code exercise missing test cases")

        # Validate markdown
        if "content" in content:
            markdown_errors = ContentValidator.validate_markdown(content["content"])
            errors.extend(markdown_errors)

        return errors

    @staticmethod
    def validate_markdown(markdown: str) -> List[str]:
        errors = []

        # Check for proper heading hierarchy
        lines = markdown.split('\n')
        heading_levels = []

        for line in lines:
            if line.startswith('#'):
                level = len(line.split()[0])
                heading_levels.append(level)

        # Ensure we start with h1
        if heading_levels and heading_levels[0] != 1:
            errors.append("Content should start with # (h1) heading")

        # Check for ASCII-only content
        if not all(ord(char) < 128 for char in markdown):
            errors.append("Content contains non-ASCII characters")

        return errors

    @staticmethod
    def validate_timeline_event(event: Dict[str, Any]) -> List[str]:
        errors = []

        # Validate year
        if "year" not in event:
            errors.append("Timeline event missing year")
        elif not -12000 <= event["year"] <= 2025:
            errors.append(f"Year {event['year']} outside valid range")

        # Validate importance
        if "importance" in event:
            if not 1 <= event["importance"] <= 5:
                errors.append("Importance must be between 1 and 5")

        # Validate coordinates if present
        if "coordinates" in event:
            coords = event["coordinates"]
            if "lat" not in coords or "lng" not in coords:
                errors.append("Invalid coordinates format")
            elif not (-90 <= coords["lat"] <= 90 and -180 <= coords["lng"] <= 180):
                errors.append("Coordinates out of valid range")

        return errors
```
