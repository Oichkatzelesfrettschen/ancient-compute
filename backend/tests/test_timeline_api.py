"""
Ancient Compute - Timeline API Tests

Comprehensive tests for timeline content delivery endpoints and models.
Tests cover:
- Era model and endpoints
- Exercise model, progress tracking, and submissions
- Module relationships with eras and exercises
- Lesson and exercise content delivery
- Full timeline hierarchical structure
"""

import pytest
from sqlalchemy.orm import Session

from src.database import get_db
from src.models import Era, Module, Lesson, Exercise, User, ExerciseProgress, ExerciseSubmission


class TestEraModel:
    """Test Era model creation and relationships."""

    def test_era_creation(self, test_db):
        """Test creating an Era record."""
        from src.database import SessionLocal
        db = SessionLocal()

        era = Era(
            label="ancient",
            full_name="Ancient Foundations",
            description="Period of ancient computation",
            historical_context="3000 BC - 500 AD",
            start_year=-3000,
            end_year=500,
            color="#CD5C5C",
            icon="📜",
            order=1,
        )
        db.add(era)
        db.commit()
        db.refresh(era)

        assert era.id is not None
        assert era.label == "ancient"
        assert era.full_name == "Ancient Foundations"
        assert era.start_year == -3000
        assert era.end_year == 500
        assert era.order == 1

        db.close()

    def test_era_module_relationship(self, test_db):
        """Test Era to Module one-to-many relationship."""
        from src.database import SessionLocal
        db = SessionLocal()

        era = Era(
            label="ancient",
            full_name="Ancient Foundations",
            description="Period of ancient computation",
            historical_context="3000 BC - 500 AD",
            start_year=-3000,
            end_year=500,
            color="#CD5C5C",
            icon="📜",
            order=1,
        )
        db.add(era)
        db.commit()

        module = Module(
            era_id=era.id,
            slug="babylonian-algorithms",
            title="Babylonian Algorithms",
            description="Understanding Babylonian mathematical practices",
            era_enum="ancient",
            start_year=-2000,
            end_year=-1800,
            sequence_order=1,
            is_published=True,
        )
        db.add(module)
        db.commit()
        db.refresh(era)

        assert len(era.modules) == 1
        assert era.modules[0].title == "Babylonian Algorithms"

        db.close()


class TestExerciseModel:
    """Test Exercise model and related progress tracking."""

    def test_exercise_creation(self, test_db):
        """Test creating an Exercise record."""
        from src.database import SessionLocal
        db = SessionLocal()

        era = Era(
            label="ancient",
            full_name="Ancient Foundations",
            description="Period of ancient computation",
            historical_context="3000 BC - 500 AD",
            start_year=-3000,
            end_year=500,
            color="#CD5C5C",
            icon="📜",
            order=1,
        )
        db.add(era)
        db.commit()

        module = Module(
            era_id=era.id,
            slug="babylonian-algorithms",
            title="Babylonian Algorithms",
            description="Understanding Babylonian mathematical practices",
            era_enum="ancient",
            start_year=-2000,
            end_year=-1800,
            sequence_order=1,
            is_published=True,
        )
        db.add(module)
        db.commit()

        exercise = Exercise(
            module_id=module.id,
            slug="sexagesimal-arithmetic",
            title="Sexagesimal Arithmetic",
            description="Practice arithmetic in base 60",
            problem_statement="Convert decimal to base 60",
            languages_supported=["python", "haskell"],
            difficulty="beginner",
            sequence_order=1,
            test_cases=[
                {"input": "60", "expected": "1:0"},
                {"input": "120", "expected": "2:0"},
            ],
            time_limit_seconds=30,
            memory_limit_mb=128,
            estimated_minutes=15,
        )
        db.add(exercise)
        db.commit()
        db.refresh(exercise)

        assert exercise.id is not None
        assert exercise.title == "Sexagesimal Arithmetic"
        assert "python" in exercise.languages_supported
        assert len(exercise.test_cases) == 2

        db.close()

    def test_exercise_progress_tracking(self, test_db):
        """Test ExerciseProgress model for tracking user attempts."""
        from src.database import SessionLocal
        db = SessionLocal()

        user = User(
            email="test@example.com",
            username="testuser",
            hashed_password="hashed_pwd",
        )
        db.add(user)
        db.commit()

        era = Era(
            label="ancient",
            full_name="Ancient Foundations",
            description="Period of ancient computation",
            historical_context="3000 BC - 500 AD",
            start_year=-3000,
            end_year=500,
            color="#CD5C5C",
            icon="📜",
            order=1,
        )
        db.add(era)
        db.commit()

        module = Module(
            era_id=era.id,
            slug="babylonian-algorithms",
            title="Babylonian Algorithms",
            description="Understanding Babylonian mathematical practices",
            era_enum="ancient",
            start_year=-2000,
            end_year=-1800,
            sequence_order=1,
            is_published=True,
        )
        db.add(module)
        db.commit()

        exercise = Exercise(
            module_id=module.id,
            slug="sexagesimal-arithmetic",
            title="Sexagesimal Arithmetic",
            description="Practice arithmetic in base 60",
            problem_statement="Convert decimal to base 60",
            languages_supported=["python"],
            difficulty="beginner",
            sequence_order=1,
            test_cases=[{"input": "60", "expected": "1:0"}],
            time_limit_seconds=30,
            memory_limit_mb=128,
        )
        db.add(exercise)
        db.commit()

        progress = ExerciseProgress(
            user_id=user.id,
            exercise_id=exercise.id,
            attempts=3,
            best_score=75,
            is_completed=False,
        )
        db.add(progress)
        db.commit()
        db.refresh(progress)

        assert progress.attempts == 3
        assert progress.best_score == 75
        assert not progress.is_completed

        db.close()

    def test_exercise_submission_tracking(self, test_db):
        """Test ExerciseSubmission for recording individual code submissions."""
        from src.database import SessionLocal
        db = SessionLocal()

        user = User(
            email="test@example.com",
            username="testuser",
            hashed_password="hashed_pwd",
        )
        db.add(user)
        db.commit()

        era = Era(
            label="ancient",
            full_name="Ancient Foundations",
            description="Period of ancient computation",
            historical_context="3000 BC - 500 AD",
            start_year=-3000,
            end_year=500,
            color="#CD5C5C",
            icon="📜",
            order=1,
        )
        db.add(era)
        db.commit()

        module = Module(
            era_id=era.id,
            slug="babylonian-algorithms",
            title="Babylonian Algorithms",
            description="Understanding Babylonian mathematical practices",
            era_enum="ancient",
            start_year=-2000,
            end_year=-1800,
            sequence_order=1,
            is_published=True,
        )
        db.add(module)
        db.commit()

        exercise = Exercise(
            module_id=module.id,
            slug="sexagesimal-arithmetic",
            title="Sexagesimal Arithmetic",
            description="Practice arithmetic in base 60",
            problem_statement="Convert decimal to base 60",
            languages_supported=["python"],
            difficulty="beginner",
            sequence_order=1,
            test_cases=[{"input": "60", "expected": "1:0"}],
            time_limit_seconds=30,
            memory_limit_mb=128,
        )
        db.add(exercise)
        db.commit()

        submission = ExerciseSubmission(
            user_id=user.id,
            exercise_id=exercise.id,
            submitted_code="def convert(n): return f'{n//60}:{n%60}'",
            language="python",
            passed_tests=1,
            total_tests=1,
            score_percentage=100,
            execution_output="Test passed",
            is_successful=True,
            execution_time_ms=45,
            memory_used_mb=12,
        )
        db.add(submission)
        db.commit()
        db.refresh(submission)

        assert submission.language == "python"
        assert submission.passed_tests == 1
        assert submission.score_percentage == 100
        assert submission.is_successful

        db.close()


class TestTimelineEndpoints:
    """Test timeline API endpoints."""

    def setup_method(self):
        """Setup test data before each test."""
        from src.database import SessionLocal
        self.db = SessionLocal()

        # Create test eras
        self.era1 = Era(
            label="ancient",
            full_name="Ancient Foundations",
            description="Period of ancient computation",
            historical_context="3000 BC - 500 AD",
            start_year=-3000,
            end_year=500,
            color="#CD5C5C",
            icon="📜",
            order=1,
        )
        self.db.add(self.era1)
        self.db.commit()

        # Create test modules
        self.module1 = Module(
            era_id=self.era1.id,
            slug="babylonian-algorithms",
            title="Babylonian Algorithms",
            description="Understanding Babylonian mathematical practices",
            era_enum="ancient",
            start_year=-2000,
            end_year=-1800,
            sequence_order=1,
            is_published=True,
        )
        self.db.add(self.module1)
        self.db.commit()

    def teardown_method(self):
        """Clean up after each test."""
        self.db.close()

    def test_list_eras_endpoint(self, client):
        """Test GET /timeline/eras endpoint."""
        response = client.get("/timeline/eras")

        assert response.status_code == 200
        data = response.json()
        assert "eras" in data
        assert "count" in data

    def test_get_era_detail_endpoint(self, client):
        """Test GET /timeline/eras/{era_id} endpoint."""
        from src.database import SessionLocal
        db = SessionLocal()
        era = db.query(Era).first()

        if era:
            response = client.get(f"/timeline/eras/{era.id}")
            assert response.status_code == 200
            data = response.json()
            assert data["id"] == era.id
            assert data["label"] == era.label

        db.close()

    def test_list_modules_endpoint(self, client):
        """Test GET /timeline/modules endpoint."""
        response = client.get("/timeline/modules")

        assert response.status_code == 200
        data = response.json()
        assert "modules" in data
        assert "count" in data

    def test_get_module_detail_endpoint(self, client):
        """Test GET /timeline/modules/{module_id} endpoint."""
        from src.database import SessionLocal
        db = SessionLocal()
        module = db.query(Module).first()

        if module:
            response = client.get(f"/timeline/modules/{module.id}")
            assert response.status_code == 200
            data = response.json()
            assert data["id"] == module.id
            assert "lessons" in data
            assert "exercises" in data

        db.close()

    def test_get_timeline_metadata_endpoint(self, client):
        """Test GET /timeline/metadata endpoint."""
        response = client.get("/timeline/metadata")

        assert response.status_code == 200
        data = response.json()
        assert "statistics" in data
        assert "totalEras" in data["statistics"]
        assert "totalModules" in data["statistics"]
        assert "erasSummary" in data

    def test_get_full_timeline_endpoint(self, client):
        """Test GET /timeline/full endpoint."""
        response = client.get("/timeline/full")

        assert response.status_code == 200
        data = response.json()
        assert "timeline" in data
        assert "metadata" in data
        assert isinstance(data["timeline"]["eras"], list)

    def test_era_not_found(self, client):
        """Test 404 error for non-existent era."""
        response = client.get("/timeline/eras/99999")
        assert response.status_code == 404
        assert "Era not found" in response.json()["detail"]

    def test_module_not_found(self, client):
        """Test 404 error for non-existent module."""
        response = client.get("/timeline/modules/99999")
        assert response.status_code == 404
        assert "Module not found" in response.json()["detail"]

    def test_lesson_not_found(self, client):
        """Test 404 error for non-existent lesson."""
        response = client.get("/timeline/lessons/99999")
        assert response.status_code == 404
        assert "Lesson not found" in response.json()["detail"]

    def test_exercise_not_found(self, client):
        """Test 404 error for non-existent exercise."""
        response = client.get("/timeline/exercises/99999")
        assert response.status_code == 404
        assert "Exercise not found" in response.json()["detail"]

    def test_era_camel_case_response(self, client):
        """Test that API responses use camelCase field names."""
        from src.database import SessionLocal
        db = SessionLocal()
        era = db.query(Era).first()

        if era:
            response = client.get(f"/timeline/eras/{era.id}")
            data = response.json()
            assert "fullName" in data
            assert "historicalContext" in data
            assert "startYear" in data
            assert "endYear" in data
            assert "moduleCount" in data

        db.close()


class TestTimelineHierarchy:
    """Test hierarchical relationships in timeline data."""

    def test_era_module_hierarchy(self, test_db):
        """Test that eras contain properly nested modules."""
        from src.database import SessionLocal
        db = SessionLocal()

        era = Era(
            label="ancient",
            full_name="Ancient Foundations",
            description="Period of ancient computation",
            historical_context="3000 BC - 500 AD",
            start_year=-3000,
            end_year=500,
            color="#CD5C5C",
            icon="📜",
            order=1,
        )
        db.add(era)
        db.commit()

        module1 = Module(
            era_id=era.id,
            slug="babylonian",
            title="Babylonian",
            description="Babylonian mathematics",
            era_enum="ancient",
            start_year=-2000,
            end_year=-1800,
            sequence_order=1,
            is_published=True,
        )
        module2 = Module(
            era_id=era.id,
            slug="greek",
            title="Greek Logic",
            description="Greek logical systems",
            era_enum="ancient",
            start_year=-500,
            end_year=-300,
            sequence_order=2,
            is_published=True,
        )
        db.add(module1)
        db.add(module2)
        db.commit()
        db.refresh(era)

        assert len(era.modules) == 2
        assert era.modules[0].title == "Babylonian"
        assert era.modules[1].title == "Greek Logic"

        db.close()

    def test_module_exercise_cascade_delete(self, test_db):
        """Test that deleting a module cascades to exercises."""
        from src.database import SessionLocal
        db = SessionLocal()

        era = Era(
            label="ancient",
            full_name="Ancient Foundations",
            description="Period of ancient computation",
            historical_context="3000 BC - 500 AD",
            start_year=-3000,
            end_year=500,
            color="#CD5C5C",
            icon="📜",
            order=1,
        )
        db.add(era)
        db.commit()

        module = Module(
            era_id=era.id,
            slug="babylonian",
            title="Babylonian",
            description="Babylonian mathematics",
            era_enum="ancient",
            start_year=-2000,
            end_year=-1800,
            sequence_order=1,
            is_published=True,
        )
        db.add(module)
        db.commit()

        exercise = Exercise(
            module_id=module.id,
            slug="sexagesimal",
            title="Sexagesimal Arithmetic",
            description="Base 60 arithmetic",
            problem_statement="Convert to base 60",
            languages_supported=["python"],
            difficulty="beginner",
            sequence_order=1,
            test_cases=[],
        )
        db.add(exercise)
        db.commit()

        exercise_id = exercise.id
        assert db.query(Exercise).filter(Exercise.id == exercise_id).first() is not None

        # Delete module
        db.delete(module)
        db.commit()

        # Verify exercise was also deleted (cascade)
        assert db.query(Exercise).filter(Exercise.id == exercise_id).first() is None

        db.close()
