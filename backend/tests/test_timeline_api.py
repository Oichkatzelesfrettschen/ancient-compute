"""
Ancient Compute - Timeline API Tests

Comprehensive tests for timeline content delivery endpoints and models.
"""

import importlib
import pytest
from src.models import Era, Module, Exercise, ExerciseProgress, ExerciseSubmission, User


def _db_available() -> bool:
    try:
        importlib.import_module('src.database')
        importlib.import_module('src.models')
        return True
    except Exception:
        return False

pytestmark = pytest.mark.skipif(not _db_available(), reason="database/models unavailable in this environment")


class TestEraModel:
    """Test Era model creation and relationships."""

    def test_era_creation(self, db, test_db):
        """Test creating an Era record."""
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

    def test_era_module_relationship(self, db, test_db):
        """Test Era to Module one-to-many relationship."""
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


class TestExerciseModel:
    """Test Exercise model and related progress tracking."""

    def test_exercise_creation(self, db, test_db):
        """Test creating an Exercise record."""
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

    def test_exercise_progress_tracking(self, db, test_db):
        """Test ExerciseProgress model for tracking user attempts."""
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

    def test_exercise_submission_tracking(self, db, test_db):
        """Test ExerciseSubmission for recording individual code submissions."""
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


class TestTimelineEndpoints:
    """Test timeline API endpoints."""

    @pytest.fixture(autouse=True)
    def setup(self, db, test_db):
        """Setup test data before each test."""
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
        db.add(self.era1)
        db.commit()

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
        db.add(self.module1)
        db.commit()

    def test_list_eras_endpoint(self, client):
        """Test GET /timeline/eras endpoint."""
        response = client.get("/api/v1/timeline/eras")

        assert response.status_code == 200
        data = response.json()
        assert "eras" in data
        assert "count" in data

    def test_get_era_detail_endpoint(self, client, db):
        """Test GET /timeline/eras/{era_id} endpoint."""
        era = db.query(Era).first()

        if era:
            response = client.get(f"/api/v1/timeline/eras/{era.id}")
            assert response.status_code == 200
            data = response.json()
            assert data["id"] == era.id
            assert data["label"] == era.label

    def test_list_modules_endpoint(self, client):
        """Test GET /timeline/modules endpoint."""
        response = client.get("/api/v1/timeline/modules")

        assert response.status_code == 200
        data = response.json()
        assert "modules" in data
        assert "count" in data

    def test_get_module_detail_endpoint(self, client, db):
        """Test GET /timeline/modules/{module_id} endpoint."""
        module = db.query(Module).first()

        if module:
            response = client.get(f"/api/v1/timeline/modules/{module.id}")
            assert response.status_code == 200
            data = response.json()
            assert data["id"] == module.id
            assert data["slug"] == module.slug


class TestTimelineHierarchy:
    """Test full timeline hierarchical delivery."""

    def test_era_module_hierarchy(self, db, test_db, client):
        """Test that eras include their modules."""
        era = Era(
            label="medieval",
            full_name="Medieval Algorithms",
            description="Islamic and European mathematics",
            historical_context="500 - 1500 AD",
            start_year=500,
            end_year=1500,
            color="#4682B4",
            order=2,
        )
        db.add(era)
        db.commit()

        module = Module(
            era_id=era.id,
            slug="al-khwarizmi",
            title="Al-Khwarizmi and Algebra",
            description="The origins of the word algorithm",
            era_enum="medieval",
            start_year=800,
            end_year=850,
            sequence_order=1,
        )
        db.add(module)
        db.commit()

        response = client.get(f"/api/v1/timeline/eras/{era.id}")
        assert response.status_code == 200
        data = response.json()
        assert "modules" in data
        assert len(data["modules"]) == 1
        assert data["modules"][0]["slug"] == "al-khwarizmi"

    def test_module_exercise_cascade_delete(self, db, test_db):
        """Test that deleting an era cascades to modules."""
        era = Era(
            label="prehistory",
            full_name="Prehistoric Counting",
            description="Tally marks and bones",
            historical_context="20,000 BC",
            start_year=-20000,
            end_year=-3000,
            color="#8B4513",
            order=0,
        )
        db.add(era)
        db.commit()

        module = Module(
            era_id=era.id,
            slug="ishango-bone",
            title="The Ishango Bone",
            description="Early evidence of prime numbers",
            era_enum="prehistory",
            start_year=-20000,
            end_year=-18000,
            sequence_order=1,
        )
        db.add(module)
        db.commit()

        db.delete(era)
        db.commit()

        # Module should be gone
        deleted_module = db.query(Module).filter(Module.slug == "ishango-bone").first()
        assert deleted_module is None
