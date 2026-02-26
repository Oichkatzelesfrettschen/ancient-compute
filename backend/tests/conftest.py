# Ancient Compute - Pytest Configuration
import pytest


def pytest_configure(config):
    """Register custom markers."""
    config.addinivalue_line("markers", "physics: physics/simulation tests")


# ---------------------------------------------------------------------------
# Shared emulator fixtures
# ---------------------------------------------------------------------------

@pytest.fixture
def analytical_engine():
    """Fresh Analytical Engine instance (no physics)."""
    from backend.src.emulator.analytical_engine import Engine
    return Engine()


@pytest.fixture
def babbage_number():
    """Factory fixture for BabbageNumber values."""
    from backend.src.emulator.columns import BabbageNumber

    def _make(value):
        return BabbageNumber(value)

    return _make


@pytest.fixture
def simulation_config():
    """Default SimulationConfig at 30 RPM."""
    from backend.src.emulator.simulation.state import SimulationConfig
    return SimulationConfig(rpm=30.0)


@pytest.fixture
def simulation_engine(simulation_config):
    """SimulationEngine wired to default config."""
    from backend.src.emulator.simulation.engine import SimulationEngine
    return SimulationEngine(simulation_config)


# Conditional imports for API testing (optional)
# Guard with importorskip so pytest reports a clear skip reason
# rather than silently swallowing import errors.
_fastapi = pytest.importorskip("fastapi", reason="FastAPI not installed")
_sqlalchemy = pytest.importorskip("sqlalchemy", reason="SQLAlchemy not installed")

try:
    from src.database import Base, get_db
    from src.main import app
    _HAS_DB = True
except ImportError:
    _HAS_DB = False

if _HAS_DB:
    from fastapi.testclient import TestClient
    from sqlalchemy import create_engine
    from sqlalchemy.orm import sessionmaker
    from sqlalchemy.pool import StaticPool

    SQLALCHEMY_DATABASE_URL = "sqlite:///:memory:"

    engine = create_engine(
        SQLALCHEMY_DATABASE_URL,
        connect_args={"check_same_thread": False},
        poolclass=StaticPool,
    )
    TestingSessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)

    def override_get_db():
        """Override database dependency for testing."""
        try:
            db = TestingSessionLocal()
            yield db
        finally:
            db.close()

    @pytest.fixture(scope="function")
    def db():
        """Get a database session for testing."""
        connection = engine.connect()
        transaction = connection.begin()
        session = TestingSessionLocal(bind=connection)
        yield session
        session.close()
        transaction.rollback()
        connection.close()

    @pytest.fixture(scope="function")
    def test_db():
        """Create a fresh database schema for each test."""
        Base.metadata.create_all(bind=engine)
        yield
        Base.metadata.drop_all(bind=engine)

    @pytest.fixture(scope="function")
    def client(test_db):
        """Create a test client with database override."""
        app.dependency_overrides[get_db] = override_get_db
        with TestClient(app) as test_client:
            yield test_client
        app.dependency_overrides.clear()
