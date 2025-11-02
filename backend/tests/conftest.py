# Ancient Compute - Pytest Configuration
import pytest

# Conditional imports for API testing (optional)
try:
    from fastapi.testclient import TestClient
    from sqlalchemy import create_engine
    from sqlalchemy.orm import sessionmaker
    from sqlalchemy.pool import StaticPool
    from src.database import Base, get_db
    from src.main import app
    
    # Create in-memory SQLite database for testing
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
    def test_db():
        """Create a fresh database for each test."""
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

except Exception:
    # Dependencies not installed or incompatible; skip API fixtures
    pass
