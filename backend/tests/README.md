# Backend Test Suite

**Date**: 2026-02-26

## Running Tests

```bash
# All unit tests (excludes known failures)
make test-unit

# Physics/simulation tests only
make test-physics

# Active contract gate (pre-merge)
make test-active

# Full backend suite
make test-backend
```

## Test Database Strategy

### Production
- PostgreSQL via Docker Compose (`postgres:5432`)
- Connection string in `backend/src/config.py` / `.env`

### Testing
- **SQLite in-memory** via `conftest.py`
- The `test_db` fixture creates/drops all tables per test function
- The `client` fixture overrides FastAPI's `get_db` dependency with the SQLite session
- No PostgreSQL required for running tests locally

### Alembic Migrations
- Migrations target PostgreSQL by default (`alembic.ini`)
- Override with `DATABASE_URL=sqlite:///dev.db` for local development
- Roundtrip verified: `upgrade head` then `downgrade base` then `upgrade head`

## Markers

| Marker | Purpose | Command |
|--------|---------|---------|
| `physics` | Physics/simulation tests | `pytest -m physics` |
| `slow` | Long-running tests | `pytest -m "not slow"` to skip |

## Known Issues

- `test_leibniz_reckoner.py`: Carry propagation logic needs work (pre-existing)
- `test_pascaline.py`: Two carry-chain tests fail (pre-existing)
- `test_tools_router.py`, `test_cross_language.py`, `test_phase4_w1_api.py`: Collection errors due to missing Docker/DB fixtures; excluded from local runs

## Fixtures

Shared fixtures in `conftest.py`:

| Fixture | Scope | Description |
|---------|-------|-------------|
| `analytical_engine` | function | Fresh Analytical Engine (no physics) |
| `babbage_number` | function | Factory for BabbageNumber values |
| `simulation_config` | function | Default SimulationConfig at 30 RPM |
| `simulation_engine` | function | SimulationEngine with default config |
| `db` | function | SQLAlchemy session with transaction rollback |
| `test_db` | function | Create/drop schema per test |
| `client` | function | FastAPI TestClient with DB override |
