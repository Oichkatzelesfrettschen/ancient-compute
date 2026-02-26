# Backend Tests

## Running Tests

### Unit tests (no external deps)

```sh
pytest backend/tests/unit/ -v
make test-unit
```

### Full suite (integration + unit)

```sh
pytest backend/tests/ -v
```

### Excluding DB-dependent tests

Tests that use `db`, `test_db`, or `client` fixtures require a running
PostgreSQL database.  Without one, they **skip** automatically.

```sh
pytest backend/tests/ -m "not db"
```

## Database Tests

```sh
export DATABASE_URL=postgresql://user:pass@localhost:5432/ancient_compute_test
pytest backend/tests/ -m db
```

## Markers

| Marker | Purpose |
|--------|---------|
| `physics` | Physics/simulation tests |
| `db` | Requires database connection |
