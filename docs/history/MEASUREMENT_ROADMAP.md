Measurement Roadmap (Q1)

Planning Context
- Domain plan classification: active-domain
- Canonical strategy roadmap: `docs/general/MASTER_ROADMAP.md`
- Canonical execution tracker: `docs/general/TODO_TRACKER.md`

Phase 1: Baselines
- Document hardware/software env; create runbook.
- Collect emulator op latencies (10–30 runs): NOP, ADD, SUB, MULT, DIV, SQRT, LOAD, STOR, JMP.
- Collect backend API latency (p50/p99), CPU/mem; frontend build/test durations.
Phase 2: Instrumentation & Scripts
- Low-overhead timers for emulator; shell scripts for build/test; psutil for CPU/mem.
Phase 3: Analysis
- CSV outputs with min/avg/max/stddev; simple percentiles; plots optional.
Phase 4: Monitoring
- Makefile targets; scheduled runs; regression alerts on changes.
