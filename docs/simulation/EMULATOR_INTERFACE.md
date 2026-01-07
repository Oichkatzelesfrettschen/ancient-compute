# Common Emulator Interface

Purpose: standardize logic-only emulators across mechanisms.

Required methods
- step(input): apply one operation or event
- run(inputs): batch execution (list of inputs)
- state(): return serializable state
- reset(): restore initial state

Required metadata
- mechanism_name
- tier (0-3)
- deterministic (bool)
- sources (local paths or URLs)

State expectations
- Use plain dicts or dataclasses with primitive types
- Avoid side effects outside the instance
