# Ancient Compute - Language Service Implementations
from .c_service import CExecutor
from .python_service import PythonExecutor
from .haskell_service import HaskellExecutor

__all__ = [
    'CExecutor',
    'PythonExecutor',
    'HaskellExecutor',
]


def get_executor(language: str):
    """Factory function to get executor for a language"""
    executors = {
        "c": CExecutor,
        "python": PythonExecutor,
        "haskell": HaskellExecutor,
    }
    executor_class = executors.get(language.lower())
    if executor_class:
        return executor_class()
    return None
