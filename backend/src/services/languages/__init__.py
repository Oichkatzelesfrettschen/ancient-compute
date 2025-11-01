# Ancient Compute - Language Service Implementations
from .c_service import CService
from .python_service import PythonExecutor
from .haskell_service import HaskellService
from .babbage_assembly_service import BabbageAssemblyService

__all__ = [
    'CService',
    'PythonExecutor',
    'HaskellService',
    'BabbageAssemblyService',
]


def get_executor(language: str):
    """Factory function to get executor for a language"""
    executors = {
        "c": CService,
        "python": PythonExecutor,
        "haskell": HaskellService,
        "babbage-assembly": BabbageAssemblyService,
    }
    executor_class = executors.get(language.lower())
    if executor_class:
        return executor_class()
    return None
