# Ancient Compute - Language Service Implementations
from .c_service import CService
from .python_service import PythonService
from .haskell_service import HaskellService
from .babbage_assembly_service import BabbageAssemblyService
from .lisp_service import LISPService
from .idris_service import IDRISService
from .systemf_service import SystemFService
from .java_service import JavaService

__all__ = [
    "CService",
    "PythonService",
    "HaskellService",
    "BabbageAssemblyService",
    "LISPService",
    "IDRISService",
    "SystemFService",
    "JavaService",
]


def get_executor(language: str):
    """Factory function to get executor for a language"""
    executors = {
        "c": CService,
        "python": PythonService,
        "haskell": HaskellService,
        "babbage-assembly": BabbageAssemblyService,
        "lisp": LISPService,
        "idris2": IDRISService,
        "idris": IDRISService,
        "systemf": SystemFService,
        "system-f": SystemFService,
        "java": JavaService,
    }
    executor_class = executors.get(language.lower())
    if executor_class:
        return executor_class()
    return None
