# Ancient Compute - Haskell Execution Service
from ..base_executor import BaseExecutor


class HaskellExecutor(BaseExecutor):
    """Haskell executor with GHC compilation"""

    def __init__(self):
        super().__init__(
            language="haskell",
            docker_image="ancient-compute/haskell:latest",
            timeout=15
        )

    def _get_command(self, code_path: str) -> str:
        """GHC compile and execute command"""
        return "ghc -O2 -Wall -o /tmp/program Main.hs && /tmp/program < input.txt"
