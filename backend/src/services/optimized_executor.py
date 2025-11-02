"""
Ancient Compute - Optimized Execution Service

High-performance wrapper around ExecutionOrchestrator with integrated
result caching and database query optimization.
"""

from typing import Optional, List, Dict, Any, Tuple

from typing import TYPE_CHECKING
try:
    from sqlalchemy.orm import Session  # type: ignore
except Exception:  # pragma: no cover - allow running without SQLAlchemy
    Session = Any  # type: ignore

from .execution_orchestrator import ExecutionOrchestrator
from .execution_cache import get_execution_cache, ExecutionCache
from .query_cache import get_query_cache, QueryCache
from .base_executor import ExecutionResult, ExecutionStatus


class OptimizedExecutor:
    """
    High-performance code executor with integrated caching.

    Combines execution result caching and database query optimization
    to minimize latency and resource usage.
    """

    def __init__(
        self,
        orchestrator: Optional[ExecutionOrchestrator] = None,
        execution_cache: Optional[ExecutionCache] = None,
        query_cache: Optional[QueryCache] = None,
    ):
        """
        Initialize optimized executor.

        Args:
            orchestrator: ExecutionOrchestrator instance (created if None)
            execution_cache: ExecutionCache instance (uses global if None)
            query_cache: QueryCache instance (uses global if None)
        """
        self.orchestrator = orchestrator or ExecutionOrchestrator()
        self.execution_cache = execution_cache or get_execution_cache()
        self.query_cache = query_cache or get_query_cache()

    async def execute_code(
        self,
        code: str,
        language: str,
        exercise: Optional[Any] = None,
        timeout: int = 10,
        use_cache: bool = True,
        input_data: str = "",
    ) -> ExecutionResult:
        """
        Execute code with caching and optimization.

        Args:
            code: Source code to execute
            language: Programming language
            exercise: Optional Exercise model
            timeout: Execution timeout in seconds
            use_cache: Whether to use result cache
            input_data: Optional stdin input

        Returns:
            ExecutionResult (cached or newly computed)
        """
        # Check cache first if enabled
        if use_cache:
            cached_result = self.execution_cache.get(
                language, code, input_data
            )
            if cached_result is not None:
                return cached_result

        # Execute code
        result = await self.orchestrator.execute_code(
            code=code,
            language=language,
            exercise=exercise,
            timeout=timeout,
        )

        # Cache successful results
        if use_cache:
            self.execution_cache.set(language, code, result, input_data)

        return result

    async def validate_test_cases(
        self,
        execution_result: ExecutionResult,
        test_cases: List[Dict[str, Any]],
        exercise_language: str = "python",
    ) -> Tuple[List[Dict[str, Any]], int, int]:
        """
        Validate execution against test cases.

        Delegates to orchestrator's test validation.
        """
        return await self.orchestrator.validate_test_cases(
            execution_result=execution_result,
            test_cases=test_cases,
            exercise_language=exercise_language,
        )

    def get_exercise_with_optimization(
        self, db: Session, exercise_id: int
    ) -> Optional[Any]:
        """
        Get exercise with optimized queries (eager loading).

        Uses query cache and eager loading to minimize database round-trips.
        """
        return self.query_cache.get_exercise_with_relations(
            db, exercise_id
        )

    def get_supported_languages(self) -> List[str]:
        """Get list of supported programming languages."""
        return self.orchestrator.get_supported_languages()

    def get_cache_stats(self) -> Dict[str, Dict[str, Any]]:
        """Get statistics from all caches."""
        return {
            "execution_cache": self.execution_cache.get_stats(),
            "query_cache": self.query_cache.get_stats(),
        }

    def clear_caches(self) -> None:
        """Clear all caches (useful for testing)."""
        self.execution_cache.clear()
        self.query_cache.clear()

    def invalidate_exercise_cache(self, exercise_id: int) -> int:
        """
        Invalidate cache entries related to specific exercise.

        Args:
            exercise_id: Exercise ID to invalidate

        Returns:
            Number of cache entries invalidated
        """
        return self.query_cache.invalidate(pattern=f"*exercise={exercise_id}*")


# Global optimized executor instance
_optimized_executor = None


def get_optimized_executor() -> OptimizedExecutor:
    """Get global optimized executor instance."""
    global _optimized_executor
    if _optimized_executor is None:
        _optimized_executor = OptimizedExecutor()
    return _optimized_executor
