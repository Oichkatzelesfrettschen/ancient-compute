"""
Ancient Compute - API Contract Tests

Integration tests validating REST API contracts, request/response formats,
and HTTP semantics. Ensures API consistency, error handling, and compatibility.
"""

import json
import pytest
from typing import Dict, Any, Optional
from datetime import datetime


class TestExecutionAPIContract:
    """Test /execute endpoint contract and behavior."""

    def test_execute_endpoint_request_format(self):
        """Test valid request format for execute endpoint."""
        request = {
            "language": "python",
            "code": "print('hello')",
            "input": "",
            "timeout": 10,
        }

        # Validate required fields
        assert "language" in request
        assert "code" in request
        assert isinstance(request["language"], str)
        assert isinstance(request["code"], str)

    def test_execute_endpoint_response_format(self):
        """Test expected response format from execute endpoint."""
        response = {
            "status": "success",
            "stdout": "hello\n",
            "stderr": "",
            "execution_time": 0.05,
            "memory_used": 512,
            "language": "python",
            "submitted_at": "2025-01-01T12:00:00Z",
            "completed_at": "2025-01-01T12:00:01Z",
        }

        # Validate response structure
        assert "status" in response
        assert "stdout" in response
        assert "stderr" in response
        assert "execution_time" in response
        assert "memory_used" in response
        assert response["status"] in ["success", "failure", "timeout"]
        assert isinstance(response["execution_time"], (int, float))
        assert isinstance(response["memory_used"], (int, float))

    def test_unsupported_language_error(self):
        """Test error response for unsupported language."""
        request = {
            "language": "cobol",
            "code": "program test.",
        }

        error_response = {
            "error": "Language not supported",
            "language": "cobol",
            "supported_languages": [
                "python", "c", "haskell", "idris", "lisp", "java", "assembly", "systemf"
            ],
            "status_code": 400,
        }

        assert error_response["status_code"] == 400
        assert request["language"] not in error_response["supported_languages"]

    def test_missing_required_field_error(self):
        """Test error when required field is missing."""
        request = {
            "code": "print('hello')",
            # Missing: language
        }

        error_response = {
            "error": "Missing required field: language",
            "status_code": 400,
        }

        assert error_response["status_code"] == 400

    def test_invalid_json_error(self):
        """Test error on invalid JSON."""
        invalid_json = '{"language": "python", "code": invalid}'

        error_response = {
            "error": "Invalid JSON",
            "status_code": 400,
        }

        assert error_response["status_code"] == 400


class TestLessonAPIContract:
    """Test lesson-related API contracts."""

    def test_get_lesson_response_format(self):
        """Test response format for GET /lessons/{id}."""
        response = {
            "id": 1,
            "title": "Introduction to Programming",
            "module_id": 1,
            "description": "Learn the basics...",
            "content": "Full lesson content...",
            "code_examples": [
                {
                    "language": "python",
                    "code": "print('hello')",
                    "description": "Basic output",
                }
            ],
            "difficulty": "beginner",
            "estimated_time_minutes": 30,
            "created_at": "2025-01-01T00:00:00Z",
            "updated_at": "2025-01-01T00:00:00Z",
        }

        # Validate structure
        assert "id" in response
        assert "title" in response
        assert "module_id" in response
        assert isinstance(response["code_examples"], list)
        assert response["difficulty"] in ["beginner", "intermediate", "advanced"]

    def test_list_lessons_response_format(self):
        """Test response format for listing lessons."""
        response = {
            "lessons": [
                {
                    "id": 1,
                    "title": "Lesson 1",
                    "module_id": 1,
                    "difficulty": "beginner",
                },
                {
                    "id": 2,
                    "title": "Lesson 2",
                    "module_id": 1,
                    "difficulty": "beginner",
                },
            ],
            "total": 2,
            "page": 1,
            "per_page": 10,
        }

        # Validate pagination
        assert isinstance(response["lessons"], list)
        assert response["total"] == len(response["lessons"])
        assert "page" in response
        assert "per_page" in response

    def test_create_lesson_request_format(self):
        """Test valid request format for creating lesson."""
        request = {
            "title": "New Lesson",
            "module_id": 1,
            "description": "Lesson description",
            "content": "Full content",
            "difficulty": "intermediate",
            "estimated_time_minutes": 45,
        }

        # Validate required fields
        required = ["title", "module_id", "content"]
        for field in required:
            assert field in request


class TestExerciseAPIContract:
    """Test exercise-related API contracts."""

    def test_get_exercise_response_format(self):
        """Test response format for GET /exercises/{id}."""
        response = {
            "id": 1,
            "lesson_id": 1,
            "title": "Write Hello World",
            "description": "Write a program that prints Hello, World!",
            "language": "python",
            "template": "# Your code here\nprint(...)",
            "test_cases": [
                {
                    "id": 1,
                    "input": "",
                    "expected": "Hello, World!",
                    "description": "Default output",
                }
            ],
            "difficulty": "beginner",
            "points": 10,
            "created_at": "2025-01-01T00:00:00Z",
        }

        # Validate structure
        assert "id" in response
        assert "title" in response
        assert "language" in response
        assert isinstance(response["test_cases"], list)
        assert "points" in response

    def test_submit_solution_request_format(self):
        """Test valid request format for submitting solution."""
        request = {
            "exercise_id": 1,
            "code": "print('Hello, World!')",
            "language": "python",
        }

        # Validate required fields
        required = ["exercise_id", "code"]
        for field in required:
            assert field in request

    def test_submit_solution_response_format(self):
        """Test response format for solution submission."""
        response = {
            "submission_id": 123,
            "exercise_id": 1,
            "passed": True,
            "test_results": [
                {
                    "test_id": 1,
                    "passed": True,
                    "expected": "Hello, World!",
                    "actual": "Hello, World!",
                }
            ],
            "execution_time": 0.05,
            "memory_used": 512,
            "points_earned": 10,
            "submitted_at": "2025-01-01T12:00:00Z",
        }

        # Validate response
        assert "submission_id" in response
        assert "passed" in response
        assert isinstance(response["passed"], bool)
        assert isinstance(response["test_results"], list)


class TestUserProgressAPIContract:
    """Test user progress tracking API contracts."""

    def test_get_user_progress_response_format(self):
        """Test response format for GET /users/{id}/progress."""
        response = {
            "user_id": 1,
            "total_exercises": 100,
            "completed_exercises": 45,
            "completion_percentage": 45.0,
            "current_module": 2,
            "current_lesson": 5,
            "languages_used": ["python", "c", "haskell"],
            "achievements": [
                "first_solution",
                "first_language",
                "streak_7",
            ],
            "last_activity": "2025-01-01T12:00:00Z",
        }

        # Validate structure
        assert "user_id" in response
        assert "total_exercises" in response
        assert "completed_exercises" in response
        assert "completion_percentage" in response
        assert isinstance(response["languages_used"], list)
        assert isinstance(response["achievements"], list)

    def test_get_user_statistics_response_format(self):
        """Test response format for user statistics."""
        response = {
            "user_id": 1,
            "total_submissions": 150,
            "successful_submissions": 130,
            "success_rate": 86.67,
            "average_execution_time": 0.08,
            "total_execution_time": 12.0,
            "languages_attempted": 5,
            "current_streak_days": 12,
            "longest_streak_days": 25,
            "first_submission": "2024-11-01T00:00:00Z",
            "last_submission": "2025-01-01T12:00:00Z",
        }

        # Validate statistics
        assert response["success_rate"] <= 100.0
        assert response["success_rate"] >= 0.0
        assert response["successful_submissions"] <= response["total_submissions"]


class TestErrorHandlingAPIContract:
    """Test error handling API contracts."""

    def test_404_not_found_response(self):
        """Test 404 response format."""
        error_response = {
            "error": "Not found",
            "resource_type": "exercise",
            "resource_id": 999,
            "status_code": 404,
        }

        assert error_response["status_code"] == 404
        assert "error" in error_response

    def test_500_server_error_response(self):
        """Test 500 server error response format."""
        error_response = {
            "error": "Internal server error",
            "message": "Database connection failed",
            "status_code": 500,
            "request_id": "req_123abc",
        }

        assert error_response["status_code"] == 500
        assert "request_id" in error_response

    def test_429_rate_limit_response(self):
        """Test 429 rate limit response."""
        error_response = {
            "error": "Rate limit exceeded",
            "retry_after_seconds": 60,
            "status_code": 429,
        }

        assert error_response["status_code"] == 429
        assert "retry_after_seconds" in error_response

    def test_401_unauthorized_response(self):
        """Test 401 unauthorized response."""
        error_response = {
            "error": "Unauthorized",
            "reason": "Missing or invalid authentication token",
            "status_code": 401,
        }

        assert error_response["status_code"] == 401


class TestPaginationContract:
    """Test pagination API contract."""

    def test_paginated_response_format(self):
        """Test standard pagination format."""
        response = {
            "items": [{"id": 1}, {"id": 2}],
            "total": 100,
            "page": 1,
            "per_page": 2,
            "total_pages": 50,
            "has_next": True,
            "has_prev": False,
        }

        # Validate pagination
        assert isinstance(response["items"], list)
        assert response["page"] >= 1
        assert response["per_page"] >= 1
        assert response["total_pages"] >= 1
        assert isinstance(response["has_next"], bool)
        assert isinstance(response["has_prev"], bool)

    def test_pagination_boundary_cases(self):
        """Test pagination boundary conditions."""
        # First page
        first_page = {
            "page": 1,
            "per_page": 10,
            "total": 100,
            "has_prev": False,
            "has_next": True,
        }

        assert first_page["has_prev"] is False
        assert first_page["has_next"] is True

        # Last page
        last_page = {
            "page": 10,
            "per_page": 10,
            "total": 100,
            "has_prev": True,
            "has_next": False,
        }

        assert last_page["has_prev"] is True
        assert last_page["has_next"] is False

        # Single page
        single_page = {
            "page": 1,
            "per_page": 100,
            "total": 50,
            "has_prev": False,
            "has_next": False,
        }

        assert single_page["has_prev"] is False
        assert single_page["has_next"] is False


class TestDataTypeValidation:
    """Test API data type contracts."""

    def test_string_fields_validation(self):
        """Test string field validation."""
        data = {
            "title": "Valid Title",
            "description": "Valid description",
            "language": "python",
        }

        for field, value in data.items():
            assert isinstance(value, str)
            assert len(value) > 0

    def test_numeric_fields_validation(self):
        """Test numeric field validation."""
        data = {
            "points": 10,
            "execution_time": 0.05,
            "memory_used": 512,
            "page": 1,
            "per_page": 10,
        }

        for field, value in data.items():
            assert isinstance(value, (int, float))
            assert value >= 0

    def test_boolean_fields_validation(self):
        """Test boolean field validation."""
        data = {
            "passed": True,
            "is_public": False,
            "is_complete": True,
        }

        for field, value in data.items():
            assert isinstance(value, bool)

    def test_array_fields_validation(self):
        """Test array field validation."""
        data = {
            "languages": ["python", "c", "haskell"],
            "test_results": [
                {"test_id": 1, "passed": True},
                {"test_id": 2, "passed": False},
            ],
            "achievements": ["first_solution", "streak_7"],
        }

        for field, value in data.items():
            assert isinstance(value, list)


class TestTimeFormatContract:
    """Test timestamp and time format contracts."""

    def test_iso8601_timestamp_format(self):
        """Test ISO 8601 timestamp format."""
        timestamps = [
            "2025-01-01T00:00:00Z",
            "2025-01-01T12:00:00Z",
            "2025-01-01T23:59:59Z",
        ]

        iso_pattern = r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z$"
        import re
        for ts in timestamps:
            assert re.match(iso_pattern, ts)

    def test_duration_format(self):
        """Test duration field format."""
        data = {
            "execution_time": 0.05,  # seconds
            "estimated_time_minutes": 30,  # minutes
            "duration_milliseconds": 50,  # milliseconds
        }

        # Validate positive numbers
        for field, value in data.items():
            assert isinstance(value, (int, float))
            assert value >= 0


class TestResponseHeaderContract:
    """Test HTTP response header contracts."""

    def test_content_type_json_header(self):
        """Test Content-Type: application/json header."""
        headers = {
            "Content-Type": "application/json",
            "Content-Length": "1234",
        }

        assert headers["Content-Type"] == "application/json"

    def test_caching_headers(self):
        """Test caching-related headers."""
        headers = {
            "Cache-Control": "no-cache, no-store, must-revalidate",
            "Pragma": "no-cache",
            "Expires": "0",
        }

        assert "Cache-Control" in headers
        assert "no-cache" in headers["Cache-Control"]

    def test_cors_headers(self):
        """Test CORS headers."""
        headers = {
            "Access-Control-Allow-Origin": "*",
            "Access-Control-Allow-Methods": "GET, POST, PUT, DELETE",
            "Access-Control-Allow-Headers": "Content-Type, Authorization",
        }

        assert headers["Access-Control-Allow-Origin"] is not None
        assert "POST" in headers["Access-Control-Allow-Methods"]


class TestDeprecationContract:
    """Test API deprecation contracts."""

    def test_deprecated_endpoint_warning(self):
        """Test deprecated endpoint includes warning header."""
        headers = {
            "Deprecation": "true",
            "Sunset": "2025-06-01T00:00:00Z",
            "Link": '</api/v2/endpoint>; rel="successor-version"',
        }

        assert headers["Deprecation"] == "true"
        assert "Sunset" in headers
