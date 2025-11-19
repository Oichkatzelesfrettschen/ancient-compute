# Ancient Compute Backend - Authentication Middleware
"""
JWT-based authentication middleware for securing API endpoints.

Implements:
- JWT token validation
- User authentication
- Protected endpoint decorator
- Token refresh mechanism
"""

import jwt
from datetime import datetime, timedelta
from typing import Optional
from fastapi import HTTPException, Security, Depends
from fastapi.security import HTTPBearer, HTTPAuthorizationCredentials
from pydantic import BaseModel
from sqlalchemy.orm import Session

from ..config import settings
from ..database import get_db
from ..models.user import User

security = HTTPBearer()


class TokenData(BaseModel):
    """JWT token payload structure"""

    user_id: int
    username: str
    email: str
    exp: datetime


class UserResponse(BaseModel):
    """User information returned after authentication"""

    id: int
    username: str
    email: str
    is_active: bool = True


def create_access_token(
    user_id: int, username: str, email: str, expires_delta: Optional[timedelta] = None
) -> str:
    """
    Create JWT access token for authenticated user.

    Args:
        user_id: User's database ID
        username: User's username
        email: User's email address
        expires_delta: Optional custom expiration time (default: 30 minutes)

    Returns:
        Encoded JWT token string
    """
    if expires_delta:
        expire = datetime.utcnow() + expires_delta
    else:
        expire = datetime.utcnow() + timedelta(minutes=30)

    to_encode = {"sub": user_id, "username": username, "email": email, "exp": expire}

    encoded_jwt = jwt.encode(to_encode, settings.SECRET_KEY, algorithm="HS256")
    return encoded_jwt


def decode_token(token: str) -> TokenData:
    """
    Decode and validate JWT token.

    Args:
        token: JWT token string

    Returns:
        TokenData object with user information

    Raises:
        HTTPException: If token is invalid or expired
    """
    try:
        payload = jwt.decode(token, settings.SECRET_KEY, algorithms=["HS256"])
        user_id: int = payload.get("sub")
        username: str = payload.get("username")
        email: str = payload.get("email")
        exp: datetime = datetime.fromtimestamp(payload.get("exp"))

        if user_id is None or username is None:
            raise HTTPException(status_code=401, detail="Invalid token: missing user data")

        return TokenData(user_id=user_id, username=username, email=email, exp=exp)

    except jwt.ExpiredSignatureError:
        raise HTTPException(status_code=401, detail="Token has expired")
    except jwt.InvalidTokenError:
        raise HTTPException(status_code=401, detail="Invalid token")


async def get_current_user(
    credentials: HTTPAuthorizationCredentials = Security(security),
    db: Session = Depends(get_db),
) -> UserResponse:
    """
    Validate JWT token and return current user.

    This is a dependency injection function for FastAPI endpoints.
    Use with `current_user: User = Depends(get_current_user)` in endpoint signature.

    Args:
        credentials: HTTP Authorization credentials (Bearer token)
        db: Database session for user verification

    Returns:
        UserResponse object with authenticated user information

    Raises:
        HTTPException: If authentication fails or user not found

    Example:
        @app.get("/protected")
        async def protected_endpoint(current_user: UserResponse = Depends(get_current_user)):
            return {"message": f"Hello {current_user.username}"}
    """
    token = credentials.credentials
    token_data = decode_token(token)

    # Query user from database to verify they still exist and are active
    db_user = db.query(User).filter(User.id == token_data.user_id).first()

    if db_user is None:
        raise HTTPException(status_code=404, detail="User not found")

    user = UserResponse(
        id=db_user.id,
        username=db_user.username,
        email=db_user.email,
        is_active=db_user.is_active,
    )

    if not user.is_active:
        raise HTTPException(status_code=403, detail="User account is inactive")

    return user


async def get_current_active_user(
    current_user: UserResponse = Depends(get_current_user),
) -> UserResponse:
    """
    Get current active user (additional validation layer).

    Args:
        current_user: User from get_current_user dependency

    Returns:
        UserResponse object if user is active

    Raises:
        HTTPException: If user is inactive
    """
    if not current_user.is_active:
        raise HTTPException(status_code=403, detail="Inactive user")
    return current_user
