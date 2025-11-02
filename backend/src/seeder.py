# Ancient Compute - Database Seeder

from .database import SessionLocal, engine, Base
from .models import Era, Module, Lesson, User


def seed_database():
    """Populate the database with initial data."""
    db = SessionLocal()

    try:
        # Create a placeholder user
        if not db.query(User).first():
            user = User(email="test@example.com", hashed_password="placeholder")
            db.add(user)
            db.commit()

        # Create eras
        if not db.query(Era).first():
            eras = [
                Era(name="Prehistory", start_year=-20000, end_year=-3000),
                Era(name="Ancient", start_year=-3000, end_year=500),
                Era(name="Medieval", start_year=500, end_year=1500),
                Era(name="Early Modern", start_year=1500, end_year=1800),
                Era(name="Modern", start_year=1800, end_year=1950),
                Era(name="Contemporary", start_year=1950, end_year=2025),
            ]
            db.add_all(eras)
            db.commit()

        # Create modules and lessons
        if not db.query(Module).first():
            prehistory_era = db.query(Era).filter_by(name="Prehistory").first()
            ancient_era = db.query(Era).filter_by(name="Ancient").first()

            modules = [
                Module(
                    era_id=prehistory_era.id,
                    slug="prehistoric-computation",
                    title="Prehistoric Computation",
                    description="The origins of counting and computation.",
                    era_enum="prehistory",
                    start_year=-20000,
                    end_year=-3000,
                    sequence_order=1,
                ),
                Module(
                    era_id=ancient_era.id,
                    slug="babylonian-mathematics",
                    title="Babylonian Mathematics",
                    description="The mathematics of the Babylonians.",
                    era_enum="ancient",
                    start_year=-3000,
                    end_year=-500,
                    sequence_order=2,
                ),
            ]
            db.add_all(modules)
            db.commit()

            prehistory_module = db.query(Module).filter_by(slug="prehistoric-computation").first()
            babylonian_module = db.query(Module).filter_by(slug="babylonian-mathematics").first()

            lessons = [
                Lesson(
                    module_id=prehistory_module.id,
                    slug="tally-sticks",
                    title="Tally Sticks",
                    lesson_type="reading",
                    sequence_order=1,
                    description="The earliest form of counting.",
                ),
                Lesson(
                    module_id=babylonian_module.id,
                    slug="base-60-system",
                    title="Base-60 System",
                    lesson_type="reading",
                    sequence_order=1,
                    description="The Babylonian number system.",
                ),
            ]
            db.add_all(lessons)
            db.commit()

        print("Database seeded successfully.")

    finally:
        db.close()


if __name__ == "__main__":
    Base.metadata.create_all(bind=engine)
    seed_database()
