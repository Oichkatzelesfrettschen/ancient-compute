# KFG (Khipu Field Guide) Normalized Schema

Files
- `docs/sources/quipu/kfg_normalized/QU001.json`
- `docs/sources/quipu/kfg_normalized/UR001.json`

Top-level keys
- `investigator_num`: stable artifact id
- `khipu`: key/value metadata from the Khipu sheet
- `primary_cord`: key/value metadata from the PrimaryCord sheet
- `clusters`: list of cluster text entries (free-form)
- `cords`: list of cord rows from the Cords sheet

Cord row fields (observed)
- `Cord_Name`, `Twist`, `Attachment`, `Knots`, `Length`, `Termination`, `Thickness`, `Color`, `Value`, `Alt_Value`, `Position`, `Notes`

Notes
- Many sheets contain human text and comments prefixed with `!`; parser ignores these.
- The `Knots` field encodes knot type/count/position and is not yet decoded.
