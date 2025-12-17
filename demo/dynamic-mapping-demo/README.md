# Dynamic Table/Column Mapping Demo - mORMot 2

This demo demonstrates how to dynamically map ORM table and column names from a configuration file at runtime, without modifying source code.

## Problem

When working with existing databases or when table/column names need to be configurable, hardcoding names in ORM classes is inflexible:

```pascal
TDbBirthdayRecord = class(TOrmNoCase)
published
  property DbCountry: RawUtf8 ...;        // What if DB uses "country_code"?
  property DbBirthdayDate: TDateTime ...; // What if DB uses "celebration_date"?
```

**Forum Discussion:** https://synopse.info/forum/viewtopic.php?id=7462

## Solution: OrmMapExternal + MapField

mORMot 2 provides `OrmMapExternal` and `MapField` to establish runtime mappings between ORM property names and actual database identifiers:

```pascal
// Map ORM class to external DB with custom table name
Mapping := OrmMapExternal(Model, TDbBirthdayRecord, ExternalDB, 'BirthdayCelebration');

// Map individual columns
Mapping^.MapField('DbCountry', 'country_code');
Mapping^.MapField('DbBirthdayDate', 'celebration_date');
```

## Configuration File

The demo reads mappings from `mapping_config.json`:

```json
{
  "tableName": "BirthdayCelebration",
  "columns": {
    "DbCountry": "country_code",
    "DbBirthdayDate": "celebration_date",
    "DbBirthdayDateExtraInfo": "extra_notes"
  }
}
```

Simply edit this file to change table/column names without recompiling.

## Files

| File                      | Description                           |
|---------------------------|---------------------------------------|
| `DynamicMappingDemo.dpr`  | Main program source                   |
| `DynamicMappingDemo.lpi`  | Lazarus project file                  |
| `mapping_config.json`     | Configuration (auto-created on first run) |

## Building

### With Free Pascal / Lazarus

```bash
lazbuild DynamicMappingDemo.lpi
```

### With Delphi

Open `DynamicMappingDemo.dpr` in Delphi IDE and compile.

## Usage

1. **Run the demo:**
   ```
   DynamicMappingDemo.exe
   ```

2. **First run** creates `mapping_config.json` with default mappings

3. **Edit configuration** to customize table/column names

4. **Run again** to see the new mappings applied

## Output Example

```
==============================================
 Dynamic Table/Column Mapping Demo (mORMot2)
==============================================

=== Configuration ===
Table name: BirthdayCelebration
Column mappings:
  DbCountry -> country_code
  DbBirthdayDate -> celebration_date
  DbBirthdayDateExtraInfo -> extra_notes

=== Actual SQL Schema (from external DB) ===
CREATE TABLE BirthdayCelebration(
  ID INTEGER PRIMARY KEY,
  country_code TEXT COLLATE NOCASE,
  celebration_date TEXT COLLATE NOCASE,
  extra_notes TEXT COLLATE NOCASE
)

=== Adding Records ===
Added record with ID: 1
Added record with ID: 2
Added record with ID: 3
```

## Key Points

- **ORM queries use property names** - mORMot automatically translates to mapped column names
- **No code changes needed** - just edit the JSON config file
- **Works with any external database** - SQLite, PostgreSQL, MySQL, etc.

## License

This demo is part of the mORMot 2 framework examples.
See https://github.com/synopse/mORMot2 for licensing information.
