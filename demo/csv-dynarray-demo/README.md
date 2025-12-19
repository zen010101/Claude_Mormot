# CSV to Dynamic Array Demo

This demo shows how to use mORMot 2's `DynArrayLoadCsv` function to parse CSV files into typed dynamic arrays of records.

## Source

Based on forum discussion: https://synopse.info/forum/viewtopic.php?id=7469

## The Problem

When using `DynArrayLoadCsv` without proper RTTI registration, it always returns `false`:

```pascal
type
  TCsvItem = packed record
    qty: integer;
    price: integer;
  end;
  TCsvItems = array of TCsvItem;

var
  csvItems: TCsvItems;
  csvContent: RawUtf8;
begin
  csvContent := RawUtf8FromFile('test.csv');

  // This FAILS - returns false!
  if DynArrayLoadCsv(csvItems, csvContent, TypeInfo(TCsvItems)) then
    // ...
```

## The Solution

You must register RTTI for your record type so that `DynArrayLoadCsv` can match CSV header names to record fields:

```pascal
const
  _TCsvItem = 'qty:integer price:integer';

begin
  // Register RTTI BEFORE any CSV parsing
  Rtti.RegisterFromText([TypeInfo(TCsvItem), _TCsvItem]);

  // Now DynArrayLoadCsv will work
  if DynArrayLoadCsv(csvItems, csvContent, TypeInfo(TCsvItems)) then
    // ...
end.
```

For units (not program files), you can use the `initialization` section:

```pascal
unit MyTypes;

interface
type
  TCsvItem = packed record
    qty: integer;
    price: integer;
  end;

implementation
uses mormot.core.rtti;

const
  _TCsvItem = 'qty:integer price:integer';

initialization
  Rtti.RegisterFromText([TypeInfo(TCsvItem), _TCsvItem]);
end.
```

## Key Points

| Requirement          | Description                                                   |
|----------------------|---------------------------------------------------------------|
| RTTI Registration    | Call `Rtti.RegisterFromText` before any CSV parsing           |
| Field Names          | RTTI field names must match CSV headers (case-insensitive)    |
| TypeInfo             | Register `TypeInfo(TCsvItem)` (record), not the array type    |
| Program vs Unit      | In programs, call at start of main block; in units, use initialization |

## How It Works

`DynArrayLoadCsv` internally:

1. Gets the RTTI for the array element type (the record)
2. Checks that `rt.Props.Count > 0` - requires registered properties
3. Parses the CSV header line
4. Matches each header to a record field by name using `rt.Props.Find(s)`
5. For each data row, sets field values using RTTI

Without RTTI registration, `rt.Props.Count = 0` and the function returns `false` immediately.

## Building

### Free Pascal (Lazarus)

```bash
lazbuild CsvDynArrayDemo.lpi
```

### Delphi

```bash
dcc64 CsvDynArrayDemo.dpr
```

## Running

Make sure `test.csv` is in the same directory as the executable:

```
qty,price
4,50
12,100
```

Expected output:

```
=== mORMot 2 DynArrayLoadCsv Demo ===

CSV Content:
------------
qty,price
4,50
12,100

DynArrayLoadCsv: OK
Loaded 2 items:

  [0] qty=4, price=50, subtotal=200
  [1] qty=12, price=100, subtotal=1200

Total value: 1400
```

## Common Mistakes

1. **Forgetting RTTI registration** - Most common cause of `DynArrayLoadCsv` returning `false`

2. **Mismatched field names** - CSV header `quantity` won't match record field `qty`

3. **Wrong TypeInfo** - Must register the record type, not the array type

4. **Empty CSV** - At least a header line and one data line are required

5. **Using initialization in program files** - `initialization` sections only work in units; in program files, call `Rtti.RegisterFromText` at the start of the main block

## See Also

- `mormot.core.search.pas` - Contains `DynArrayLoadCsv` implementation
- `mormot.core.rtti.pas` - RTTI registration functions
- `mormot.core.data.pas` - TDynArray wrapper
