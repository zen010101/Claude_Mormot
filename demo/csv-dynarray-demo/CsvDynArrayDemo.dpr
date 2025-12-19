/// CSV to Dynamic Array Demo using mORMot 2 DynArrayLoadCsv
// - demonstrates how to parse CSV files into typed record arrays
// - shows the importance of RTTI registration for record types
// - source: https://synopse.info/forum/viewtopic.php?id=7469
program CsvDynArrayDemo;

{$I mormot.defines.inc}

{$ifdef MSWINDOWS}
  {$APPTYPE CONSOLE}
{$endif}

uses
  {$I mormot.uses.inc}
  SysUtils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.rtti,
  mormot.core.data,
  mormot.core.search;

type
  /// record to hold each CSV row
  // - field names MUST match CSV header names (case-insensitive)
  TCsvItem = packed record
    qty: integer;
    price: integer;
  end;
  TCsvItems = array of TCsvItem;

const
  /// RTTI text definition for TCsvItem
  // - field order must match the record definition
  // - this enables DynArrayLoadCsv to find fields by name
  _TCsvItem = 'qty:integer price:integer';

procedure RunDemo;
var
  csvItems: TCsvItems;
  csvContent: RawUtf8;
  i: integer;
  total: integer;
begin
  WriteLn('=== mORMot 2 DynArrayLoadCsv Demo ===');
  WriteLn;

  // Load CSV content from file
  csvContent := RawUtf8FromFile('test.csv');
  if csvContent = '' then
  begin
    WriteLn('Error: test.csv not found!');
    WriteLn('Please create test.csv with content:');
    WriteLn('  qty,price');
    WriteLn('  4,50');
    WriteLn('  12,100');
    Exit;
  end;

  WriteLn('CSV Content:');
  WriteLn('------------');
  WriteLn(csvContent);
  WriteLn;

  // Parse CSV into dynamic array
  if DynArrayLoadCsv(csvItems, csvContent, TypeInfo(TCsvItems)) then
  begin
    WriteLn('DynArrayLoadCsv: OK');
    WriteLn('Loaded ', Length(csvItems), ' items:');
    WriteLn;

    total := 0;
    for i := 0 to High(csvItems) do
    begin
      WriteLn(Format('  [%d] qty=%d, price=%d, subtotal=%d',
        [i, csvItems[i].qty, csvItems[i].price,
         csvItems[i].qty * csvItems[i].price]));
      Inc(total, csvItems[i].qty * csvItems[i].price);
    end;

    WriteLn;
    WriteLn('Total value: ', total);
  end
  else
  begin
    WriteLn('DynArrayLoadCsv: FAILED');
    WriteLn;
    WriteLn('Common causes:');
    WriteLn('  1. RTTI not registered for record type');
    WriteLn('  2. CSV header names do not match record field names');
    WriteLn('  3. Empty or malformed CSV content');
  end;
end;

begin
  // IMPORTANT: Register RTTI for the record type BEFORE any CSV parsing
  // Without this, DynArrayLoadCsv cannot find record fields by name
  Rtti.RegisterFromText([TypeInfo(TCsvItem), _TCsvItem]);

  try
    RunDemo;
  except
    on E: Exception do
      WriteLn('Exception: ', E.Message);
  end;
  WriteLn;
  WriteLn('Press Enter to exit...');
  ReadLn;
end.
