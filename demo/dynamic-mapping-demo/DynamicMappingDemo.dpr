/// Dynamic Table/Column Mapping Demo for mORMot 2
// - demonstrates OrmMapExternal with MapField for runtime column mapping
// - forum discussion: https://synopse.info/forum/viewtopic.php?id=7462
program DynamicMappingDemo;

{$I mormot.defines.inc}

{$APPTYPE CONSOLE}

uses
  {$I mormot.uses.inc}
  SysUtils,
  Classes,
  mormot.core.base,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.json,
  mormot.core.variants,
  mormot.core.os,
  mormot.orm.base,
  mormot.orm.core,
  mormot.orm.sql,
  mormot.rest.sqlite3,
  mormot.db.core,
  mormot.db.sql,
  mormot.db.sql.sqlite3,
  mormot.db.raw.sqlite3,
  mormot.db.raw.sqlite3.static;

type
  /// Our ORM class with generic property names
  TDbBirthdayRecord = class(TOrmNoCase)
  private
    fDbCountry: RawUtf8;
    fDbBirthdayDate: TDateTime;
    fDbBirthdayDateExtraInfo: RawUtf8;
  published
    property DbCountry: RawUtf8 read fDbCountry write fDbCountry;
    property DbBirthdayDate: TDateTime read fDbBirthdayDate write fDbBirthdayDate;
    property DbBirthdayDateExtraInfo: RawUtf8 read fDbBirthdayDateExtraInfo write fDbBirthdayDateExtraInfo;
  end;

var
  Model: TOrmModel;
  Rest: TRestServerDB;
  ExternalDB: TSqlDBSQLite3ConnectionProperties;
  Rec: TDbBirthdayRecord;
  Config: IDocDict;
  Columns: IDocDict;
  ConfigFile: TFileName;
  TableName: RawUtf8;
  ID: TID;
  Key: RawUtf8;
  JsonContent: RawUtf8;
  Mapping: POrmMapping;

procedure LoadConfig;
const
  DEFAULT_CONFIG =
    '{' + #13#10 +
    '  "tableName": "BirthdayCelebration",' + #13#10 +
    '  "columns": {' + #13#10 +
    '    "DbCountry": "country_code",' + #13#10 +
    '    "DbBirthdayDate": "celebration_date",' + #13#10 +
    '    "DbBirthdayDateExtraInfo": "extra_notes"' + #13#10 +
    '  }' + #13#10 +
    '}';
begin
  // Load config from JSON file
  ConfigFile := Executable.ProgramFilePath + 'mapping_config.json';

  if not FileExists(ConfigFile) then
  begin
    WriteLn('Config file not found: ', ConfigFile);
    WriteLn('Creating default config...');
    FileFromString(DEFAULT_CONFIG, ConfigFile);
    JsonContent := DEFAULT_CONFIG;
    WriteLn('Default config created.');
  end
  else
  begin
    // Load existing config - read file and parse JSON
    JsonContent := StringFromFile(ConfigFile);
    WriteLn('Loaded config from: ', ConfigFile);
  end;

  // Parse JSON
  Config := DocDict(JsonContent);

  // Display loaded config
  WriteLn;
  WriteLn('=== Configuration ===');
  TableName := Config.S['tableName'];
  WriteLn('Table name: ', TableName);
  WriteLn('Column mappings:');
  Columns := Config.D['columns'];
  for Key in Columns.Keys do
    WriteLn('  ', Key, ' -> ', Columns.S[Key]);
  WriteLn;
end;

procedure SetupModelWithMapping;
begin
  // Create the ORM model
  Model := TOrmModel.Create([TDbBirthdayRecord]);

  // Create external SQLite connection (as a file or :memory:)
  ExternalDB := TSqlDBSQLite3ConnectionProperties.Create(
    Executable.ProgramFilePath + 'birthday.db3', '', '', '');

  // Map the ORM class to external DB with custom table name from config
  Mapping := OrmMapExternal(Model, TDbBirthdayRecord, ExternalDB, TableName);

  // Map individual columns from config
  WriteLn('=== Applying Column Mappings ===');
  Columns := Config.D['columns'];
  for Key in Columns.Keys do
  begin
    Mapping^.MapField(Key, Columns.S[Key]);
    WriteLn('Mapped: ', Key, ' -> ', Columns.S[Key]);
  end;
  WriteLn;
end;

procedure DemoOperations;
var
  Schema: RawUtf8;
  Rows: ISqlDBRows;
begin
  // Create REST server using the model (internally managed)
  Rest := TRestServerDB.Create(Model, ':memory:');
  try
    Rest.Server.CreateMissingTables;

    // Show mapping info
    WriteLn('=== External Table Info ===');
    WriteLn('ORM Table: ', TDbBirthdayRecord.SqlTableName);
    WriteLn('External Table: ', TableName);
    WriteLn;

    // Show actual SQL schema in the external database
    WriteLn('=== Actual SQL Schema (from external DB) ===');
    Rows := ExternalDB.Execute(
      'SELECT sql FROM sqlite_master WHERE type=''table'' AND name=?', [TableName]);
    if (Rows <> nil) and Rows.Step then
    begin
      Schema := Rows.ColumnUtf8(0);
      WriteLn(Schema);
    end;
    WriteLn;

    // Create and add records
    WriteLn('=== Adding Records ===');
    Rec := TDbBirthdayRecord.Create;
    try
      Rec.DbCountry := 'USA';
      Rec.DbBirthdayDate := EncodeDate(1990, 7, 4);
      Rec.DbBirthdayDateExtraInfo := 'Independence Day celebration';
      ID := Rest.Orm.Add(Rec, true);
      WriteLn('Added record with ID: ', ID);

      Rec.DbCountry := 'France';
      Rec.DbBirthdayDate := EncodeDate(1989, 7, 14);
      Rec.DbBirthdayDateExtraInfo := 'Bastille Day';
      ID := Rest.Orm.Add(Rec, true);
      WriteLn('Added record with ID: ', ID);

      Rec.DbCountry := 'Germany';
      Rec.DbBirthdayDate := EncodeDate(1990, 10, 3);
      Rec.DbBirthdayDateExtraInfo := 'German Unity Day';
      ID := Rest.Orm.Add(Rec, true);
      WriteLn('Added record with ID: ', ID);
    finally
      Rec.Free;
    end;
    WriteLn;

    // Retrieve and display records
    WriteLn('=== Retrieved Records ===');
    Rec := TDbBirthdayRecord.Create;
    try
      if Rest.Orm.Retrieve(1, Rec) then
      begin
        WriteLn('Record 1:');
        WriteLn('  Country (DbCountry -> ', Columns.S['DbCountry'], '): ', Rec.DbCountry);
        WriteLn('  Date (DbBirthdayDate -> ', Columns.S['DbBirthdayDate'], '): ', DateToStr(Rec.DbBirthdayDate));
        WriteLn('  Extra (DbBirthdayDateExtraInfo -> ', Columns.S['DbBirthdayDateExtraInfo'], '): ', Rec.DbBirthdayDateExtraInfo);
      end;
      WriteLn;

      if Rest.Orm.Retrieve(2, Rec) then
      begin
        WriteLn('Record 2:');
        WriteLn('  Country: ', Rec.DbCountry);
        WriteLn('  Date: ', DateToStr(Rec.DbBirthdayDate));
        WriteLn('  Extra Info: ', Rec.DbBirthdayDateExtraInfo);
      end;
    finally
      Rec.Free;
    end;
    WriteLn;

    // Show that we can query by property name, ORM translates to mapped column
    WriteLn('=== Query Demo ===');
    WriteLn('Querying where DbCountry = "France"...');
    WriteLn('(ORM translates DbCountry to "', Columns.S['DbCountry'], '" in SQL)');
    Rec := TDbBirthdayRecord.Create;
    try
      if Rest.Orm.Retrieve('DbCountry=?', [], ['France'], Rec) then
      begin
        WriteLn('Found: ', Rec.DbCountry, ' - ', DateToStr(Rec.DbBirthdayDate));
      end;
    finally
      Rec.Free;
    end;

  finally
    Rest.Free;
  end;
end;

begin
  try
    WriteLn('==============================================');
    WriteLn(' Dynamic Table/Column Mapping Demo (mORMot2)');
    WriteLn('==============================================');
    WriteLn;

    // Step 1: Load configuration
    LoadConfig;

    // Step 2: Setup model with external mappings
    SetupModelWithMapping;

    // Step 3: Demonstrate CRUD operations
    DemoOperations;

    WriteLn;
    WriteLn('=== Demo Complete ===');
    WriteLn('Edit "mapping_config.json" to change table/column names.');
    WriteLn('Press Enter to exit...');
    ReadLn;

  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.ClassName, ' - ', E.Message);
      ReadLn;
    end;
  end;

  ExternalDB.Free;
  Model.Free;
end.
