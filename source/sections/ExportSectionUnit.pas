unit ExportSectionUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  Classes,
  SysUtils,
  INIFiles,

  procmat,
  SectionUnit;

type

  TPEExportDirectoryTable = record
    Flags, TimeDateStamp: Cardinal;
    MajorVersion, MinorVersion: Word;
    NameRVA: Cardinal;
    OrdinalBase: Cardinal;
    EATentriesCount: Cardinal;
    NamePointersCount: Cardinal;
    AddressTableRVA: Cardinal;
    NamePointerTableRVA: Cardinal;
    OrdinalTableRVA: Cardinal;
  end;

  TExportFunction = record
    MemOffset: Cardinal; // function's entry point memory offset
    CodeSectionOffset: Cardinal; // Offset relative to function's code section
    Ordinal: Cardinal; // ordinal number of function
    Section: Integer; // index of (code) section containing this function
    Name: string;
  end;

  TExportSection = class(TSection)
  private
    fFunctionCount: Integer;
    fFunctions: array of TExportFunction;
    function GetFunction(Index: Integer): TExportFunction;
  public
    constructor Create(ExecFile: TObject); overload;
    constructor CreateFromPEFile(InputFile: TStream; FileOffset, ExportRVA, ExportDataSize, ImageBase: cardinal; aName: string; aExecFile: TObject);
    constructor CreateFromNEFile(InputFile: TStream; ResidentTableOffset, NonResidentTableOffset, NonResidentTableSize: Cardinal; EntryTable: array of TEntryTableEntry; aName: string; aExecFile: TObject); overload;

    destructor Destroy; override;
    procedure SaveToFile(DHF: TStream; var DAS: TextFile); override;
    procedure LoadFromFile(DHF: TStream; var DAS: TextFile); overload; override;

    property FunctionCount: Integer read fFunctionCount;
    property Functions[Index: Integer]: TExportFunction read GetFunction;
  end;


implementation


uses
  ExecFileUnit,
  CodeSectionUnit,
  NEFileUnit,
  PEFileUnit;



constructor TExportSection.Create(ExecFile: TObject);
begin
  fTyp := stExport;
  fExecFile := ExecFile;
end;



destructor TExportSection.Destroy;
begin
  inherited;
end;



constructor TExportSection.CreateFromPEFile(InputFile: TStream; FileOffset, ExportRVA, ExportDataSize, ImageBase: Cardinal; aName: string; aExecFile: TObject);
var
  ExportStream: TMemorystream;
  ExportDirTable: TPEExportDirectoryTable;
  FunctionRVA: Cardinal;

  i: Integer;
  PEFile: TPEFile;
  ForwardFunction: string;
  StreamPosition: Cardinal;

  OrdinalTable: array of Word;
  NamePointersTable: array of Cardinal;

begin
  inherited Create(aName, aExecFile);
  fTyp:= stExport;

  PEFile := ExecFile as TPEFile;

  // Read Export section from file to temporary stream
  ExportStream := TMemoryStream.Create;
  InputFile.Seek(FileOffset, 0);
  ExportStream.CopyFrom(InputFile, ExportDataSize);
  ExportStream.Position := 0;

  ExportStream.Read(ExportDirTable, 40);
  fFunctionCount := ExportDirTable.EATentriesCount;
  SetLength(fFunctions, fFunctionCount);

  // Read names of named fFunctions
  // Read Ordinal table
  SetLength(OrdinalTable, ExportDirTable.NamePointersCount);
  ExportStream.Seek(ExportDirTable.OrdinalTableRVA - ExportRVA, 0);
  ExportStream.Read(OrdinalTable[0], 2 * ExportDirTable.NamePointersCount);

  // Read Name pointers table
  SetLength(NamePointersTable, ExportDirTable.NamePointersCount);
  ExportStream.Seek(ExportDirTable.NamePointerTableRVA - ExportRVA, 0);
  ExportStream.Read(NamePointersTable[0], 4 * ExportDirTable.NamePointersCount);

  // Read fFunctions' names
  for i := 0 to Integer(ExportDirTable.NamePointersCount) - 1 do
    ReadStringFromStream(ExportStream, NamePointersTable[i] - ExportRVA, fFunctions[OrdinalTable[i]].name);
    // this should be correct according to PE specification !
    // ReadStringFromStream(ExportStream, NamePointersTable[i] - ExportRVA, fFunctions[OrdinalTable[i] - DirTable.OrdinalBase].name);


  // Read addresses of all function
  // Read fFunctions' mem. addresses from Export Address Table
  ExportStream.Seek(ExportDirTable.AddressTableRVA - ExportRVA, 0);
  for i := 0 to fFunctionCount - 1 do begin
    ExportStream.Read(FunctionRVA, 4);
    fFunctions[i].Ordinal := Cardinal(i) + ExportDirTable.OrdinalBase;

    // FunctionRVA is really RVA of symbol in code/data(?) section
    if (FunctionRVA < ExportRVA) or (FunctionRVA >= ExportRVA + ExportDataSize) then begin
      fFunctions[i].MemOffset := FunctionRVA + ImageBase;
      fFunctions[i].Section := PEFile.GetSectionNumberFromRVA(FunctionRVA);
      if fFunctions[i].Section = -1 then
        fFunctions[i].CodeSectionOffset := 0
      else
        fFunctions[i].CodeSectionOffset := fFunctions[i].MemOffset - (PEFile.Sections[fFunctions[i].Section] as TCodeSection).MemOffset;
    end

    // FunctionRVA is ForwardRVA
    else begin
      StreamPosition := ExportStream.Position;
      ReadStringFromStream(ExportStream, FunctionRVA - ExportRVA, ForwardFunction);
      ExportStream.Position := StreamPosition;
      fFunctions[i].Name := fFunctions[i].Name + ' (forwarded to ' + ForwardFunction + ')';
      fFunctions[i].Section := -1;
      fFunctions[i].CodeSectionOffset := 0;
    end;
  end;

  ExportStream.Free;
end;



constructor TExportSection.CreateFromNEFile(InputFile: TStream; ResidentTableOffset, NonResidentTableOffset, NonResidentTableSize: Cardinal; EntryTable: array of TEntryTableEntry; aName: string; aExecFile: TObject);

  procedure FindEntryPoint(Ordinal: Cardinal; var Segment: Integer; var Address: Cardinal);
  var
    i: Integer;
  begin
    i := 0;
    with (ExecFile as TNEFile) do
      while i < Length(EntryTable) do begin
        if Ordinal <= EntryTable[i].count then begin
          case EntryTable[i].typ of
            etFixed: begin
              Address := EntryTable[i].Fixed[Ordinal - 1].Offset;
              Segment := -1;
            end;
            etMovable: begin
              Address := EntryTable[i].Movable[Ordinal - 1].Offset;
              Segment := EntryTable[i].Movable[Ordinal - 1].Segment - 1;
            end;
          end;
          Exit;
        end;
        Dec(Ordinal, EntryTable[i].count);
        Inc(i);
      end;
  end;


  procedure ReadNameTable(const TableOffset: Cardinal; var FirstName: string);
  var
    FunIndex: Integer;
    FirstNameLength: Byte;
    FunNameLength: Byte;
  begin
    FunIndex := fFunctionCount;
    InputFile.Position := TableOffset;
    InputFile.Read(FirstNameLength, 1);
    SetLength(FirstName, FirstNameLength);
    InputFile.Read(FirstName[1], FirstNameLength);
    InputFile.Seek(2, soCurrent);

    InputFile.Read(FunNameLength, 1);
    while FunNameLength > 1 do begin
      SetLength(fFunctions, FunIndex+1);
      SetLength(fFunctions[FunIndex].Name, FunNameLength);
      InputFile.Read(fFunctions[FunIndex].Name[1], FunNameLength);
      InputFile.Read(fFunctions[FunIndex].Ordinal, 2);
      FindEntryPoint(fFunctions[FunIndex].Ordinal, fFunctions[FunIndex].Section, fFunctions[FunIndex].CodeSectionOffset);
      fFunctions[FunIndex].MemOffset:= fFunctions[FunIndex].CodeSectionOffset;
      InputFile.Read(FunNameLength, 1);
      Inc(FunIndex);
    end;
    fFunctionCount := FunIndex;
  end;

var
  ModuleName, ModuleDescription: string;

begin
  inherited Create(aName, aExecFile);
  fTyp := stExport;

  // Read names from Resident-Name Table:
  ReadNameTable(ResidentTableOffset, ModuleName);

  // Read names from Non-Resident-Name Table:
  ReadNameTable(NonResidentTableOffset, ModuleDescription);
end;



procedure TExportSection.SaveToFile(DHF: TStream; var DAS: TextFile);
var
  FunctionIndex: Integer;
begin
  inherited SaveToFile(DHF, DAS);
  DHF.Write(fFunctionCount, 4);
  for FunctionIndex := 0 to fFunctionCount - 1 do begin
    DHF.Write(fFunctions[FunctionIndex], SizeOf(TExportFunction) - 4);
    StreamWriteAnsiString(DHF, fFunctions[FunctionIndex].name);
  end;
end;



procedure TExportSection.LoadFromFile(DHF: TStream; var DAS: TextFile);
var
  FunctionIndex: Integer;
begin
  inherited LoadFromFile(DHF, DAS);
  DHF.Read(fFunctionCount, 4);
  SetLength(fFunctions, fFunctionCount);
  for FunctionIndex := 0 to fFunctionCount - 1 do begin
    DHF.Read(fFunctions[FunctionIndex], SizeOf(TExportFunction) - 4);
    fFunctions[FunctionIndex].name:= StreamReadAnsiString(DHF);
  end;
end;



function TExportSection.GetFunction(Index: Integer): TExportFunction;
begin
  Result := fFunctions[Index];
end;


end.
