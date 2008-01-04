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
    Flags, TimeDateStamp: cardinal;
    MajorVersion, MinorVersion: word;
    NameRVA: cardinal;
    OrdinalBase: cardinal;
    EATentriesCount: cardinal;
    NamePointersCount: cardinal;
    AddressTableRVA: cardinal;
    NamePointerTableRVA: cardinal;
    OrdinalTableRVA: cardinal;
  end;

  TExportFunction = record
    MemOffset: cardinal; // function's entry point memory offset
    CodeSectionOffset: cardinal; // Offset relative to function's code section
    Ordinal: cardinal; // ordinal number of function
    Section: integer; // index of (code) section containing this function
    Name: string;
  end;

  TExportSection = class(TSection)
  private
    fFunctionCount: integer;
    fFunctions: array of TExportFunction;
    function GetFunction(Index: integer): TExportFunction;
  public
    constructor Create(ExecFile: TObject); overload;
    constructor CreateFromPEFile(InputFile: TStream; FileOffset, ExportRVA, ExportDataSize, ImageBase: cardinal; aName: string; aExecFile: TObject);
    constructor CreateFromNEFile(InputFile: TStream; ResidentTableOffset, NonResidentTableOffset, NonResidentTableSize: cardinal; EntryTable: array of TEntryTableEntry; aName: string; aExecFile: TObject); overload;

    destructor Destroy; override;
    function SaveToFile  (DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; override;
    function LoadFromFile(DHF: TStream; var DAS: TextFile): boolean; overload; override;

    property FunctionCount: integer read fFunctionCount;
    property Functions[Index: integer]: TExportFunction read GetFunction;
  end;


implementation


uses
  ExecFileUnit,
  CodeSectionUnit,
  NEFileUnit,
  PEFileUnit;



constructor TExportSection.Create(ExecFile: TObject);
begin
  fTyp:= stExport;
  fExecFile:= ExecFile;
end;



destructor TExportSection.Destroy;
begin
  inherited;
end;



constructor TExportSection.CreateFromPEFile(InputFile: TStream; FileOffset, ExportRVA, ExportDataSize, ImageBase: cardinal; aName: string; aExecFile: TObject);
var
  ExportStream: TMemorystream;
  ExportDirTable: TPEExportDirectoryTable;
  FunctionRVA: cardinal;

  i: integer;
  PEFile: TPEFile;
  ForwardFunction: string;
  StreamPosition: cardinal;

  OrdinalTable: array of word;
  NamePointersTable: array of cardinal;

begin
  inherited Create(aName, aExecFile);
  fTyp:= stExport;

  pefile:= ExecFile as TPEFile;

  // Read Export section from file to temporary stream
  ExportStream:= TMemoryStream.Create;
  InputFile.Seek(FileOffset, 0);
  ExportStream.CopyFrom(InputFile, ExportDataSize);
  ExportStream.Position:= 0;

  ExportStream.Read(ExportDirTable, 40);
  fFunctionCount:= ExportDirTable.EATentriesCount;
  SetLength(fFunctions, fFunctionCount);

  // Read names of named fFunctions
  // Read Ordinal table
  SetLength(OrdinalTable, ExportDirTable.NamePointersCount);
  ExportStream.Seek(ExportDirTable.OrdinalTableRVA - ExportRVA, 0);
  ExportStream.Read(OrdinalTable[0], 2*ExportDirTable.NamePointersCount);

  // Read Name pointers table
  SetLength(NamePointersTable, ExportDirTable.NamePointersCount);
  ExportStream.Seek(ExportDirTable.NamePointerTableRVA - ExportRVA, 0);
  ExportStream.Read(NamePointersTable[0], 4*ExportDirTable.NamePointersCount);

  // Read fFunctions' names
  for i:= 0 to integer(ExportDirTable.NamePointersCount) - 1 do
    ReadStringFromStream(ExportStream, NamePointersTable[i] - ExportRVA, fFunctions[OrdinalTable[i]].name);
    // this should be correct according to PE specification !
    // ReadStringFromStream(ExportStream, NamePointersTable[i] - ExportRVA, fFunctions[OrdinalTable[i] - DirTable.OrdinalBase].name);


  // Read addresses of all function
  // Read fFunctions' mem. addresses from Export Address Table
  ExportStream.Seek(ExportDirTable.AddressTableRVA - ExportRVA, 0);
  for i:= 0 to fFunctionCount - 1 do begin
    ExportStream.Read(FunctionRVA, 4);
    fFunctions[i].Ordinal:= cardinal(i) + ExportDirTable.OrdinalBase;

    // FunctionRVA is really RVA of symbol in code/data(?) section
    if (FunctionRVA < ExportRVA) or (FunctionRVA >= ExportRVA + ExportDataSize) then begin
      fFunctions[i].MemOffset:= FunctionRVA + ImageBase;
      fFunctions[i].Section:= PEFile.GetSectionNumberFromRVA(FunctionRVA);
      if fFunctions[i].Section = -1 then
        fFunctions[i].CodeSectionOffset:= 0
      else
        fFunctions[i].CodeSectionOffset:= fFunctions[i].MemOffset - (PEFile.Sections[fFunctions[i].section] as TCodeSection).MemOffset;
    end

    // FunctionRVA is ForwardRVA
    else begin
      StreamPosition:= ExportStream.Position;
      ReadStringFromStream(ExportStream, FunctionRVA - ExportRVA, ForwardFunction);
      ExportStream.Position:= StreamPosition;
      fFunctions[i].Name:= fFunctions[i].Name + ' (forwarded to ' + ForwardFunction + ')';
      fFunctions[i].Section:= -1;
      fFunctions[i].CodeSectionOffset:= 0;
    end;
  end;

  ExportStream.Free;
end;



constructor TExportSection.CreateFromNEFile(InputFile: TStream; ResidentTableOffset, NonResidentTableOffset, NonResidentTableSize: cardinal; EntryTable: array of TEntryTableEntry; aName: string; aExecFile: TObject);

  procedure FindEntryPoint(Ordinal: cardinal; var segment: integer; var address: cardinal);
  var i: integer;
  begin
    i:=0;
    with (ExecFile as TNEFile) do
      while i < Length(EntryTable) do begin
        if ordinal <= EntryTable[i].count then begin
          case EntryTable[i].typ of
            etFixed: begin
              address:= EntryTable[i].Fixed[ordinal-1].Offset;
              segment:= -1;
            end;
            etMovable: begin
              address:= EntryTable[i].Movable[ordinal-1].Offset;
              segment:= EntryTable[i].Movable[ordinal-1].Segment-1;
            end;
          end;
          Exit;
        end;
        dec(ordinal,EntryTable[i].count);
        inc(i);
      end;
  end;


  procedure ReadNameTable(const TableOffset: cardinal; var FirstName: string);
  var
    FunIndex: integer;
    FirstNameLength: byte;
    FunNameLength: byte;
  begin
    FunIndex:= fFunctionCount;
    InputFile.Position:= TableOffset;
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
    fFunctionCount:= FunIndex;
  end;

var
  ModuleName, ModuleDescription: string;

begin
  inherited Create(aName, aExecFile);
  fTyp:= stExport;

  // Read names from Resident-Name Table:
  ReadNameTable(ResidentTableOffset, ModuleName);

  // Read names from Non-Resident-Name Table:
  ReadNameTable(NonResidentTableOffset, ModuleDescription);
end;



function TExportSection.SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean;
var
  FunctionIndex: integer;
begin
  inherited SaveToFile(DHF, DAS, SaveOptions);
  DHF.Write(fFunctionCount, 4);
  for FunctionIndex := 0 to fFunctionCount - 1 do begin
    DHF.Write(fFunctions[FunctionIndex], SizeOf(TExportFunction)-4);
    StreamWriteAnsiString(DHF, fFunctions[FunctionIndex].name);
  end;
  result:= true;
end;



function TExportSection.LoadFromFile(DHF: TStream; var DAS: TextFile):boolean;
var
  FunctionIndex: integer;
begin
  inherited LoadFromFile(DHF, DAS);
  DHF.Read(fFunctionCount, 4);
  SetLength(fFunctions, fFunctionCount);
  for FunctionIndex := 0 to fFunctionCount - 1 do begin
    DHF.Read(fFunctions[FunctionIndex], SizeOf(TExportFunction) - 4);
    fFunctions[FunctionIndex].name:= StreamReadAnsiString(DHF);
  end;
  result:= true;
end;



function TExportSection.GetFunction(Index: integer): TExportFunction;
begin
  result:= fFunctions[Index];
end;


end.
