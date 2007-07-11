unit ExportSectionUnit;

{$INCLUDE 'DELVER.INC'}

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
    FunctionCount: integer;
    Functions: array of TExportFunction;
    constructor Create(EFile:TObject); overload;
    constructor CreateFromPEFile(InputFile: TStream; FileOffset, ExportRVA, ExportDataSize, ImageBase: cardinal; aName: string; aExecFile: TObject);
    constructor CreateFromNEFile(InputFile: TStream; ResidentTableOffset, NonResidentTableOffset, NonResidentTableSize: cardinal; aName: string; aExecFile: TObject); overload;

    destructor Destroy; override;
    function SaveToFile  (DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; override;
    function LoadFromFile(DHF: TStream; var DAS: TextFile):boolean; overload; override;
  end;



implementation

uses
  ExecFileUnit,
  CodeSectionUnit,
  NEFileUnit,
  PEFileUnit;

constructor TExportSection.Create(efile:TObject);
begin
  fTyp:=stExport;
  fExecFile:=efile;
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
  FunctionCount:= ExportDirTable.EATentriesCount;
  SetLength(Functions, FunctionCount);

  // Read names of named functions
  // Read Ordinal table
  SetLength(OrdinalTable, ExportDirTable.NamePointersCount);
  ExportStream.Seek(ExportDirTable.OrdinalTableRVA - ExportRVA, 0);
  ExportStream.Read(OrdinalTable[0], 2*ExportDirTable.NamePointersCount);

  // Read Name pointers table
  SetLength(NamePointersTable, ExportDirTable.NamePointersCount);
  ExportStream.Seek(ExportDirTable.NamePointerTableRVA - ExportRVA, 0);
  ExportStream.Read(NamePointersTable[0], 4*ExportDirTable.NamePointersCount);

  // Read functions' names
  for i:= 0 to integer(ExportDirTable.NamePointersCount) - 1 do
    ReadStringFromStream(ExportStream, NamePointersTable[i] - ExportRVA, functions[OrdinalTable[i]].name);
    // this should be correct according to PE specification !
    // ReadStringFromStream(ExportStream, NamePointersTable[i] - ExportRVA, functions[OrdinalTable[i] - DirTable.OrdinalBase].name);


  // Read addresses of all function
  // Read functions' mem. addresses from Export Address Table
  ExportStream.Seek(ExportDirTable.AddressTableRVA - ExportRVA, 0);
  for i:= 0 to FunctionCount - 1 do begin
    ExportStream.Read(FunctionRVA, 4);
    functions[i].Ordinal:= i + ExportDirTable.OrdinalBase;

    // FunctionRVA is really RVA of symbol in code/data(?) section
    if (FunctionRVA < ExportRVA) or (FunctionRVA >= ExportRVA + ExportDataSize) then begin
      functions[i].MemOffset:= FunctionRVA + ImageBase;
      functions[i].Section:= PEFile.GetSectionNumberFromRVA(FunctionRVA);
      if functions[i].Section = -1 then
        functions[i].CodeSectionOffset:= 0
      else
        functions[i].CodeSectionOffset:= functions[i].MemOffset - (PEFile.Sections[functions[i].section] as TCodeSection).MemOffset;
    end

    // FunctionRVA is ForwardRVA
    else begin
      StreamPosition:= ExportStream.Position;
      ReadStringFromStream(ExportStream, FunctionRVA - ExportRVA, ForwardFunction);
      ExportStream.Position:= StreamPosition;
      functions[i].Name:= functions[i].Name + ' (forwarded to ' + ForwardFunction + ')';
      functions[i].Section:= -1;
      functions[i].CodeSectionOffset:= 0;
    end;
  end;


  ExportStream.Free;
end;




constructor TExportSection.CreateFromNEFile(InputFile: TStream; ResidentTableOffset, NonResidentTableOffset, NonResidentTableSize: cardinal; aName: string; aExecFile: TObject);

  procedure FindEntryPoint(Ordinal: cardinal; var segment: integer; var address: cardinal);
  var i: integer;
  begin
    i:=0;
    with (ExecFile as TNEFile) do
      while i < Length(EntryTable) do begin
        if ordinal <= EntryTable[i].count then begin
          case EntryTable[i].typ of
            etFixed: begin
              address:=EntryTable[i].Fixed[ordinal-1].Offset;
              segment:=-1;
            end;
            etMovable: begin
              address:=EntryTable[i].Movable[ordinal-1].Offset;
              segment:=EntryTable[i].Movable[ordinal-1].Segment-1;
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
    FunIndex:= FunctionCount;
    InputFile.Position:= TableOffset;
    InputFile.Read(FirstNameLength, 1);
    SetLength(FirstName, FirstNameLength);
    InputFile.Read(FirstName[1], FirstNameLength);
    InputFile.Seek(2, soCurrent);

    InputFile.Read(FunNameLength, 1);
    while FunNameLength > 1 do begin
      SetLength(Functions, FunIndex+1);
      SetLength(Functions[FunIndex].Name, FunNameLength);
      InputFile.Read(Functions[FunIndex].Name[1], FunNameLength);
      InputFile.Read(Functions[FunIndex].Ordinal, 2);
      FindEntryPoint(Functions[FunIndex].Ordinal, Functions[FunIndex].Section, Functions[FunIndex].CodeSectionOffset);
      InputFile.Read(FunNameLength, 1);
      Inc(FunIndex);
    end;
    FunctionCount:= FunIndex;
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
var i: integer;
begin
  inherited SaveToFile(DHF, DAS, SaveOptions);
  DHF.Write(FunctionCount, 4);
  for i:=0 to FunctionCount do begin
    DHF.Write(functions[i], SizeOf(TExportFunction)-4);
    StreamWriteAnsiString(DHF, functions[i].name);
  end;
  result:= true;
end;



function TExportSection.LoadFromFile(DHF: TStream; var DAS: TextFile):boolean;
var i: integer;
begin
  inherited LoadFromFile(DHF, DAS);
  DHF.Read(FunctionCount, 4);
  SetLength(functions, FunctionCount);
  for i:=0 to FunctionCount - 1 do begin
    DHF.Read(functions[i], SizeOf(TExportFunction) - 4);
    functions[i].name:= StreamReadAnsiString(DHF);
  end;
  result:= true;
end;



end.
