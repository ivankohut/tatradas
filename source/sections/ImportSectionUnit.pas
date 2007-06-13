{ TODO:
  - odstranit FunctionTableSRVA v PEImport...
}

unit ImportSectionUnit;

{$INCLUDE 'DELVER.INC'}

interface

uses
  Classes,
  INIFiles,
  SysUtils,

  procmat,
  SectionUnit;

type

  TImportFunction = record
    AddressRVA: cardinal;
//    RVAtoname: cardinal;
    Hint: word;
    ByOrdinal: boolean;
    Ordinal: cardinal;
    MemAddress: cardinal; // added in 2.9.8 alpha
    Name: string;
    vyskyty: array of cardinal; // Memory addresses
  end;

  TImportModul = record
    FunctionCount: cardinal;
    Functions: array of TImportFunction;
    Name: string;
  end;

  TImportDirectoryEntry = record
    LookupTableRVA: cardinal;
    Reserved1, Reserved2: cardinal;
    NameRVA: cardinal;
    AddressTableRVA: cardinal;
  end;

  TImportSection = class(TSection)
    ModulCount: integer;
    TotalFunctionCount: integer;
    Moduls: array of TImportModul;
    constructor Create(efile:TObject); overload;
//    constructor Create(a:TStream; RVA,Offset,size,ImTableRVA: cardinal; efile:TObject); overload;
//    constructor CreateFromPEFile(a:TStream; RVA,Offset,size,ImTableRVA: cardinal; efile:TObject); overload;
    constructor CreateFromPEFile(InputFile: TStream; ImTableRVA: cardinal; ImageBase: cardinal; aName: string; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aExecFile: TObject); virtual;
    constructor CreateFromNEFile(a:TStream; ModuleTableOffset, ImportTableOffset, ModuleCount, ImportTableSize: cardinal; efile:TObject);
//    constructor CreateFromELFFile(a:TStream; Offset,Size,StrTabOffset,StrTabSize: cardinal; efile:TObject);
    destructor Destroy; override;

    function SaveToFile  (DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; override;
    function LoadFromFile(DHF: TStream; var DAS: TextFile):boolean; overload; override;

    function AddFunctionOccurence(IndexAddress: cardinal; CallInstrAddress: cardinal): string;
//    procedure AddFunctionOccurence(ModulIndex, FunctionIndex: integer; Address: cardinal);
  end;

  TSymbolTableEntry = record
    st_name: cardinal;
    st_value: cardinal;
    st_size: cardinal;
    st_info: byte;
    st_other: byte;
    st_shndx: word;
  end;

  TSymbol = record
    name: string;
  end;

  TELFImportSection = class(TImportSection)
    Symbols: array of TSymbol;
    SymbolCount: integer;
    SymbolTable: array of TSymbolTableEntry;
    constructor Create(a:TStream; Offset,Size,StrTabOffset,StrTabSize: cardinal; efile:TObject); overload;
  end;

implementation

uses
  ExecFileUnit,
  CodeSectionUnit;


constructor TImportSection.Create(efile:TObject);
begin
  fTyp:=stImport;
  fExecFile:=efile;
end;



destructor TImportSection.Destroy;
begin
  inherited;
end;



function TImportSection.AddFunctionOccurence(IndexAddress: cardinal; CallInstrAddress: cardinal): string;
var
  ModIndex, FunIndex: integer;
  ModulIndex, FunctionIndex: integer;
  FunctionFound: boolean;
  label FOUND;
begin
  FunctionFound:=false;

  // Find function and its modul
  for ModIndex:=0 to ModulCount - 1 do
    for FunIndex:=0 to Moduls[ModIndex].FunctionCount - 1 do
      if IndexAddress = moduls[ModIndex].functions[FunIndex].MemAddress then begin
        ModulIndex:= ModIndex;
        FunctionIndex:= FunIndex;
        FunctionFound:= True;
        goto FOUND;
      end;

FOUND:

  if FunctionFound then begin
    // Add function's occurence into its 'vyskyty' array
    with moduls[ModulIndex].functions[FunctionIndex] do begin
      SetLength(vyskyty, Length(vyskyty) + 1);
      vyskyty[high(vyskyty)]:=CallInstrAddress;

      if ByOrdinal then
        result:=IntToHex(Ordinal, 8) + '''(Ordinal)' + ' from ''' + moduls[ModulIndex].name
      else
        result:= name +''' from ''' + moduls[ModulIndex].name;
    end;

  end;
end;



constructor TImportSection.CreateFromPEFile(InputFile: TStream; ImTableRVA, ImageBase: cardinal; aName: string; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aExecFile: TObject);
//constructor TImportSection.CreateFromPEFile(a:TStream; RVA,Offset,size,ImTableRVA: cardinal; efile:TObject);
var
  InputStream: TMemoryStream;
  RVA: cardinal;
  ModulIndex: cardinal;
  DirEntry: TImportDirectoryEntry;
  FunctionTableSRVA:cardinal;

  FunctionIndex: cardinal;
  LookupTableEntry: cardinal;
  IsOrdinal: boolean;
  NameRVA_or_Ordinal: cardinal;

begin
  inherited Create(aName, MaxInt, aExecFile);
  fTyp:= stImport;

  TotalFunctionCount:= 0;
  RVA:= aMemOffset - ImageBase;

  // Read Import section from file to temporary stream
  InputStream:= TMemoryStream.Create;
  InputFile.Seek(aFileOffset, 0);
  InputStream.CopyFrom(InputFile, aFileSize);
  InputStream.Seek(ImTableRVA - RVA, 0);

  // Read import moduls
  InputStream.Read(DirEntry, 20);
  ModulIndex:= 0;
  while DirEntry.NameRVA <> 0 do begin
    SetLength(Moduls, ModulIndex + 1);                               
    ReadStringFromStream(InputStream, DirEntry.NameRVA - RVA, moduls[ModulIndex].Name);

    // Zistenie SRVA tab. funkcii mudulu }
    // asdasdasd bleble
    if DirEntry.LookupTableRVA <> 0 then
      FunctionTableSRVA:= DirEntry.LookupTableRVA - RVA
    else begin
      FunctionTableSRVA:= DirEntry.AddressTableRVA - RVA;
      raise Exception.Create('DirEntry.LookupTableRVA is NULL');
    end;

    // Read modul functions
    InputStream.Seek(FunctionTableSRVA, 0);
    InputStream.Read(LookupTableEntry, 4);
    FunctionIndex:= 0;
    while LookupTableEntry <> 0 do begin
      SetLength(Moduls[ModulIndex].Functions, FunctionIndex + 1);                          
      Moduls[ModulIndex].Functions[FunctionIndex].MemAddress:= DirEntry.AddressTableRVA + FunctionIndex*SizeOf(LookupTableEntry) + ImageBase;
      NameRVA_or_Ordinal := (LookupTableEntry and $7FFFFFFF);

      IsOrdinal:= 0 <> (LookupTableEntry and $80000000);
      Moduls[ModulIndex].Functions[FunctionIndex].ByOrdinal:= IsOrdinal;
      // Import by ordinal
      if IsOrdinal then begin
        Moduls[ModulIndex].Functions[FunctionIndex].Ordinal:= NameRVA_or_Ordinal;
      end
      // Import by name
      else begin
        InputStream.Seek(NameRVA_or_Ordinal - RVA, 0);
        InputStream.Read(Moduls[ModulIndex].Functions[FunctionIndex].Hint, 2);
        ReadStringFromStream(InputStream, NameRVA_or_Ordinal-RVA+2, Moduls[ModulIndex].Functions[FunctionIndex].Name);
      end;

      Inc(FunctionIndex);
      InputStream.Seek(FunctionTableSRVA + FunctionIndex*4, 0);
      InputStream.Read(LookupTableEntry, 4);
    end;
    Moduls[ModulIndex].FunctionCount:= FunctionIndex;
    Inc(TotalFunctionCount, FunctionIndex);

    Inc(ModulIndex);
    InputStream.Seek(ImTableRVA - RVA + ModulIndex*SizeOf(DirEntry), 0);
    InputStream.Read(DirEntry, 20);
  end;                                                                    

  ModulCount:= ModulIndex;
  InputStream.Free;
end;



constructor TELFImportSection.Create(a:TStream; Offset,Size,StrTabOffset,StrTabSize: cardinal; efile:TObject);
var
    i:integer;           // loop variable
begin
  fTyp:=stImport;
  fExecFile:=efile;

  SymbolCount:=Size div SizeOf(TSymbolTableEntry);
  SetLength(SymbolTable,SymbolCount);
  a.Position:=Offset;
  a.Read(SymbolTable[0],SymbolCount * SizeOf(TSymbolTableEntry));
  SetLength(Symbols,SymbolCount);
  SetLength(moduls,1);
  modulcount:=1;
  moduls[0].name:='modul1';
  SetLength(moduls[0].functions,SymbolCount);
  moduls[0].functioncount:=SymbolCount;
  for i:=0 to SymbolCount-1 do begin
    ReadStringFromStream(a,SymbolTable[i].st_name + StrTabOffset,Symbols[i].name);
    moduls[0].functions[i].name:=Symbols[i].name;
    moduls[0].functions[i].addressRVA:=SymbolTable[i].st_value;
  end;
end;



constructor TImportSection.CreateFromNEFile(a:TStream; ModuleTableOffset, ImportTableOffset, ModuleCount, ImportTableSize: cardinal; efile:TObject);
var
    i,j:integer;           // loop variable
    adresa: word;
    dlzka: byte;
    indexy: array of word;
begin
  fTyp:=stImport;
  fExecFile:=efile;

  TotalFunctionCount:=0;
  modulcount:= ModuleCount;
  SetLength(moduls,modulcount);
  SetLength(indexy,modulcount+1);
  a.Position:= ModuleTableOffset;
  a.Read(indexy[0],modulcount*2);
  indexy[modulcount]:=ImportTableSize+1;

  for i:=0 to modulcount-1 do begin
    a.Position:=ImportTableOffset + indexy[i];
    a.Read(dlzka,1);
    SetLength(moduls[i].name,dlzka);
    a.Read(moduls[i].name[1],dlzka);
    j:=0;
    a.Read(dlzka,1);
    while (dlzka > 0) and (a.Position < ImportTableOffset + indexy[i+1]) do begin
      SetLength(moduls[i].functions,j+1);
      SetLength(moduls[i].functions[j].name,dlzka);
      a.Read(moduls[i].functions[j].name[1],dlzka);
      a.Read(dlzka,1);
      inc(j);
    end;
    moduls[i].functioncount:=j;
    inc(TotalFunctionCount,j);
  end;
  indexy:=nil;
end;



function TImportSection.SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean;
var
  ModulIndex, FunctionIndex, OccurIndex: integer;
  OccurenceCount: integer;
begin
  inherited SaveToFile(DHF, DAS, SaveOptions);
  DHF.Write(TotalFunctionCount, 4);
  DHF.Write(ModulCount, 4);
  for ModulIndex:= 0 to ModulCount - 1 do begin
    with moduls[ModulIndex] do begin
      StreamWriteAnsiString(DHF, Name);
      DHF.Write(FunctionCount, 4);
      for FunctionIndex:= 0 to FunctionCount - 1 do begin
        DHF.Write(functions[FunctionIndex], SizeOf(TImportFunction) - 8);
        with functions[FunctionIndex] do begin
          StreamWriteAnsiString(DHF, name);
          OccurenceCount:= Length(vyskyty);
          DHF.Write(OccurenceCount, 4);
          for OccurIndex:= 0 to OccurenceCount - 1 do
            DHF.Write(vyskyty[OccurIndex], 4);
        end;
      end;
    end;
  end;
  result:=true;
end;



function TImportSection.LoadFromFile(DHF: TStream; var DAS: TextFile): boolean;
var
  ModulIndex, FunctionIndex, OccurIndex: integer;
  OccurenceCount: integer;
begin
  inherited LoadFromFile(DHF, DAS);
  DHF.Read(TotalFunctionCount, 4);
  DHF.Read(ModulCount, 4);
  SetLength(moduls, Modulcount);
  for ModulIndex:= 0 to ModulCount - 1 do begin
    with moduls[ModulIndex] do begin
      Name:= StreamReadAnsiString(DHF);
      DHF.Read(FunctionCount, 4);
      SetLength(functions, FunctionCount);
      for FunctionIndex:= 0 to FunctionCount - 1 do begin
        DHF.Read(functions[FunctionIndex], SizeOf(TImportFunction) - 8);
        with functions[FunctionIndex] do begin
          Name:= StreamReadAnsiString(DHF);
          DHF.Read(OccurenceCount, 4);
          SetLength(vyskyty, OccurenceCount);
          for OccurIndex:= 0 to OccurenceCount - 1 do
            DHF.Read(vyskyty[OccurIndex], 4);
        end;
      end;
    end;
  end;
  result:=true;
end;



end.
