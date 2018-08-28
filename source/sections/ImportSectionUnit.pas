{ TODO:
  - odstranit FunctionTableSRVA v PEImport...
  - osetrit aj ine RelocationAddressTypes ako 32bit pointer
}

unit ImportSectionUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  Classes,
  INIFiles,
  SysUtils,
  // project units
  procmat,
  SectionUnit;

type

  TImportFunctionOccurence = record
    Address: Cardinal;
    SectionIndex: Integer;
  end;

  TImportFunction = record
    AddressRVA: Cardinal;
//    RVAtoname: Cardinal;
    Hint: Word;
    ByOrdinal: Boolean;
    Ordinal: Cardinal;
    MemAddress: Cardinal; // added in 2.9.8 alpha
    Name: string;
    Occurs: array of TImportFunctionOccurence;  // Memory addresses
  end;

  TImportModul = record
    FunctionCount: Integer;
    Functions: array of TImportFunction;
    Name: string;
  end;

  TImportDirectoryEntry = record
    LookupTableRVA: Cardinal;
    Reserved1, Reserved2: Cardinal;
    NameRVA: Cardinal;
    AddressTableRVA: Cardinal;
  end;

  TNESectionImports = record
    Offset: Cardinal;
    SectionIndex: Integer;
    SectionFileOffset: Cardinal;
  end;

  TImportSection = class(TSection)
    ModulCount: Integer;
    TotalFunctionCount: Integer;
    Moduls: array of TImportModul;
    constructor Create(efile: TObject); overload;
    constructor CreateFromPEFile(InputFile: TStream; ImTableRVA, ImageBase: Cardinal; aFileOffset, aFileSize, aMemOffset, aMemSize: Cardinal; aName: string; aExecFile: TObject);
    constructor CreateFromNEFile(InputFile: TStream; ModuleTableOffset, ImportTableOffset, aModulCount, ImportTableSize: Cardinal; SegmentImports: array of TNESectionImports; aName: string; aExecFile: TObject);
//    constructor CreateFromELFFile(a: TStream; Offset, Size, StrTabOffset, StrTabSize: Cardinal; efile: TObject);
    constructor CreateFromELFFile(InputFile: TStream; Offset, Size, StringTableOffset, StringTableSize: Cardinal; aName: string; aExecFile: TObject);
    destructor Destroy; override;

    procedure SaveToFile(DHF: TStream; var DAS: TextFile); override;
    procedure LoadFromFile(DHF: TStream; var DAS: TextFile); overload; override;

    function AddFunctionOccurence(IndexAddress: Cardinal; CallInstrAddress: Cardinal; aSectionIndex: Integer): string;
//    procedure AddFunctionOccurence(ModulIndex, FunctionIndex: Integer; Address: Cardinal);
  end;

  TSymbolTableEntry = record
    st_name: Cardinal;
    st_value: Cardinal;
    st_size: Cardinal;
    st_info: Byte;
    st_other: Byte;
    st_shndx: Word;
  end;


const

//  ELF32_ST_TYPE:
  STT_FUNC = 2;

{
  TSymbol = record
    name: string;
  end;

  TELFImportSection = class(TImportSection)
    Symbols: array of TSymbol;
    SymbolCount: integer;
    SymbolTable: array of TSymbolTableEntry;
    constructor Create(a:TStream; Offset,Size,StrTabOffset,StrTabSize: cardinal; efile:TObject); overload;
  end;
}
implementation

uses
  ExecFileUnit,
  CodeSectionUnit;



constructor TImportSection.Create(efile: TObject);
begin
  fTyp := stImport;
  fExecFile := efile;
end;



destructor TImportSection.Destroy;
begin
  inherited;
end;



function TImportSection.AddFunctionOccurence(IndexAddress: Cardinal; CallInstrAddress: Cardinal; aSectionIndex: Integer): string;
var
  ModIndex, FunIndex: Integer;
  ModulIndex, FunctionIndex: Integer;
  FunctionFound: Boolean;
label
  FOUND;
begin
  FunctionFound := False;
  ModulIndex := -1;
  FunctionIndex := -1;
  Result := ''; // does not work correctly without this command

  // Find function and its modul
  for ModIndex := 0 to ModulCount - 1 do
    for FunIndex := 0 to Moduls[ModIndex].FunctionCount - 1 do
      if IndexAddress = moduls[ModIndex].functions[FunIndex].MemAddress then begin
        ModulIndex := ModIndex;
        FunctionIndex := FunIndex;
        FunctionFound := True;
        goto FOUND;
      end;

  FOUND:

    if FunctionFound then begin
    // Add function's occurence into its 'vyskyty' array
      with moduls[ModulIndex].functions[FunctionIndex] do begin
        SetLength(Occurs, Length(Occurs) + 1);
        Occurs[High(Occurs)].Address := CallInstrAddress;
        Occurs[High(Occurs)].SectionIndex := aSectionIndex;

        if ByOrdinal then
          Result := IntToHex(Ordinal, 8) + '''(Ordinal)' + ' from ''' + moduls[ModulIndex].Name
        else
          Result := Name + ''' from ''' + Moduls[ModulIndex].Name;
      end;

    end;
end;



constructor TImportSection.CreateFromPEFile(InputFile: TStream; ImTableRVA, ImageBase: Cardinal; aFileOffset, aFileSize, aMemOffset, aMemSize: Cardinal; aName: string; aExecFile: TObject);
var
  InputStream: TMemoryStream;
  RVA: Cardinal;
  ModulIndex: Cardinal;
  DirEntry: TImportDirectoryEntry;
  FunctionTableSRVA: Cardinal;

  FunctionIndex: Cardinal;
  LookupTableEntry: Cardinal;
  IsOrdinal: Boolean;
  NameRVA_or_Ordinal: Cardinal;

begin
  inherited Create(aName, aExecFile);
  fTyp := stImport;

  TotalFunctionCount := 0;
  RVA := aMemOffset - ImageBase;

  // Read Import section from file to temporary stream
  InputStream := TMemoryStream.Create;
  InputFile.Seek(aFileOffset, 0);
  InputStream.CopyFrom(InputFile, aFileSize);
  InputStream.Seek(ImTableRVA - RVA, 0);

  // Read import moduls
  InputStream.Read(DirEntry, 20);
  ModulIndex := 0;
  while DirEntry.NameRVA <> 0 do begin
    SetLength(Moduls, ModulIndex + 1);
    ReadStringFromStream(InputStream, DirEntry.NameRVA - RVA, moduls[ModulIndex].Name);

    // LookupTableRVA and AddressTable should point to tables with exactly the same content
    // However some files have LookupTableRVA set to zero, so we use AddressTableRVA instead of LookupTableRVA
    // => we read functions' names RVAs (or ordinals) from AddressTable instead of LookupTable
    if DirEntry.LookupTableRVA <> 0 then
      FunctionTableSRVA := DirEntry.LookupTableRVA - RVA
    else
      FunctionTableSRVA := DirEntry.AddressTableRVA - RVA;

    // Read modul functions
    InputStream.Seek(FunctionTableSRVA, 0);
    InputStream.Read(LookupTableEntry, 4);
    FunctionIndex := 0;
    while LookupTableEntry <> 0 do begin
      SetLength(Moduls[ModulIndex].Functions, FunctionIndex + 1);
      Moduls[ModulIndex].Functions[FunctionIndex].MemAddress := DirEntry.AddressTableRVA + FunctionIndex * SizeOf(LookupTableEntry) + ImageBase;
      NameRVA_or_Ordinal := (LookupTableEntry and $7FFFFFFF);

      IsOrdinal := 0 <> (LookupTableEntry and $80000000);
      Moduls[ModulIndex].Functions[FunctionIndex].ByOrdinal := IsOrdinal;
      // Import by ordinal
      if IsOrdinal then begin
        Moduls[ModulIndex].Functions[FunctionIndex].Ordinal := NameRVA_or_Ordinal;
      end
      // Import by name
      else begin
        InputStream.Seek(NameRVA_or_Ordinal - RVA, 0);
        InputStream.Read(Moduls[ModulIndex].Functions[FunctionIndex].Hint, 2);
        ReadStringFromStream(InputStream, NameRVA_or_Ordinal - RVA + 2, Moduls[ModulIndex].Functions[FunctionIndex].Name);
      end;

      Inc(FunctionIndex);
      InputStream.Seek(FunctionTableSRVA + FunctionIndex * 4, 0);
      InputStream.Read(LookupTableEntry, 4);
    end;
    Moduls[ModulIndex].FunctionCount := FunctionIndex;
    Inc(TotalFunctionCount, FunctionIndex);

    Inc(ModulIndex);
    InputStream.Seek(ImTableRVA - RVA + ModulIndex * SizeOf(DirEntry), 0);
    InputStream.Read(DirEntry, 20);
  end;

  ModulCount := ModulIndex;
  InputStream.Free;
end;



constructor TImportSection.CreateFromELFFile(InputFile: TStream; Offset, Size, StringTableOffset, StringTableSize: Cardinal; aName: string; aExecFile: TObject);
var
  SymbolCount: Cardinal;
  SymbolTable: array of TSymbolTableEntry;

  SymbolIndex, FunctionIndex: Integer;
begin
  inherited Create(aName, aExecFile);
  fTyp := stImport;
  fExecFile := aExecFile;

  // Read Symbol table
  SymbolCount := Size div SizeOf(TSymbolTableEntry);
  SetLength(SymbolTable, SymbolCount);
  InputFile.Position := Offset;
  InputFile.Read(SymbolTable[0], SymbolCount * SizeOf(TSymbolTableEntry));

  // Set up first (and last :) ) modul
  SetLength(moduls, 1);
  ModulCount := 1;
  Moduls[0].Name := 'modul1';
  SetLength(Moduls[0].Functions, SymbolCount);

  // Extract functions from symbols
  FunctionIndex := 0;
  for SymbolIndex := 0 to SymbolCount - 1 do
    if ((SymbolTable[SymbolIndex].st_info and $0F) = STT_FUNC) then begin
      ReadStringFromStream(InputFile, SymbolTable[SymbolIndex].st_name + StringTableOffset, Moduls[0].Functions[FunctionIndex].Name);
      Moduls[0].Functions[FunctionIndex].AddressRVA := SymbolTable[SymbolIndex].st_value;
      Inc(FunctionIndex);
    end;
  Moduls[0].FunctionCount := FunctionIndex;
  SetLength(Moduls[0].Functions, FunctionIndex);
end;



constructor TImportSection.CreateFromNEFile(InputFile: TStream; ModuleTableOffset, ImportTableOffset, aModulCount, ImportTableSize: Cardinal; SegmentImports: array of TNESectionImports; aName: string; aExecFile: TObject);

const
  RelocAddrType_32bPointer = 3;

  RelocType_InternalReference = 0;
  RelocType_ImportedOrdinal = 1;
  RelocType_ImportedName = 2;
  RelocType_OSFixup = 3;

type
  TRelocationItem = record
    RelocAddressType: Byte;
    RelocType: Byte;
    Offset: Word;
    Data1: Word; // meaning depeds on RelocType
    Data2: Word; // meaning depeds on RelocType
  end;

var
  i, j, k: Integer;
  FunctionNameLength: Byte;
  FunctionName: ShortString;
  ModulNameLength: Byte;
  indexy: array of Word;
  RelocationCount: Word;
  RelocationItem: TRelocationItem;
  RelocationOffset: Word;

begin
  inherited Create(aName, aExecFile);
  fTyp := stImport;

  TotalFunctionCount := 0;
  ModulCount := aModulCount;
  SetLength(Moduls, ModulCount);
  SetLength(indexy, ModulCount + 1);
  InputFile.Position := ModuleTableOffset;
  InputFile.Read(indexy[0], ModulCount * 2);
  indexy[ModulCount] := ImportTableSize + 1;

  // Read names of moduls
  for i := 0 to ModulCount - 1 do begin
    InputFile.Position := ImportTableOffset + indexy[i];
    InputFile.Read(ModulNameLength, 1);
    SetLength(Moduls[i].Name, ModulNameLength);
    InputFile.Read(Moduls[i].Name[1], ModulNameLength);
  end;
  indexy := nil;

  // Process relocation data of segments
  for i := 0 to Length(SegmentImports) - 1 do begin
    InputFile.Position := SegmentImports[i].Offset;
    InputFile.Read(RelocationCount, 2);
    for j := 0 to Integer(RelocationCount) - 1 do begin
      InputFile.Position := SegmentImports[i].Offset + 2 + Cardinal(j) * SizeOf(TRelocationItem);
      InputFile.Read(RelocationItem, SizeOf(TRelocationItem));

      if RelocationItem.RelocAddressType <> RelocAddrType_32bPointer then
        Continue;


      k := 0;
      case RelocationItem.RelocType of

        RelocType_ImportedOrdinal: begin
          with Moduls[RelocationItem.Data1 - 1] do begin

            // Check if we already have the function
            while k < FunctionCount do begin
              if Functions[k].Ordinal = RelocationItem.Data2 then
                Break;
              Inc(k);
            end;

            // Add function if we do not have it
            if k = FunctionCount then begin
              Inc(FunctionCount);
              SetLength(Functions, FunctionCount);
              Functions[k].ByOrdinal := True;
              Functions[k].Ordinal := RelocationItem.Data2;
            end;
          end;
        end;

        RelocType_ImportedName: begin
          with Moduls[RelocationItem.Data1 - 1] do begin

            // Read function name from Import Name Table
            InputFile.Position := ImportTableOffset + RelocationItem.Data2;
            InputFile.Read(FunctionNameLength, 1);
            SetLength(FunctionName, FunctionNameLength);
            InputFile.Read(FunctionName[1], FunctionNameLength);

            // Check if we already have the function
            while k < FunctionCount do begin
              if Functions[k].Name = FunctionName then
                Break;
              Inc(k);
            end;

            // Add function if we do not have it
            if k = FunctionCount then begin
              Inc(FunctionCount);
              SetLength(Functions, FunctionCount);
              Functions[k].ByOrdinal := False;
              Functions[k].Name := FunctionName;
            end;
          end;
        end;

        else
          Continue;
      end;

      // Process relocation chain
      RelocationOffset := RelocationItem.Offset;
      with Moduls[RelocationItem.Data1 - 1].Functions[k] do
        repeat
          SetLength(Occurs, Length(Occurs) + 1);
          Occurs[High(Occurs)].Address := RelocationOffset - 1;
          Occurs[High(Occurs)].SectionIndex := SegmentImports[i].SectionIndex;

          InputFile.Position := SegmentImports[i].SectionFileOffset + RelocationOffset;
          InputFile.Read(RelocationOffset, 2);
        until RelocationOffset = $FFFF;

    end;
  end;

end;



procedure TImportSection.SaveToFile(DHF: TStream; var DAS: TextFile);
var
  ModulIndex, FunctionIndex, OccurIndex: Integer;
  OccurenceCount: Integer;
begin
  inherited SaveToFile(DHF, DAS);
  DHF.Write(TotalFunctionCount, 4);
  DHF.Write(ModulCount, 4);
  for ModulIndex := 0 to ModulCount - 1 do begin
    with moduls[ModulIndex] do begin
      StreamWriteAnsiString(DHF, Name);
      DHF.Write(FunctionCount, 4);
      for FunctionIndex := 0 to FunctionCount - 1 do begin
        DHF.Write(functions[FunctionIndex], SizeOf(TImportFunction) - 8);
        with functions[FunctionIndex] do begin
          StreamWriteAnsiString(DHF, Name);
          OccurenceCount := Length(Occurs);
          DHF.Write(OccurenceCount, 4);
          for OccurIndex := 0 to OccurenceCount - 1 do
            DHF.Write(Occurs[OccurIndex], SizeOf(TImportFunctionOccurence));
        end;
      end;
    end;
  end;
end;



procedure TImportSection.LoadFromFile(DHF: TStream; var DAS: TextFile);
var
  ModulIndex, FunctionIndex, OccurIndex: Integer;
  OccurenceCount: Integer;
begin
  inherited LoadFromFile(DHF, DAS);
  DHF.Read(TotalFunctionCount, 4);
  DHF.Read(ModulCount, 4);
  SetLength(Moduls, ModulCount);
  for ModulIndex := 0 to ModulCount - 1 do begin
    with Moduls[ModulIndex] do begin
      Name := StreamReadAnsiString(DHF);
      DHF.Read(FunctionCount, 4);
      SetLength(functions, FunctionCount);
      for FunctionIndex := 0 to FunctionCount - 1 do begin
        DHF.Read(functions[FunctionIndex], SizeOf(TImportFunction) - 8);
        with functions[FunctionIndex] do begin
          Name := StreamReadAnsiString(DHF);
          DHF.Read(OccurenceCount, 4);
          SetLength(Occurs, OccurenceCount);
          for OccurIndex := 0 to OccurenceCount - 1 do
            DHF.Read(Occurs[OccurIndex], SizeOf(TImportFunctionOccurence));
        end;
      end;
    end;
  end;
end;



end.
