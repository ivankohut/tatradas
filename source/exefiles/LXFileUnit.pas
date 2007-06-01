unit LXFileUnit;

interface

uses classes, 
     procmat,
     ExecFileUnit,
     SectionUnit,
     CodeSectionUnit;

type
  TLXInfoBlock = record
    LXSign: word;
    ByteOrder,WordOrder:byte;
    ExeFormatLevel:cardinal;
    CPUType:word;
    TargetOS:word;
    ModuleVersion:cardinal;
    ModyleTypeFlags:cardinal;
    MemoryPages:cardinal;
    InitCS,InitEIP,InitSS,InitESP:cardinal;
    MemPageSize:cardinal;
    LastPageBytes:cardinal;
    FixupSize,FixupChecksum:cardinal;
    LoaderSize,LoaderCheckSum:cardinal;
    ObjectTableOffset,ObjectTableEntries:cardinal;
    ObjectPageMapOffset,ObjectIterateDataMapOffset:cardinal;
    ResourceTableOffset,ResourceTableEntries:cardinal;
    ResidentNameTableOffset:cardinal;
    EntryTableOffset:cardinal;
    ModuleDirectivesTableOffset,ModuleDirectivesEntries:cardinal;
    FixupPageTableOffset,FixupRecordTableOffset:cardinal;
    ImportModuleNameTableOffset:cardinal;
    ImportModulesCount:cardinal;
    ImportProcNameTableOffset:cardinal;
    PerpageChecksumTableOffset:cardinal;
    DataPagesOffsetTop:cardinal;
    PreloadPageCount:cardinal;
    NonResidentTableOffsetTop:cardinal;
    NonResidentNamesTabelLength:cardinal;
    NonResidentNamesTableChecksum:cardinal;
    AutomaticDataObject:cardinal;
    DebugInfoOffset,DebugInfoLength:cardinal;
    PreloadPageNumber,DemandPageNumber:cardinal;
    ExtraHeapAllocation:cardinal;
    Unknown:cardinal;
  end;

  TLEObjectTableEntry = record
    VirtualSegmentSize:cardinal;
    RelocationBaseAddress:cardinal;
    Flags:cardinal;
    PageMapIndex:cardinal;
    PageMapEntries:cardinal;
    Unknown:cardinal;
  end;
{
  TLXFile = class(TExecutableFile)
    constructor Create(a:TStream; pc:TPageControl); override;
    destructor Destroy(); override;
    procedure Vypis(var memo:TMemo;var Combo:TComboBox; var ObjectTableData:TObjectTableData); override;
    procedure SaveToFile(var a:TMemoryStream); override;
    procedure LoadFromFile(var a:TMemoryStream); override;
    function GetModulAndFunction(address:string; a:cardinal):string; override;
  end;
}
implementation
{
constructor TLXFile.Create(a:TStream;var b:TSectionInfo);
begin
end;

procedure TLXFile.Vypis(var memo:TMemo;var Combo:TComboBox; var ObjectTableData:TObjectTableData);
begin
end;

procedure TLXFile.SaveToFile(var a:TMemoryStream);
begin
end;

procedure TLXFile.LoadFromFile(var a:TMemoryStream);
begin
end;

destructor TLXFile.Destroy();
begin
end;

function TLXFile.GetModulAndFunction(address:string; a:cardinal):string;
begin
end;
}

end.
