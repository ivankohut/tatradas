unit LEFileUnit;

interface

type
  TLEInfoBlock = record
    LESign: word;
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
  TLEFile = class(TExecutableFile)
    constructor Create(a:TStream; pc:TPageControl); override;
    destructor Destroy(); override;
    procedure Vypis(var memo:TMemo;var Combo:TComboBox; var ObjectTableData:TObjectTableData); override;
    procedure SaveToFile(var a:TMemoryStream); override;
    procedure LoadFromFile(var a:TMemoryStream); override;
  end;
}
implementation
{
constructor TLEFile.Create(a:TStream;var b:TSectionInfo);
begin
end;

procedure TLEFile.Vypis(var memo:TMemo;var Combo:TComboBox; var ObjectTableData:TObjectTableData);
begin
end;

procedure TLEFile.SaveToFile(var a:TMemoryStream);
begin
end;

procedure TLEFile.LoadFromFile(var a:TMemoryStream);
begin
end;

destructor TLEFile.Destroy();
begin
end;

}

end.
