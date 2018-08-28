unit LEFileUnit;

interface

type
  TLEInfoBlock = record
    LESign: Word;
    ByteOrder, WordOrder: Byte;
    ExeFormatLevel: Cardinal;
    CPUType: Word;
    TargetOS: Word;
    ModuleVersion: Cardinal;
    ModyleTypeFlags: Cardinal;
    MemoryPages: Cardinal;
    InitCS, InitEIP, InitSS, InitESP: Cardinal;
    MemPageSize: Cardinal;
    LastPageBytes: Cardinal;
    FixupSize, FixupChecksum: Cardinal;
    LoaderSize, LoaderCheckSum: Cardinal;
    ObjectTableOffset, ObjectTableEntries: Cardinal;
    ObjectPageMapOffset, ObjectIterateDataMapOffset: Cardinal;
    ResourceTableOffset, ResourceTableEntries: Cardinal;
    ResidentNameTableOffset: Cardinal;
    EntryTableOffset: Cardinal;
    ModuleDirectivesTableOffset, ModuleDirectivesEntries: Cardinal;
    FixupPageTableOffset, FixupRecordTableOffset: Cardinal;
    ImportModuleNameTableOffset: Cardinal;
    ImportModulesCount: Cardinal;
    ImportProcNameTableOffset: Cardinal;
    PerpageChecksumTableOffset: Cardinal;
    DataPagesOffsetTop: Cardinal;
    PreloadPageCount: Cardinal;
    NonResidentTableOffsetTop: Cardinal;
    NonResidentNamesTabelLength: Cardinal;
    NonResidentNamesTableChecksum: Cardinal;
    AutomaticDataObject: Cardinal;
    DebugInfoOffset, DebugInfoLength: Cardinal;
    PreloadPageNumber, DemandPageNumber: Cardinal;
    ExtraHeapAllocation: Cardinal;
    Unknown: Cardinal;
  end;

  TLEObjectTableEntry = record
    VirtualSegmentSize: Cardinal;
    RelocationBaseAddress: Cardinal;
    Flags: Cardinal;
    PageMapIndex: Cardinal;
    PageMapEntries: Cardinal;
    Unknown: Cardinal;
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
