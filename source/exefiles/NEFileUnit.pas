{ TODO:
    - export, import
    - detekcia sekci pre HexEditor nie je este OK
    - AdvancedInfo
}

unit NEFileUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  Classes,
  SysUtils,

  StringRes,
  procmat,
  ExecFileUnit,
  SectionUnit,
  DummySectionUnit,
  CodeSectionUnit,
  ImportSectionUnit,
  ExportSectionUnit;

const
  btConstant = $FE;
  btMovable = $FF;

type
  TNEHeader = record
    sign:word;
    linkerversion,linkerrevision:byte;
    entrytableRO:word;
    entrytablesize:word;
    reserved:cardinal;
    flags:word;
    AutomaticDataSegmentNumber:word;
    InitialHeapSize:word;
    InitialStackSize:word;
    _IP,_CS: word;
    _SP,_SS: word;
    SegmentTableEntryNumber:word;
    ModuleReferenceTableEntryNumber:word;
    NonResidentNameTableSize:word;
    SegmentTableRO:word;
    ResourceTableRO:word;
    ResidentNameTableRO:word;
    ModuleReferenceTableRO:Word;
    ImportedNameTableRO:word;
    NonResidentNameTableOffset:cardinal;
    MoveableEntrypointsNumber:word;
    ShiftCount:word;
    ResourceSegmentNumber:word;
    TargetOS:byte;
    AdditionalInfo:byte;
    FLAOffset,FLASize:word;
    reserved2:word;
    ExpectedVersionNumber:word
  end;

  TSegmentTableEntry = record
    Offset:word;
    Size:word;
    Flags:word;
    AllocationSize:word;
  end;

  TResourceInformationBlock = record
    TypeID: word;
    count: word;
    reserved: cardinal;
  end;

  TResourceEntry = record
    Offset: word;
    Size: word;
    Flags: word;
    resID: word;
    reserved: cardinal;
  end;

  TEntryTableEntryType = (etEmpty,etFixed,etMovable);


  TFixedBundle = record
    Flag: byte;
    Offset: word;
  end;

  TMovableBundle = record
    Flag: byte;
    Int3F: word;
    Segment: byte;
    Offset: word;
  end;

  TEntryTableEntry = record
     count: byte;
     typ: TEntryTableEntryType;
     Fixed: array of TFixedBundle;
     Movable: array of TMovableBundle;
  end;

  TNEFile = class(TExecutableFile)
    header: TNEHeader;
    NEOffset: cardinal;
    SegmentTable: array of TSegmentTableEntry;
    EntryTable: array of TEntryTableEntry;
//       ResourceTable:
//       RsidentNameTable:

    constructor Create(a: TStream; aFileName: TFileName); overload; override;
    constructor Create; overload;
    destructor Destroy; override;
    function SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; override;
    function LoadFromFile(DHF: TStream; var DAS: TextFile): boolean; override;
//    function LoadFromFile(var f: TextFile; a:TMemoryStream):boolean; override;

//    function GetAdvancedInfo: TExecFileAdvancedInfo; override;

    function GetSegmentType(Index: integer): TSectionType;

  end;

implementation

const
  c_NEFileAdvancedInfoCount = 3;


constructor TNEFile.Create(a: TStream; aFileName: TFileName);
var
// NEoffset: cardinal;
    i:integer;
    temp8: byte;
    temp16: word;
    RInfoBlock: TResourceInformationBlock;
    cs: TCodeSection;
    buncount: byte;
    buntyp: byte;
    dlzka: byte;
    typ,count: byte;

begin
  inherited;
// Zistenie pozicie NE hlavicky
  a.Seek(60,0);
  a.Read(NEoffset,4);
// Nacitanie NE hlavicky
  a.Seek(NEoffset,0);
  a.Read(header,SizeOf(TNEHeader));
// Nacitanie "Segment Table"


  SetLength(SegmentTable,header.SegmentTableEntryNumber);
  for i:=0 to header.SegmentTableEntryNumber-1 do a.Read(SegmentTable[i],SizeOf(TSegmentTableEntry));
{
// Prejdenie cez "Resource Table"
  a.Seek(2,1); // preskocenie "aligment shift count"
  repeat
    a.Read(RInfoBlock,sizeof(TResourceInformationBlock)); // precitanie "information block"
    if RInfoBlock.TypeID = 0 then break;
    a.Seek(RInfoBlock.count*SizeOf(TResourceEntry),1);    // preskocenie "resource entry table"
  until false;
  repeat
    a.Read(temp8,1);
    if temp8=0 then break;
    a.Seek(temp8,1);
  until false;
// Prejdenie cez "Resident-Name Table"
  repeat
    a.Read(temp8,1);
    if temp8=0 then break;
    a.Seek(temp8+2,1);
  until false;
// Prejdenie cez "Module Reference Table"
  a.Seek(header.ModulReferenceTableEntryNumber)
}


//  SetLength(fSections, header.SegmentTableEntryNumber);

  for i:=0 to Sections.Count-1 do begin
    case GetSegmentType(i) of

    // Code segment
      stCode: begin
    // vecny problem: velkost segmentu v subore a pamati, obcas je pamat vacsia, ale aj kod v nej, ale subor je mensi, t.j. treba asi doplnit nulami
    //      neviem, ci subor hoci je mensi, predsa obsahuje chybajuce data (niekedy ano)
        cs:=TCodeSection.Create(a, false, SegmentTable[i].offset*16, SegmentTable[i].Size, SegmentTable[i].offset*16, SegmentTable[i].AllocationSize, i, 'N/A - code', self);
        if i=header._CS-1 then
          cs.EntryPointAddress:=header._IP;
    ////      cs.CodeSectionNumber:=High(Sections);
       // fSections[i]:=cs;
      end;

      stImport: begin
      //  fSections[i]:=TDummySection.Create('Modules', SegmentTable[i].offset*16, SegmentTable[i].Size, SegmentTable[i].offset*16, SegmentTable[i].AllocationSize, i, self);
{
        if header.ModuleReferenceTableEntryNumber > 0 then begin
          fSections[i]:= TImportSection.CreateFromNEFile(a, NEOffset + header.ModuleReferenceTableRO, NEOffset + header.ImportedNameTableRO, header.ModuleReferenceTableEntryNumber, header.EntryTableRO-header.ImportedNameTableRO, self);
        end;
}
      end;

      stExport: begin
        a.Position:=NEOffset + header.ResidentNameTableRO;
        a.Read(temp8,1);
        if temp8 <> 0 then begin
          fExportSection:= TExportSection.CreateFromNEFile(a,NEOffset + header.ResidentNameTableRO, header.NonResidentNameTableOffset, header.NonResidentNameTableSize, self);
     //     fSections[i]:=ExportSection;
        end;
      end;


      stDummy:
//        fSections[i]:=TDummySection.Create('???', SegmentTable[i].offset*16, SegmentTable[i].Size, SegmentTable[i].offset*16, SegmentTable[i].AllocationSize, i, self);

    end;
  end;

  // Set Import and Export for all code sections
  for i:=0 to Sections.Count-1 do
    if Sections[i].typ = stCode then begin
      (Sections[i] as TCodeSection).Exportt:=ExportSection;
    end;


//  CodeSectionsCount:=SectionCount;
//  CodeSectionNumber:=0;


// Entry Table:
  a.Position:=header.EntryTableRO + NEOffset;
  i:=0;
  a.Read(count,1);
  while count <> 0 do begin
    SetLength(EntryTable,i+1);
    EntryTable[i].count:=count;
    a.Read(typ,1);
    case Typ of
      0: EntryTable[i].typ:= etEmpty;
      $FF: begin
        EntryTable[i].typ:= etMovable;
        SetLength(EntryTable[i].Movable,count);
        a.Read(EntryTable[i].Movable[0],count*SizeOf(TMovableBundle));
      end
      else begin
        EntryTable[i].typ:= etFixed;
        SetLength(EntryTable[i].Fixed,count);
        a.Read(EntryTable[i].Fixed[0],count*SizeOf(TFixedBundle));
      end;
    end;
    Inc(i);
    a.Read(count,1);
  end;



{
  for i:=0 to CodeSectionCount-1 do begin


  end;
}
  fFormatDescription:='NE - New Executable (16-bit)';
  fExecFormat := NE;
end;

constructor TNEFile.Create;
begin
  fFormatDescription:='NE - New Executable (16-bit)';
end;



function TNEFile.SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean;
var
  i:integer;
begin
  if soProject in SaveOptions then begin
    DHF.Write(header, SizeOf(Header));
    for i:=0 to header.SegmentTableEntryNumber-1 do DHF.Write(SegmentTable[i], SizeOf(TSegmentTableEntry));
  end;
  result:=inherited SaveToFile(DHF, DAS, SaveOptions);
end;



function TNEFile.LoadFromFile(DHF: TStream; var DAS: TextFile): boolean; 
var i: integer;
begin
  DHF.Read(header,SizeOf(header));
  SetLength(SegmentTable, header.SegmentTableEntryNumber);
  for i:=0 to header.SegmentTableEntryNumber-1 do                                // Object Table
    DHF.Read(SegmentTable[i], sizeof(TSegmentTableEntry));
  result:=inherited LoadFromFile(DHF, DAS);
end;

destructor TNEFile.Destroy;
begin
  inherited;
end;

{
function TNEFile.GetAdvancedInfo: TExecFileAdvancedInfo;
begin
  result:=TExecFileAdvancedInfo.Create(c_NEFileAdvancedInfoCount);
  result.Add('', 'Module reference table number', IntToStr(header.ModuleReferenceTableEntryNumber));
  result.Add('', 'Module reference table file offset', IntToHex(header.ModuleReferenceTableRO + NEOffset,8));
  result.Add('', 'Import Module name table file offset', IntToHex(header.ImportedNameTableRO + NEOffset,8));

//    result.Add('', , );
end;
}


function TNEFile.GetSegmentType(Index: integer): TSectionType;
begin

  // Code segment
  if (SegmentTable[Index].Flags and word(1)) = 0 then begin
    result:=stCode;
    Exit;
  end;

  // Import
  if header.ModuleReferenceTableEntryNumber > 0 then
    if (SegmentTable[Index].Offset <= header.ModuleReferenceTableRO+NEOffset) and (header.ModuleReferenceTableRO+NEOffset <= SegmentTable[Index].Offset + SegmentTable[Index].Size) then begin
      result:=stImport;
      Exit;
    end;


  result:= stDummy;
end;



end.