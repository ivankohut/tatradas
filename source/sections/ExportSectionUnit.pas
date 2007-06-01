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
    flags,timedate:cardinal;
    major,minor:word;
    namerva:cardinal;
    ordinalbase:cardinal;
    eatentriescount:cardinal;
    namepointerscount:cardinal;
    addresstableRVA:cardinal;
    namepointertableRVA:cardinal;
    ordinaltablerva:cardinal;
  end;

  TExportFunction = record
//    EntryPointVA: cardinal;   // VirtualAddress

//    EntryPointRVA:cardinal;  // Relative Virtual Address
    
    MemOffset: cardinal;  // 
    CodeSectionOffset: cardinal;   // Offset relative to function's code section  
    Ordinal: cardinal;
    
    Section: integer; // kodova sekcie obsahujuca exp. funkciu
    name: string;
  end;

  TExportSection = class(TSection)
    functioncount: integer;
    functions:array of TExportFunction;
    constructor Create(efile:TObject); overload;
    destructor Destroy; override;
{$IFDEF GUI_B}
    procedure GotoFunction(index: integer); virtual; abstract;
{$ENDIF}
    function SaveToFile(var f:TextFile; a:TStream; SaveOptions: TSaveOptions):boolean; override;
    function LoadFromFile(var f: TextFile; a: TStream):boolean; override;
///    procedure Translate(ini: TMemINIFile; error:string); override;
  end;


  TPEExportSection = class(TExportSection)
    constructor Create(InputFile: TStream; ExportTableRVA, ImageBase: cardinal; aName: string; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aSectionIndex: integer; aExecFile: TObject);
//    constructor Create(InputFile: TStream; ExportRVA, FileOffset, FileSize, ExportTableRVA:cardinal; ImageBase: cardinal; efile:TObject); overload;
{$IFDEF GUI_B}
    procedure GotoFunction(index: integer); override;
{$ENDIF}
  end;

  TNEExportSection = class(TExportSection)
    constructor Create(a:TStream; ResidentTableOffset, NonResidentTableOffset, NonResidentTableSize: cardinal; efile:TObject); overload;
{$IFDEF GUI_B}
    procedure GotoFunction(index: integer); override;
{$ENDIF}
  end;



implementation

uses ExecFileUnit,CodeSectionUnit,
//NEFileUnit,
PEFileUnit;

constructor TExportSection.Create(efile:TObject);
begin
  fTyp:=stExport;
  execfile:=efile;
end;

destructor TExportSection.Destroy;
begin
  inherited;
end;

function TExportSection.SaveToFile(var f:TextFile; a:TStream; SaveOptions: TSaveOptions):boolean;
var i: integer;
begin
  a.Write(FunctionCount,4);
  for i:=0 to functioncount do begin
    a.Write(functions[i],sizeof(TExportFunction)-4);
    a.Write(pchar(functions[i].name)^,length(functions[i].name)+1);
  end;
  result:=true;
end;

function TExportSection.LoadFromFile(var f: TextFile; a: TStream):boolean;
var i:integer;
begin
  a.Read(FunctionCount,4);
  setlength(functions,FunctionCount);
  for i:=0 to FunctionCount-1 do begin
    a.Read(functions[i],sizeof(TExportFunction)-4);
    ReadStringFromStream(a,a.Position,functions[i].name);
  end;
{$IFDEF GUI_B}
{
  tab:=TExportTabSheet.Create(Ctrls.PageControl,self);
  TabSheet:=tab;
}
{$ENDIF}
  result:=true;
end;


//============================================================================================================
// TPEExportSection class
//============================================================================================================

constructor TPEExportSection.Create(InputFile: TStream; ExportTableRVA, ImageBase: cardinal; aName: string; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aSectionIndex: integer; aExecFile: TObject);
var
  ExportStream: TMemorystream;
  DirTable:TPEExportDirectoryTable;
  FunctionRVA: cardinal;
  ExportRVA: cardinal;

  i:integer;
  e:cardinal;
  ordinal:word;
  pefile: TPEFile;
begin
  inherited Create(aName, aFileOffset, aFileSize, aMemOffset, aMemSize, aSectionIndex, aExecFile);
  fTyp:=stExport;
  ExportRVA:=MemOffset - ImageBase;

  pefile:=ExecFile as TPEFile;
  // Nacitanie export section casti vstupneho suboru (InputFile) do streamu (ExportStream) 
  InputFile.Seek(FileOffset, 0);
  ExportStream:=TMemoryStream.Create;
  ExportStream.CopyFrom(InputFile, FileSize);

//  a:=LoadDataFromFile(subor,objecttable.objecttables[objecttable.exportobjectnumber].size,objecttable.objecttables[objecttable.exportobjectnumber].offset);

  ExportStream.Seek(ExportTableRVA - ExportRVA, 0);
  ExportStream.Read(DirTable, 40);
  SetLength(functions, DirTable.eatentriescount);
  ExportStream.Seek(DirTable.addresstableRVA-ExportRVA, 0);
  FunctionCount:=DirTable.eatentriescount;

  for i:=0 to integer(DirTable.eatentriescount)-1 do begin
//    ExportStream.Read(functions[i].EntryPointRVA, 4);
    ExportStream.Read(FunctionRVA, 4);
    functions[i].MemOffset:=FunctionRVA + ImageBase;
    functions[i].ordinal:=i + DirTable.OrdinalBase;
    if FunctionRVA <> 0 then begin
      functions[i].section:=pefile.GetSectionNumberFromRVA(FunctionRVA);
      if functions[i].section = -1 then Continue;
      functions[i].CodeSectionOffset:= functions[i].MemOffset - pefile.Sections[functions[i].section].MemOffset;
    end
    else begin
      functions[i].section:=0;
      functions[i].CodeSectionOffset:=0;
    end;
{
    if functions[i].entrypointRVA < objecttable.objecttables[GetObjectNumberFromRVA(exportt.functions[1].entrypointRVA)].rva then
      functions[i].name:='! INVALID RVA !';
}
  end;
//  exportRVA:=objecttable.objecttables[GetObjectNumberFromRVA(functions[1].entrypointRVA)].rva;
  for i:=0 to integer(DirTable.namepointerscount) - 1 do begin
    ExportStream.Seek(DirTable.ordinaltablerva-ExportRVA+2*i, 0);
    ExportStream.Read(ordinal, 2);
    ExportStream.Seek(DirTable.namepointertableRVA-ExportRVA+4*i, 0);
    ExportStream.Read(e,4);
    ReadStringFromStream(ExportStream, e-ExportRVA, functions[ordinal].name);
    if functions[i].MemOffset <> ImageBase then begin
      functions[i].section:=pefile.Sections.GetSectionIndexFromMemOffset(functions[i].MemOffset);
      if functions[i].section = -1 then continue;
      functions[i].CodeSectionOffset:= functions[i].MemOffset - PEFile.Sections[functions[i].section].MemOffset;
    end
    else begin
      functions[i].section:=0;
      functions[i].CodeSectionOffset:=0;
    end;
  end;
  ExportStream.Free;
end;



{$IFDEF GUI_B}

procedure TPEExportSection.GotoFunction(index: integer);
var i:integer;
begin
{
  i:=functions[index].section;
  if i = -1 then Exit;
  with ExecFile as TExecutableFile do begin
    (Sections[i] as TCodeSection).tab.GotoPosition(
      (Sections[i] as TCodeSection).tab.GetPosition(functions[index].offset)-1,0);
  end;
  Ctrls.PageControl.ActivePage:=((ExecFile as TExecutableFile).Sections[i] as TCodeSection).tab;
  Ctrls.PageControl.OnChange(nil);
}
end;
{$ENDIF}

//============================================================================================================
// TNEExportTabSheet class
//============================================================================================================


constructor TNEExportSection.Create(a:TStream; ResidentTableOffset, NonResidentTableOffset, NonResidentTableSize: cardinal; efile:TObject);
var i: integer;
    dlzka: byte;
    ModuleName1,ModuleName2: string;
{
  procedure FindEntryPoint(ordinal: cardinal; var segment: integer; var address: cardinal);
  var i: integer;
  begin
    i:=0;
    with (ExecFile as TNEFile) do
      while true do begin
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
}
begin
{
  typ:=stExport;
  execfile:=efile;
  self.Ctrls:=Ctrls;
// Resident:
  a.position:=ResidentTableOffset;
  a.Read(dlzka,1);
  SetLength(ModuleName1,dlzka);
  a.Read(ModuleName1[1],dlzka);
  a.Seek(2,1);

  i:=0;
  a.Read(dlzka,1);
  while dlzka > 1 do begin
    SetLength(functions,i+1);
    SetLength(functions[i].name,dlzka);
    a.Read(functions[i].name[1],dlzka);
    a.Read(functions[i].ordinal,2);
    FindEntryPoint(functions[i].ordinal,functions[i].section,functions[i].offset);
    inc(i);
    a.Read(dlzka,1);
  end;
  functioncount:=i;
// Non-Resident:
  a.position:=NonResidentTableOffset;
  a.Read(dlzka,1);
  SetLength(ModuleName2,dlzka);
  a.Read(ModuleName2[1],dlzka);
  a.Seek(2,1);

  a.Read(dlzka,1);
  while dlzka > 1 do begin
    SetLength(functions,i+1);
    SetLength(functions[i].name,dlzka);
    a.Read(functions[i].name[1],dlzka);
    a.Read(functions[i].ordinal,2);
    FindEntryPoint(functions[i].ordinal,functions[i].section,functions[i].offset);
    inc(i);
    a.Read(dlzka,1);
  end;
  functioncount:=i;

}
{$IFDEF GUI_B}
{
  tab:=TExportTabSheet.Create(Ctrls.PageControl,self);
  TabSheet:=tab;
}
{$ENDIF}
end;

{$IFDEF GUI_B}
procedure TNEExportSection.GotoFunction(index: integer);
var i:integer;
begin
{
  i:=functions[index].section;
  if i = -1 then Exit;
  with ExecFile as TExecutableFile do begin
    (Sections[i] as TCodeSection).tab.GotoPosition(
      (Sections[i] as TCodeSection).tab.GetPosition(functions[index].offset)-1,0);
  end;
  Ctrls.PageControl.ActivePage:=((ExecFile as TExecutableFile).Sections[i] as TCodeSection).tab;
  Ctrls.PageControl.OnChange(nil);
}
end;
{$ENDIF}

end.
