{
}

unit MZFileUnit;

{$INCLUDE 'delver.inc'}

interface

uses Classes, SysUtils,
     procmat,
     StringRes,
     ExecFileUnit,
     SectionUnit,
     CodeSectionUnit;

type
  TRelocEntry = record
    offset,segment: word;
  end;

  TRelocationSection = class(TSection)
    offset: cardinal;
    count: cardinal;
    relocs: array of TRelocEntry;
    constructor Create(a: TStream; offset: cardinal; count: cardinal);
    function LoadFromFile(var f:TextFile; a:TStream):boolean; override;
    function SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; override;
  end;

  TMZFile = class(TExecutableFile)
  private
    RelocationSectionNumber: integer;
  public
    header:TMZHeader;
    constructor Create(InputStream: TStream; aFileName: TFileName); overload; override;
    destructor Destroy(); override;
    function SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; override;
    function LoadFromFile(DHF: TStream; var DAS: TextFile): boolean; override;
//    function LoadFromFile(var f:TextFile; a:TMemoryStream):boolean; override;
//    function GetAdvancedInfo: TExecFileAdvancedInfo; override;
  end;

implementation

const
  c_MZFileAdvancedInfoCount = 10;


//******************************************************************************
// TMZFile class
//******************************************************************************


constructor TMZFile.Create(InputStream: TStream; aFileName: TFileName);
var
//  rs: TRelocationSection;
  CodeSection: TCodeSection;
  CodeSize:cardinal;
begin
  inherited;
  fExecFormat:=MZ;
  fFormatDescription:='MZ - DOS executable (16-bit)';

  InputStream.Seek(0,0);
  InputStream.Read(header,40);
  CodeSize:=(header.pagecount-1)*512 + HEADER.pageremainder - header.headersize*16;

  // Code section
  if CodeSize > 0 then begin
    CodeSection:=TCodeSection.Create(InputStream, false, 'N/A', header.headersize*16, CodeSize, header.headersize*16, CodeSize, 0, self);
    CodeSection.EntryPointAddress:={CodeSection.LogOffset}word(header.reloCS*16 + header.EXEIP);
    Sections.Add(CodeSection);
  end;
{
  // Relocation sections
  if header.RelocCount > 0 then begin
    RelocationSectionNumber:=length(Sections);
    setlength(Sections,RelocationSectionNumber+1);
    rs:=TRelocationSection.Create(a,header.RelocTableOffset,header.RelocCount);
    Sections[RelocationSectionNumber]:=rs;
  end;
}
end;



function TMZFile.SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; 
begin
  if soProject in SaveOptions then
    DHF.Write(header,sizeof(header));
  result:=inherited SaveToFile(DHF, DAS, SaveOptions);
end;



function TMZFile.LoadFromFile(DHF: TStream; var DAS: TextFile): boolean;

begin
  DHF.Read(header,sizeof(Header));
  result:=inherited LoadFromFile(DHF, DAS);
end;



destructor TMZFile.Destroy;
begin
  inherited;
end;


{
function TMZFile.GetAdvancedInfo: TExecFileAdvancedInfo;
begin
  result:=TExecFileAdvancedInfo.Create(c_MZFileAdvancedInfoCount);
  result.Add('', 'Page remainder:', IntToHex(header.pageremainder,4));
  result.Add('', 'Page count:', IntToStr(header.pagecount)+'(dec)');
  result.Add('', 'Relocations:', IntToStr(header.RelocCount)+'(dec)');
  result.Add('', 'Header size:', IntToHex(header.minmem,4));
  result.Add('', 'MinMem:', IntToHex(header.minmem,4));
  result.Add('', 'MaxMem:', IntToHex(header.maxmem,4));
  result.Add('', 'SS:SP', IntToHex(header.reloSS,4)+':'+IntToHex(header.EXESP,4));
  result.Add('', 'Check sum:', IntToHex(header.ChkSum,4));
  result.Add('', 'CS:IP', IntToHex(header.reloCS,4)+':'+IntToHex(header.EXEIP,4));
  result.Add('', 'Overlay:', IntToHex(header.Overlay,4));
end;
}

//******************************************************************************
// TRelocationSection class
//******************************************************************************


constructor TRelocationSection.Create(a: TStream; offset: cardinal; count: cardinal);
begin
  self.count:=count;
  setlength(Relocs,count);
  self.offset:=offset;
  a.Seek(Offset,0);
  a.Read(Relocs[0],Count * 4);
end;



function TRelocationSection.SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean;
begin
  ;
end;



function TRelocationSection.LoadFromFile(var f:TextFile; a:TStream):boolean;
begin
   ;
end;

end.