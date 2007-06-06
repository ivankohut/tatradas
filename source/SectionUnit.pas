unit SectionUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  Classes,
  SysUtils,
  IniFiles,

  procmat;

type
  TSectionType = (stCode, stImport, stExport, stRelocation, stResource, stDummy);

  TSection = class
   private
    fName: string;
    fFileOffset: cardinal;
    fFileSize: cardinal;
    fMemOffset: cardinal;
    fMemSize: cardinal;
   protected
    fTyp: TSectionType;
    fSectionIndex: integer;  // index in ExecFile's 'Sections' array

//     Status: TSectionStatus;
   public
    ExecFile: TObject; // TExecutableFile;

    constructor Create(aName: string; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aSectionIndex: integer; aExecFile: TObject); virtual;

    // prefered
    function SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; overload; virtual; abstract;

    // deprecated
    function LoadFromFile(DHF: TFileStream; DAS: TTextFileStream):boolean; overload; virtual; abstract;
    function LoadFromFile(var f: TextFile; a: TStream):boolean; overload; virtual; abstract;
    // preferred
    function LoadFromFile(DHF: TStream; var DAS: TextFile): boolean; overload; virtual; abstract;


    property Typ: TSectionType read fTyp;
    property SectionIndex: integer read fSectionIndex;

    property Name: string read fName;
    property FileOffset: cardinal read fFileOffset;
    property FileSize: cardinal read fFileSize;
    property MemOffset: cardinal read fMemOffset;
    property MemSize: cardinal read fMemSize;
  end;


  TSections = class
   private
    fSections: array of TSection;
    fReadOnly: boolean;
    fCount: integer;

    function GetSection(Index: integer): TSection; 
   public
    constructor Create;
    destructor Destroy; override;

    procedure Add(ASection: TSection);
    procedure MakeReadOnly;

    function GetSectionNameFromFileOffset(Offset: cardinal): string; virtual;          // Nazov sekcie (ak nejaky ma)
    function GetSectionIndexFromFileOffset(Offset: cardinal): integer; virtual;  // Index sekcie v poli Sections
    function GetSectionIndexFromMemOffset(Offset: cardinal): integer; virtual;  // Index sekcie v poli Sections
    function GetSectionOffsetFromFileOffset(Offset: cardinal): cardinal; virtual;  // Offset v danej sekcii

    property Count: integer read fCount;
    property Items[Index: integer]: TSection read GetSection; default;
  end;


implementation

{ TSection }

constructor TSection.Create(aName: string; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aSectionIndex: integer; aExecFile: TObject);
begin
  fName:=aName;
  fFileOffset:=aFileOffset;
  fFileSize:=aFileSize;
  fMemOffset:=aMemOffset;
  fMemSize:=aMemSize;
  fSectionIndex:=aSectionIndex;
  ExecFile:=aExecFile;
end;

{ TSections }



constructor TSections.Create;
begin
  fReadOnly:=false;
  fCount:=0;
end;



destructor TSections.Destroy;
var
  i: integer;
begin
  for i:=0 to fCount - 1 do
    fSections[i].Free;
end;




function TSections.GetSectionNameFromFileOffset(
  Offset: cardinal): string;
var i:integer;
begin
  for i:=0 to Count - 1 do begin
    if (Offset >= fSections[i].FileOffset) and (Offset < fSections[i].FileOffset + fSections[i].FileSize) then begin
      result:= fSections[i].name;
      Exit;
{ toto spravim neskor
      for j:=CodeSectionNumber to CodeSectionNumber+CodeSectionsCount-1 do
        if (Sections[j] as TCodeSection).InSection(ObjectTable[i].rva) then result:=result+' - '+CodeSectionStr+IntToStr((Sections[j] as TCodeSection).SectionNumber);
}
    end;
  end;
  result:='not section';
end;



function TSections.GetSectionOffsetFromFileOffset(Offset: cardinal): cardinal;
var index: integer;
begin
  Index:=GetSectionIndexFromFileOffset(Offset);
  if Index <> -1 then
    Result:=Offset - fSections[Index].FileOffset
  else
    result:=MaxCardinal;
end;



function TSections.GetSectionIndexFromFileOffset(Offset: cardinal): integer;
var i: integer;
begin
  for i:=0 to Count - 1 do
    if (Offset >= fSections[i].FileOffset) and (Offset < fSections[i].FileOffset + fSections[i].FileSize) then begin
      result:=i;
      Exit;
    end;
  result:=-1;
end;



function TSections.GetSectionIndexFromMemOffset(Offset: cardinal): integer;
var i: integer;
begin
  for i:=0 to Count - 1 do
    if (Offset >= fSections[i].MemOffset) and (Offset < fSections[i].MemOffset + fSections[i].MemSize) then begin
      result:=i;
      Exit;
    end;
  result:=-1;
end;



procedure TSections.Add(ASection: TSection);
begin
  if not fReadOnly then begin
    Inc(fCount);
    SetLength(fSections, self.Count);
    ASection.fSectionIndex:= Count - 1;
    fSections[fCount-1]:= ASection;
  end
  else
    raise Exception.Create('Class data read-only');
end;



procedure TSections.MakeReadOnly;
begin
  fReadOnly:= true;
end;



function TSections.GetSection(Index: integer): TSection;
begin
  if (Index >= 0) and (Index < fCount)  then
    result:= fSections[Index]
  else
    raise Exception.Create('Array index out of bounds');
end;



end.
