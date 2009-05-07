unit SectionUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  Classes,
  SysUtils,
  IniFiles,

  procmat;

type
  TSectionType = (stCode, stImport, stExport, stRelocation, stResource);

  TSection = class
   private
    fName: string;

   protected
    fExecFile: TObject; // TExecutableFile;
    fTyp: TSectionType;
    fSectionIndex: Integer;  // index in ExecFile's 'Sections' array

   public
    constructor Create(aName: string; aExecFile: TObject); overload; virtual;

    procedure SaveToFile(DHF: TStream; var DAS: TextFile); virtual;
    function LoadFromFile(DHF: TStream; var DAS: TextFile): boolean; virtual;

    property Name: string read fName;
    property Typ: TSectionType read fTyp;
    property SectionIndex: Integer read fSectionIndex;
    property ExecFile: TObject read fExecFile;
  end;



  TSections = class
   private
    fSections: array of TSection;
    fReadOnly: Boolean;
    fCount: Integer;

    function GetSection(Index: Integer): TSection;
   public
    constructor Create;
    destructor Destroy; override;

    procedure Add(ASection: TSection);
    procedure MakeReadOnly;
{
    function GetSectionNameFromFileOffset(Offset: cardinal): string; virtual;          // Nazov sekcie (ak nejaky ma)
    function GetSectionIndexFromFileOffset(Offset: cardinal): integer; virtual;  // Index sekcie v poli Sections
    function GetSectionOffsetFromFileOffset(Offset: cardinal): cardinal; virtual;  // Offset v danej sekcii
}

    function GetSectionIndexFromMemOffset(Offset: Cardinal): Integer; virtual;  // Index (kodovej) sekcie v poli Sections

    property Count: Integer read fCount;
    property Items[Index: Integer]: TSection read GetSection; default;
  end;


implementation

uses CodeSectionUnit;

{ TSection }

constructor TSection.Create(aName: string; aExecFile: TObject);
begin
  fName := aName;
  fExecFile := aExecFile;
end;



procedure TSection.SaveToFile(DHF: TStream; var DAS: TextFile);
begin
  StreamWriteAnsiString(DHF, fName);
  DHF.Write(fSectionIndex, 4);
end;



function TSection.LoadFromFile(DHF: TStream; var DAS: TextFile): Boolean;
begin
  fName := StreamReadAnsiString(DHF);
  DHF.Read(fSectionIndex, 4);
  Result := True;
end;


{ TSections }


constructor TSections.Create;
begin
  fReadOnly := False;
  fCount := 0;
end;



destructor TSections.Destroy;
var
  i: Integer;
begin
  for i := 0 to fCount - 1 do
    fSections[i].Free;
end;



{
function TSections.GetSectionNameFromFileOffset(
  Offset: cardinal): string;
var i:integer;
begin
  for i:=0 to Count - 1 do begin
    if (Offset >= fSections[i].FileOffset) and (Offset < fSections[i].FileOffset + fSections[i].FileSize) then begin
      result:= fSections[i].name;
      Exit;
// toto spravim neskor
//      for j:=CodeSectionNumber to CodeSectionNumber+CodeSectionsCount-1 do
//        if (Sections[j] as TCodeSection).InSection(ObjectTable[i].rva) then result:=result+' - '+CodeSectionStr+IntToStr((Sections[j] as TCodeSection).SectionNumber);

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
}



function TSections.GetSectionIndexFromMemOffset(Offset: Cardinal): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if fSections[i].Typ = stCode then
      with fSections[i] as TCodeSection do
        if (Offset >= MemOffset) and (Offset < MemOffset + MemSize) then begin
          Result := i;
          Exit;
        end;
  Result := -1;
end;



procedure TSections.Add(ASection: TSection);
begin
  if not fReadOnly then begin
    Inc(fCount);
    SetLength(fSections, self.Count);
    ASection.fSectionIndex := Count - 1;
    fSections[fCount - 1] := ASection;
  end
  else
    raise EIllegalState.Create('Class data read-only');
end;



procedure TSections.MakeReadOnly;
begin
  fReadOnly := True;
end;



function TSections.GetSection(Index: Integer): TSection;
begin
  if (Index >= 0) and (Index < fCount)  then
    result := fSections[Index]
  else
    raise EIllegalState.Create('Array index out of bounds');
end;



end.
