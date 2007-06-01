unit DummySectionUnit;

{$INCLUDE 'DELVER.INC'}

interface

uses
  SectionUnit;
  
type

  TDummySection = class(TSection)
    constructor Create(aName: string; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aSectionIndex: integer; aExecFile: TObject); override;
  end;

implementation

constructor TDummySection.Create(aName: string; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aSectionIndex: integer; aExecFile: TObject);
begin
  inherited Create(aName, aFileOffset, aFileSize, aMemOffset, aMemSize, aSectionIndex, aExecFile);
  fTyp:=stDummy;
end;  

end.
