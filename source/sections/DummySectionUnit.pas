unit DummySectionUnit;

{$INCLUDE 'DELVER.INC'}

interface

uses
  SectionUnit;

type

  TDummySection = class(TSection)
    constructor Create(aName: string; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aSectionIndex: integer; aExecFile: TObject);
  end;

implementation

constructor TDummySection.Create(aName: string; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aSectionIndex: integer; aExecFile: TObject);
begin
  inherited Create(aName, aExecFile);
  fTyp:=stDummy;
end;

end.
