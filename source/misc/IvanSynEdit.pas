unit IvanSynEdit;

interface

uses
  SynEdit,
  SynEditTypes,
  SynEditTextBuffer,
  {$IFDEF LCL}
  SynEditTextBase,
  {$ENDIF}

  Graphics;

type

  { TIvanSynEdit }

  TIvanSynEdit = class(TSynEdit)
  {$IFDEF LCL}
  private
    FActiveLineColor: TColor;
    FRedoList: TSynEditUndoList;
    procedure SetActiveLineColor(const AValue: TColor);
  public
    property ActiveLineColor: TColor Read FActiveLineColor Write SetActiveLineColor;
    procedure HookTextBuffer(aBuffer: TSynEditStringList; aUndo, aRedo: TSynEditUndoList);
    property UndoList;
    property RedoList: TSynEditUndoList Read FRedoList;
  {$ENDIF}
  end;

implementation

{ TIvanSynEdit }

{$IFDEF LCL}

procedure TIvanSynEdit.SetActiveLineColor(const AValue: TColor);
begin
  if FActiveLineColor <> AValue then
    FActiveLineColor := AValue;
end;


procedure TIvanSynEdit.HookTextBuffer(aBuffer: TSynEditStringList;
  aUndo, aRedo: TSynEditUndoList);
begin

end;

{$ENDIF}


end.

