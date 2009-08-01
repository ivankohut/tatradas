unit myedits;

interface

uses
  SysUtils, StdCtrls, Classes, Windows, StrUtils;

type
//  TMyEditMode = (emBinary,emOctal,emDecimal,emHexadecimal);

  TPositiveEdit = class(TEdit)
  private
    fValue: cardinal;
    fMaxValue: cardinal;
    function GetAsCardinal: cardinal;
    procedure SetAsCardinal(AValue: cardinal);
  public
//    procedure Change(Sender: TObject); virtual; abstract;
    property AsCardinal: cardinal read GetAsCardinal write SetAsCardinal;
    property MaxValue: cardinal read fMaxValue write fMaxValue;
  end;


const
  HEX_PREFIX = '0x';
  HEX_FONT = 'Courier New';
  HEX_DIGITS = ['0'..'9','A'..'F'];

type
  THexPositiveEdit = class(TPositiveEdit)
  private
    fLastKeyPressChar: Word;
    procedure MyHexEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
//    procedure MyHexEditKeyPress(Sender: TObject; var Key: Char);
  public
    //procedure Change(Sender: TObject); override;
    procedure Change; override;
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;
  end;


  TDecimalPositiveEdit = class(TPositiveEdit)
  private
    procedure MyNumEditKeyPress(Sender: TObject; var Key: Char);
  public
    procedure Change; override;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  Math,
  StringUtilities
  ;


function TPositiveEdit.GetAsCardinal: cardinal;
begin
  result := fValue;
end;



procedure TPositiveEdit.SetAsCardinal(AValue: cardinal);
begin
  fValue := AValue;
end;


{ TPositiveHexEdit }


constructor THexPositiveEdit.Create(AOwner: TComponent);
begin
  inherited;
//  OnChange := Change;
//  OnKeyPress := MyHexEditKeyPress;
  OnKeyDown := MyHexEditKeyDown;
  Font.Name := HEX_FONT;
  Font.Size := 10;
  MaxLength := Length(HEX_PREFIX) + 8;
  Text := HEX_PREFIX;
end;



procedure THexPositiveEdit.SetFocus;
begin
  inherited;
  SelStart := Length(HEX_PREFIX);
  SelLength := Length(Text) - SelStart;
end;


{
procedure THexPositiveEdit.MyHexEditKeyPress(Sender: TObject; var Key: Char);
begin
  try
    // BACKSPACE
    if Key = #8 then
      Exit;

    if Upcase(Key) in HEX_DIGITS then
      Key := Upcase(Key)
    else
      Key := #0;
  finally
    if Key <> #0 then
      fLastKeyPressChar := Key;
  end;
end;
}


procedure THexPositiveEdit.MyHexEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_BACK) or (Key = VK_DELETE) or (Key = VK_RIGHT) or (Key = VK_LEFT) or (Key = VK_HOME) or (Key = VK_END) then
    fLastKeyPressChar := Key
  else if (Key < 128) then begin
    if UpCase(Char(Key)) in HEX_DIGITS then begin
      Key := Ord(UpCase(Char(Key)));
      fLastKeyPressChar := Key;
    end
    else
      Key := 0;
  end;
end;


procedure THexPositiveEdit.Change;
var
  CandidateValue: cardinal;
  CaretPositionAfterChange: Integer;
  NewCaretPosition: integer;
begin
  // During creation Parent is not set => we can not use SelStart
  if Parent = nil then begin
    Text := HEX_PREFIX;
    Exit;
  end;

  CaretPositionAfterChange := SelStart;
  if Text = '' then
    Text := HEX_PREFIX;

  if Text = HEX_PREFIX then begin
    fValue := 0;
    NewCaretPosition := 2;      
  end
  else begin
//    NewCaretPosition := Max(2, SelStart - 1);
    NewCaretPosition := CaretPositionAfterChange;
    if LeftStr(Text, 2) = HEX_PREFIX then
      CandidateValue := StrToInt64Def('$' + Copy(Text, Length(HEX_PREFIX) + 1, 8), MaxValue + 1)
    else begin
      CandidateValue := StrToInt64Def('$' + Copy(Text, 1, 8), MaxValue + 1);
      NewCaretPosition := NewCaretPosition + Length(HEX_PREFIX);
    end;

    if CandidateValue <= MaxValue then begin
      Text := HEX_PREFIX + CarToHex(candidateValue, 0);
      fValue := candidateValue;
      //Inc(NewCaretPosition);
    end
    else begin
      Text := HEX_PREFIX + CarToHex(fValue, 0);
      if CaretPositionAfterChange < Length(HEX_PREFIX) then
        NewCaretPosition := Length(Text)
      else if UpCase(Char(fLastKeyPressChar)) in HEX_DIGITS then
        Dec(NewCaretPosition);
    end;


    {
    CandidateValue := StrToInt64Def('$' + Copy(Text, Length(HEX_PREFIX) + 1, 8), MaxValue + 1);
    if (Copy(Text, 1, 2) = HEX_PREFIX) and (candidateValue <= MaxValue) then
      fValue := candidateValue
    else begin
      CaretPosition := Max(2, SelStart - 1);
      Text := HEX_PREFIX + CarToHex(fValue, 0);
      SelStart := CaretPosition;
    end;
    }
  end;
  SelStart := NewCaretPosition;
  inherited;
end;



constructor TDecimalPositiveEdit.Create(AOwner: TComponent);
begin
  inherited;
  OnKeyPress := MyNumEditKeyPress;
  Font.Name := HEX_FONT;
  Font.Size := 10;
  MaxLength := 10;
end;



procedure TDecimalPositiveEdit.MyNumEditKeyPress(Sender: TObject; var Key: Char);
begin
  // BACKSPACE
  if Key = #8 then
    Exit;

  if not (Key in ['0'..'9']) then
    Key := #0;
end;



procedure TDecimalPositiveEdit.Change;
var
  CandidateValue: cardinal;
  CaretPositionAfterChange: Integer;
begin
  if Text <> '' then begin
    CaretPositionAfterChange := SelStart;
    CandidateValue := StrToInt64Def(Text, MaxValue + 1);
    if CandidateValue <= MaxValue then
      fValue := CandidateValue
    else begin
      Text := CarToStr(fValue);
      SelStart := CaretPositionAfterChange - 1;
    end;
  end;
  inherited;
end;



end.
