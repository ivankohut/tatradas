unit myedits;

interface

uses SysUtils, StdCtrls, Classes, Windows;

type
//  TMyEditMode = (emBinary,emOctal,emDecimal,emHexadecimal);

  TPositiveEdit = class(TEdit)
  private
    fValue: cardinal;
    fMaxValue: cardinal;
    function GetAsCardinal: cardinal;
    procedure SetAsCardinal(AValue: cardinal);
  public
    procedure Change(Sender: TObject); virtual; abstract;
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
    procedure MyHexEditKeyPress(Sender: TObject; var Key: Char);
  public
    procedure Change(Sender: TObject); override;
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;
  end;


  TDecimalPositiveEdit = class(TPositiveEdit)
  private
    procedure MyNumEditKeyPress(Sender: TObject; var Key: Char);
  public
    procedure Change(Sender: TObject); override;
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
  OnChange := Change;
  OnKeyPress := MyHexEditKeyPress;
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



procedure THexPositiveEdit.MyHexEditKeyPress(Sender: TObject; var Key: Char);
begin
  // BACKSPACE
  if Key = #8 then
    Exit;

  if Upcase(Key) in HEX_DIGITS then
    Key := Upcase(Key)
  else
    Key := #0;
end;



procedure THexPositiveEdit.Change(Sender: TObject);
var
  CandidateValue: cardinal;
  CaretPosition: integer;
begin
  // During creation Parent is not set => we can not use SelStart
  if Parent = nil then begin
    Text := HEX_PREFIX;
    Exit;
  end;


  CaretPosition := Max(2, SelStart - 1);
  if Text = HEX_PREFIX then
    fValue := 0
  else begin
    CandidateValue := StrToInt64Def('$' + Copy(Text, Length(HEX_PREFIX) + 1, 8), MaxValue + 1);
    if (Copy(Text, 1, 2) = HEX_PREFIX) and (candidateValue <= MaxValue) then
      fValue := candidateValue
    else begin
      Text := HEX_PREFIX + CarToHex(fValue, 8);
      SelStart := CaretPosition;
    end;
  end;
end;



constructor TDecimalPositiveEdit.Create(AOwner: TComponent);
begin
  inherited;
  OnKeyPress := MyNumEditKeyPress;
  OnChange := Change;
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



procedure TDecimalPositiveEdit.Change(Sender: TObject);
var
  CandidateValue: cardinal;
  CaretPosition: integer; 
begin
  // During creation Parent is not set => we can not use SelStart
  if Parent = nil then begin
    Text := HEX_PREFIX;
    Exit;
  end;

  CaretPosition := Max(0, SelStart - 1);
  if Text <> '' then begin
    CandidateValue := StrToInt64Def(Text, MaxValue + 1);
    if CandidateValue <= MaxValue then
      fValue := CandidateValue
    else
      Text := CarToStr(fValue);
  end;
end;



end.
