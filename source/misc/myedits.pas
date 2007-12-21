unit myedits;

interface

uses SysUtils, StdCtrls, Classes, Windows;

type
//  TMyEditMode = (emBinary,emOctal,emDecimal,emHexadecimal);

  TMyEdit = class(TEdit)
   public
    MaxValue: cardinal;
    procedure Change(Sender: TObject); virtual; abstract;
   private
    function GetAsCardinal: cardinal; virtual; abstract;
    procedure SetAsCardinal(Value: cardinal); virtual; abstract;
   public
    property AsCardinal: cardinal read GetAsCardinal write SetAsCardinal;
  end;

  TMyHexEdit = class(TMyEdit)
   public
//    mode: TMyEditMode;
    constructor Create(AOwner: TComponent); override;
    procedure SetFocus; override;
   private
    BeforeStr: string;
    procedure MyHexEditKeyPress(Sender: TObject; var Key: Char);
    procedure Change(Sender: TObject); override;
    function GetAsCardinal: cardinal; override;
    procedure SetAsCardinal(Value:cardinal); override;
   public
  end;

  TMyNumEdit = class(TMyEdit)
   public
    constructor Create(AOwner: TComponent); override;
   private
    BeforeStr: string;
    procedure MyNumEditKeyPress(Sender: TObject; var Key: Char);
    procedure Change(Sender: TObject); override;
    function GetAsCardinal: cardinal; override;
    procedure SetAsCardinal(Value: cardinal); override;
   public
//    property AsInteger:integer read GetAsInteger write SetAsInteger; //(Value: Integer);
  end;

implementation

constructor TMyHexEdit.Create(AOwner: TComponent);
begin
  inherited;
  OnChange:=Change;
  OnKeyPress:=MyHexEditKeyPress;
  Text:='0x';
  BeforeStr:= Text;
  Font.Name:='Courier New';
  Font.Size:=10;
  MaxLength:=10;
end;

procedure TMyHexEdit.SetFocus;
begin
  inherited;
  SelStart:=2;
  SelLength:=Length(Text)-2;
end;

procedure TMyHexEdit.MyHexEditKeyPress(Sender: TObject; var Key: Char);
//var pom:string;
begin
  BeforeStr:=Text;
  if Key = #8 then Exit; // BACKSPACE
{
  if Key = '-' then begin
    pom:=Text;
    if Text[1]='-' then pom[1]:=' '
    else pom[1]:='-';
    Text:=pom;
    Key:=#0;
    Exit;
  end;
}
  if Key in ['a'..'f'] then Key:=UpCase(Key);
  if not (Key in ['0'..'9','A'..'F']) then Key:=#0;
end;

procedure TMyHexEdit.Change(Sender: TObject);
var teststr:string;
begin
  TestStr:=Copy(Text,1,2);
  if (TestStr <> '0x') or (AsCardinal > MaxValue) then Text:=beforestr;
  beforestr:=Text;
end;

function TMyHexEdit.GetAsCardinal: cardinal;
begin
  if Text='0x' then result:=0
  else result:=StrToInt64('$'+Copy(Text,3,8));
end;

procedure TMyHexEdit.SetAsCardinal(Value: cardinal);
begin
  Text:='0x' + IntToHex(Value,8);
//  if Value > 0 then Text:=' 0x' + IntToHex(Value,8);
//  else Text:='-0x' + IntToHex(Value,8);
end;



constructor TMyNumEdit.Create(AOwner: TComponent);
begin
  inherited;
  OnKeyPress:=MyNumEditKeyPress;
  OnChange:=Change;
  Font.Name:='Courier New';
  Font.Size:=10;
  MaxLength:=10;
end;

procedure TMyNumEdit.MyNumEditKeyPress(Sender: TObject; var Key: Char);
begin
  BeforeStr:=Text;
  if Key = #8 then Exit; // BACKSPACE
  if not (Key in ['0'..'9']) then Key:=#0;
end;

function TMyNumEdit.GetAsCardinal: cardinal;
begin
  if text='' then result:=0
  else result:=StrToInt64(Text);
end;

procedure TMyNumEdit.SetAsCardinal(Value: cardinal);
begin
  Text:=IntToStr(Value);
end;

procedure TMyNumEdit.Change(Sender: TObject);
begin
  if Text <> '' then begin
    if (StrToInt64Def(Text,-1) = -1) or (StrToInt64Def(Text,-1) > $FFFFFFFF)  then Text:=BeforeStr;
    if AsCardinal > MaxValue then Text:=BeforeStr;
  end;
  BeforeStr:=Text;
end;

end.
