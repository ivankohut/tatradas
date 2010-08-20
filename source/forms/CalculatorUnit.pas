{ TODO:
  - padne pri zapornych cislach v hex, treba sa s tym pohrat
  - nie kalkulacka, ale konvertor medzi roznymi formatmi
    - bin, hex, dec
    - signed & unsigned
    - byte, word, ... qword, float ...
}


unit CalculatorUnit;

interface

uses
  IniFiles,
  SysUtils,
  Variants,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  ExtCtrls,
  Buttons,

  procmat,
  myedits,
  StringRes,
  TranslatorUnit;

type

  TCalcOperation = (coPlus, coMinus, coMul, coDiv, coMod);

  TCalculator = class(TForm, ITranslatable)
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    HexadecimalLabel: TLabel;
    DecimalLabel: TLabel;
    BinaryLabel: TLabel;
    OctalLabel: TLabel;
    Panel4: TPanel;
    Bevel1: TBevel;
    PlusButton: TSpeedButton;
    MinusButton: TSpeedButton;
    ModButton: TSpeedButton;
    MulButton: TSpeedButton;
    DivButton: TSpeedButton;
    EqualButton: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure EqualButtonClick(Sender: TObject);
    procedure PlusButtonClick(Sender: TObject);
    procedure MinusButtonClick(Sender: TObject);
    procedure ModButtonClick(Sender: TObject);
    procedure MulButtonClick(Sender: TObject);
    procedure DivButtonClick(Sender: TObject);

    procedure Translate;
  private
    Changing: boolean;
    DataBuffer: Int64;
    Operation: TCalcOperation;

    CurrentEdit: TPositiveEdit;

    DecEdit: TDecimalPositiveEdit;
    HexEdit: THexPositiveEdit;
    procedure EditChange(Sender: TObject);
    procedure EditEnter(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

var
  Calculator: TCalculator;

implementation

{$R *.lfm}

procedure TCalculator.EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
{
  case key of
    Ord('+'),VK_ADD: PlusButtonClick(PlusButton);
    Ord('-'),VK_SUBTRACT: begin
      MinusButtonClick(MinusButton);
      key:=0;
    end;
    Ord('*'),VK_MULTIPLY: MulButtonClick(MulButton);
    Ord('/'),VK_DIVIDE: DivButtonClick(DivButton);
    Ord('M'): ModButtonClick(ModButton);
    VK_RETURN: EqualButtonClick(EqualButton);
    VK_ESCAPE: self.Close;
  end;
}
end;

procedure TCalculator.EditChange(Sender: TObject);
var ChangedEdit: TPositiveEdit;
begin
  inherited;
  ChangedEdit:=(Sender as TPositiveEdit);

  if ChangedEdit.Text='' then exit;
  if ChangedEdit.Text='$' then exit;
  if ChangedEdit.Text='-' then exit;

  if Changing then Exit;
  Changing:=true;

  if DecEdit <> ChangedEdit then DecEdit.AsCardinal:=ChangedEdit.AsCardinal;
  Application.ProcessMessages;
  if HexEdit <> ChangedEdit then HexEdit.AsCardinal:=ChangedEdit.AsCardinal;
  Application.ProcessMessages;
{
  if BinEdit <> ChangedEdit then BinEdit.AsInteger:=ChangedEdit.AsInteger;
  Application.ProcessMessages;
  if OctEdit <> ChangedEdit then OctEdit.AsInteger:=ChangedEdit.AsInteger;
  Application.ProcessMessages;
}
  Changing:=false;
end;

procedure TCalculator.FormCreate(Sender: TObject);
begin
  DecEdit:= TDecimalPositiveEdit.Create(Panel1);
  DecEdit.Parent:=Panel1;
  DecEdit.Align:=alClient;
//  DecEdit.BaseFormat:=Number;
  DecEdit.OnChange:=EditChange;
  DecEdit.OnEnter:=EditEnter;
  DecEdit.OnKeyDown:=EditKeyDown;

  HexEdit:=THexPositiveEdit.Create(Panel2);
  HexEdit.Parent:=Panel2;
  HexEdit.Align:=alClient;
//  HexEdit.BaseFormat:=Hexadecimal;
  HexEdit.OnChange:=EditChange;
  HexEdit.OnEnter:=EditEnter;
  HexEdit.OnKeyDown:=EditKeyDown;
{
  BinEdit:=TPBBinHexEdit.Create(Panel3);
  BinEdit.Parent:=Panel3;
  BinEdit.Align:=alClient;
  BinEdit.BaseFormat:=Binary;
  BinEdit.OnChange:=EditChange;
  BinEdit.OnEnter:=EditEnter;
  BinEdit.OnKeyDown:=EditKeyDown;

  OctEdit:=TPBBinHexEdit.Create(Panel4);
  OctEdit.Parent:=Panel4;
  OctEdit.Align:=alClient;
  OctEdit.BaseFormat:=Binary;
  OctEdit.OnChange:=EditChange;
  OctEdit.OnEnter:=EditEnter;
  OctEdit.OnKeyDown:=EditKeyDown;
}
  DataBuffer:=0;
  CurrentEdit:=DecEdit;
end;

procedure TCalculator.EditEnter(Sender: TObject);
begin
  CurrentEdit:=Sender as TPositiveEdit;
end;

procedure TCalculator.EqualButtonClick(Sender: TObject);
begin
{
  case Operation of
    coPlus: DecEdit.AsInteger:=DataBuffer + DecEdit.AsInteger;
    coMinus: DecEdit.AsInteger:=DataBuffer - DecEdit.AsInteger;
    coMul: DecEdit.AsInteger:=DataBuffer * DecEdit.AsInteger;
    coDiv:
      if DecEdit.AsInteger = 0 then MessageDlg(DivisionByZeroStr,mtError,[mbOk],0)
      else DecEdit.AsInteger:=DataBuffer div DecEdit.AsInteger;
    coMod:
      if DecEdit.AsInteger = 0 then MessageDlg(DivisionByZeroStr,mtError,[mbOk],0)
      else DecEdit.AsInteger:=DataBuffer mod DecEdit.AsInteger;
  end;
  DataBuffer:=0;
  CurrentEdit.SetFocus;
  CurrentEdit.SelectAll;
}
end;

procedure TCalculator.PlusButtonClick(Sender: TObject);
begin
{
  Operation:=coPlus;
  DataBuffer:=CurrentEdit.AsInteger;
  CurrentEdit.SetFocus;
  CurrentEdit.SelectAll;
}
end;

procedure TCalculator.MinusButtonClick(Sender: TObject);
begin
{
  if CurrentEdit <> DecEdit then begin
    Operation:=coMinus;
    DataBuffer:=CurrentEdit.AsInteger;
    CurrentEdit.SetFocus;
    CurrentEdit.SelectAll;
  end;
}
end;

procedure TCalculator.ModButtonClick(Sender: TObject);
begin
  Operation:=coMod;
//  DataBuffer:=CurrentEdit.AsInteger;
  CurrentEdit.SetFocus;
  CurrentEdit.SelectAll;
end;

procedure TCalculator.MulButtonClick(Sender: TObject);
begin
  Operation:=coMul;
//  DataBuffer:=CurrentEdit.AsInteger;
  CurrentEdit.SetFocus;
  CurrentEdit.SelectAll;
end;

procedure TCalculator.DivButtonClick(Sender: TObject);
begin
  Operation:=coDiv;
//  DataBuffer:=CurrentEdit.AsInteger;
  CurrentEdit.SetFocus;
  CurrentEdit.SelectAll;
end;

procedure TCalculator.Translate;
begin
  Caption:= Translator.TranslateControl('CalculatorForm', 'Caption');
  DecimalLabel.Caption:= Translator.TranslateControl('CalculatorForm', 'DecimalLabel');
  HexadecimalLabel.Caption:= Translator.TranslateControl('CalculatorForm', 'HexadecimalLabel');
  BinaryLabel.Caption:= Translator.TranslateControl('CalculatorForm', 'BinaryLabel');
  OctalLabel.Caption:= Translator.TranslateControl('CalculatorForm', 'OctalLabel');
end;

end.




