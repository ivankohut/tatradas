unit AdvancedDisassembleFormUnit;

interface

uses
{$IFDEF MSWINDOWS}
  Controls, Forms, Dialogs, StdCtrls,
{$ENDIF}
{$IFDEF LINUX}
  QControls, QForms, QStdCtrls, QExtCtrls,
{$ENDIF}
  SysUtils,
  Classes,
  myedits,
  IniFiles,

  procmat,
  TranslatorUnit;

type
  TAdvancedDisassembleForm = class(TForm, ITranslatable)
    OptionsGroupBox: TGroupBox;
    BytesRadioButton: TRadioButton;
    MaxRadioButton: TRadioButton;
    NormalRadioButton: TRadioButton;
    OKButton: TButton;
    CancelButton: TButton;
    Bit1632GroupBox: TGroupBox;
    bit16Radiobutton: TRadioButton;
    bit32Radiobutton: TRadioButton;
    RecursiveCheckBox: TCheckBox;
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure BytesRadioButtonClick(Sender: TObject);
    procedure MaxRadioButtonClick(Sender: TObject);
    procedure NormalRadioButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ValueChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    BytesBinHexEdit: TDecimalPositiveEdit;//TPBBinHexEdit;
    MaxAddressBinHexEdit: THexPositiveEdit;//TPBBinHexEdit;
    fIsChanging: Boolean; // true, ak prebieha spracovanie OnChange nejakeho editu
    function GetOptions: TDisassembleFormOptions;
  public
    property Options: TDisassembleFormOptions read GetOptions;
    procedure Translate;
    procedure SetMaxAddressMaxValue(AValue: Cardinal);
  end;

var
  AdvancedDisassembleForm: TAdvancedDisassembleForm;
  SelectedEdit: TCustomEdit;


implementation


{$R *.dfm}


procedure TAdvancedDisassembleForm.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;



procedure TAdvancedDisassembleForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;



function TAdvancedDisassembleForm.GetOptions: TDisassembleFormOptions;
begin
  if BytesRadioButton.Checked then
    Result.Option := dtBytes
  else if MaxRadioButton.Checked then
    Result.Option := dtMaxAddress
  else if NormalRadioButton.Checked then
    Result.Option := dtNormal
  else
    raise EIllegalState.Create('No RadioButton checked');

  Result.Recursive := RecursiveCheckbox.Checked;
  Result.Bit32 := bit32Radiobutton.Checked;
  Result.Value := TPositiveEdit(SelectedEdit).AsCardinal;
end;



procedure TAdvancedDisassembleForm.BytesRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled := False;
  SelectedEdit := BytesBinHexEdit;
  SelectedEdit.Enabled := True;
  SelectedEdit.SetFocus;
end;



procedure TAdvancedDisassembleForm.MaxRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled := False;
  SelectedEdit := MaxAddressBinHexEdit;
  SelectedEdit.Enabled := True;
  SelectedEdit.SetFocus;
end;



procedure TAdvancedDisassembleForm.NormalRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled := False;
end;



procedure TAdvancedDisassembleForm.FormCreate(Sender: TObject);
begin
  BytesBinHexEdit:=TDecimalPositiveEdit.Create(self);//TPBBinHexEdit.Create(self);
  BytesBinHexEdit.Left:= 128;
  BytesBinHexEdit.Top:= 24;
//  BytesBinHexEdit.BaseFormat:=Number;
  BytesBinHexEdit.Parent:= self.OptionsGroupBox;
  BytesBinHexEdit.Enabled:= false;
  BytesBinHexEdit.OnChange:= ValueChange;
  BytesBinHexEdit.MaxValue:= $FFFFFFFF;

  MaxAddressBinHexEdit := THexPositiveEdit.Create(self);//TPBBinHexEdit.Create(self);
  MaxAddressBinHexEdit.Left := 128;
  MaxAddressBinHexEdit.Top := 56;
//  MaxAddressBinHexEdit.BaseFormat:=HexaDecimal;
  MaxAddressBinHexEdit.Parent := self.OptionsGroupBox;
  MaxAddressBinHexEdit.Enabled := False;
  MaxAddressBinHexEdit.OnChange := ValueChange;

  BytesBinHexEdit.AsCardinal := 0;
  SelectedEdit := BytesBinHexEdit;

  RecursiveCheckBox.Checked := True;
  bit32Radiobutton.Checked := True;
  BytesRadioButton.Checked := True;
end;



procedure TAdvancedDisassembleForm.ValueChange(Sender: TObject);
var
  MyEdit: TPositiveEdit;
begin
  if fIsChanging then
    Exit
  else
    fIsChanging := True;
  MyEdit := TPositiveEdit(Sender);
  MyEdit.Change(Sender);
  if MyEdit.Text = '' then
    Exit;
  fIsChanging := False;
end;



procedure TAdvancedDisassembleForm.SetMaxAddressMaxValue(AValue: Cardinal);
begin
  MaxAddressBinHexEdit.MaxValue := AValue;
end;



procedure TAdvancedDisassembleForm.Translate;
begin
  Caption := Translator.TranslateControl('AdvancedDisassembleForm', 'Caption');
  OptionsGroupBox.Caption := Translator.TranslateControl('AdvancedDisassembleForm', 'Options');
  BytesRadioButton.Caption := Translator.TranslateControl('AdvancedDisassembleForm', 'Bytes');
  MaxRadioButton.Caption := Translator.TranslateControl('AdvancedDisassembleForm', 'Max');
  NormalRadioButton.Caption := Translator.TranslateControl('AdvancedDisassembleForm', 'Normal');
  Bit1632GroupBox.Caption := Translator.TranslateControl('AdvancedDisassembleForm', 'Bit1632GroupBox');
//  bit16Radiobutton.Caption := Translator.TranslateControl('AdvancedDisassembleForm', '');
//  bit32Radiobutton.Caption := Translator.TranslateControl('AdvancedDisassembleForm', '');
  RecursiveCheckBox.Caption := Translator.TranslateControl('AdvancedDisassembleForm', 'Recursive');

  OKButton.Caption := Translator.TranslateControl('Common', 'OKButton');
  CancelButton.Caption := Translator.TranslateControl('Common', 'CancelButton');
end;



procedure TAdvancedDisassembleForm.FormShow(Sender: TObject);
begin
  SelectedEdit.Enabled := True;
  SelectedEdit.SetFocus;
end;



end.
