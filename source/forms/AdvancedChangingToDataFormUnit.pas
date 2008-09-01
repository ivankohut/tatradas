unit AdvancedChangingToDataFormUnit;

interface

uses
{$IFDEF MSWINDOWS}
  Controls, Forms, StdCtrls, ExtCtrls,
{$ENDIF}
{$IFDEF LINUX}
  QControls, QForms, QStdCtrls, QExtCtrls,
{$ENDIF}
  SysUtils,
  Classes,
  IniFiles,

  procmat,
  TranslatorUnit,
  myedits;

type
  TAdvancedChangingToDataForm = class(TForm, ITranslatable)
    OKButton: TButton;
    CancelButton: TButton;
    DataTypeGroupBox: TGroupBox;
    DataTypeComboBox: TComboBox;
    SignGroupBox: TGroupBox;
    SignedRadioButton: TRadioButton;
    UnsignedRadioButton: TRadioButton;
    OptionsGroupBox: TGroupBox;
    ItemsRadioButton: TRadioButton;
    BytesRadioButton: TRadioButton;
    MaxRadioButton: TRadioButton;
    EndSectionRadioButton: TRadioButton;
    CodeRadioButton: TRadioButton;
    procedure ItemsRadioButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BytesRadioButtonClick(Sender: TObject);
    procedure MaxRadioButtonClick(Sender: TObject);
    procedure EndSectionRadioButtonClick(Sender: TObject);
    procedure CodeRadioButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure UnsignedRadioButtonClick(Sender: TObject);
    procedure SignedRadioButtonClick(Sender: TObject);
    procedure DataTypeComboBoxChange(Sender: TObject);
    procedure ValueChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  public
    MaxAddressHexEdit: THexPositiveEdit;
    BytesBinHexEdit: TDecimalPositiveEdit;
    ItemsBinHexEdit: TDecimalPositiveEdit;
    SelectedEdit: TPositiveEdit;
    Options: TDataChangeOptions;
    procedure Translate;
  end;

var
  AdvancedChangingToDataForm: TAdvancedChangingToDataForm;

implementation

{$R *.dfm}

procedure TAdvancedChangingToDataForm.FormCreate(Sender: TObject);
begin
  ItemsBinHexEdit:= TDecimalPositiveEdit.Create(self);
  ItemsBinHexEdit.Left:=132;
  ItemsBinHexEdit.Top:=24;
//  ItemsBinHexEdit.BaseFormat:=Number;
  ItemsBinHexEdit.Parent:=self.OptionsGroupBox;
  ItemsBinHexEdit.Enabled:=true;
  ItemsBinHexEdit.OnChange:=ValueChange;
  ItemsBinHexEdit.MaxValue:=$FFFFFFFF;
  BytesBinHexEdit := TDecimalPositiveEdit.Create(self); //TPBBinHexEdit.Create(self);
  BytesBinHexEdit.Left:=132;
  BytesBinHexEdit.Top:=56;
//  BytesBinHexEdit.BaseFormat:=Number;
  BytesBinHexEdit.Parent:=self.OptionsGroupBox;
  BytesBinHexEdit.Enabled:=false;
  BytesBinHexEdit.OnChange:=ValueChange;
  BytesBinHexEdit.MaxValue:=$FFFFFFFF;
  MaxAddressHexEdit:=THexPositiveEdit.Create(self); //TPBBinHexEdit.Create(self);
  MaxAddressHexEdit.Left:=132;
  MaxAddressHexEdit.Top:=88;
//  MaxAddressHexEdit.BaseFormat:=HexaDecimal;
  MaxAddressHexEdit.Parent:=self.OptionsGroupBox;
  MaxAddressHexEdit.Enabled:=false;
  MaxAddressHexEdit.OnChange:=ValueChange;

  DataTypeComboBox.ItemIndex:=0;
  DataTypeComboBoxChange(DataTypeComboBox);
  options.datatype:=0; //byte
  options.datatypesize:=1;
  options.signed:=false;
  options.option:=dcItems;
  options.value:=0;
  SelectedEdit:=ItemsBinHexEdit;
end;

procedure TAdvancedChangingToDataForm.ItemsRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled:=false;
  SelectedEdit:=ItemsBinHexEdit;
  SelectedEdit.Enabled:=true;
  SelectedEdit.SetFocus;
  options.option:=dcItems;
end;

procedure TAdvancedChangingToDataForm.BytesRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled:=false;
  SelectedEdit:=BytesBinHexEdit;
  SelectedEdit.Enabled:=true;
  SelectedEdit.SetFocus;
  options.option:=dcBytes;
end;

procedure TAdvancedChangingToDataForm.MaxRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled:=false;
  SelectedEdit:=MaxAddressHexEdit;
  SelectedEdit.Enabled:=true;
  SelectedEdit.SetFocus;
  options.option:=dcMaxAddress;
end;

procedure TAdvancedChangingToDataForm.EndSectionRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled:=false;
  options.option:=dcEndSection;
end;

procedure TAdvancedChangingToDataForm.CodeRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled:=false;
  options.option:=dcCode;
end;

procedure TAdvancedChangingToDataForm.OKButtonClick(Sender: TObject);
begin
  if not UnsignedRadioButton.Enabled then Options.signed:=false;
  ModalResult:=mrOK;
end;

procedure TAdvancedChangingToDataForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TAdvancedChangingToDataForm.UnsignedRadioButtonClick(
  Sender: TObject);
begin
  options.signed:=false;
end;

procedure TAdvancedChangingToDataForm.SignedRadioButtonClick(
  Sender: TObject);
begin
  options.signed:=true;
end;

procedure TAdvancedChangingToDataForm.DataTypeComboBoxChange(
  Sender: TObject);
begin
  options.datatype:=DataTypeComboBox.ItemIndex;
  UnSignedRadioButton.Enabled:=Options.datatype <= dtQword;
  SignedRadioButton.Enabled:=Options.datatype <= dtQword;
//  SignGroupBox.Enabled:=Options.datatype <= dtQword;
end;

procedure TAdvancedChangingToDataForm.ValueChange(Sender: TObject);
var
  MyEdit: TPositiveEdit;
begin
  MyEdit := TPositiveEdit(Sender);
  MyEdit.Change(Sender);
  if MyEdit.Text='' then Exit;
  options.value:=MyEdit.AsCardinal;
end;

procedure TAdvancedChangingToDataForm.FormDestroy(Sender: TObject);
begin
  MaxAddressHexEdit.Destroy;
  BytesBinHexEdit.Destroy;
  ItemsBinHexEdit.Destroy;
end;

procedure TAdvancedChangingToDataForm.Translate;
begin
  Caption:= Translator.TranslateControl('AdvancedChangingToDataForm','Caption');
  OptionsGroupBox.Caption:= Translator.TranslateControl('AdvancedChangingToDataForm','Options');
  ItemsRadioButton.Caption:= Translator.TranslateControl('AdvancedChangingToDataForm','Items');
  BytesRadioButton.Caption:= Translator.TranslateControl('AdvancedChangingToDataForm','Bytes');
  MaxRadioButton.Caption:= Translator.TranslateControl('AdvancedChangingToDataForm','Max');
  EndSectionRadioButton.Caption:= Translator.TranslateControl('AdvancedChangingToDataForm','EndSection');
  CodeRadioButton.Caption:= Translator.TranslateControl('AdvancedChangingToDataForm','Code');
  SignGroupBox.Caption:= Translator.TranslateControl('AdvancedChangingToDataForm','Sign');
  UnsignedRadioButton.Caption:= Translator.TranslateControl('AdvancedChangingToDataForm','Unsigned');
  SignedRadioButton.Caption:= Translator.TranslateControl('AdvancedChangingToDataForm','Signed');
  DataTypeGroupBox.Caption:= Translator.TranslateControl('AdvancedChangingToDataForm','DataType');
  OKButton.Caption:= Translator.TranslateControl('Common','OKButton');
  CancelButton.Caption:= Translator.TranslateControl('Common','CancelButton');
end;

end.
