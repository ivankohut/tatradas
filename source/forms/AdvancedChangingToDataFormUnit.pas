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
    procedure DataTypeComboBoxChange(Sender: TObject);
    procedure ValueChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    MaxAddressHexEdit: THexPositiveEdit;
    BytesBinHexEdit: TDecimalPositiveEdit;
    ItemsBinHexEdit: TDecimalPositiveEdit;
    SelectedEdit: TPositiveEdit;
    function GetOptions: TDataChangeOptions;
  public
    procedure Translate;
    procedure SetMaxAdressMaxValue(AValue: Cardinal);
    property Options: TDataChangeOptions read GetOptions;
  end;

var
  AdvancedChangingToDataForm: TAdvancedChangingToDataForm;

  
implementation


{$R *.dfm}


procedure TAdvancedChangingToDataForm.SetMaxAdressMaxValue(AValue: Cardinal);
begin
  MaxAddressHexEdit.MaxValue := AValue;
end;



function TAdvancedChangingToDataForm.GetOptions: TDataChangeOptions;
begin
  if ItemsRadioButton.Checked then
    Result.Option := dcItems
  else if BytesRadioButton.Checked then
    Result.Option := dcBytes
  else if MaxRadioButton.Checked then
    Result.Option := dcMaxAddress
  else if EndSectionRadioButton.Checked then
    Result.Option := dcEndSection
  else if CodeRadioButton.Checked then
    Result.Option := dcCode
  else
    raise EIllegalState.Create('TAdvancedChangingToDataForm.GetOptions: No RadioButton selected');

  Result.DataType := DataTypeComboBox.ItemIndex;
  Result.Signed := SignedRadioButton.Checked;
  Result.Value := SelectedEdit.AsCardinal;
  Result.DatatypeSize := 1; // ???
end;



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

  DataTypeComboBox.ItemIndex := 0;
  DataTypeComboBoxChange(DataTypeComboBox);
  UnsignedRadioButton.Checked := True;
  ItemsRadioButton.Checked := True;
  SelectedEdit := ItemsBinHexEdit;
  SelectedEdit.AsCardinal := 0;
end;



procedure TAdvancedChangingToDataForm.ItemsRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled := False;
  SelectedEdit := ItemsBinHexEdit;
  SelectedEdit.Enabled := True;
  SelectedEdit.SetFocus;
end;



procedure TAdvancedChangingToDataForm.BytesRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled := False;
  SelectedEdit := BytesBinHexEdit;
  SelectedEdit.Enabled := True;
  SelectedEdit.SetFocus;
end;



procedure TAdvancedChangingToDataForm.MaxRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled := False;
  SelectedEdit := MaxAddressHexEdit;
  SelectedEdit.Enabled := True;
  SelectedEdit.SetFocus;
end;



procedure TAdvancedChangingToDataForm.EndSectionRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled := False;
end;



procedure TAdvancedChangingToDataForm.CodeRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled := False;
end;



procedure TAdvancedChangingToDataForm.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;



procedure TAdvancedChangingToDataForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;



procedure TAdvancedChangingToDataForm.DataTypeComboBoxChange(Sender: TObject);
var
  SignGroupBoxEnabled: Boolean;
begin
  SignGroupBoxEnabled := (DataTypeComboBox.ItemIndex <= dtQword);
  UnSignedRadioButton.Enabled := SignGroupBoxEnabled;
  SignedRadioButton.Enabled := SignGroupBoxEnabled;
end;



procedure TAdvancedChangingToDataForm.ValueChange(Sender: TObject);
begin
  TPositiveEdit(Sender).Change(Sender);
end;



procedure TAdvancedChangingToDataForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MaxAddressHexEdit);
  FreeAndNil(BytesBinHexEdit);
  FreeAndNil(ItemsBinHexEdit);
end;



procedure TAdvancedChangingToDataForm.Translate;
begin
  Caption := Translator.TranslateControl('AdvancedChangingToDataForm', 'Caption');
  OptionsGroupBox.Caption := Translator.TranslateControl('AdvancedChangingToDataForm', 'Options');
  ItemsRadioButton.Caption := Translator.TranslateControl('AdvancedChangingToDataForm', 'Items');
  BytesRadioButton.Caption := Translator.TranslateControl('AdvancedChangingToDataForm', 'Bytes');
  MaxRadioButton.Caption := Translator.TranslateControl('AdvancedChangingToDataForm', 'Max');
  EndSectionRadioButton.Caption := Translator.TranslateControl('AdvancedChangingToDataForm', 'EndSection');
  CodeRadioButton.Caption := Translator.TranslateControl('AdvancedChangingToDataForm', 'Code');
  SignGroupBox.Caption := Translator.TranslateControl('AdvancedChangingToDataForm', 'Sign');
  UnsignedRadioButton.Caption := Translator.TranslateControl('AdvancedChangingToDataForm', 'Unsigned');
  SignedRadioButton.Caption := Translator.TranslateControl('AdvancedChangingToDataForm', 'Signed');
  DataTypeGroupBox.Caption := Translator.TranslateControl('AdvancedChangingToDataForm', 'DataType');
  OKButton.Caption := Translator.TranslateControl('Common', 'OKButton');
  CancelButton.Caption := Translator.TranslateControl('Common', 'CancelButton');
end;



end.
