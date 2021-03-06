unit AdvancedChangingToDataFormUnit;

interface

uses
  Controls, Forms, StdCtrls, ExtCtrls,
  SysUtils,
  Classes,
  IniFiles,
  // project units
  ExceptionsUnit,
  procmat,
  Translatables,
  myedits;

type

  { TAdvancedChangingToDataForm }

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
    PanelForItemsCountEdit: TPanel;
    PanelForBytesCountEdit: TPanel;
    PanelForMaxAddressEdit: TPanel;
    procedure ItemsRadioButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BytesRadioButtonClick(Sender: TObject);
    procedure MaxRadioButtonClick(Sender: TObject);
    procedure EndSectionRadioButtonClick(Sender: TObject);
    procedure CodeRadioButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure DataTypeComboBoxChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  private
    MaxAddressHexEdit: THexPositiveEdit;
    BytesBinHexEdit: TDecimalPositiveEdit;
    ItemsBinHexEdit: TDecimalPositiveEdit;
    fSelectedEdit: TPositiveEdit;
    fMaxAddressMinValue: Cardinal;
    function GetOptions: TDataChangeOptions;
    procedure SetSelectedEdit(AEdit: TPositiveEdit);
  public
    function Translatable: TTranslatable;
    procedure SetMaxAdressMaxValue(AValue: Cardinal);
    procedure SetMaxAdressMinValue(AValue: Cardinal);
    property Options: TDataChangeOptions read GetOptions;
  end;

var
  AdvancedChangingToDataForm: TAdvancedChangingToDataForm;


implementation


{$R *.lfm}


procedure TAdvancedChangingToDataForm.SetMaxAdressMaxValue(AValue: Cardinal);
begin
  MaxAddressHexEdit.MaxValue := AValue;
end;



procedure TAdvancedChangingToDataForm.SetMaxAdressMinValue(AValue: Cardinal);
begin
  fMaxAddressMinValue := AValue;
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
  Result.Value := fSelectedEdit.AsCardinal;
end;



procedure TAdvancedChangingToDataForm.FormCreate(Sender: TObject);
begin
  // Create ItemsCountEdit
  ItemsBinHexEdit := TDecimalPositiveEdit.Create(self);
  ItemsBinHexEdit.Parent := PanelForItemsCountEdit;
  ItemsBinHexEdit.Align := alClient;
  ItemsBinHexEdit.Enabled := False;
  ItemsBinHexEdit.MaxValue := $FFFFFFFF;

  // Create BytesCountEdit
  BytesBinHexEdit := TDecimalPositiveEdit.Create(self);
  BytesBinHexEdit.Parent := PanelForBytesCountEdit;
  BytesBinHexEdit.Align := alClient;
  BytesBinHexEdit.Enabled := False;
  BytesBinHexEdit.MaxValue := $FFFFFFFF;

  // Create MaxAddressEdit
  MaxAddressHexEdit := THexPositiveEdit.Create(self);
  MaxAddressHexEdit.Parent := PanelForMaxAddressEdit;
  MaxAddressHexEdit.Align := alClient;
  MaxAddressHexEdit.Enabled := False;

  // Set default values
  DataTypeComboBox.ItemIndex := 0;
  DataTypeComboBoxChange(DataTypeComboBox);
  UnsignedRadioButton.Checked := True;
  ItemsRadioButton.Checked := True;
  SetSelectedEdit(ItemsBinHexEdit);
end;



procedure TAdvancedChangingToDataForm.SetSelectedEdit(AEdit: TPositiveEdit);
begin
  if fSelectedEdit <> nil then
    fSelectedEdit.Enabled := False;
  if AEdit <> nil then begin
    fSelectedEdit := AEdit;
    fSelectedEdit.Enabled := True;
    if (self.Visible and self.Enabled) then
      fSelectedEdit.SetFocus;
  end;
end;



procedure TAdvancedChangingToDataForm.ItemsRadioButtonClick(Sender: TObject);
begin
  SetSelectedEdit(ItemsBinHexEdit);
end;



procedure TAdvancedChangingToDataForm.BytesRadioButtonClick(Sender: TObject);
begin
  SetSelectedEdit(BytesBinHexEdit);
end;



procedure TAdvancedChangingToDataForm.MaxRadioButtonClick(Sender: TObject);
begin
  SetSelectedEdit(MaxAddressHexEdit);
end;



procedure TAdvancedChangingToDataForm.EndSectionRadioButtonClick(Sender: TObject);
begin
  SetSelectedEdit(nil);
end;



procedure TAdvancedChangingToDataForm.CodeRadioButtonClick(Sender: TObject);
begin
  SetSelectedEdit(nil);
end;



procedure TAdvancedChangingToDataForm.OKButtonClick(Sender: TObject);
begin
  if (not MaxRadioButton.Checked) or (MaxAddressHexEdit.AsCardinal >= fMaxAddressMinValue) then
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



procedure TAdvancedChangingToDataForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(MaxAddressHexEdit);
  FreeAndNil(BytesBinHexEdit);
  FreeAndNil(ItemsBinHexEdit);
end;



function TAdvancedChangingToDataForm.Translatable: TTranslatable;
begin
  Result := TTranslatableSimpleForm.Create(
    self,
    'AdvancedChangingToDataForm',
    [
      TTranslatableCaption.Create(OptionsGroupBox, 'Options'),
      TTranslatableCaption.Create(ItemsRadioButton, 'Items'),
      TTranslatableCaption.Create(BytesRadioButton, 'Bytes'),
      TTranslatableCaption.Create(MaxRadioButton, 'Max'),
      TTranslatableCaption.Create(EndSectionRadioButton, 'EndSection'),
      TTranslatableCaption.Create(CodeRadioButton, 'Code'),
      TTranslatableCaption.Create(SignGroupBox, 'Sign'),
      TTranslatableCaption.Create(UnsignedRadioButton, 'Unsigned'),
      TTranslatableCaption.Create(SignedRadioButton, 'Signed'),
      TTranslatableCaption.Create(DataTypeGroupBox, 'DataType')
    ],
    [OKButton, CancelButton]
  );
end;



end.
