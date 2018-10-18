unit AdvancedDisassembleFormUnit;

interface

uses
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  SysUtils,
  Classes,
  IniFiles,
  // project units
  myedits,
  ExceptionsUnit,
  procmat,
  Translatables;

type

  { TAdvancedDisassembleForm }

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
    PanelForBytesCountEdit: TPanel;
    PanelForMaxAddressEdit: TPanel;
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure BytesRadioButtonClick(Sender: TObject);
    procedure MaxRadioButtonClick(Sender: TObject);
    procedure NormalRadioButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    fSelectedEdit: TPositiveEdit;
    BytesBinHexEdit: TDecimalPositiveEdit;
    MaxAddressBinHexEdit: THexPositiveEdit;
    function GetOptions: TDisassembleFormOptions;
    procedure SetSelectedEdit(AEdit: TPositiveEdit);
  public
    property Options: TDisassembleFormOptions read GetOptions;
    function Translatable: TTranslatable;
    procedure SetMaxAddressMaxValue(AValue: Cardinal);
  end;

var
  AdvancedDisassembleForm: TAdvancedDisassembleForm;


implementation


{$R *.lfm}


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
  Result.Value := fSelectedEdit.AsCardinal;
end;



procedure TAdvancedDisassembleForm.SetSelectedEdit(AEdit: TPositiveEdit);
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



procedure TAdvancedDisassembleForm.BytesRadioButtonClick(Sender: TObject);
begin
  SetSelectedEdit(BytesBinHexEdit);
end;



procedure TAdvancedDisassembleForm.MaxRadioButtonClick(Sender: TObject);
begin
  SetSelectedEdit(MaxAddressBinHexEdit);
end;



procedure TAdvancedDisassembleForm.NormalRadioButtonClick(Sender: TObject);
begin
  SetSelectedEdit(nil);
end;



procedure TAdvancedDisassembleForm.FormCreate(Sender: TObject);
begin
  BytesBinHexEdit := TDecimalPositiveEdit.Create(self);
  BytesBinHexEdit.Parent := PanelForBytesCountEdit;
  BytesBinHexEdit.Align := alClient;
  BytesBinHexEdit.Enabled := False;
  BytesBinHexEdit.MaxValue := $FFFFFFFF;

  MaxAddressBinHexEdit := THexPositiveEdit.Create(self);
  MaxAddressBinHexEdit.Parent := PanelForMaxAddressEdit;
  MaxAddressBinHexEdit.Align := alClient;
  MaxAddressBinHexEdit.Enabled := False;

  BytesBinHexEdit.AsCardinal := 0;
  SetSelectedEdit(BytesBinHexEdit);

  RecursiveCheckBox.Checked := True;
  bit32Radiobutton.Checked := True;
  BytesRadioButton.Checked := True;
end;



procedure TAdvancedDisassembleForm.SetMaxAddressMaxValue(AValue: Cardinal);
begin
  MaxAddressBinHexEdit.MaxValue := AValue;
end;



function TAdvancedDisassembleForm.Translatable: TTranslatable;
begin
  Result := TTranslatableSimpleForm.Create(
    self,
    'AdvancedDisassembleForm',
    [
      TTranslatableCaption.Create(OptionsGroupBox, 'Options'),
      TTranslatableCaption.Create(BytesRadioButton, 'Bytes'),
      TTranslatableCaption.Create(MaxRadioButton, 'Max'),
      TTranslatableCaption.Create(NormalRadioButton, 'Normal'),
      TTranslatableCaption.Create(Bit1632GroupBox, 'Bit1632GroupBox'),
      TTranslatableCaption.Create(RecursiveCheckBox, 'Recursive')
    ],
    [OKButton, CancelButton]
  );
end;



procedure TAdvancedDisassembleForm.FormShow(Sender: TObject);
begin
  if fSelectedEdit.Enabled then
    fSelectedEdit.SetFocus;
end;



end.
