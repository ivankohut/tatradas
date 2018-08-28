unit SaveOptionsFormUnit;

interface

uses
  Controls, Forms, StdCtrls,
  SysUtils,
  Classes,
  IniFiles,
  // project units
  ExceptionsUnit,
  procmat,
  TranslatorUnit;

type
  TSaveOptionsForm = class(TForm, ITranslatable)
    OKButton: TButton;
    CancelButton: TButton;
    ReferencesGroupBox: TGroupBox;
    JumpCheckBox: TCheckBox;
    CallCheckBox: TCheckBox;
    ExportCheckBox: TCheckBox;
    ImportCheckBox: TCheckBox;
    EntryPointCheckBox: TCheckBox;
    InstructionGroupBox: TGroupBox;
    AddressCheckBox: TCheckBox;
    ParsedCheckBox: TCheckBox;
    DisassembledCheckBox: TCheckBox;
    MainGroupBox: TGroupBox;
    CustomRadioButton: TRadioButton;
    DisassemblyRadioButton: TRadioButton;
    NASMRadioButton: TRadioButton;
    procedure ProjectRadioButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure CustomRadioButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    procedure SetExtSettingsEnabled(AValue: Boolean);
    function GetExportCustomDASOptions: TExportCustomDASOptions;
    function GetExportOption: TExportOption;
  public
    procedure Translate;
    property ExportCustomDASOptions: TExportCustomDASOptions read GetExportCustomDASOptions;
    property ExportOption: TExportOption read GetExportOption;
  end;

var
  SaveOptionsForm: TSaveOptionsForm;


implementation


{$R *.lfm}


function TSaveOptionsForm.GetExportCustomDASOptions: TExportCustomDASOptions;
begin
  Result := [];
  if AddressCheckBox.Checked then
    Result := Result + [soAddress];
  if ParsedCheckBox.Checked then
    Result := Result + [soParsed];
  if DisassembledCheckBox.Checked then
    Result := Result + [soDisassembled];
  if JumpCheckBox.Checked then
    Result := Result + [soJump];
  if CallCheckBox.Checked then
    Result := Result + [soCall];
  if ExportCheckBox.Checked then
    Result := Result + [soExport];
  if ImportCheckBox.Checked then
    Result := Result + [soImport];
  if EntryPointCheckBox.Checked then
    Result := Result + [soEntrypoint];
end;



function TSaveOptionsForm.GetExportOption: TExportOption;
begin
  if DisassemblyRadioButton.Checked then
    Result := eoDAS
  else if CustomRadioButton.Checked then
    Result := eoCustomDAS
  else if NASMRadioButton.Checked then
    Result := eoNASM
  else
    raise EIllegalState.Create('TSaveOptionsForm.GetExportOption: No radio button selected');
end;



procedure TSaveOptionsForm.SetExtSettingsEnabled(AValue: Boolean);
begin
  ReferencesGroupBox.Enabled := AValue;
  InstructionGroupBox.Enabled := AValue;
  AddressCheckBox.Enabled := AValue;
  ParsedCheckBox.Enabled := AValue;
  DisassembledCheckBox.Enabled := AValue;
  JumpCheckBox.Enabled := AValue;
  CallCheckBox.Enabled := AValue;
  ExportCheckBox.Enabled := AValue;
  ImportCheckBox.Enabled := AValue;
  EntryPointCheckBox.Enabled := AValue;
end;



procedure TSaveOptionsForm.ProjectRadioButtonClick(Sender: TObject);
begin
  SetExtSettingsEnabled(False);
end;



procedure TSaveOptionsForm.CustomRadioButtonClick(Sender: TObject);
begin
  SetExtSettingsEnabled(True);
end;



procedure TSaveOptionsForm.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;



procedure TSaveOptionsForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;



procedure TSaveOptionsForm.FormShow(Sender: TObject);
begin
  SetExtSettingsEnabled(CustomRadioButton.Checked);
end;



procedure TSaveOptionsForm.Translate;
begin
  Caption := Translator.TranslateControl('SaveOptionsForm', 'Caption');

  MainGroupBox.Caption := Translator.TranslateControl('SaveOptionsForm', 'MainGroupBox');
  ReferencesGroupBox.Caption := Translator.TranslateControl('SaveOptionsForm', 'ReferencesGroupBox');
  InstructionGroupBox.Caption := Translator.TranslateControl('SaveOptionsForm', 'InstructionGroupBox');

  DisassemblyRadioButton.Caption := Translator.TranslateControl('SaveOptionsForm', 'DisassemblyRadioButton');
  CustomRadioButton.Caption := Translator.TranslateControl('SaveOptionsForm', 'CustomRadioButton');
  NASMRadioButton.Caption := Translator.TranslateControl('SaveOptionsForm', 'NASMRadioButton');
  AddressCheckBox.Caption := Translator.TranslateControl('SaveOptionsForm', 'AddressCheckBox');
  ParsedCheckBox.Caption := Translator.TranslateControl('SaveOptionsForm', 'ParsedCheckBox');
  DisassembledCheckBox.Caption := Translator.TranslateControl('SaveOptionsForm', 'DisassembledCheckBox');
  JumpCheckBox.Caption := Translator.TranslateControl('SaveOptionsForm', 'JumpCheckBox');
  CallCheckBox.Caption := Translator.TranslateControl('SaveOptionsForm', 'CallCheckBox');
  ExportCheckBox.Caption := Translator.TranslateControl('SaveOptionsForm', 'ExportCheckBox');
  ImportCheckBox.Caption := Translator.TranslateControl('SaveOptionsForm', 'ImportCheckBox');
  EntryPointCheckBox.Caption := Translator.TranslateControl('SaveOptionsForm', 'EntryPointCheckBox');

  CancelButton.Caption := Translator.TranslateControl('Common', 'CancelButton');
end;


end.
