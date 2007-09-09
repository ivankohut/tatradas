unit SaveOptionsFormUnit;

interface

uses
{$IFDEF MSWINDOWS}
  Controls, Forms, StdCtrls,
{$ENDIF}
{$IFDEF LINUX}
  QControls, QForms, QStdCtrls, 
{$ENDIF}
  SysUtils,
  Classes,
  IniFiles,

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
    ProjectRadioButton: TRadioButton;
    CustomRadioButton: TRadioButton;
    DisassemblyRadioButton: TRadioButton;
    NASMRadioButton: TRadioButton;
    procedure ProjectRadioButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure CustomRadioButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    SaveOptions: TSaveOptions;
    procedure Translate;
    { Public declarations }
  end;

var
  SaveOptionsForm: TSaveOptionsForm;

implementation

{$R *.dfm}

procedure TSaveOptionsForm.ProjectRadioButtonClick(Sender: TObject);
begin
  ReferencesGroupBox.Enabled:=false;
  InstructionGroupBox.Enabled:=false;
  AddressCheckBox.Enabled:=false;
  ParsedCheckBox.Enabled:=false;
  DisassembledCheckBox.Enabled:=false;
  JumpCheckBox.Enabled:=false;
  CallCheckBox.Enabled:=false;
  ExportCheckBox.Enabled:=false;
  ImportCheckBox.Enabled:=false;
  EntryPointCheckBox.Enabled:=false;
end;

procedure TSaveOptionsForm.OKButtonClick(Sender: TObject);
begin
  SaveOptions:=[];
  if ProjectRadioButton.Checked then SaveOptions:=[soProject]
  else if DisassemblyRadioButton.Checked then SaveOptions:=[soDisassembly]
  else if NASMRadioButton.Checked then SaveOptions:=[soNASM]
  else begin
    if AddressCheckBox.Checked then SaveOptions:=SaveOptions + [soAddress];
    if ParsedCheckBox.Checked then SaveOptions:=SaveOptions + [soParsed];
    if DisassembledCheckBox.Checked then SaveOptions:=SaveOptions + [soDisassembled];
    if JumpCheckBox.Checked then SaveOptions:=SaveOptions + [soJump];
    if CallCheckBox.Checked then SaveOptions:=SaveOptions + [soCall];
    if ExportCheckBox.Checked then SaveOptions:=SaveOptions + [soExport];
    if ImportCheckBox.Checked then SaveOptions:=SaveOptions + [soImport];
    if EntryPointCheckBox.Checked then SaveOptions:=SaveOptions + [soEntrypoint];
  end;
  ModalResult:=mrOK;
end;

procedure TSaveOptionsForm.CustomRadioButtonClick(Sender: TObject);
begin
  ReferencesGroupBox.Enabled:=true;
  InstructionGroupBox.Enabled:=true;
  AddressCheckBox.Enabled:=true;
  ParsedCheckBox.Enabled:=true;
  DisassembledCheckBox.Enabled:=true;
  JumpCheckBox.Enabled:=true;
  CallCheckBox.Enabled:=true;
  ExportCheckBox.Enabled:=true;
  ImportCheckBox.Enabled:=true;
  EntryPointCheckBox.Enabled:=true;
end;

procedure TSaveOptionsForm.FormShow(Sender: TObject);
begin
  if CustomRadioButton.Checked then CustomRadioButtonClick(nil)
  else ProjectRadioButtonClick(nil);
end;

procedure TSaveOptionsForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TSaveOptionsForm.Translate;
begin
  Caption:= Translator.TranslateControl('SaveOptionsForm','Caption');

  MainGroupBox.Caption:= Translator.TranslateControl('SaveOptionsForm','MainGroupBox');
  ReferencesGroupBox.Caption:= Translator.TranslateControl('SaveOptionsForm','ReferencesGroupBox');
  InstructionGroupBox.Caption:= Translator.TranslateControl('SaveOptionsForm','InstructionGroupBox');

  ProjectRadioButton.Caption:= Translator.TranslateControl('SaveOptionsForm','ProjectRadioButton');
  DisassemblyRadioButton.Caption:= Translator.TranslateControl('SaveOptionsForm','DisassemblyRadioButton');
  CustomRadioButton.Caption:= Translator.TranslateControl('SaveOptionsForm','CustomRadioButton');
  NASMRadioButton.Caption:= Translator.TranslateControl('SaveOptionsForm','NASMRadioButton');
  AddressCheckBox.Caption:= Translator.TranslateControl('SaveOptionsForm','AddressCheckBox');
  ParsedCheckBox.Caption:= Translator.TranslateControl('SaveOptionsForm','ParsedCheckBox');
  DisassembledCheckBox.Caption:= Translator.TranslateControl('SaveOptionsForm','DisassembledCheckBox');
  JumpCheckBox.Caption:= Translator.TranslateControl('SaveOptionsForm','JumpCheckBox');
  CallCheckBox.Caption:= Translator.TranslateControl('SaveOptionsForm','CallCheckBox');
  ExportCheckBox.Caption:= Translator.TranslateControl('SaveOptionsForm','ExportCheckBox');
  ImportCheckBox.Caption:= Translator.TranslateControl('SaveOptionsForm','ImportCheckBox');
  EntryPointCheckBox.Caption:= Translator.TranslateControl('SaveOptionsForm','EntryPointCheckBox');

  CancelButton.Caption:= Translator.TranslateControl('Common','CancelButton');
end;


end.
