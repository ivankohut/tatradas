unit SaveOptionsFormUnit;

interface

uses
{$IFDEF MSWINDOWS}
  Controls, Forms, StdCtrls,
{$ENDIF}
{$IFDEF LINUX}
  QControls, QForms, QStdCtrls, 
{$ENDIF}
  SysUtils, Classes, IniFiles,
  procmat;

type
  TSaveOptionsForm = class(TForm)
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
    procedure Translate(ini: TMemINIFile; error: string);
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

procedure TSaveOptionsForm.Translate(ini: TMemINIFile; error: string);
begin
  Caption:=ini.ReadString('SaveOptionsForm','Caption',error);

  MainGroupBox.Caption:=ini.ReadString('SaveOptionsForm','MainGroupBox',error);
  ReferencesGroupBox.Caption:=ini.ReadString('SaveOptionsForm','ReferencesGroupBox',error);
  InstructionGroupBox.Caption:=ini.ReadString('SaveOptionsForm','InstructionGroupBox',error);

  ProjectRadioButton.Caption:=ini.ReadString('SaveOptionsForm','ProjectRadioButton',error);
  DisassemblyRadioButton.Caption:= ini.ReadString('SaveOptionsForm','DisassemblyRadioButton',error);
  CustomRadioButton.Caption:=ini.ReadString('SaveOptionsForm','CustomRadioButton',error);
  NASMRadioButton.Caption:=ini.ReadString('SaveOptionsForm','NASMRadioButton',error);
  AddressCheckBox.Caption:=ini.ReadString('SaveOptionsForm','AddressCheckBox',error);
  ParsedCheckBox.Caption:=ini.ReadString('SaveOptionsForm','ParsedCheckBox',error);
  DisassembledCheckBox.Caption:=ini.ReadString('SaveOptionsForm','DisassembledCheckBox',error);
  JumpCheckBox.Caption:=ini.ReadString('SaveOptionsForm','JumpCheckBox',error);
  CallCheckBox.Caption:=ini.ReadString('SaveOptionsForm','CallCheckBox',error);
  ExportCheckBox.Caption:=ini.ReadString('SaveOptionsForm','ExportCheckBox',error);
  ImportCheckBox.Caption:=ini.ReadString('SaveOptionsForm','ImportCheckBox',error);
  EntryPointCheckBox.Caption:=ini.ReadString('SaveOptionsForm','EntryPointCheckBox',error);

  CancelButton.Caption:=ini.ReadString('Common','CancelButton',error);
end;


end.
