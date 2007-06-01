unit OptionsFormUnit;

interface

uses
{$IFDEF MSWINDOWS}
  Controls, Forms, Dialogs, StdCtrls, ComCtrls,
{$ENDIF}
{$IFDEF LINUX}
  QControls, QForms, QDialogs, QStdCtrls, QComCtrls,
{$ENDIF}
  SysUtils, Classes, INIFiles,
  procmat;

type
  TOptionsForm = class(TForm)
    DisassemblerGroupBox: TGroupBox;
    DisasmCheckBox: TCheckBox;
    DetectStringCheckBox: TCheckBox;
    OpeningGroupBox: TGroupBox;
    UserRadioButton: TRadioButton;
    AutomaticRadioButton: TRadioButton;
    ToDataChangeGroupBox: TGroupBox;
    RemoveJumpRefCheckBox: TCheckBox;
    RemoveImportRefCheckBox: TCheckBox;
    OKButton: TButton;
    CancelButton: TButton;
    RemoveExportRefCheckBox: TCheckBox;
    Label1: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    Options: TTatraDASOptions;
    procedure Translate(ini: TMemINIFile; error: string);
    procedure SaveSettings(ini: TMemINIFile);
    procedure LoadSettings(ini: TMemINIFile);

    { Public declarations }
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.dfm}

procedure TOptionsForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TOptionsForm.OKButtonClick(Sender: TObject);
begin
  Options.autoformat:=AutomaticRadioButton.Checked;
  Options.procdetect:=DisasmCheckBox.Checked;
  Options.stringdetect:=DetectStringCheckBox.Checked;
  Options.removejump:=RemoveJumpRefCheckBox.Checked;
  Options.removeimport:=RemoveImportRefCheckBox.Checked;
  Options.removeexport:=RemoveExportRefCheckBox.Checked;

  ModalResult:=mrOK;
end;


procedure TOptionsForm.FormShow(Sender: TObject);
begin
  AutomaticRadioButton.Checked:=Options.autoformat;
  DisasmCheckBox.Checked:=Options.procdetect;
  DetectStringCheckBox.Checked:=Options.stringdetect;
  RemoveJumpRefCheckBox.Checked:=Options.removejump;
  RemoveImportRefCheckBox.Checked:=Options.removeimport;
  RemoveExportRefCheckBox.Checked:=Options.removeexport;
end;


procedure TOptionsForm.SaveSettings(ini: TMemINIFile);
begin
  ini.WriteBool('Options','AutoFormat',Options.autoformat);
  ini.WriteBool('Options','DetectProcedures',Options.procdetect);
  ini.WriteBool('Options','DetectStrings',Options.stringdetect);
  ini.WriteBool('Options','RemoveJumpRefs',Options.removejump);
  ini.WriteBool('Options','RemoveImportRefs',Options.removeimport);
  ini.WriteBool('Options','RemoveExportRefs',Options.removeexport);
end;

procedure TOptionsForm.LoadSettings(ini: TMemINIFile);
begin
  Options.autoformat:=ini.ReadBool('Options','AutoFormat',true);
  Options.procdetect:=ini.ReadBool('Options','DetectProcedures',true);
  Options.stringdetect:=ini.ReadBool('Options','DetectStrings',false);
  Options.removejump:=ini.ReadBool('Options','RemoveJumpRefs',true);
  Options.removeimport:=ini.ReadBool('Options','RemoveImportRefs',true);
  Options.removeexport:=ini.ReadBool('Options','RemoveExportRefs',true);
end;

procedure TOptionsForm.Translate(ini: TMemINIFile; error: string);
begin
  Caption:=ini.ReadString('OptionsForm','Caption',error);
  OpeningGroupBox.Caption:=ini.ReadString('OptionsForm','OpeningGroupBox',error);
  DisassemblerGroupBox.Caption:=ini.ReadString('OptionsForm','DisassemblerGroupBox',error);
  ToDataChangeGroupBox.Caption:=ini.ReadString('OptionsForm','ToDataChangeGroupBox',error);

  UserRadioButton.Caption:=ini.ReadString('OptionsForm','UserRadioButton',error);
  AutomaticRadioButton.Caption:=ini.ReadString('OptionsForm','AutomaticRadioButton',error);

  DisasmCheckBox.Caption:=ini.ReadString('OptionsForm','DisasmCheckBox',error);
  DetectStringCheckBox.Caption:=ini.ReadString('OptionsForm','DetectStringCheckBox',error);

  RemoveJumpRefCheckBox.Caption:=ini.ReadString('OptionsForm','RemoveJumpRefCheckBox',error);
  RemoveImportRefCheckBox.Caption:=ini.ReadString('OptionsForm','RemoveImportRefCheckBox',error);
  RemoveExportRefCheckBox.Caption:=ini.ReadString('OptionsForm','RemoveExportRefCheckBox',error);

  OKButton.Caption:=ini.ReadString('Common','OKButton',error);
  CancelButton.Caption:=ini.ReadString('Common','CancelButton',error);
end;


end.
