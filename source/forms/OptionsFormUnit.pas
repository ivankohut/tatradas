unit OptionsFormUnit;

interface

uses
  Controls, Forms, Dialogs, StdCtrls, ComCtrls,
  SysUtils,
  Classes,
  INIFiles,
  // project units
  procmat,
  TranslatorUnit;

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
    Options: TTatraDASOptions;
  public
    //procedure Translate;
    procedure SaveSettings(ini: TMemINIFile);
    procedure LoadSettings(ini: TMemINIFile);
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.lfm}

procedure TOptionsForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;



procedure TOptionsForm.OKButtonClick(Sender: TObject);
begin
  Options.autoformat := AutomaticRadioButton.Checked;
  Options.procdetect := DisasmCheckBox.Checked;
  Options.stringdetect := DetectStringCheckBox.Checked;
  Options.removejump := RemoveJumpRefCheckBox.Checked;
  Options.removeimport := RemoveImportRefCheckBox.Checked;
  Options.removeexport := RemoveExportRefCheckBox.Checked;

  ModalResult := mrOK;
end;



procedure TOptionsForm.FormShow(Sender: TObject);
begin
  AutomaticRadioButton.Checked := Options.autoformat;
  DisasmCheckBox.Checked := Options.procdetect;
  DetectStringCheckBox.Checked := Options.stringdetect;
  RemoveJumpRefCheckBox.Checked := Options.removejump;
  RemoveImportRefCheckBox.Checked := Options.removeimport;
  RemoveExportRefCheckBox.Checked := Options.removeexport;
end;



procedure TOptionsForm.SaveSettings(ini: TMemINIFile);
begin
  ini.WriteBool('Options', 'AutoFormat', Options.autoformat);
  ini.WriteBool('Options', 'DetectProcedures', Options.procdetect);
  ini.WriteBool('Options', 'DetectStrings', Options.stringdetect);
  ini.WriteBool('Options', 'RemoveJumpRefs', Options.removejump);
  ini.WriteBool('Options', 'RemoveImportRefs', Options.removeimport);
  ini.WriteBool('Options', 'RemoveExportRefs', Options.removeexport);
end;



procedure TOptionsForm.LoadSettings(ini: TMemINIFile);
begin
  Options.autoformat := ini.ReadBool('Options', 'AutoFormat', True);
  Options.procdetect := ini.ReadBool('Options', 'DetectProcedures', True);
  Options.stringdetect := ini.ReadBool('Options', 'DetectStrings', False);
  Options.removejump := ini.ReadBool('Options', 'RemoveJumpRefs', True);
  Options.removeimport := ini.ReadBool('Options', 'RemoveImportRefs', True);
  Options.removeexport := ini.ReadBool('Options', 'RemoveExportRefs', True);
end;



//procedure TOptionsForm.Translate;
//begin
  //Caption := Translator.TranslateControl('OptionsForm', 'Caption');
  //OpeningGroupBox.Caption := Translator.TranslateControl('OptionsForm', 'OpeningGroupBox');
  //DisassemblerGroupBox.Caption := Translator.TranslateControl('OptionsForm', 'DisassemblerGroupBox');
  //ToDataChangeGroupBox.Caption := Translator.TranslateControl('OptionsForm', 'ToDataChangeGroupBox');
  //
  //UserRadioButton.Caption := Translator.TranslateControl('OptionsForm', 'UserRadioButton');
  //AutomaticRadioButton.Caption := Translator.TranslateControl('OptionsForm', 'AutomaticRadioButton');
  //
  //DisasmCheckBox.Caption := Translator.TranslateControl('OptionsForm', 'DisasmCheckBox');
  //DetectStringCheckBox.Caption := Translator.TranslateControl('OptionsForm', 'DetectStringCheckBox');
  //
  //RemoveJumpRefCheckBox.Caption := Translator.TranslateControl('OptionsForm', 'RemoveJumpRefCheckBox');
  //RemoveImportRefCheckBox.Caption := Translator.TranslateControl('OptionsForm', 'RemoveImportRefCheckBox');
  //RemoveExportRefCheckBox.Caption := Translator.TranslateControl('OptionsForm', 'RemoveExportRefCheckBox');
  //
  //OKButton.Caption := Translator.TranslateControl('Common', 'OKButton');
  //CancelButton.Caption := Translator.TranslateControl('Common', 'CancelButton');
//end;


end.
