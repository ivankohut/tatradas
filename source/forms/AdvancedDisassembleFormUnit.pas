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
  TatraDASFormUnit;

type
  TAdvancedDisassembleForm = class(TTatraDASForm)
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
    procedure RecursiveCheckBoxClick(Sender: TObject);
    procedure bit16RadiobuttonClick(Sender: TObject);
    procedure bit32RadiobuttonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    BytesBinHexEdit: TMyNumEdit;//TPBBinHexEdit;
    MaxAddressBinHexEdit: TMyHexEdit;//TPBBinHexEdit;
    Options: TDisassembleFormOptions;
    procedure Translate(ini: TMemINIFile); override;
  private
    changing: boolean; // true, ak prebieha spracovanie OnChange nejakeho editu
  end;

var
  AdvancedDisassembleForm: TAdvancedDisassembleForm;
  SelectedEdit: TCustomEdit;

implementation

{$R *.dfm}

procedure TAdvancedDisassembleForm.OKButtonClick(Sender: TObject);
begin
  ModalResult:=mrOK
end;

procedure TAdvancedDisassembleForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TAdvancedDisassembleForm.BytesRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled:=false;
  SelectedEdit:=BytesBinHexEdit;
  SelectedEdit.Enabled:=true;
  SelectedEdit.SetFocus;
  Options.option:=dtBytes;
end;

procedure TAdvancedDisassembleForm.MaxRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled:=false;
  SelectedEdit:=MaxAddressBinHexEdit;
  SelectedEdit.Enabled:=true;
  SelectedEdit.SetFocus;
  Options.option:=dtMaxAddress;
end;

procedure TAdvancedDisassembleForm.NormalRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled:=false;
  Options.option:=dtNormal;
end;

procedure TAdvancedDisassembleForm.FormCreate(Sender: TObject);
begin
  BytesBinHexEdit:=TMyNumEdit.Create(self);//TPBBinHexEdit.Create(self);
  BytesBinHexEdit.Left:= 128;
  BytesBinHexEdit.Top:= 24;
//  BytesBinHexEdit.BaseFormat:=Number;
  BytesBinHexEdit.Parent:= self.OptionsGroupBox;
  BytesBinHexEdit.Enabled:= false;
  BytesBinHexEdit.OnChange:= ValueChange;
  BytesBinHexEdit.MaxValue:= $FFFFFFFF;

  MaxAddressBinHexEdit:= TMyHexEdit.Create(self);//TPBBinHexEdit.Create(self);
  MaxAddressBinHexEdit.Left:= 128;
  MaxAddressBinHexEdit.Top:= 56;
//  MaxAddressBinHexEdit.BaseFormat:=HexaDecimal;
  MaxAddressBinHexEdit.Parent:=self.OptionsGroupBox;
  MaxAddressBinHexEdit.Enabled:=false;
  MaxAddressBinHexEdit.OnChange:=ValueChange;

  SelectedEdit:=BytesBinHexEdit;

  Options.Recursive:= true;
  Options.Bit32:= true;
  Options.Option:= dtBytes;
  Options.Value:= 0;
end;

procedure TAdvancedDisassembleForm.ValueChange(Sender: TObject);
var MyEdit: TMyEdit;
begin
  if changing then exit else changing:=true;
  MyEdit:=TMyEdit(Sender);
  MyEdit.Change(Sender);
  if MyEdit.Text='' then Exit;
  options.value:=MyEdit.AsCardinal;
  changing:=false;
end;

procedure TAdvancedDisassembleForm.Translate(ini: TMemINIFile);
begin
  Caption:=ini.ReadString('AdvancedDisassembleForm','Caption',TranslateErrorStr);
  OptionsGroupBox.Caption:= ini.ReadString('AdvancedDisassembleForm','Options',TranslateErrorStr);
  BytesRadioButton.Caption:=ini.ReadString('AdvancedDisassembleForm','Bytes',TranslateErrorStr);
  MaxRadioButton.Caption:=ini.ReadString('AdvancedDisassembleForm','Max',TranslateErrorStr);
  NormalRadioButton.Caption:=ini.ReadString('AdvancedDisassembleForm','Normal',TranslateErrorStr);
  Bit1632GroupBox.Caption:=ini.ReadString('AdvancedDisassembleForm','Bit1632GroupBox',TranslateErrorStr);
//  bit16Radiobutton.Caption:=ini.ReadString('AdvancedDisassembleForm','',TranslateErrorStr);
//  bit32Radiobutton.Caption:=ini.ReadString('AdvancedDisassembleForm','',TranslateErrorStr);
  RecursiveCheckBox.Caption:=ini.ReadString('AdvancedDisassembleForm','Recursive',TranslateErrorStr);

  OKButton.Caption:=ini.ReadString('Common','OKButton',TranslateErrorStr);
  CancelButton.Caption:=ini.ReadString('Common','CancelButton',TranslateErrorStr);
end;

procedure TAdvancedDisassembleForm.RecursiveCheckBoxClick(Sender: TObject);
begin
  Options.recursive:=RecursiveCheckbox.Checked;
end;

procedure TAdvancedDisassembleForm.bit16RadiobuttonClick(Sender: TObject);
begin
  Options.bit32:=false;
end;

procedure TAdvancedDisassembleForm.bit32RadiobuttonClick(Sender: TObject);
begin
  Options.bit32:=true;
end;

procedure TAdvancedDisassembleForm.FormShow(Sender: TObject);
begin
  SelectedEdit.Enabled:=true;
  SelectedEdit.SetFocus;
end;

end.
