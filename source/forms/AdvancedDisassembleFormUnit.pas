unit AdvancedDisassembleFormUnit;

interface

uses
{$IFDEF MSWINDOWS}
  Controls, Forms, Dialogs, StdCtrls,
{$ENDIF}
{$IFDEF LINUX}
  QControls, QForms, QStdCtrls, QExtCtrls,
{$ENDIF}
  SysUtils, Classes, myedits, INIFiles,
  procmat;

type
  TAdvancedDisassembleForm = class(TForm)
    OptionsGroupBox: TGroupBox;
    ItemsRadioButton: TRadioButton;
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
    procedure ItemsRadioButtonClick(Sender: TObject);
    procedure BytesRadioButtonClick(Sender: TObject);
    procedure MaxRadioButtonClick(Sender: TObject);
    procedure NormalRadioButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ValueChange(Sender: TObject);
    procedure RecursiveCheckBoxClick(Sender: TObject);
    procedure bit16RadiobuttonClick(Sender: TObject);
    procedure bit32RadiobuttonClick(Sender: TObject);
  public
    ItemsBinHexEdit: TMyNumEdit;//TPBBinHexEdit;
    BytesBinHexEdit: TMyNumEdit;//TPBBinHexEdit;
    MaxAddressBinHexEdit: TMyHexEdit;//TPBBinHexEdit;
    Options: TDisassembleFormOptions;
    procedure Translate(ini: TMemINIFile; error: string);
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

procedure TAdvancedDisassembleForm.ItemsRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled:=false;
  SelectedEdit:=ItemsBinHexEdit;
  SelectedEdit.Enabled:=true;
  SelectedEdit.SetFocus;
  Options.option:=dcItems;
end;

procedure TAdvancedDisassembleForm.BytesRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled:=false;
  SelectedEdit:=BytesBinHexEdit;
  SelectedEdit.Enabled:=true;
  SelectedEdit.SetFocus;
  Options.option:=dcBytes;
end;

procedure TAdvancedDisassembleForm.MaxRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled:=false;
  SelectedEdit:=MaxAddressBinHexEdit;
  SelectedEdit.Enabled:=true;
  SelectedEdit.SetFocus;
  Options.option:=dcMaxAddress;
end;

procedure TAdvancedDisassembleForm.NormalRadioButtonClick(Sender: TObject);
begin
  SelectedEdit.Enabled:=false;
  Options.option:=dcNormal;
end;

procedure TAdvancedDisassembleForm.FormCreate(Sender: TObject);
begin
  ItemsBinHexEdit:=TMyNumEdit.Create(self);//TPBBinHexEdit.Create(self);
  ItemsBinHexEdit.Left:=128;
  ItemsBinHexEdit.Top:=24;
//  ItemsBinHexEdit.BaseFormat:=Number;
  ItemsBinHexEdit.Parent:=self.OptionsGroupBox;
  ItemsBinHexEdit.Enabled:=true;
  ItemsBinHexEdit.OnChange:=ValueChange;
  ItemsBinHexEdit.MaxValue:=$FFFFFFFF;
  BytesBinHexEdit:=TMyNumEdit.Create(self);//TPBBinHexEdit.Create(self);
  BytesBinHexEdit.Left:=128;
  BytesBinHexEdit.Top:=56;
//  BytesBinHexEdit.BaseFormat:=Number;
  BytesBinHexEdit.Parent:=self.OptionsGroupBox;
  BytesBinHexEdit.Enabled:=false;
  BytesBinHexEdit.OnChange:=ValueChange;
  BytesBinHexEdit.MaxValue:=$FFFFFFFF;
  MaxAddressBinHexEdit:=TMyHexEdit.Create(self);//TPBBinHexEdit.Create(self);
  MaxAddressBinHexEdit.Left:=128;
  MaxAddressBinHexEdit.Top:=88;
//  MaxAddressBinHexEdit.BaseFormat:=HexaDecimal;
  MaxAddressBinHexEdit.Parent:=self.OptionsGroupBox;
  MaxAddressBinHexEdit.Enabled:=false;
  MaxAddressBinHexEdit.OnChange:=ValueChange;

  SelectedEdit:=ItemsBinHexEdit;

  Options.recursive:=true;
  Options.bit32:=true;
  Options.option:=dcItems;
  Options.value:=0;
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

procedure TAdvancedDisassembleForm.Translate(ini: TMemINIFile; error: string);
begin
  Caption:=ini.ReadString('AdvancedDisassembleForm','Caption',error);
  OptionsGroupBox.Caption:= ini.ReadString('AdvancedDisassembleForm','Options',error);
  ItemsRadioButton.Caption:=ini.ReadString('AdvancedDisassembleForm','Items',error);
  BytesRadioButton.Caption:=ini.ReadString('AdvancedDisassembleForm','Bytes',error);
  MaxRadioButton.Caption:=ini.ReadString('AdvancedDisassembleForm','Max',error);
  NormalRadioButton.Caption:=ini.ReadString('AdvancedDisassembleForm','Normal',error);
  Bit1632GroupBox.Caption:=ini.ReadString('AdvancedDisassembleForm','Bit1632GroupBox',error);
//  bit16Radiobutton.Caption:=ini.ReadString('AdvancedDisassembleForm','',error);
//  bit32Radiobutton.Caption:=ini.ReadString('AdvancedDisassembleForm','',error);
  RecursiveCheckBox.Caption:=ini.ReadString('AdvancedDisassembleForm','Recursive',error);

  OKButton.Caption:=ini.ReadString('Common','OKButton',error);
  CancelButton.Caption:=ini.ReadString('Common','CancelButton',error);
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

end.
