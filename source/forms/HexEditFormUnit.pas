{ TODO:
}
unit HexEditFormUnit;

interface

uses
  Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, Dialogs,
  SysUtils,
  Classes,
  IniFiles,
  StrUtils,
  MPHexEditor,
  // project units
  procmat,
  StringUtilities,
  StringRes,
  TranslatorUnit;

type
  THexEditForm = class(TForm, ITranslatable)
    SaveAsButton: TButton;
    HexEditPanel: TPanel;
    HexEditSaveDialog: TSaveDialog;
    StatusBar1: TStatusBar;
    GotoAddressButton: TButton;
    UnsignedWordLabel: TLabel;
    UnsignedDwordLabel: TLabel;
    Bevel1: TBevel;
    SignedWordLabel: TLabel;
    SignedDwordLabel: TLabel;
    UnsignedLabel: TLabel;
    SignedLabel: TLabel;
    UnsignedByteLabel: TLabel;
    SignedByteLabel: TLabel;
    Bevel2: TBevel;
    SignedByteDataLabel: TLabel;
    SignedWordDataLabel: TLabel;
    SignedDwordDataLabel: TLabel;
    UnsignedByteDataLabel: TLabel;
    UnsignedWordDataLabel: TLabel;
    UnsignedDwordDataLabel: TLabel;
    procedure SaveAsButtonClick(Sender: TObject);
    procedure HexEditChangePosition(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GotoAddressButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    HexEdit: TMPHexEditor;
  public
    procedure OpenAndLoad(aFileName: string);
    procedure Translate;
  end;


var
  HexEditForm: THexEditForm;

implementation

{$R *.lfm}

uses
  MainFormUnit, GotoAddressFormUnit, ExecFileUnit, MessageFormUnit;



procedure THexEditForm.OpenAndLoad(aFileName: string);
begin
  try
    HexEdit.LoadFromFile(aFileName);
  except
    on EFOpenError do
      DisplayMessage(CouldNotOpenReadWriteFileStr, mtError, [mbOK]);
    else
      raise;
  end;
  Caption := 'HexEditor - ' + MainForm.ExecFile.FileName;
  Show;
end;



procedure THexEditForm.SaveAsButtonClick(Sender: TObject);
begin
  HexEditSaveDialog.Execute;
  if HexEditSaveDialog.FileName = '' then
    Exit;
  HexEdit.SaveToFile(HexEditSaveDialog.FileName);
  HexEdit.Modified := False;
end;



procedure THexEditForm.HexEditChangePosition(Sender: TObject);
var
  RegionIndex: Integer;
  RegionOffset: Cardinal;
  Buffer: Cardinal;
begin
  Buffer := 0;
  HexEdit.ReadBuffer(Buffer, HexEdit.GetCursorPos, Min(4, HexEdit.DataSize - HexEdit.GetCursorPos));
  UnsignedByteDataLabel.Caption := IntToHex(Byte(Buffer), 2);
  UnsignedWordDataLabel.Caption := IntToHex(Word(Buffer), 4);
  UnsignedDwordDataLabel.Caption := IntToHex(Buffer, 8);
  SignedByteDataLabel.Caption := IntToSignedHex(ShortInt(Buffer), 2);
  SignedWordDataLabel.Caption := IntToSignedHex(SmallInt(Buffer), 4);
  SignedDwordDataLabel.Caption := IntToSignedHex(Integer(Buffer), 8);
  StatusBar1.Panels[0].Text := FileOffsetStr + ': ' + HexEdit.GetOffsetString(HexEdit.GetCursorPos);
  if MainForm.ExecFile <> nil then
    if MainForm.ExecFile.FullPath = HexEdit.FileName then begin
      RegionIndex := MainForm.ExecFile.Regions.GetIndexFromOffset(HexEdit.GetCursorPos);
      if RegionIndex <> -1 then begin
        RegionOffset := Cardinal(HexEdit.GetCursorPos) - MainForm.ExecFile.Regions[RegionIndex].Offset;
        StatusBar1.Panels[1].Text := SectionStr + ': ' + MainForm.ExecFile.Regions[RegionIndex].Name;
        StatusBar1.Panels[2].Text := SectionOffsetStr + ': ' + IntToHex(RegionOffset, 8);
      end
      else begin
        StatusBar1.Panels[1].Text := SectionStr + ': ' + UnusedSpaceStr;
        StatusBar1.Panels[2].Text := SectionOffsetStr + ': ';
      end;
    end;
end;



procedure THexEditForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if HexEdit.Modified then begin
    case DisplayMessage(InjectStr(FileModifiedStr, ['''' + ExtractFileName(HexEdit.FileName) + '''']), mtWarning, [mbYes, mbNo, mbCancel]) of
      mrYes: SaveAsButtonClick(self);
      mrNo: ;
      mrCancel: begin
        Action := caNone;
        Exit;
      end;
    end;
  end;
  HexEdit.CreateEmptyFile('');
  Action := caHide;
end;



procedure THexEditForm.GotoAddressButtonClick(Sender: TObject);
begin
  GotoAddressForm.MinAddress := 0;
  GotoAddressForm.MaxAddress := NonNegative(HexEdit.DataSize - 1);
  if GotoAddressForm.ShowModal = mrOK then
    HexEdit.Seek(GotoAddressForm.Address, 0);
  HexEdit.SetFocus;
end;



procedure THexEditForm.Translate;
begin
  SaveAsButton.Caption := Translator.TranslateControl('HexEditForm', 'SaveAsButton');
  GotoAddressButton.Caption := Translator.TranslateControl('HexEditForm', 'GotoAddressButton');
  UnsignedLabel.Caption := Translator.TranslateControl('HexEditForm', 'UnsignedLabel');
  SignedLabel.Caption := Translator.TranslateControl('HexEditForm', 'SignedLabel');
end;



procedure THexEditForm.FormCreate(Sender: TObject);
begin
  HexEdit := TMPHexEditor.Create(HexEditPanel);
  HexEdit.Parent := HexEditPanel;
  HexEdit.Align := alClient;
  HexEdit.OnSelectionChanged := HexEditChangePosition;
  HexEditPanel.Anchors := [akLeft, akTop, akRight, akBottom];
  StatusBar1.Panels[3].Text := TatraDASFullNameVersion;
end;



procedure THexEditForm.FormDestroy(Sender: TObject);
begin
  HexEdit.Free;
  HexEdit := nil;
end;



end.
