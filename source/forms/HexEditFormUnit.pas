{ TODO:
    zamysliet sa nad permanentnym HexEdit-om
  DONE:
}
unit HexEditFormUnit;

interface

uses
{$IFDEF MSWINDOWS}
  Controls, Forms, StdCtrls, ExtCtrls, ComCtrls, Dialogs,
{$ENDIF}
{$IFDEF LINUX}
  QControls, QForms, QStdCtrls, QExtCtrls, QComCtrls, QDialogs,
{$ENDIF}
  SysUtils,
  Classes,
  IniFiles,

  MPHexEditor,

  procmat,
  StringRes,
  TatraDASFormUnit;

type
  THexEditForm = class(TTatraDASForm)
    SaveAsButton: TButton;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
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
    procedure FormShow(Sender: TObject);
    procedure GotoAddressButtonClick(Sender: TObject);
  public
    HexEdit: TMPHexEditor;
    procedure Translate(ini: TMemINIFile); override;
  end;


var
  HexEditForm: THexEditForm;

implementation

{$R *.dfm}

uses MainFormUnit, GotoAddressFormUnit, ExecFileUnit;

procedure THexEditForm.SaveAsButtonClick(Sender: TObject);
begin
  SaveDialog1.Execute;
  if SaveDialog1.FileName = '' then Exit;
  HexEdit.SaveToFile(SaveDialog1.FileName);
  HexEdit.Modified:=false;
end;

procedure THexEditForm.HexEditChangePosition(Sender: TObject);
var
  RegionIndex: integer;
  RegionOffset: cardinal;
  Buffer: cardinal;
begin
  Buffer:=0;
  HexEdit.ReadBuffer(Buffer,HexEdit.GetCursorPos,4);
  UnsignedByteDataLabel.Caption:=IntToHex(byte(Buffer),2);
  UnsignedWordDataLabel.Caption:=IntToHex(word(Buffer),4);
  UnsignedDwordDataLabel.Caption:=IntToHex(Buffer,8);
  SignedByteDataLabel.Caption:=IntToSignedHex(shortint(Buffer),2);
  SignedWordDataLabel.Caption:=IntToSignedHex(smallint(Buffer),4);
  SignedDwordDataLabel.Caption:=IntToSignedHex(integer(Buffer),8);
  StatusBar1.Panels[0].Text:=FileOffsetStr + ': '+HexEdit.GetOffsetString(HexEdit.GetCursorPos);
  if MainForm.ExecFile <> nil then
    if MainForm.ExecFile.FullPath = HexEdit.FileName then begin
       RegionIndex:= MainForm.ExecFile.Regions.GetIndexFromOffset(HexEdit.GetCursorPos);
       if RegionIndex <> -1 then begin
         RegionOffset:= HexEdit.GetCursorPos - MainForm.ExecFile.Regions[RegionIndex].Offset;
         StatusBar1.Panels[1].Text:=SectionStr + ': ' + MainForm.ExecFile.Regions[RegionIndex].Name;
         StatusBar1.Panels[2].Text:=SectionOffsetStr + ': '+IntToHex(RegionOffset, 8);
       end  
       else begin
         StatusBar1.Panels[1].Text:=SectionStr + ': ' + 'unused space';
         StatusBar1.Panels[2].Text:=SectionOffsetStr + ': ';
       end;
    end;
end;

procedure THexEditForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if HexEdit = nil then Exit;
  if HexEdit.Modified then begin
    case MessageDlg(FileModifiedStr,mtWarning,[mbYes,mbNo,mbCancel],0) of
      mrYes: SaveAsButtonClick(self);
      mrNo: ;
      mrCancel: begin
        Action:=caNone;
        Exit;
      end;
    end;
  end;
  HexEdit.Free;
  HexEdit:=nil;
  Action:=caHide;
end;

procedure THexEditForm.FormShow(Sender: TObject);
begin
  HexEdit:=TMPHexEditor.Create(Panel1);
  HexEdit.Parent:=Panel1;
  HexEdit.Align:=alClient;
  HexEdit.OnSelectionChanged:=HexEditChangePosition;
  Panel1.Anchors:=[akLeft,akTop,akRight,akBottom];
  HexEdit.LoadFromFile(MainForm.ExecFile.FullPath);
  Caption:='HexEditor - ' + MainForm.ExecFile.FileName;
  StatusBar1.Panels[3].Text:=TatraDASFullNameVersion;
end;

procedure THexEditForm.GotoAddressButtonClick(Sender: TObject);
begin
  GotoAddressForm.GotoAddressEdit.Text:='';
  GotoAddressForm.MaxAddress:=Nezaporne(HexEdit.DataSize-1);
  if GotoAddressForm.ShowModal = mrOK then HexEdit.Seek(GotoAddressForm.address,0);
  HexEdit.SetFocus;
end;

procedure THexEditForm.Translate(ini: TMemINIFile);
begin
  SaveAsButton.Caption:=ini.ReadString('HexEditForm','SaveAsButton',TranslateErrorStr);
  GotoAddressButton.Caption:=ini.ReadString('HexEditForm','GotoAddressButton',TranslateErrorStr);
  UnsignedLabel.Caption:=ini.ReadString('HexEditForm','UnsignedLabel',TranslateErrorStr);
  SignedLabel.Caption:=ini.ReadString('HexEditForm','SignedLabel',TranslateErrorStr);
end;

end.
