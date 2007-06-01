{ TODO:

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
  SysUtils, Classes, INIFiles,
  procmat, StringRes,
  MPHexEditor;

type
  THexEditForm = class(TForm)
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
    procedure Translate(ini: TMemINIFile; error: string);
  end;


var
  HexEditForm: THexEditForm;

implementation

{$R *.dfm}

uses MainFormUnit, GotoAddressFormUnit;

procedure THexEditForm.SaveAsButtonClick(Sender: TObject);
begin
  SaveDialog1.Execute;
  if SaveDialog1.FileName = '' then Exit;
  HexEdit.SaveToFile(SaveDialog1.FileName);
  HexEdit.Modified:=false;
end;

procedure THexEditForm.HexEditChangePosition(Sender: TObject);
var SectionOffset: cardinal;
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
  if MainForm.ExecFile<>nil then
    If MainForm.ExecFile.fullpath=HexEdit.FileName then begin
       SectionOffset:=MainForm.ExecFile.Sections.GetSectionOffsetFromFileOffset(HexEdit.GetCursorPos);
       if SectionOffset<>$FFFFFFFF then begin
         StatusBar1.Panels[1].Text:=SectionStr + ': '+MainForm.ExecFile.Sections.GetSectionNameFromFileOffset(HexEdit.GetCursorPos);
         StatusBar1.Panels[2].Text:=SectionOffsetStr + ': '+IntToHex(SectionOffset,8);
       end
       else begin
         StatusBar1.Panels[1].Text:=SectionStr + ':';
         StatusBar1.Panels[2].Text:=SectionOffsetStr + ':';
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
  HexEdit.LoadFromFile(MainForm.ExecFile.fullpath);
  Caption:='HexEditor - '+MainForm.ExecFile.filename;
  StatusBar1.Panels[3].Text:=TatraDASFullNameVersion;
end;

procedure THexEditForm.GotoAddressButtonClick(Sender: TObject);
begin
  GotoAddressForm.GotoAddressEdit.Text:='';
  GotoAddressForm.MaxAddress:=Nezaporne(HexEdit.DataSize-1);
  if GotoAddressForm.ShowModal = mrOK then HexEdit.Seek(GotoAddressForm.address,0);
  HexEdit.SetFocus;
end;

procedure THexEditForm.Translate(ini: TMemINIFile; error: string);
begin
  SaveAsButton.Caption:=ini.ReadString('HexEditForm','SaveAsButton',error);
  GotoAddressButton.Caption:=ini.ReadString('HexEditForm','GotoAddressButton',error);
  UnsignedLabel.Caption:=ini.ReadString('HexEditForm','UnsignedLabel',error);
  SignedLabel.Caption:=ini.ReadString('HexEditForm','SignedLabel',error);
end;

end.
