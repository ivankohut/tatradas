unit UnknownFileFormUnit;
{
  Formular na zadanie potrebnych informacii o subore neznameho formatu.
  Uzivatel musi zadat zaciatocnu adresu v subore, na ktorej moze neskor zacat
  disassemblovanie.
  Zaroven musi urcit ci sa ma subor interpretovat ako 16 bitovy alebo 32 bitovy.
}
interface

uses
{$IFDEF MSWINDOWS}
  ComCtrls, StdCtrls, ExtCtrls, Forms, Dialogs, Controls,
{$ENDIF}
{$IFDEF LINUX}
  QComCtrls, QStdCtrls, QExtCtrls, QForms, QDialogs, QControls,
{$ENDIF}

  SysUtils,
  Classes,
  INIFiles,

  procmat,
  CustomFileUnit,
  StringRes,
  TranslatorUnit,
  myedits;

type
  TUnknownFileFormatForm = class(TForm, ITranslatable)
    OKButton: TButton;
    CancelButton: TButton;
    Panel1: TPanel;
    UnknownInfoLabel: TLabel;
    Bevel1: TBevel;
    OffsetLabel: TLabel;
    Bevel2: TBevel;
    FileNameLabel: TLabel;
    FileSizeLabel: TLabel;
    FileNameDataLabel: TLabel;
    FileSizeDataLabel: TLabel;
    EntryPointLabel: TLabel;
    SizeLabel: TLabel;
    HexNoteLabel: TLabel;
    SizeEndOffsetGroupBox: TGroupBox;
    SizeRadioButton: TRadioButton;
    EndOffsetRadioButton: TRadioButton;
    Bit1632GroupBox: TGroupBox;
    bit16Radiobutton: TRadioButton;
    bit32RadioButton: TRadioButton;
    procedure OKButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function GetParameters: TCustomFileParameters;
  public
    StartOffsetEdit: THexPositiveEdit;
    EntrypointEdit: THexPositiveEdit;
    SizeEdit: THexPositiveEdit; 
    filename: string;
    filesize: cardinal;
    StartOffset: cardinal;
    Entrypoint: cardinal;
    Size: cardinal;
    Bit32: boolean;
    procedure Translate;
    property Parameters: TCustomFileParameters read GetParameters;
    { Public declarations }
  end;


var
  UnknownFileFormatForm: TUnknownFileFormatForm;

implementation

{$R *.dfm}

{
  Po stlaceni tlacidla OK funkcia overi ci dana adresa zadana uzivatelom je
  platna, t.j. ci nie je vacsia ako velkost suboru.
  Ak je to v poriadku, tak nastavi premennu "Bit32" podla CheckBoxu, urcujucu
  16 alebo 32 bitovu interpretaciu suboru.
}
procedure TUnknownFileFormatForm.OKButtonClick(Sender: TObject);
var error: string;
begin
  StartOffset:=StartOffsetEdit.AsCardinal;
  if (StartOffset>FileSize) then error:= InvalidStartOffsetStr
  else begin
    Size:=SizeEdit.AsCardinal;
    if SizeRadioButton.Checked then       // velkost kodovej sekcie
      if (Size > (FileSize - StartOffset)) then  error:=InvalidSizeStr
      else begin
        EntryPoint:=EntrypointEdit.AsCardinal;
        if (Entrypoint < StartOffset) or (Entrypoint > (StartOffset+Size)) then error:=InvalidEntrypointStr
        else begin
          Bit32:=Bit32RadioButton.Checked;
          ModalResult:=mrOK;
          Exit;
        end;
      end
    else                                 // koniec kodovej sekcie
      if (Size > FileSize) then  error:=InvalidSizeStr
      else begin
        EntryPoint:=EntrypointEdit.AsCardinal;
        if (Entrypoint < StartOffset) or (Entrypoint > Size) then error:=InvalidEntrypointStr
        else begin
          Size:=Size-StartOffset;
          Bit32:=Bit32RadioButton.Checked;
          ModalResult:=mrOK;
          Exit;
        end;
      end;
  end;    
  MessageDlg(error,mtWarning,[mbOK], 0);
  StartOffsetEdit.SetFocus;
end;

{
  Po otvoreni formularu sa nastavi focus na komponent sluziaci na zadanie
  zaciatocnej adresy pre disassemblovanie
}
procedure TUnknownFileFormatForm.FormActivate(Sender: TObject);
begin
  FileNameDataLabel.Left:=FileNameLabel.Left + FileNameLabel.Width + 10;
  FileSizeDataLabel.Left:=FileSizeLabel.Left + FileSizeLabel.Width + 10;
  FileNameDataLabel.Caption:=FileName;
  FileSizeDataLabel.Caption:=IntToHex(FileSize,8);
  StartOffsetEdit.MaxValue:=FileSize;
  EntrypointEdit.MaxValue:=FileSize;
  SizeEdit.MaxValue:=Filesize;
  SizeEdit.AsCardinal:=FileSize;
  StartOffsetEdit.SetFocus;
end;

procedure TUnknownFileFormatForm.Translate;
begin
  Caption:= Translator.TranslateControl('UnknownFileFormatForm','Caption');
  UnknownInfoLabel.Caption:= Translator.TranslateControl('UnknownFileFormatForm','UnknownInfoLabel');
  FileNameLabel.Caption:= Translator.TranslateControl('UnknownFileFormatForm','FileNameLabel');
  FileSizeLabel.Caption:= Translator.TranslateControl('UnknownFileFormatForm','FileSizeLabel');

  OffsetLabel.Caption:= Translator.TranslateControl('UnknownFileFormatForm','OffsetLabel');
  EntryPointLabel.Caption:= Translator.TranslateControl('UnknownFileFormatForm','EntryPointLabel');
  SizeLabel.Caption:= Translator.TranslateControl('UnknownFileFormatForm','SizeLabel');

  Bit1632GroupBox.Caption:= Translator.TranslateControl('UnknownFileFormatForm','Bit1632GroupBox');
  SizeEndOffsetGroupBox.Caption:= Translator.TranslateControl('UnknownFileFormatForm','SizeEndOffsetGroupBox');
  SizeRadioButton.Caption:= Translator.TranslateControl('UnknownFileFormatForm','SizeRadioButton');
  EndOffsetRadioButton.Caption:= Translator.TranslateControl('UnknownFileFormatForm','EndOffsetRadioButton');
  HexNoteLabel.Caption:= Translator.TranslateControl('UnknownFileFormatForm','HexNoteLabel');
  CancelButton.Caption:= Translator.TranslateControl('Common','CancelButton');
end;


procedure TUnknownFileFormatForm.FormCreate(Sender: TObject);
begin
  StartOffsetEdit:= THexPositiveEdit.Create(Panel1);
  StartOffsetEdit.Left:=248;
  StartOffsetEdit.Top:=124;
  StartOffsetEdit.Width:=169;
//  StartOffsetEdit.BaseFormat:=Hexadecimal;
  StartOffsetEdit.Parent:=Panel1;

  EntrypointEdit:= THexPositiveEdit.Create(Panel1); //TPBBinHexEdit.Create(Panel1);
  EntrypointEdit.Left:=248;
  EntrypointEdit.Top:=152;
  EntrypointEdit.Width:=169;
//  EntrypointEdit.BaseFormat:=Hexadecimal;
  EntrypointEdit.Parent:=Panel1;

  SizeEdit:= THexPositiveEdit.Create(Panel1); //TPBBinHexEdit.Create(Panel1);
  SizeEdit.Left:=248;
  SizeEdit.Top:=180;
  SizeEdit.Width:=169;
//  SizeEdit.BaseFormat:=Hexadecimal;
  SizeEdit.Parent:=Panel1;

end;



function TUnknownFileFormatForm.GetParameters: TCustomFileParameters;
begin
  Result.EntrypointOffset := Entrypoint;
  Result.FileOffset := StartOffset;
  Result.Size := Size;
  Result.Bit32 := Bit32;
end;



end.
