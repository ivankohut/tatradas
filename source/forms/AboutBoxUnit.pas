unit AboutBoxUnit;

interface

uses
{$IFDEF MSWINDOWS}
  Forms, ExtCtrls, StdCtrls, Controls, Graphics, ExtActns, ActnList,
{$ENDIF}
{$IFDEF LINUX}
  QForms, QExtCtrls, QStdCtrls, QControls, QGraphics, QActnList,
{$ENDIF}

  Classes,
  SysUtils,
  INIFiles,

  procmat,
  TranslatorUnit;

type
  TAboutBox = class(TForm, ITranslatable)
    Panel1: TPanel;
    ProductName: TLabel;
    VersionLabel: TLabel;
    LicenseLabel: TLabel;
    OKButton: TButton;
    AuthorLabel: TLabel;
    ActionList1: TActionList;
    BrowseURL1: TBrowseURL;
    Panel2: TPanel;
    Image1: TImage;
    Panel3: TPanel;
    URLLabel: TLabel;
    GraphicsLabel: TLabel;
    AuthorData: TLabel;
    GraphicsDataLabel: TLabel;
    LicenseDataLabel: TLabel;
    DateLabel: TLabel;
    procedure OKButtonClick(Sender: TObject);
    procedure URLLabelMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure URLLabelMouseLeave(Sender: TObject);
    procedure URLLabelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);

    procedure Translate;
  end;

var
  AboutBox: TAboutBox;

implementation

uses MainFormUnit;

{$R *.dfm}

procedure TAboutBox.OKButtonClick(Sender: TObject);
begin
  ModalResult:= mrOK;
end;



procedure TAboutBox.URLLabelMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  Self.URLLabel.Font.Style:=[fsUnderline];
end;



procedure TAboutBox.URLLabelMouseLeave(Sender: TObject);
begin
  Self.URLLabel.Font.Style:=[];
end;



procedure TAboutBox.URLLabelClick(Sender: TObject);
begin
  ActionList1.Actions[0].Execute;
end;



procedure TAboutBox.Translate;
begin
  Caption:= Translator.TranslateControl('AboutBoxForm','Caption');
  AuthorLabel.Caption:= Translator.TranslateControl('AboutBoxForm','AutorLabel');
  GraphicsLabel.Caption:= Translator.TranslateControl('AboutBoxForm','GraphicsLabel');
  VersionLabel.Caption:= Translator.TranslateControl('AboutBoxForm','VersionLabel') + ' ' + ShortTatraDASVersion;
  DateLabel.Caption:= Translator.TranslateControl('AboutBoxForm','DateLabel') + ' ' + TatraDASDate + ' (DMY)'; // + ' (DD.MM.YYYY)';
  LicenseLabel.Caption:= Translator.TranslateControl('AboutBoxForm','LicenseLabel');
  URLLabel.Hint:= Translator.TranslateControl('AboutBoxForm','URLLabelHint');
end;



procedure TAboutBox.FormActivate(Sender: TObject);
begin
  VersionLabel.Left:= (Panel1.Width - VersionLabel.Width) div 2;
  DateLabel.Left:= (Panel1.Width - DateLabel.Width) div 2;
end;


end.
