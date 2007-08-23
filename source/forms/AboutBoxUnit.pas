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
  TatraDASFormUnit;

type
  TAboutBox = class(TTatraDASForm)
    Panel1: TPanel;
    ProductName: TLabel;
    VersionLabel: TLabel;
    LicenseLabel: TLabel;
    OKButton: TButton;
    AuthorLabel: TLabel;
    ActionList1: TActionList;
    BrowseURL1: TBrowseURL;
    Button1: TButton;
    Panel2: TPanel;
    Image1: TImage;
    Panel3: TPanel;
    URLLabel: TLabel;
    GraphicsLabel: TLabel;
    procedure OKButtonClick(Sender: TObject);
    procedure URLLabelMouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure URLLabelMouseLeave(Sender: TObject);
    procedure URLLabelClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);

    procedure Translate(INI: TMemINIFile); override;
  end;

var
  AboutBox: TAboutBox;

implementation

uses MainFormUnit;

{$R *.dfm}

procedure TAboutBox.OKButtonClick(Sender: TObject);
begin
  Close;
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

procedure TAboutBox.Button1Click(Sender: TObject);
begin
{
  Showmessage(Format('TatraDAS version: %s' + #13+
                     'TatraDAS build date: %s'+#13+
                     'TatraDAS Project version: %s',
                      [inttohex(TatraDASVersion,8), inttohex(TatraDASDate,8), inttohex(TatraDASProjectVersion,8)] ));
}
end;

procedure TAboutBox.Translate(ini: TMemINIFile);
begin
  URLLabel.Hint:=ini.ReadString('AboutBoxForm','URLLabelHint', TranslateErrorStr);
  Caption:=ini.ReadString('AboutBoxForm','Caption', TranslateErrorStr);
  AuthorLabel.Caption:=ini.ReadString('AboutBoxForm','AutorLabel', TranslateErrorStr);
  GraphicsLabel.Caption:=ini.ReadString('AboutBoxForm','GraphicsLabel', TranslateErrorStr);
  VersionLabel.Caption:=ini.ReadString('AboutBoxForm','VersionLabel', TranslateErrorStr) + ' ' + ShortTatraDASVersion + '   ' + IntToHex(TatraDASDate,8) + ' (DMY)';
  LicenseLabel.Caption:=ini.ReadString('AboutBoxForm','LicenseLabel', TranslateErrorStr) + ' GNU GPL';
end;


procedure TAboutBox.FormActivate(Sender: TObject);
begin
  VersionLabel.Left:=(Panel1.Width - VersionLabel.Width) div 2;
end;

end.
