unit AboutBoxUnit;

interface

uses
  Classes, SysUtils, IniFiles,
  Forms, ExtCtrls, StdCtrls, Controls, Graphics, ActnList,
  // project units
  procmat,
  TranslatorUnit;

type
  TAboutBox = class(TForm, ITranslatable)
    Panel1: TPanel;
    ProductName: TLabel;
    VersionLabel: TLabel;
    LicenseLabel: TLabel;
    OKButton: TButton;
    ProgrammingLabel: TLabel;
    Panel2: TPanel;
    Image1: TImage;
    Panel3: TPanel;
    URLLabel: TLabel;
    GraphicsLabel: TLabel;
    ProgrammingDataLabel: TLabel;
    GraphicsDataLabel: TLabel;
    LicenseDataLabel: TLabel;
    ReleaseDateLabel: TLabel;
    CompilerLabel: TLabel;
    CompilerDataLabel: TLabel;
    procedure OKButtonClick(Sender: TObject);
    procedure URLLabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure URLLabelMouseLeave(Sender: TObject);
    procedure URLLabelClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);

    procedure Translate;
    procedure FormCreate(Sender: TObject);
  end;

var
  AboutBox: TAboutBox;

implementation

uses LCLIntf;

{$R *.lfm}

procedure TAboutBox.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;



procedure TAboutBox.URLLabelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  Self.URLLabel.Font.Style := [fsUnderline];
end;



procedure TAboutBox.URLLabelMouseLeave(Sender: TObject);
begin
  Self.URLLabel.Font.Style := [];
end;



procedure TAboutBox.URLLabelClick(Sender: TObject);
begin
  OpenURL(URLLabel.Caption);
end;



procedure TAboutBox.Translate;
begin
  Caption := Translator.TranslateControl('AboutBoxForm', 'Caption');
  ProgrammingLabel.Caption := Translator.TranslateControl('AboutBoxForm', 'ProgrammingLabel');
  GraphicsLabel.Caption := Translator.TranslateControl('AboutBoxForm', 'GraphicsLabel');
  VersionLabel.Caption := Translator.TranslateControl('AboutBoxForm', 'VersionLabel') + ' ' + ShortTatraDASVersion;
  ReleaseDateLabel.Caption := Translator.TranslateControl('AboutBoxForm', 'ReleaseDateLabel') + ' ' + TatraDASDate + ' (DMY)'; // + ' (DD.MM.YYYY)';
  LicenseLabel.Caption := Translator.TranslateControl('AboutBoxForm', 'LicenseLabel');
  URLLabel.Hint := Translator.TranslateControl('AboutBoxForm', 'URLLabelHint');
  CompilerLabel.Caption := Translator.TranslateControl('AboutBoxForm', 'CompilerLabel');
end;



procedure TAboutBox.FormActivate(Sender: TObject);
begin
  VersionLabel.Left := (Panel1.Width - VersionLabel.Width) div 2;
  ReleaseDateLabel.Left := (Panel1.Width - ReleaseDateLabel.Width) div 2;
end;



procedure TAboutBox.FormCreate(Sender: TObject);
begin
  CompilerDataLabel.Caption := GetCompilerNameAndVersion;
  URLLabel.Caption := TatraDASWebSiteURL;
end;



end.
