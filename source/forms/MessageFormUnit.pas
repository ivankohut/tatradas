unit MessageFormUnit;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FileUtil, LResources;

type
  TMessageForm = class(TForm)
    MessageLabel: TLabel;
  private
    procedure ButtonClick(Sender: TObject);
    procedure AddButton(AButtonCaption: string; AResultValue: Integer);
  public
    class function DisplayMessage(AMessage: string; AMessageType: TMsgDlgType; AButtons: TMsgDlgButtons): Integer;
  end;
  
  function DisplayMessage(AMessage: string; AMessageType: TMsgDlgType; AButtons: TMsgDlgButtons): Integer;


var
  MessageForm: TMessageForm;

implementation

uses
  TranslatorUnit, ExceptionsUnit;

{$R *.dfm}


function DisplayMessage(AMessage: string; AMessageType: TMsgDlgType; AButtons: TMsgDlgButtons): Integer;
begin
  Result := TMessageForm.DisplayMessage(AMessage, AMessageType, AButtons);
end;



procedure TMessageForm.AddButton(AButtonCaption: string; AResultValue: Integer);
var
  Button: TButton;
begin
  Button := TButton.Create(MessageForm);
  Button.Parent := MessageForm;
  Button.Caption := AButtonCaption;
  Button.Left := MessageForm.Width;
  Button.Top := 56;
  Button.Width := 75;
  Button.Height := 25;
  Button.OnClick := ButtonClick;
  Button.Tag := AResultValue;
  Width := Width + Button.Width + 16;
end;



procedure TMessageForm.ButtonClick(Sender: TObject);
begin
  ModalResult := (Sender as TButton).Tag;
end;



class function TMessageForm.DisplayMessage(AMessage: string; AMessageType: TMsgDlgType; AButtons: TMsgDlgButtons): Integer;
begin
  Application.CreateForm(TMessageForm, MessageForm);

  MessageForm.MessageLabel.Caption := AMessage;

  if mbYes in AButtons then
    MessageForm.AddButton(Translator.TranslateControl('Common', 'YesButton'), mrYes);
  if mbNo in AButtons then
    MessageForm.AddButton(Translator.TranslateControl('Common', 'NoButton'), mrNo);
  if mbOk in AButtons then
    MessageForm.AddButton(Translator.TranslateControl('Common', 'OKButton'), mrOk);
  if mbCancel in AButtons then
    MessageForm.AddButton(Translator.TranslateControl('Common', 'CancelButton'), mrCancel);

  case AMessageType of
    mtConfirmation: MessageForm.Caption := Translator.TranslateControl('Common', 'Confirmation');
    mtWarning: MessageForm.Caption := Translator.TranslateControl('Common', 'Warning');
    mtError: MessageForm.Caption := Translator.TranslateControl('Common', 'Error');
    mtInformation: MessageForm.Caption := Translator.TranslateControl('Common', 'Information');
    mtCustom: raise EIllegalState.Create('TMessageForm.DisplayMessage: Unsupported message type');
  end;

  Result := MessageForm.ShowModal;

  MessageForm.Release;
end;


end.
