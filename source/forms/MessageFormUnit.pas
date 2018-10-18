unit MessageFormUnit;

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FileUtil,
  // project units
  Translatables;

type

  { TMessageForm }

  TMessageForm = class(TForm, ITranslatable)
    MessageLabel: TLabel;
  private
    FTranslatableWidgets: TTranslatableWidgetArray;
    procedure AddTranslatableWidget(Control: TControl; Name: string);
    procedure ButtonClick(Sender: TObject);
    procedure AddButton(CaptionKey: string; ModalResult: Integer);
    procedure Initialize(Message: string; MessageType: TMsgDlgType; Buttons: TMsgDlgButtons);
  public
    function Translatable: TTranslatable;
  end;

function DisplayMessage(Message: string; MessageType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer;


var
  MessageForm: TMessageForm;

implementation

uses
  ExceptionsUnit, TranslatorUnit;

{$R *.lfm}

function DisplayMessage(Message: string; MessageType: TMsgDlgType; Buttons: TMsgDlgButtons): Integer;
begin
  Application.CreateForm(TMessageForm, MessageForm);
  MessageForm.Initialize(Message, MessageType, Buttons);
  Translator.TranslateComponent(MessageForm);
  Result := MessageForm.ShowModal;
  MessageForm.Release;
end;



procedure TMessageForm.Initialize(Message: string; MessageType: TMsgDlgType; Buttons: TMsgDlgButtons);
var
  CaptionKey: string;
begin
  MessageForm.MessageLabel.Caption := Message;
  if mbYes in Buttons then
    MessageForm.AddButton('YesButton', mrYes);
  if mbNo in Buttons then
    MessageForm.AddButton('NoButton', mrNo);
  if mbOK in Buttons then
    MessageForm.AddButton('OKButton', mrOK);
  if mbCancel in Buttons then
    MessageForm.AddButton('CancelButton', mrCancel);

  case MessageType of
    mtConfirmation: CaptionKey := 'Confirmation';
    mtWarning: CaptionKey := 'Warning';
    mtError: CaptionKey := 'Error';
    mtInformation: CaptionKey := 'Information';
    mtCustom: raise EIllegalState.Create('TMessageForm.DisplayMessage: Unsupported message type');
  end;
  AddTranslatableWidget(self, CaptionKey);
end;



procedure TMessageForm.AddButton(CaptionKey: string; ModalResult: Integer);
var
  Button: TButton;
begin
  Button := TButton.Create(self);
  Button.Parent := self;
  Button.Left := self.Width;
  Button.Top := 56;
  Button.Width := 75;
  Button.Height := 25;
  Button.OnClick := ButtonClick;
  Button.Tag := ModalResult;
  Width := Width + Button.Width + 16;
  AddTranslatableWidget(Button, CaptionKey);
end;



procedure TMessageForm.AddTranslatableWidget(Control: TControl; Name: string);
begin
  SetLength(FTranslatableWidgets, Length(FTranslatableWidgets) + 1);
  FTranslatableWidgets[Length(FTranslatableWidgets) - 1] := TTranslatableCaption.Create(Control, Name);
end;



function TMessageForm.Translatable: TTranslatable;
begin
  Result := TTranslatableGroup.Create('Common', FTranslatableWidgets);
end;



procedure TMessageForm.ButtonClick(Sender: TObject);
begin
  ModalResult := (Sender as TButton).Tag;
end;



end.
