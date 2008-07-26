object GotoLineForm: TGotoLineForm
  Left = 355
  Top = 251
  BorderStyle = bsDialog
  Caption = 'Go to line ...'
  ClientHeight = 81
  ClientWidth = 240
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GotoLineLabel: TLabel
    Left = 13
    Top = 20
    Width = 53
    Height = 13
    Caption = 'Target line:'
  end
  object CancelButton: TButton
    Left = 156
    Top = 48
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = CancelButtonClick
  end
  object OKButton: TButton
    Left = 72
    Top = 48
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = OKButtonClick
  end
end
