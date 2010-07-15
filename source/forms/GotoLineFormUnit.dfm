object GotoLineForm: TGotoLineForm
  Left = 355
  Top = 251
  BorderStyle = bsDialog
  Caption = 'Go to line ...'
  ClientHeight = 88
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

  object GotoLineLabel: TLabel
    Left = 13
    Top = 22
    Width = 53
    Height = 13
    Caption = 'Target line:'
  end
  object CancelButton: TButton
    Left = 158
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = CancelButtonClick
  end
  object OKButton: TButton
    Left = 74
    Top = 56
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = OKButtonClick
  end
  object PanelForEdit: TPanel
    Left = 104
    Top = 16
    Width = 129
    Height = 25
    BevelOuter = bvNone
    Caption = 'PanelForEdit'
    TabOrder = 2
  end
end
