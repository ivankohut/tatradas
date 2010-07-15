object GoToAddressForm: TGoToAddressForm
  Left = 607
  Top = 251
  BorderStyle = bsDialog
  Caption = 'Go to address...'
  ClientHeight = 81
  ClientWidth = 240
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96

  object GotoAddressLabel: TLabel
    Left = 13
    Top = 20
    Width = 74
    Height = 13
    Caption = 'Target address:'
  end
  object OKButton: TButton
    Left = 74
    Top = 48
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 158
    Top = 48
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = CancelButtonClick
  end
  object PlaceForEditPanel: TPanel
    Left = 96
    Top = 14
    Width = 137
    Height = 25
    BevelOuter = bvNone
    Caption = 'Place for Edit'
    TabOrder = 2
  end
end
