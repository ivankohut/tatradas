object InsertCommentForm: TInsertCommentForm
  Left = 195
  Top = 163
  BorderStyle = bsDialog
  Caption = 'Insert comment '
  ClientHeight = 81
  ClientWidth = 345
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object InsertCommentLabel: TLabel
    Left = 8
    Top = 20
    Width = 47
    Height = 13
    Caption = 'Comment:'
  end
  object InsertCommentEdit: TEdit
    Left = 72
    Top = 16
    Width = 265
    Height = 21
    TabOrder = 0
  end
  object OKButton: TButton
    Left = 178
    Top = 48
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 262
    Top = 48
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = CancelButtonClick
  end
end
