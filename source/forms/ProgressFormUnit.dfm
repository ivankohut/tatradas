object ProgressForm: TProgressForm
  Left = 457
  Top = 294
  Width = 337
  Height = 123
  BorderStyle = bsSizeToolWin
  Caption = 'ProgressForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ProgressLabel: TLabel
    Left = 8
    Top = 8
    Width = 67
    Height = 13
    Caption = 'ProgressLabel'
  end
  object CancelButton: TButton
    Left = 248
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 0
    OnClick = CancelButtonClick
  end
  object ProgressBar1: TProgressBar
    Left = 8
    Top = 24
    Width = 313
    Height = 25
    Min = 0
    Max = 100
    TabOrder = 1
  end
  object PauseButton: TButton
    Left = 160
    Top = 64
    Width = 75
    Height = 25
    Caption = 'Pause'
    TabOrder = 2
    OnClick = PauseButtonClick
  end
end
