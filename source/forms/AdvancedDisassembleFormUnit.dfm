object AdvancedDisassembleForm: TAdvancedDisassembleForm
  Left = 502
  Top = 236
  BorderStyle = bsDialog
  Caption = 'Advanced disassemble'
  ClientHeight = 245
  ClientWidth = 281
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object OptionsGroupBox: TGroupBox
    Left = 8
    Top = 8
    Width = 265
    Height = 121
    Caption = 'Options'
    TabOrder = 0
    object BytesRadioButton: TRadioButton
      Left = 16
      Top = 24
      Width = 113
      Height = 17
      Caption = 'Bytes count:'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = BytesRadioButtonClick
    end
    object MaxRadioButton: TRadioButton
      Left = 16
      Top = 56
      Width = 113
      Height = 17
      Caption = 'Max address:'
      TabOrder = 1
      OnClick = MaxRadioButtonClick
    end
    object NormalRadioButton: TRadioButton
      Left = 16
      Top = 88
      Width = 113
      Height = 17
      Caption = 'Normal'
      TabOrder = 2
      OnClick = NormalRadioButtonClick
    end
  end
  object OKButton: TButton
    Left = 114
    Top = 210
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 198
    Top = 210
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = CancelButtonClick
  end
  object Bit1632GroupBox: TGroupBox
    Left = 8
    Top = 144
    Width = 265
    Height = 57
    Caption = '16/32 bit mode'
    TabOrder = 3
    object bit16Radiobutton: TRadioButton
      Left = 16
      Top = 24
      Width = 113
      Height = 17
      Caption = '16 bit'
      TabOrder = 0
      OnClick = bit16RadiobuttonClick
    end
    object bit32Radiobutton: TRadioButton
      Left = 136
      Top = 24
      Width = 113
      Height = 17
      Caption = '32 bit'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = bit32RadiobuttonClick
    end
  end
  object RecursiveCheckBox: TCheckBox
    Left = 8
    Top = 216
    Width = 97
    Height = 17
    Caption = 'Recursive'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = RecursiveCheckBoxClick
  end
end
