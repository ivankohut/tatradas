object AdvancedDisassembleForm: TAdvancedDisassembleForm
  Left = 289
  Top = 163
  BorderStyle = bsDialog
  Caption = 'Advanced disassemble'
  ClientHeight = 278
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
  PixelsPerInch = 96
  TextHeight = 13
  object OptionsGroupBox: TGroupBox
    Left = 8
    Top = 8
    Width = 265
    Height = 161
    Caption = 'Options'
    TabOrder = 0
    object ItemsRadioButton: TRadioButton
      Left = 16
      Top = 24
      Width = 113
      Height = 17
      Caption = 'Instructions count:'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = ItemsRadioButtonClick
    end
    object BytesRadioButton: TRadioButton
      Left = 16
      Top = 56
      Width = 113
      Height = 17
      Caption = 'Bytes count:'
      TabOrder = 1
      OnClick = BytesRadioButtonClick
    end
    object MaxRadioButton: TRadioButton
      Left = 16
      Top = 88
      Width = 113
      Height = 17
      Caption = 'Max address:'
      TabOrder = 2
      OnClick = MaxRadioButtonClick
    end
    object NormalRadioButton: TRadioButton
      Left = 16
      Top = 120
      Width = 113
      Height = 17
      Caption = 'Normal'
      TabOrder = 3
      OnClick = NormalRadioButtonClick
    end
  end
  object OKButton: TButton
    Left = 114
    Top = 242
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 198
    Top = 242
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = CancelButtonClick
  end
  object Bit1632GroupBox: TGroupBox
    Left = 8
    Top = 176
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
    Top = 248
    Width = 97
    Height = 17
    Caption = 'Recursive'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = RecursiveCheckBoxClick
  end
end
