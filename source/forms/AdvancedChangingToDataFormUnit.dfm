object AdvancedChangingToDataForm: TAdvancedChangingToDataForm
  Left = 19
  Top = 111
  BorderStyle = bsDialog
  Caption = 'Advanced changing options'
  ClientHeight = 235
  ClientWidth = 442
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object OKButton: TButton
    Left = 274
    Top = 202
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 358
    Top = 202
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = CancelButtonClick
  end
  object DataTypeGroupBox: TGroupBox
    Left = 8
    Top = 8
    Width = 161
    Height = 81
    Caption = 'Choose data type'
    TabOrder = 2
    object DataTypeComboBox: TComboBox
      Left = 16
      Top = 28
      Width = 129
      Height = 21
      ItemHeight = 13
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnChange = DataTypeComboBoxChange
      Items.Strings = (
        'BYTE (8 bits)'
        'WORD (16 bits)'
        'DWORD (32 bits)'
        'QWORD (64 bits)'
        'Floating point SINGLE (32 bits)'
        'Floating point DOUBLE (64 bits)'
        'Floating point DOUBLE EXTENDED (80 bits)')
    end
  end
  object SignGroupBox: TGroupBox
    Left = 8
    Top = 104
    Width = 161
    Height = 89
    Caption = 'Signed or unsigned'
    TabOrder = 3
    object SignedRadioButton: TRadioButton
      Left = 16
      Top = 56
      Width = 113
      Height = 17
      Caption = 'signed'
      TabOrder = 0
    end
    object UnsignedRadioButton: TRadioButton
      Left = 16
      Top = 24
      Width = 113
      Height = 17
      Caption = 'unsigned'
      Checked = True
      TabOrder = 1
      TabStop = True
    end
  end
  object OptionsGroupBox: TGroupBox
    Left = 184
    Top = 8
    Width = 249
    Height = 185
    Caption = 'Conditions'
    TabOrder = 4
    object ItemsRadioButton: TRadioButton
      Left = 16
      Top = 24
      Width = 113
      Height = 17
      Caption = 'Items count'
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
      Caption = 'Bytes count'
      TabOrder = 1
      OnClick = BytesRadioButtonClick
    end
    object MaxRadioButton: TRadioButton
      Left = 16
      Top = 88
      Width = 113
      Height = 17
      Caption = 'Max address'
      TabOrder = 2
      OnClick = MaxRadioButtonClick
    end
    object EndSectionRadioButton: TRadioButton
      Left = 16
      Top = 120
      Width = 113
      Height = 17
      Caption = 'End of section'
      TabOrder = 3
      OnClick = EndSectionRadioButtonClick
    end
    object CodeRadioButton: TRadioButton
      Left = 16
      Top = 152
      Width = 113
      Height = 17
      Caption = 'Code begins'
      TabOrder = 4
      OnClick = CodeRadioButtonClick
    end
  end
end
