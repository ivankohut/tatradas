object UnknownFileFormatForm: TUnknownFileFormatForm
  Left = 363
  Top = 188
  BorderStyle = bsDialog
  Caption = 'Uknown File Format'
  ClientHeight = 394
  ClientWidth = 465
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 16
    Top = 16
    Width = 433
    Height = 321
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    object UnknownInfoLabel: TLabel
      Left = 16
      Top = 10
      Width = 404
      Height = 26
      Caption = 
        'You have choosen unknown or not supported file format file or no' +
        't executable format. To dissassemble it please insert some infor' +
        'mation:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      WordWrap = True
    end
    object Bevel1: TBevel
      Left = 16
      Top = 64
      Width = 401
      Height = 2
    end
    object OffsetLabel: TLabel
      Left = 48
      Top = 128
      Width = 186
      Height = 13
      Caption = 'Starting Offset address of code section:'
    end
    object Bevel2: TBevel
      Left = 16
      Top = 112
      Width = 401
      Height = 2
    end
    object FileNameLabel: TLabel
      Left = 16
      Top = 80
      Width = 48
      Height = 13
      Caption = 'File name:'
    end
    object FileSizeLabel: TLabel
      Left = 227
      Top = 80
      Width = 40
      Height = 13
      Caption = 'File size:'
    end
    object FileNameDataLabel: TLabel
      Left = 80
      Top = 80
      Width = 33
      Height = 13
      Caption = 'LABEL'
    end
    object FileSizeDataLabel: TLabel
      Left = 312
      Top = 80
      Width = 33
      Height = 13
      Caption = 'LABEL'
    end
    object EntryPointLabel: TLabel
      Left = 48
      Top = 156
      Width = 82
      Height = 13
      Caption = 'Entry point offset:'
    end
    object SizeLabel: TLabel
      Left = 48
      Top = 184
      Width = 86
      Height = 13
      Caption = 'Code section size:'
    end
    object HexNoteLabel: TLabel
      Left = 16
      Top = 289
      Width = 166
      Height = 13
      Caption = 'Note: All numbers are hexadecimal.'
    end
    object SizeEndOffsetGroupBox: TGroupBox
      Left = 224
      Top = 218
      Width = 193
      Height = 57
      Caption = 'Code section size/end offset'
      TabOrder = 0
      object SizeRadioButton: TRadioButton
        Left = 16
        Top = 24
        Width = 65
        Height = 17
        Caption = 'Size'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object EndOffsetRadioButton: TRadioButton
        Left = 104
        Top = 24
        Width = 73
        Height = 17
        Caption = 'End offset'
        TabOrder = 1
      end
    end
    object Bit1632GroupBox: TGroupBox
      Left = 16
      Top = 218
      Width = 193
      Height = 57
      Caption = '16/32 bit mode'
      TabOrder = 1
      object bit16Radiobutton: TRadioButton
        Left = 16
        Top = 24
        Width = 49
        Height = 17
        Caption = '16 bit'
        Checked = True
        TabOrder = 0
        TabStop = True
      end
      object bit32RadioButton: TRadioButton
        Left = 104
        Top = 24
        Width = 49
        Height = 17
        Caption = '32 bit'
        TabOrder = 1
      end
    end
  end
  object OKButton: TButton
    Left = 16
    Top = 352
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 374
    Top = 352
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
end
