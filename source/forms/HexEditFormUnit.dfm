object HexEditForm: THexEditForm
  Left = 152
  Top = 127
  Width = 811
  Height = 488
  Caption = 'Hex Editor'
  Color = clBtnFace
  Constraints.MinHeight = 336
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  DesignSize = (
    803
    461)
  PixelsPerInch = 96
  TextHeight = 13
  object UnsignedWordLabel: TLabel
    Left = 689
    Top = 288
    Width = 38
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'WORD:'
  end
  object UnsignedDwordLabel: TLabel
    Left = 689
    Top = 312
    Width = 46
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'DWORD:'
  end
  object Bevel1: TBevel
    Left = 689
    Top = 336
    Width = 103
    Height = 2
    Anchors = [akRight, akBottom]
  end
  object SignedWordLabel: TLabel
    Left = 689
    Top = 392
    Width = 38
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'WORD:'
  end
  object SignedDwordLabel: TLabel
    Left = 689
    Top = 416
    Width = 46
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'DWORD:'
  end
  object UnsignedLabel: TLabel
    Left = 705
    Top = 240
    Width = 45
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Unsigned'
  end
  object SignedLabel: TLabel
    Left = 705
    Top = 344
    Width = 33
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Signed'
  end
  object UnsignedByteLabel: TLabel
    Left = 689
    Top = 264
    Width = 31
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'BYTE:'
  end
  object SignedByteLabel: TLabel
    Left = 689
    Top = 368
    Width = 31
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'BYTE:'
  end
  object Bevel2: TBevel
    Left = 689
    Top = 232
    Width = 105
    Height = 2
    Anchors = [akRight, akBottom]
  end
  object SignedByteDataLabel: TLabel
    Left = 737
    Top = 368
    Width = 33
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'LABEL'
  end
  object SignedWordDataLabel: TLabel
    Left = 737
    Top = 392
    Width = 33
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'LABEL'
  end
  object SignedDwordDataLabel: TLabel
    Left = 737
    Top = 416
    Width = 33
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'LABEL'
  end
  object UnsignedByteDataLabel: TLabel
    Left = 737
    Top = 264
    Width = 33
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'LABEL'
  end
  object UnsignedWordDataLabel: TLabel
    Left = 737
    Top = 288
    Width = 33
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'LABEL'
  end
  object UnsignedDwordDataLabel: TLabel
    Left = 737
    Top = 312
    Width = 33
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'LABEL'
  end
  object SaveAsButton: TButton
    Left = 689
    Top = 48
    Width = 105
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Save as'
    TabOrder = 0
    OnClick = SaveAsButtonClick
  end
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 665
    Height = 425
    Caption = 'Panel1'
    TabOrder = 1
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 442
    Width = 803
    Height = 19
    Panels = <
      item
        Text = 'File offset: '
        Width = 150
      end
      item
        Text = 'Section:'
        Width = 200
      end
      item
        Text = 'Section offset:'
        Width = 140
      end
      item
        Alignment = taCenter
        Text = 'TatraDAS disassembler'
        Width = 50
      end>
    SimplePanel = False
  end
  object GotoAddressButton: TButton
    Left = 689
    Top = 8
    Width = 105
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Go to address ...'
    TabOrder = 3
    OnClick = GotoAddressButtonClick
  end
  object SaveDialog1: TSaveDialog
    Left = 16
    Top = 16
  end
end
