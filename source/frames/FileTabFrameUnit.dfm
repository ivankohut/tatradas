inherited FileTabFrame: TFileTabFrame
  inherited Panel: TPanel
    object Splitter1: TSplitter
      Left = 577
      Top = 8
      Width = 8
      Height = 261
      Cursor = crHSplit
      MinSize = 150
    end
    object Panel1: TPanel
      Left = 8
      Top = 8
      Width = 569
      Height = 261
      Align = alLeft
      BevelOuter = bvNone
      Constraints.MinWidth = 252
      TabOrder = 0
      object FileOverviewGroupBox: TGroupBox
        Left = 0
        Top = 0
        Width = 569
        Height = 129
        Align = alTop
        Caption = 'File Overview'
        Constraints.MaxHeight = 129
        Constraints.MinHeight = 129
        Constraints.MinWidth = 220
        TabOrder = 0
        object FilenameLabel: TLabel
          Left = 16
          Top = 24
          Width = 48
          Height = 13
          Caption = 'File name:'
        end
        object FullPathLabel: TLabel
          Left = 16
          Top = 50
          Width = 43
          Height = 13
          Caption = 'Full path:'
        end
        object FileSizeLabel: TLabel
          Left = 16
          Top = 75
          Width = 40
          Height = 13
          Caption = 'File size:'
        end
        object FileFormatLabel: TLabel
          Left = 16
          Top = 99
          Width = 51
          Height = 13
          Caption = 'File format:'
        end
        object BytesLabel: TLabel
          Left = 508
          Top = 74
          Width = 26
          Height = 13
          Anchors = [akTop, akRight]
          Caption = 'Bytes'
        end
        object FilenameEdit: TEdit
          Left = 80
          Top = 21
          Width = 457
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          BiDiMode = bdLeftToRight
          ParentBiDiMode = False
          ReadOnly = True
          TabOrder = 0
        end
        object FullpathEdit: TEdit
          Left = 80
          Top = 46
          Width = 457
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ReadOnly = True
          TabOrder = 1
        end
        object FilesizeEdit: TEdit
          Left = 80
          Top = 71
          Width = 425
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          ReadOnly = True
          TabOrder = 2
        end
        object FileformatEdit: TEdit
          Left = 80
          Top = 96
          Width = 457
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          BiDiMode = bdLeftToRight
          ParentBiDiMode = False
          ReadOnly = True
          TabOrder = 3
        end
      end
      object ObjectListView: TListView
        Left = 0
        Top = 137
        Width = 569
        Height = 124
        Align = alClient
        Columns = <
          item
            Caption = 'Nr.'
            Width = 30
          end
          item
            Caption = 'Name'
            Width = 70
          end
          item
            Caption = 'File Offset'
            Width = 65
          end
          item
            Caption = 'File Size'
            Width = 65
          end
          item
            Caption = 'Mem address'
            Width = 65
          end
          item
            Caption = 'Mem size'
            Width = 65
          end
          item
            Caption = 'Flags/Type'
            Width = 65
          end>
        ReadOnly = True
        RowSelect = True
        TabOrder = 1
        ViewStyle = vsReport
      end
      object Panel2: TPanel
        Left = 0
        Top = 129
        Width = 569
        Height = 8
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
      end
    end
    object AdvancedInfoPanel: TPanel
      Left = 585
      Top = 8
      Width = 184
      Height = 261
      Align = alClient
      BevelOuter = bvNone
      Caption = 'AdvancedInfoPanel'
      TabOrder = 1
      OnResize = AdvancedInfoPanelResize
      object MoreInfoLabel: TLabel
        Left = 0
        Top = 0
        Width = 184
        Height = 13
        Align = alTop
        Caption = 'More information:'
        Layout = tlBottom
      end
      object AdvancedInfoGrid: TStringGrid
        Left = 0
        Top = 13
        Width = 184
        Height = 248
        Align = alClient
        ColCount = 2
        Constraints.MinHeight = 30
        DefaultColWidth = 153
        DefaultRowHeight = 20
        FixedCols = 0
        RowCount = 1
        FixedRows = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
        ScrollBars = ssVertical
        TabOrder = 0
      end
    end
  end
end
