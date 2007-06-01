inherited FileTabFrame: TFileTabFrame
  inherited Panel: TPanel
    object FileOverviewGroupBox: TGroupBox
      Left = 8
      Top = 8
      Width = 281
      Height = 145
      Anchors = [akLeft, akTop, akRight]
      Caption = 'File Overview'
      Constraints.MinWidth = 220
      TabOrder = 0
      DesignSize = (
        281
        145)
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
        Left = 220
        Top = 74
        Width = 26
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Bytes'
      end
      object FilenameEdit: TEdit
        Left = 80
        Top = 21
        Width = 169
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        BiDiMode = bdRightToLeft
        ParentBiDiMode = False
        ReadOnly = True
        TabOrder = 0
      end
      object FullpathEdit: TEdit
        Left = 80
        Top = 46
        Width = 169
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 1
      end
      object FilesizeEdit: TEdit
        Left = 80
        Top = 71
        Width = 137
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ReadOnly = True
        TabOrder = 2
      end
      object FileformatEdit: TEdit
        Left = 80
        Top = 96
        Width = 169
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        BiDiMode = bdLeftToRight
        ParentBiDiMode = False
        ReadOnly = True
        TabOrder = 3
      end
    end
    object ObjectListView: TListView
      Left = 300
      Top = 13
      Width = 399
      Height = 140
      Anchors = [akTop, akRight]
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
    object InfoMemo: TMemo
      Left = 336
      Top = 168
      Width = 363
      Height = 253
      Anchors = [akLeft, akTop, akRight, akBottom]
      Constraints.MinHeight = 30
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 2
      Visible = False
    end
    object AdvancedInfoGrid: TStringGrid
      Left = 8
      Top = 168
      Width = 313
      Height = 250
      Anchors = [akLeft, akTop, akBottom]
      ColCount = 2
      Constraints.MinHeight = 30
      Constraints.MinWidth = 150
      DefaultColWidth = 153
      DefaultRowHeight = 20
      FixedCols = 0
      RowCount = 1
      FixedRows = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
      ScrollBars = ssVertical
      TabOrder = 3
      Visible = False
    end
  end
end
