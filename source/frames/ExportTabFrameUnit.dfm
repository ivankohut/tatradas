inherited ExportTabFrame: TExportTabFrame
  inherited Panel: TPanel
    object FunctionStringGrid: TStringGrid
      Left = 8
      Top = 8
      Width = 427
      Height = 261
      Align = alClient
      ColCount = 6
      DefaultColWidth = 48
      DefaultRowHeight = 16
      RowCount = 2
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Default'
      Font.Style = []
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goColSizing, goRowSelect]
      ParentFont = False
      TabOrder = 0
      OnDblClick = FunctionStringGridDblClick
      OnMouseDown = FunctionStringGridMouseDown
      OnSelectCell = FunctionStringGridSelectCell
      ColWidths = (
        48
        235
        81
        80
        76
        76)
    end
  end
end
