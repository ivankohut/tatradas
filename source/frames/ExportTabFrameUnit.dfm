inherited ExportTabFrame: TExportTabFrame
  inherited Panel: TPanel
    object FunctionListView: TListView
      Left = 8
      Top = 8
      Width = 427
      Height = 261
      Align = alClient
      Columns = <
        item
          Caption = 'Number'
        end
        item
          Caption = 'Name'
          Width = 150
        end
        item
          Caption = 'Section'
        end
        item
          Caption = 'Section offset'
          Width = 80
        end
        item
          Caption = 'Entrypoint'
          Width = 80
        end
        item
          Caption = 'Ordinal'
          Width = 80
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnColumnClick = FunctionListViewColumnClick
      OnDblClick = GotoFunctionClick
      OnSelectItem = FunctionListViewSelectItem
    end
  end
end
