inherited ExportTabFrame: TExportTabFrame
  inherited Panel: TPanel
    object FunctionListView: TListView
      Left = 16
      Top = 16
      Width = 465
      Height = 273
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Number'
        end
        item
          Caption = 'Name'
        end
        item
          Caption = 'Section'
        end
        item
          Caption = 'Section offset'
        end
        item
          Caption = 'Entrypoint'
        end
        item
          Caption = 'Ordinal'
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
      OnDblClick = GotoFunctionClick
      OnSelectItem = FunctionListViewSelectItem
    end
  end
end
