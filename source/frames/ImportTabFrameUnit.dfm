inherited ImportTabFrame: TImportTabFrame
  Width = 531
  Height = 378
  inherited Panel: TPanel
    Width = 531
    Height = 378
    object ModulLabel: TLabel
      Left = 16
      Top = 16
      Width = 55
      Height = 13
      Caption = 'ModulLabel'
    end
    object FunctionLabel: TLabel
      Left = 16
      Top = 56
      Width = 67
      Height = 13
      Caption = 'FunctionLabel'
    end
    object OccurHintLabel: TLabel
      Left = 16
      Top = 350
      Width = 74
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'OccurHintLabel'
    end
    object OccurLabel: TLabel
      Left = 400
      Top = 56
      Width = 55
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'OccurLabel'
    end
    object ModulComboBox: TComboBox
      Left = 104
      Top = 16
      Width = 145
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      OnChange = ModulComboBoxChange
    end
    object FunctionListView: TListView
      Left = 16
      Top = 80
      Width = 367
      Height = 265
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Number'
        end
        item
          Caption = 'Name'
          MinWidth = 100
          Width = 200
        end
        item
          Caption = 'Address'
          Width = 80
        end
        item
          Caption = 'Ordinal'
          Width = 80
        end
        item
          Caption = 'Hint'
        end>
      ReadOnly = True
      RowSelect = True
      TabOrder = 1
      ViewStyle = vsReport
      OnColumnClick = FunctionListViewColumnClick
      OnDblClick = FunctionListViewDblClick
      OnSelectItem = FunctionListViewSelectItem
    end
    object AddressListBox: TListBox
      Left = 400
      Top = 80
      Width = 117
      Height = 265
      Anchors = [akTop, akRight, akBottom]
      BevelEdges = [beTop, beRight, beBottom]
      ItemHeight = 13
      ScrollWidth = 1
      TabOrder = 2
      OnDblClick = AddressListBoxDblClick
    end
  end
end
