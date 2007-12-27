inherited ImportTabFrame: TImportTabFrame
  Width = 980
  Height = 717
  inherited Panel: TPanel
    Width = 980
    Height = 717
    object OccurHintLabel: TLabel
      Left = 16
      Top = 790
      Width = 74
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'OccurHintLabel'
    end
    object Splitter1: TSplitter
      Left = 830
      Top = 8
      Width = 11
      Height = 701
      Cursor = crHSplit
      Align = alRight
    end
    object Panel1: TPanel
      Left = 8
      Top = 8
      Width = 822
      Height = 701
      Align = alClient
      BevelOuter = bvNone
      Constraints.MinWidth = 400
      TabOrder = 0
      object FunctionListView: TListView
        Left = 0
        Top = 65
        Width = 822
        Height = 636
        Align = alClient
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
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = FunctionListViewColumnClick
        OnDblClick = FunctionListViewDblClick
        OnSelectItem = FunctionListViewSelectItem
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 822
        Height = 65
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object ModulLabel: TLabel
          Left = 0
          Top = 20
          Width = 55
          Height = 13
          Caption = 'ModulLabel'
        end
        object FunctionLabel: TLabel
          Left = 0
          Top = 48
          Width = 67
          Height = 13
          Caption = 'FunctionLabel'
        end
        object ModulComboBox: TComboBox
          Left = 80
          Top = 16
          Width = 145
          Height = 21
          ItemHeight = 13
          TabOrder = 0
          OnChange = ModulComboBoxChange
        end
      end
    end
    object Panel2: TPanel
      Left = 841
      Top = 8
      Width = 131
      Height = 701
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object FunctionCallsLabel: TLabel
        Left = 2
        Top = 48
        Width = 89
        Height = 13
        Caption = 'FunctionCallsLabel'
      end
      object AddressListBox: TListBox
        Left = 0
        Top = 65
        Width = 131
        Height = 636
        Align = alBottom
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelEdges = [beTop, beRight, beBottom]
        ItemHeight = 13
        ScrollWidth = 1
        TabOrder = 0
        OnDblClick = AddressListBoxDblClick
      end
    end
  end
end
