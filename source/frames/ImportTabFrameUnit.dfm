inherited ImportTabFrame: TImportTabFrame
  Width = 558
  ExplicitWidth = 558
  inherited Panel: TPanel
    Width = 558
    ExplicitWidth = 558
    object OccurHintLabel: TLabel
      Left = 16
      Top = 790
      Width = 72
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'OccurHintLabel'
    end
    object Splitter1: TSplitter
      Left = 408
      Top = 8
      Width = 11
      Height = 288
      Align = alRight
      ExplicitHeight = 261
    end
    object Panel1: TPanel
      Left = 8
      Top = 8
      Width = 400
      Height = 288
      Align = alClient
      BevelOuter = bvNone
      Constraints.MinWidth = 400
      TabOrder = 0
      object FunctionListView: TListView
        Left = 0
        Top = 64
        Width = 400
        Height = 224
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
        ExplicitTop = 65
        ExplicitHeight = 223
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 400
        Height = 64
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object ModulLabel: TLabel
          Left = 0
          Top = 20
          Width = 53
          Height = 13
          Caption = 'ModulLabel'
        end
        object FunctionLabel: TLabel
          Left = 0
          Top = 48
          Width = 66
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
      Left = 419
      Top = 8
      Width = 131
      Height = 288
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object FunctionCallsLabel: TLabel
        Left = 2
        Top = 48
        Width = 88
        Height = 13
        Caption = 'FunctionCallsLabel'
      end
      object AddressListBox: TListBox
        Left = 0
        Top = 64
        Width = 131
        Height = 224
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
