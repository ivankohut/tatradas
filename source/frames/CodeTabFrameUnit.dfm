inherited CodeTabFrame: TCodeTabFrame
  inherited Panel: TPanel
    object Bevel1: TBevel
      Left = 348
      Top = 72
      Width = 87
      Height = 9
      Anchors = [akTop, akRight]
    end
    object Bevel2: TBevel
      Left = 348
      Top = 152
      Width = 87
      Height = 9
      Anchors = [akTop, akRight]
    end
    object LineLabel: TLabel
      Left = 347
      Top = 248
      Width = 23
      Height = 13
      Anchors = [akRight, akBottom]
      Caption = 'Line:'
    end
    object LineDataLabel: TLabel
      Left = 371
      Top = 248
      Width = 3
      Height = 13
      Anchors = [akRight, akBottom]
    end
    object GotoEntryPointButton: TButton
      Left = 347
      Top = 8
      Width = 88
      Height = 25
      Hint = 'Jump to program Entry point'
      Anchors = [akTop, akRight]
      Caption = 'Goto Entry Point'
      TabOrder = 0
      OnClick = GotoEntryPointButtonClick
    end
    object GotoAddressButton: TButton
      Left = 346
      Top = 40
      Width = 89
      Height = 25
      Hint = 'Jump to user defined address'
      Anchors = [akTop, akRight]
      Caption = 'Go to address ...'
      TabOrder = 1
      OnClick = GotoAddressButtonClick
    end
    object FollowButton: TButton
      Left = 346
      Top = 88
      Width = 89
      Height = 25
      Hint = 'Jump to Jump or Call instruction target'
      Anchors = [akTop, akRight]
      Caption = 'Follow Jump/Call'
      Enabled = False
      TabOrder = 2
      OnClick = FollowButtonClick
    end
    object ReturnButton: TButton
      Left = 346
      Top = 120
      Width = 89
      Height = 25
      Hint = 'Return from last jump'
      Anchors = [akTop, akRight]
      Caption = 'Return Jump/Call'
      Enabled = False
      TabOrder = 3
      OnClick = ReturnButtonClick
    end
  end
  object CodePopupMenu: TPopupMenu
    Left = 16
    Top = 16
    object oggleBookmarks1: TMenuItem
      Caption = 'Toggle Bookmarks'
      object Bookmark01: TMenuItem
        Caption = 'Bookmark 0'
        ImageIndex = 0
        ShortCut = 16432
        OnClick = ToggleBookmarkClick
      end
      object Bookmark11: TMenuItem
        Caption = 'Bookmark 1'
        ImageIndex = 1
        ShortCut = 16433
        OnClick = ToggleBookmarkClick
      end
      object Bookmark21: TMenuItem
        Caption = 'Bookmark 2'
        ImageIndex = 2
        ShortCut = 16434
        OnClick = ToggleBookmarkClick
      end
      object Bookmark31: TMenuItem
        Caption = 'Bookmark 3'
        ImageIndex = 3
        ShortCut = 16435
        OnClick = ToggleBookmarkClick
      end
      object Bookmark41: TMenuItem
        Caption = 'Bookmark 4'
        ImageIndex = 4
        ShortCut = 16436
        OnClick = ToggleBookmarkClick
      end
      object Bookmark51: TMenuItem
        Caption = 'Bookmark 5'
        ImageIndex = 5
        ShortCut = 16437
        OnClick = ToggleBookmarkClick
      end
      object Bookmark61: TMenuItem
        Caption = 'Bookmark 6'
        ImageIndex = 6
        ShortCut = 16438
        OnClick = ToggleBookmarkClick
      end
      object Bookmark71: TMenuItem
        Caption = 'Bookmark 7'
        ImageIndex = 7
        ShortCut = 16439
        OnClick = ToggleBookmarkClick
      end
      object Bookmark81: TMenuItem
        Caption = 'Bookmark 8'
        ImageIndex = 8
        ShortCut = 16440
        OnClick = ToggleBookmarkClick
      end
      object Bookmark91: TMenuItem
        Caption = 'Bookmark 9'
        ImageIndex = 9
        ShortCut = 16441
        OnClick = ToggleBookmarkClick
      end
    end
    object GotoBookmarks2: TMenuItem
      Caption = 'Go to Bookmarks'
      object Bookmark02: TMenuItem
        Caption = 'Bookmark 0'
        ImageIndex = 0
        ShortCut = 32816
        OnClick = GotoBookmarkClick
      end
      object Bookmark12: TMenuItem
        Caption = 'Bookmark 1'
        ImageIndex = 1
        ShortCut = 32817
        OnClick = GotoBookmarkClick
      end
      object Bookmark22: TMenuItem
        Caption = 'Bookmark 2'
        ImageIndex = 2
        ShortCut = 32818
        OnClick = GotoBookmarkClick
      end
      object Bookmark32: TMenuItem
        Caption = 'Bookmark 3'
        ImageIndex = 3
        ShortCut = 32819
        OnClick = GotoBookmarkClick
      end
      object Bookmark42: TMenuItem
        Caption = 'Bookmark 4'
        ImageIndex = 4
        ShortCut = 32820
        OnClick = GotoBookmarkClick
      end
      object Bookmark52: TMenuItem
        Caption = 'Bookmark 5'
        ImageIndex = 5
        ShortCut = 32821
        OnClick = GotoBookmarkClick
      end
      object Bookmark62: TMenuItem
        Caption = 'Bookmark 6'
        ImageIndex = 6
        ShortCut = 32822
        OnClick = GotoBookmarkClick
      end
      object Bookmark72: TMenuItem
        Caption = 'Bookmark 7'
        ImageIndex = 7
        ShortCut = 32823
        OnClick = GotoBookmarkClick
      end
      object Bookmark82: TMenuItem
        Caption = 'Bookmark 8'
        ImageIndex = 8
        ShortCut = 32824
        OnClick = GotoBookmarkClick
      end
      object Bookmark92: TMenuItem
        Caption = 'Bookmark 9'
        ImageIndex = 9
        ShortCut = 32825
        OnClick = GotoBookmarkClick
      end
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object Changetounsigneddata3: TMenuItem
      Caption = 'Change to unsigned data'
      object BYTE8bits2: TMenuItem
        Caption = 'BYTE (8 bits)'
        OnClick = ChangeToUnsignedDataClick
      end
      object WORD16bits2: TMenuItem
        Caption = 'WORD (16 bits)'
        OnClick = ChangeToUnsignedDataClick
      end
      object DWORD32bits2: TMenuItem
        Caption = 'DWORD (32 bits)'
        OnClick = ChangeToUnsignedDataClick
      end
      object QWORD64bits2: TMenuItem
        Caption = 'QWORD (64 bits)'
        OnClick = ChangeToUnsignedDataClick
      end
      object TMenuItem
      end
    end
    object Changetosigneddata2: TMenuItem
      Caption = 'Change to signed data'
      object BYTE8bits1: TMenuItem
        Caption = 'BYTE (8 bits)'
        OnClick = ChangeToSignedDataClick
      end
      object WORD16bits1: TMenuItem
        Caption = 'WORD (16 bits)'
        OnClick = ChangeToSignedDataClick
      end
      object DWORD32bits1: TMenuItem
        Caption = 'DWORD (32 bits)'
        OnClick = ChangeToSignedDataClick
      end
      object QWORD64bits1: TMenuItem
        Caption = 'QWORD (64 bits)'
        OnClick = ChangeToSignedDataClick
      end
    end
    object Changetounsigneddata4: TMenuItem
      Caption = 'Change to float data'
      object SINGLE32bits1: TMenuItem
        Caption = 'SINGLE (32 bits)'
        OnClick = ChangeToFloatDataClick
      end
      object DOUBLE64bits1: TMenuItem
        Caption = 'DOUBLE (64 bits)'
        OnClick = ChangeToFloatDataClick
      end
      object EXTENDED80bits1: TMenuItem
        Caption = 'EXTENDED (80 bits)'
        OnClick = ChangeToFloatDataClick
      end
    end
    object Changetostringdata2: TMenuItem
      Caption = 'Change to string data'
      object Pascal1: TMenuItem
        Caption = 'Pascal'
        OnClick = ChangeToStringDataClick
      end
      object C1: TMenuItem
        Caption = 'C'
        OnClick = ChangeToStringDataClick
      end
      object PascalUnicode1: TMenuItem
        Caption = 'Pascal Unicode'
        OnClick = ChangeToStringDataClick
      end
      object CUnicode1: TMenuItem
        Caption = 'C Unicode'
        OnClick = ChangeToStringDataClick
      end
    end
    object Advancedchangetodata2: TMenuItem
      Caption = 'Advanced change to data ...'
      OnClick = AdvancedChangeToDataClick
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object Dis2: TMenuItem
      Caption = 'Disassemble'
      OnClick = NormalDisassembleClick
    end
    object Advanceddisassemble2: TMenuItem
      Caption = 'Advanced disassemble ...'
      OnClick = AdvancedDisassembleClick
    end
    object N7: TMenuItem
      Caption = '-'
    end
    object Insert2: TMenuItem
      Caption = 'Insert'
      object Comment2: TMenuItem
        Caption = 'Comment ...'
        OnClick = InsertCommentClick
      end
      object Emptyline2: TMenuItem
        Caption = 'Empty line'
        OnClick = InsertEmptyLineClick
      end
    end
    object RemoveLineMenuItem: TMenuItem
      Caption = 'Remove line'
      OnClick = RemoveLineClick
    end
  end
end
