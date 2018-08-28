{ TODO:
}

unit CodeTabFrameUnit;

interface

uses
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Menus,
  ComCtrls,
  Contnrs,
  Classes,
  SysUtils,
  Math,
  IniFiles,
  StrUtils,
  Graphics,
  // SynEdit units
  SynEdit,
  SynEditTypes,
  SynEditTextBuffer,
  TatraDASHighlighter,
  // project units
  MessageFormUnit,
  TranslatorUnit,
  TabFrameTemplateUnit,
  GotoAddressFormUnit,
  AdvancedChangingToDataFormUnit,
  AdvancedDisassembleFormUnit,
  InsertCommentFormUnit,
  StringRes,
  procmat,
  StringUtilities,
  DisassemblerTypes,
  CodeSectionUnit,
  SectionUnit,
  ProgressFormUnit,
  ProgressThreads,
  LoggerUnit;

type
  TCodeTabFrame = class(TTabFrameTemplate)
  published
    plocha: TSynEdit;
    GotoEntryPointButton: TButton;
    GotoAddressButton: TButton;
    Bevel1: TBevel;
    FollowButton: TButton;
    ReturnButton: TButton;
    LineLabel: TLabel;
    LineDataLabel: TLabel;
    CodePopupMenu: TPopupMenu;
    oggleBookmarks1: TMenuItem;
    Bookmark11: TMenuItem;
    Bookmark21: TMenuItem;
    Bookmark31: TMenuItem;
    Bookmark41: TMenuItem;
    Bookmark51: TMenuItem;
    Bookmark01: TMenuItem;
    Bookmark61: TMenuItem;
    Bookmark71: TMenuItem;
    Bookmark81: TMenuItem;
    Bookmark91: TMenuItem;
    GotoBookmarks2: TMenuItem;
    N5: TMenuItem;
    Changetounsigneddata3: TMenuItem;
    Changetosigneddata2: TMenuItem;
    Changetounsigneddata4: TMenuItem;
    Changetostringdata2: TMenuItem;
    Advancedchangetodata2: TMenuItem;
    N6: TMenuItem;
    Dis2: TMenuItem;
    Advanceddisassemble2: TMenuItem;
    N7: TMenuItem;
    Insert2: TMenuItem;
    Comment2: TMenuItem;
    Emptyline2: TMenuItem;
    RemoveLineMenuItem: TMenuItem;
    Bookmark02: TMenuItem;
    Bookmark12: TMenuItem;
    Bookmark22: TMenuItem;
    Bookmark32: TMenuItem;
    Bookmark42: TMenuItem;
    Bookmark52: TMenuItem;
    Bookmark62: TMenuItem;
    Bookmark72: TMenuItem;
    Bookmark82: TMenuItem;
    Bookmark92: TMenuItem;
    Pascal1: TMenuItem;
    C1: TMenuItem;
    PascalUnicode1: TMenuItem;
    CUnicode1: TMenuItem;
    SINGLE32bits1: TMenuItem;
    DOUBLE64bits1: TMenuItem;
    EXTENDED80bits1: TMenuItem;
    BYTE8bits1: TMenuItem;
    WORD16bits1: TMenuItem;
    DWORD32bits1: TMenuItem;
    QWORD64bits1: TMenuItem;
    BYTE8bits2: TMenuItem;
    WORD16bits2: TMenuItem;
    DWORD32bits2: TMenuItem;
    QWORD64bits2: TMenuItem;

  public
    constructor Create(AOwner: TComponent; ASection: TSection); overload; override;
    destructor Destroy; override;

  published
    procedure PlochaChange(Sender: TObject);
    procedure PlochaStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure PlochaMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    // Navigation events
    procedure GotoEntryPointButtonClick(Sender: TObject);
    procedure GotoAddressButtonClick(Sender: TObject);
    procedure GotoLineClick(Sender: TObject);
    procedure FollowButtonClick(Sender: TObject);
    procedure ReturnButtonClick(Sender: TObject);
    procedure ToggleBookmarkClick(Sender: TObject);
    procedure GotoBookmarkClick(Sender: TObject);

    // Editing events
    procedure ChangeToUnsignedDataClick(Sender: TObject);
    procedure ChangeToSignedDataClick(Sender: TObject);
    procedure ChangeToFloatDataClick(Sender: TObject);
    procedure ChangeToStringDataClick(Sender: TObject);
    procedure AdvancedChangeToDataClick(Sender: TObject);

    procedure NormalDisassembleClick(Sender: TObject);
    procedure AdvancedDisassembleClick(Sender: TObject);

    procedure InsertCommentClick(Sender: TObject);
    procedure InsertEmptyLineClick(Sender: TObject);
    procedure RemoveLineClick(Sender: TObject);

  private
    fSection: TCodeSection;

    fTargetAddress: Cardinal; // jumpable address referenced by CALL, JMP, Call from, Jump from etc.
    fJumpStack: TStack; // stack of made jumps
    fCanFollowJump: Boolean;

    fOnChangeDisassembled: TNotifyEvent;
    // Conversion routines
    procedure ChangeToData(Options: TDataChangeOptions);
    procedure ChangeToStringData(Options: TDataChangeOptions);
    procedure Disassemble(Options: TDisassembleFormOptions);

  public
    procedure UpdateActions;
    procedure FindString(SearchText: string; options: TFindOptions);
    function GetPosition(Address: Cardinal): Cardinal; // Get position in Disassembled from Address (memory address)
    procedure GotoPosition(Offset: LongInt; Origin: TSeekOrigin); // Offset zacina od 0

    procedure Translate; override;

  protected
    function GetSection: TSection; override;

  published
    property OnChangeDisassembled: TNotifyEvent read fOnChangeDisassembled write fOnChangeDisassembled;
  end;

var
  CodeTabFrame: TCodeTabFrame;


implementation


uses
  ExecFileUnit,
  ExportSectionUnit,
  PEFileUnit,
  MainFormUnit,
  ExceptionsUnit,
  GotoLineFormUnit;



constructor TCodeTabFrame.Create(AOwner: TComponent; ASection: TSection);
begin
  inherited Create(AOwner);
  fSection := aSection as TCodeSection;
  Name := 'CodeTabFrame' + IntToStr(fSection.CodeSectionIndex);
  fJumpStack := TStack.Create;
  Caption := 'Code section #' + IntToStr(fSection.CodeSectionIndex);

  // Vytvorenie Textovej plochy SynEdit
  plocha := TSynEdit.Create(Panel);
  plocha.Parent := Panel;
  plocha.SetSubComponent(True);
  plocha.TabOrder := 0;
  plocha.Width := Panel.Width - 124;
  plocha.Height := Panel.Height - 16;
  plocha.Left := 8;
  plocha.Top := 8;
  plocha.Anchors := [akLeft, akRight, akTop, akBottom];
  plocha.ReadOnly := True;
  plocha.Font.Name := 'Courier New';
  plocha.Font.Size := 8;
  plocha.HideSelection := True;
  plocha.ScrollBars := ssVertical;
//  plocha.ActiveLineColor:= $FFE8E0; FIXME

  plocha.Highlighter := TSynTatraDASSyn.Create(plocha);
  plocha.OnChange := PlochaChange;
  plocha.OnStatusChange := PlochaStatusChange;
  plocha.OnMouseDown := PlochaMouseDown;
//  plocha.HookTextBuffer(fSection.Disassembled, plocha.UndoList, plocha.RedoList); FIXME

  fCanFollowJump := False;
  LineDataLabel.Caption := '1';

  plocha.CaretY := 1;
  plocha.PopupMenu := CodePopupMenu;
end;



destructor TCodeTabFrame.Destroy;
begin
  plocha.Free;
  fJumpStack.Free;
  inherited;
end;



procedure TCodeTabFrame.PlochaMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  {$IFDEF LCL}
  plocha.CaretXY := plocha.PixelsToRowColumn(Point(X, Y));
  {$ELSE}
  plocha.CaretXY := TBufferCoord(plocha.PixelsToRowColumn(X, Y));
  {$ENDIF}
end;



procedure TCodeTabFrame.PlochaChange(Sender: TObject);
begin
  if Assigned(fOnChangeDisassembled) then
    fOnChangeDisassembled(plocha);
end;



procedure TCodeTabFrame.PlochaStatusChange(Sender: TObject; Changes: TSynStatusChanges);
var
  LineType: TLineType;
begin
  if scCaretY in Changes then begin
    LineType := GetLineType(Plocha.Lines.Strings[plocha.CaretY - 1]);

    if LineType in [ltComment, ltEmpty] then begin
      MainForm.RemoveLine.Enabled := True;
      RemoveLineMenuItem.Enabled := True;
      fCanFollowJump := False;
    end
    else begin
      MainForm.RemoveLine.Enabled := False;
      RemoveLineMenuItem.Enabled := False;

      case LineType of
        ltJumpRef, ltCallRef, ltLoopRef: begin
          fTargetAddress := StrToInt64('$' + Copy(Plocha.Lines.Strings[plocha.CaretY - 1], 13, 8));
          fCanFollowJump := True;
        end;
        ltInstruction:
          if GetTargetAddress(Plocha.Lines.Strings[plocha.CaretY - 1], fTargetAddress) then
            fCanFollowJump := (fTargetAddress >= fSection.MemOffset) and (fTargetAddress <= fSection.MaxAddress)
          else
            fCanFollowJump := False;
        else
          fCanFollowJump := False;
      end;
    end;
    UpdateActions;
    LineDataLabel.Caption := IntToStr(plocha.CaretY);
  end;
end;



procedure TCodeTabFrame.GotoPosition(Offset: LongInt; Origin: TSeekOrigin); // Offset zacina od 0
var
  Position: Integer;
begin
  case Origin of
    soBeginning: Position := NonNegative(Offset);
    soCurrent: Position := NonNegative(Plocha.CaretY - 1 + Offset);
    else
      raise Exception.Create('This should not happen.');
  end;
{ remove
  Plocha.CaretY:= Min(Position, plocha.Lines.Count - 1) + 1;
  Plocha.TopLine:= Plocha.CaretY;
}
  // Go to position (preserve caret relative Y position)
  Plocha.TopLine := (Position + 1) - (Plocha.CaretY - Plocha.TopLine);
  Plocha.CaretY := Min(Position, plocha.Lines.Count - 1) + 1;
end;



procedure TCodeTabFrame.FindString(SearchText: string; Options: TFindOptions);
var
  LineIndex: Integer;
  IsFound: Boolean;
  UpCasedSearchText: string;

begin
  LineIndex := Plocha.CaretY - 1;
  IsFound := False;
  UpCasedSearchText := UpperCase(SearchText);

  // Forward search
  if frDown in Options then begin
    Inc(LineIndex);
    // Case sensitive search
    if frMatchCase in Options then
      while (not IsFound) and (LineIndex < plocha.Lines.Count) do
        if Pos(SearchText, plocha.Lines[LineIndex]) <> 0 then
          IsFound := True
        else
          Inc(LineIndex)
    // Case insensitive search
    else
      while (not IsFound) and (LineIndex < plocha.Lines.Count) do
        if Pos(UpCasedSearchText, UpperCase(plocha.Lines[LineIndex])) <> 0 then
          IsFound := True
        else
          Inc(LineIndex);
  end

  // Backward search
  else begin
    Dec(LineIndex);
    // Case sensitive search
    if frMatchCase in Options then
      while (not IsFound) and (LineIndex >= 0) do
        if Pos(SearchText, plocha.Lines[LineIndex]) <> 0 then
          IsFound := True
        else
          Dec(LineIndex)
    // Case insensitive search
    else
      while (not IsFound) and (LineIndex >= 0) do
        if Pos(UpCasedSearchText, UpperCase(plocha.Lines[LineIndex])) <> 0 then
          IsFound := True
        else
          Dec(LineIndex);
  end;

  if IsFound then
    GotoPosition(LineIndex, soBeginning)
  else
    DisplayMessage('"' + SearchText + '" ' + NotFoundStr, mtWarning, [mbOK]);
end;



function TCodeTabFrame.GetPosition(Address: Cardinal): Cardinal; // Get position in Disassembled from Address (memory address)
begin
  Result := fSection.GetPosition(Address);
end;



procedure TCodeTabFrame.Translate;
var
  i: Integer;
begin
  (Parent as TTabSheet).Caption := Translator.TranslateControl('Code', 'Caption') + IntToStr(fSection.CodeSectionIndex);

  // Buttons - translated trough actions

  // Popisky Label
{
  BytesperInstructionLabel.Caption:=Translator.TranslateControl('Code','BytesperInstructionLabel');
  InstructionCountLabel.Caption:=Translator.TranslateControl('Code','InstructionCountLabel');
}
  LineLabel.Caption := Translator.TranslateControl('Code', 'LineLabel');

  // Hinty
  GotoEntrypointButton.Hint := Translator.TranslateControl('Code', 'GotoEntrypointButtonHint');
  GotoAddressButton.Hint := Translator.TranslateControl('Code', 'GotoAddressButtonHint');
  FollowButton.Hint := Translator.TranslateControl('Code', 'FollowButtonHint');
  ReturnButton.Hint := Translator.TranslateControl('Code', 'ReturnButtonHint');

  // Popup menu
  CodePopupMenu.Items[0].Caption := Translator.TranslateControl('Code', 'ToggleBookmark');
  CodePopupMenu.Items[0].Hint := Translator.TranslateControl('Code', 'MainToggleBookmarkHint');
  for i := 0 to CodePopupMenu.Items[0].Count - 1 do begin
    CodePopupMenu.Items[0].Items[i].Caption := Translator.TranslateControl('Code', 'Bookmark') + ' ' + IntToStr(i);
    CodePopupMenu.Items[0].Items[i].Hint := Translator.TranslateControl('Code', 'ToggleBookmarkHint') + ' ' + IntToStr(i);
  end;
  CodePopupMenu.Items[1].Caption := Translator.TranslateControl('Code', 'GotoBookmark');
  CodePopupMenu.Items[1].Hint := Translator.TranslateControl('Code', 'MainGotoBookmarkHint');
  for i := 0 to CodePopupMenu.Items[1].Count - 1 do begin
    CodePopupMenu.Items[1].Items[i].Caption := Translator.TranslateControl('Code', 'Bookmark') + ' ' + IntToStr(i);
    CodePopupMenu.Items[1].Items[i].Hint := Translator.TranslateControl('Code', 'GotoBookmarkHint') + ' ' + IntToStr(i);
  end;
  CodePopupMenu.Items[3].Caption := Translator.TranslateControl('Code', 'ChangeToUnsigned');
  CodePopupMenu.Items[3].Items[0].Caption := Translator.TranslateControl('Code', 'ChangeByte');
  CodePopupMenu.Items[3].Items[1].Caption := Translator.TranslateControl('Code', 'ChangeWord');
  CodePopupMenu.Items[3].Items[2].Caption := Translator.TranslateControl('Code', 'ChangeDword');
  CodePopupMenu.Items[3].Items[3].Caption := Translator.TranslateControl('Code', 'ChangeQword');
  CodePopupMenu.Items[4].Caption := Translator.TranslateControl('Code', 'ChangeToSigned');
  CodePopupMenu.Items[4].Items[0].Caption := Translator.TranslateControl('Code', 'ChangeByte');
  CodePopupMenu.Items[4].Items[1].Caption := Translator.TranslateControl('Code', 'ChangeWord');
  CodePopupMenu.Items[4].Items[2].Caption := Translator.TranslateControl('Code', 'ChangeDword');
  CodePopupMenu.Items[4].Items[3].Caption := Translator.TranslateControl('Code', 'ChangeQword');
  CodePopupMenu.Items[5].Caption := Translator.TranslateControl('Code', 'ChangeToFloat');
  CodePopupMenu.Items[5].Items[0].Caption := Translator.TranslateControl('Code', 'ChangeSingle');
  CodePopupMenu.Items[5].Items[1].Caption := Translator.TranslateControl('Code', 'ChangeDouble');
  CodePopupMenu.Items[5].Items[2].Caption := Translator.TranslateControl('Code', 'ChangeExtended');
  CodePopupMenu.Items[6].Caption := Translator.TranslateControl('Code', 'ChangeToString');
  CodePopupMenu.Items[6].Items[0].Caption := Translator.TranslateControl('Code', 'ChangePascal');
  CodePopupMenu.Items[6].Items[1].Caption := Translator.TranslateControl('Code', 'ChangeC');
  CodePopupMenu.Items[7].Caption := Translator.TranslateControl('Code', 'AdvancedDataChange');
  CodePopupMenu.Items[9].Caption := Translator.TranslateControl('Code', 'Disassemble');
  CodePopupMenu.Items[10].Caption := Translator.TranslateControl('Code', 'AdvancedDisassemble');
  CodePopupMenu.Items[12].Caption := Translator.TranslateControl('Code', 'Insert');
  CodePopupMenu.Items[12].Items[0].Caption := Translator.TranslateControl('Code', 'InsertComment');
  CodePopupMenu.Items[12].Items[1].Caption := Translator.TranslateControl('Code', 'InsertEmpty');
  CodePopupMenu.Items[13].Caption := Translator.TranslateControl('Code', 'Remove');
end;


// Navigation events


procedure TCodeTabFrame.GotoEntrypointButtonClick(Sender: TObject);     // Premiestnenie na Entrypoint
begin
  GotoPosition(fSection.GetPosition(fSection.EntryPointAddress + fSection.MemOffset), soBeginning);
  plocha.SetFocus;
end;



procedure TCodeTabFrame.GotoAddressButtonClick(Sender: TObject);    // Premiestnenie na zadanu adresu
begin
  GotoAddressForm.MinAddress := fSection.MemOffset;
  GotoAddressForm.MaxAddress := fSection.MaxAddress;
  if GotoAddressForm.ShowModal = mrOK then
    GotoPosition(fSection.GetPosition(GotoAddressForm.Address), soBeginning);
  plocha.SetFocus;
end;



procedure TCodeTabFrame.GotoLineClick(Sender: TObject);    // Premiestnenie na zadane cislo riadka
begin
  GotoLineForm.MaxLineIndex := fSection.Disassembled.Count;
  if GotoLineForm.ShowModal = mrOK then
    GotoPosition(GotoLineForm.LineIndex - 1, soBeginning);
  plocha.SetFocus;
end;



procedure TCodeTabFrame.FollowButtonClick(Sender: TObject);         // Nasledovanie skoku
var
  SourceAddressPtr: ^Cardinal;
begin
  New(SourceAddressPtr);
  SourceAddressPtr^ := GetLineAddress(plocha.Lines[fSection.FindAddressableLine(Plocha.CaretY - 1)]);
  fJumpStack.Push(SourceAddressPtr);
  GotoPosition(fSection.GetPosition(fTargetAddress), soBeginning);
  UpdateActions;
  plocha.SetFocus;
end;



procedure TCodeTabFrame.ReturnButtonClick(Sender: TObject);         // Navrat skoku
var
  SourceAddressPtr: ^Cardinal;
begin
  SourceAddressPtr := fJumpStack.Pop;
  GotoPosition(fSection.GetPosition(SourceAddressPtr^), soBeginning);
  UpdateActions;
  plocha.SetFocus;
end;



procedure TCodeTabFrame.GotoBookmarkClick(Sender: TObject);
begin
  Plocha.GotoBookMark((Sender as TMenuItem).MenuIndex);
end;



procedure TCodeTabFrame.ToggleBookMarkClick(Sender: TObject);
var
  Column, Row: Integer;
  BookmarkIndex: Integer;
begin
  BookmarkIndex := (Sender as TMenuItem).MenuIndex;
  plocha.GetBookMark(BookmarkIndex, Column, Row);

  // Clear bookmark
  if plocha.CaretY = Row then begin
    plocha.ClearBookMark(BookmarkIndex);
    MainForm.ToggleBookmarks.Items[BookmarkIndex].Checked := False;
    MainForm.GotoBookmarks.Items[BookmarkIndex].Checked := False;
    CodePopupMenu.Items[0].Items[BookmarkIndex].Checked := False;
    CodePopupMenu.Items[1].Items[BookmarkIndex].Checked := False;
  end

  // Set bookmark
  else begin
    Plocha.SetBookMark(BookmarkIndex, plocha.CaretX, plocha.CaretY);
    MainForm.ToggleBookmarks.Items[BookmarkIndex].Checked := True;
    MainForm.GotoBookmarks.Items[BookmarkIndex].Checked := True;
    CodePopupMenu.Items[0].Items[BookmarkIndex].Checked := True;
    CodePopupMenu.Items[1].Items[BookmarkIndex].Checked := True;
  end;
end;


// Editing events


procedure TCodeTabFrame.ChangeToUnsignedDataClick(Sender: TObject);
var
  Options: TDataChangeOptions;
begin
  Options.Signed := False;
  Options.Option := dcItems;
  Options.Value := 1;
  case (Sender as TMenuItem).MenuIndex of
    0: Options.DataType := dtByte;
    1: Options.DataType := dtWord;
    2: Options.DataType := dtDword;
    3: Options.DataType := dtQword;
  end;
  ChangeToData(Options);
end;



procedure TCodeTabFrame.ChangeToSignedDataClick(Sender: TObject);
var
  Options: TDataChangeOptions;
begin
  Options.Signed := True;
  Options.Option := dcItems;
  Options.Value := 1;
  case (Sender as TMenuItem).MenuIndex of
    0: Options.DataType := dtByte;
    1: Options.DataType := dtWord;
    2: Options.DataType := dtDword;
    3: Options.DataType := dtQword;
  end;
  ChangeToData(Options);
end;



procedure TCodeTabFrame.ChangeToFloatDataClick(Sender: TObject);
var
  Options: TDataChangeOptions;
begin
  Options.Signed := False;
  Options.Option := dcItems;
  Options.Value := 1;
  case (Sender as TMenuItem).MenuIndex of
    0: Options.DataType := dtSingle;
    1: Options.DataType := dtDouble;
    2: Options.DataType := dtDoubleEx;
  end;
  ChangeToData(Options);
end;



procedure TCodeTabFrame.ChangeToStringDataClick(Sender: TObject);
var
  Options: TDataChangeOptions;
begin
  Options.Signed := False;
  Options.Option := dcItems;
  Options.Value := 1;
  case (Sender as TMenuItem).MenuIndex of
    0: Options.DataType := dtPascalStr;
    1: Options.DataType := dtCStr;
    2: Options.DataType := dtPascalUniCodeStr;
    3: Options.DataType := dtCUniCodeStr;
  end;
  ChangeToStringData(Options);
end;



procedure TCodeTabFrame.AdvancedChangeToDataClick(Sender: TObject);
var
  LineIndex: Cardinal;
  MaxAddressMinValue: Cardinal;
begin
  LineIndex := fSection.FindAddressableLine(plocha.CaretY - 1);
  if LineIndex = $FFFFFFFF then
    MaxAddressMinValue := $FFFFFFFF
  else
    MaxAddressMinValue := GetLineAddress(plocha.Lines.Strings[LineIndex]);

  AdvancedChangingToDataForm.SetMaxAdressMinValue(MaxAddressMinValue);
  AdvancedChangingToDataForm.SetMaxAdressMaxValue(fSection.MaxAddress);
  if AdvancedChangingToDataForm.ShowModal = mrOK then
    ChangeToData(AdvancedChangingToDataForm.Options);
end;



procedure TCodeTabFrame.NormalDisassembleClick(Sender: TObject);
var
  Options: TDisassembleFormOptions;
begin
  Options.Option := dtNormal;
  Options.Value := 0;
  Options.Bit32 := fSection.Bit32;
  Options.Recursive := True;

  Disassemble(Options);
end;



procedure TCodeTabFrame.AdvancedDisassembleClick(Sender: TObject);
begin
  // podobne by sa zislo nastavit MaxValue aj pre ostatne edity
  AdvancedDisassembleForm.SetMaxAddressMaxValue(fSection.MaxAddress);
  if AdvancedDisassembleForm.ShowModal = mrOK then
    Disassemble(AdvancedDisassembleForm.Options);
end;



procedure TCodeTabFrame.InsertCommentClick(Sender: TObject);
begin
  if InsertCommentForm.ShowModal = mrOK then begin
    plocha.Lines.Insert(plocha.CaretY - 1, '; ' + InsertCommentForm.InsertCommentEdit.Text);
    PlochaStatusChange(self, [scCaretY]);
  end;
end;



procedure TCodeTabFrame.InsertEmptyLineClick(Sender: TObject);
begin
  plocha.Lines.Insert(plocha.CaretY - 1, '');
  PlochaStatusChange(self, [scCaretY, scModified]);
end;



procedure TCodeTabFrame.RemoveLineClick(Sender: TObject);
begin
  plocha.Lines.Delete(plocha.CaretY - 1);
  PlochaStatusChange(self, [scCaretY, scModified]);
end;


// Conversion routines

procedure TCodeTabFrame.ChangeToData(Options: TDataChangeOptions);

  function ComputeNewLinesCount(Options: TDataChangeOptions; StartAddress, Index: Cardinal): Integer;
  var
    i: Cardinal;
  begin
    if Options.Option = dcCode then
      for i := Index to plocha.Lines.Count - 1 do begin
        if (GetLineType(plocha.Lines[i]) <> ltInstruction) then
          Continue;
        if not IsCodeInstructionStr(Copy(plocha.Lines[i], ilInstructionMnemonicIndex, MaxInt)) then
          Continue;

        Options.Option := dcMaxAddress;
        Options.Value := GetLineAddress(plocha.Lines[i]);
        Break;
      end;

    if Options.Option = dcMaxAddress then
      Options.Value := Options.Value - fSection.MemOffset;

    Result := GetNewLinesCount(fSection.CodeSize, Options, StartAddress);
  end;

var
  i: Integer;
  ItemSize: Cardinal;
  LineIndex: Cardinal;
  StartAddress: Cardinal; // memory address
  StartOffset: Cardinal;
  NewLinesCount: Cardinal;

begin
  ItemSize := DataTypeSizes[Options.datatype];

  LineIndex := fSection.FindAddressableLine(plocha.CaretY - 1);
  if LineIndex = $FFFFFFFF then
    Exit;

  StartAddress := GetLineAddress(plocha.Lines.Strings[LineIndex]);
  StartOffset := StartAddress - fSection.MemOffset;
  NewLinesCount := ComputeNewLinesCount(Options, StartOffset, LineIndex);
  if NewLinesCount = 0 then
    Exit;

  // Clear relevant parts of DisassemblerMap
  for i := StartOffset to StartOffset + NewLinesCount * ItemSize - 1 do
    fSection.DisassemblerMap[i] := Byte(fSection.DisassemblerMap[i] and (maxByte - dfInstruction - dfPart));

  fSection.ReplaceLines(
    LineIndex,
    StartOffset,
    ItemSize * NewLinesCount,
//    fSection.GetLineFromDataEx(fSection.CodeArray[StartOffset], Options.DataType, Options.Signed, StartAddress, NewLinesCount)
    GetLineFromDataEx(fSection.CodeArray[StartOffset], Options.DataType, Options.Signed, StartAddress, NewLinesCount)
  );
  if Assigned(fOnChangeDisassembled) then
    fOnChangeDisassembled(plocha);
end;



procedure TCodeTabFrame.ChangeToStringData(Options: TDataChangeOptions);
var
  i: Cardinal;
  Line: string;
  StrLength8: Byte;
  StrLength16: Word;
  NewLines: TStrings;
  Sign: string;
  TheWideChar: Widechar;
  LineIndex: Cardinal;
  StartAddress, StartOffset: Cardinal;
  TheString: WideString;
  StringSize: Cardinal;
begin
  // zistime index riadku s adresou
  LineIndex := fSection.FindAddressableLine(plocha.CaretY - 1);
  if LineIndex = $FFFFFFFF then
    Exit;

  StartAddress := GetLineAddress(plocha.Lines.Strings[LineIndex]);
  StartOffset := StartAddress - fSection.MemOffset;

  fSection.CodeStream.Position := StartOffset;
  StrLength8 := 0;
  StrLength16 := 0;
  case Options.DataType of
    dtPascalStr: begin
      Sign := 'p';
      fSection.CodeStream.Read(StrLength8, 1);
      if (StartOffset + StrLength8) > fSection.CodeSize then
        Exit;
      SetLength(line, StrLength8);
      fSection.CodeStream.Read(line[1], StrLength8);
      TheString := line;
      StringSize := Strlength8 + 1;
    end;

    dtCStr: begin
      Sign := 'c';
      ReadStringFromStream(fSection.CodeStream, fSection.CodeStream.Position, line);
      TheString := line;
      StringSize := Length(TheString) + 1;
    end;

    dtPascalUniCodeStr: begin
      Sign := 'pu';
      fSection.CodeStream.Read(StrLength16, 2);
      if (StartOffset + 2 * StrLength16) > fSection.CodeSize then
        Exit;
      SetLength(TheString, StrLength16);
      fSection.CodeStream.Read(TheString[1], StrLength16 * 2);
      StringSize := Strlength16 + 2;
    end;

    // napr. cmd.exe
    dtCUniCodeStr: begin
      Sign := 'cu';
      fSection.CodeStream.Read(TheWideChar, 2);
      while TheWideChar <> #0 do begin
        Inc(StrLength16);
        TheString := TheString + TheWideChar;
        fSection.CodeStream.Read(TheWideChar, 2);
      end;
      StringSize := Strlength16 * 2 + 2;
    end;
    else
      raise EIllegalState.Create('ChangeToStringData: Bad DataType');
  end;

  NewLines := TStringList.Create;
  NewLines.Add(IntToHex(StartAddress, 8) + StringRightPad(' bytes: ' + IntToHex(StringSize, 8) + '(hex)', 1 + ilMaxParsedLength + 1) + Sign + 'string ''' + TheString + '''');
  for i := StartOffset to StartOffset + StringSize - 1 do
    fSection.DisassemblerMap[i] := Byte(fSection.DisassemblerMap[i] and (maxByte - dfInstruction - dfPart));

  fSection.ReplaceLines(LineIndex, StartOffset, StringSize, NewLines);
  if Assigned(fOnChangeDisassembled) then
    fOnChangeDisassembled(plocha);
end;



procedure TCodeTabFrame.Disassemble(Options: TDisassembleFormOptions);
var
  LineIndex: Integer;
  DisOptions: TDisassembleOptions;
begin
  // Find line containing an instruction
  LineIndex := plocha.CaretY - 1;
  while GetLineType(plocha.Lines[LineIndex]) <> ltInstruction do begin
    Inc(LineIndex);
    if LineIndex = plocha.Lines.Count then
      Exit;
  end;

  // Set options for disassembling
  DisOptions.Address := GetLineAddress(plocha.Lines[LineIndex]);
  case Options.Option of
    dtBytes: DisOptions.Size := Options.Value;
    dtMaxAddress:
      if Options.Value > DisOptions.Address then
        DisOptions.Size := Options.Value - DisOptions.Address
      else
        Exit;

    dtNormal: DisOptions.Size := fSection.CodeSize - (DisOptions.Address - fSection.MemOffset);
  end;
  DisOptions.Bit32 := Options.Bit32;
  DisOptions.Recursive := Options.Recursive;

  // Disassemble // and move carret to the first instruction
  ProgressForm.Execute(TDisassemblePartThread.Create(fSection, DisOptions));
  GotoPosition(fSection.GetPosition(DisOptions.Address), soBeginning);
  if Assigned(fOnChangeDisassembled) then
    fOnChangeDisassembled(plocha);
end;



function TCodeTabFrame.GetSection: TSection;
begin
  Result := fSection;
end;



procedure TCodeTabFrame.UpdateActions;
begin
  with MainForm do begin
    actFollowJump.Enabled := fCanFollowJump;
    actReturnJump.Enabled := (self.fJumpStack.Count > 0);
    actGotoEntryPoint.Enabled := fSection.HasEntryPoint;
    actGoToAddress.Enabled := (fSection.CodeSize > 0);
  end;
end;

{$R *.dfm}



end.
