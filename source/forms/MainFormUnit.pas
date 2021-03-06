unit MainFormUnit;

interface

uses
  Controls, Forms, Dialogs, ActnList,
  Graphics, StdCtrls, ComCtrls, ImgList, Menus, ExtCtrls, Grids, Buttons,
  SysUtils,
  Classes,
  IniFiles,
  Contnrs,
  StrUtils,
  // project units
  SynEdit,
  ExceptionsUnit,
  GlobalsUnit,
  FilesUnit,
  Translatables,
  StringRes,
  procmat,
  SectionUnit,
  TabFrameTemplateUnit,
  FileTabFrameUnit,
  CodeTabFrameUnit,
  ExecFileManagerUnit,
  ExecFileUnit,
  CodeSectionUnit,
//  HexEditFormUnit,
  LoggerUnit,
  ProgressThreads;

type

  { TMainForm }

  TMainForm = class(TForm, ITranslatable)
    OpenFileOpenDialog: TOpenDialog;
    MainPageControl: TPageControl;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    OpenFile1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Export1: TMenuItem;
    Search1: TMenuItem;
    Help1: TMenuItem;
    N2: TMenuItem;
    About1: TMenuItem;
    FindText1: TMenuItem;
    Searchagain1: TMenuItem;
    MenuImageList: TImageList;
    OpenProject1: TMenuItem;
    CloseFile1: TMenuItem;
    SaveProjectSaveDialog: TSaveDialog;
    Help2: TMenuItem;
    FindDialog1: TFindDialog;
    Settings1: TMenuItem;
    LanguagesMenuItem: TMenuItem;
    StatusBar2: TStatusBar;
    Goto1: TMenuItem;
    GotoEntrypoint1: TMenuItem;
    Gotoaddress1: TMenuItem;
    FollowJUMPCALL1: TMenuItem;
    ReturnfromJUMPCALL1: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    Disassemble1: TMenuItem;
    N5: TMenuItem;
    Edit1: TMenuItem;
    Disassemble2: TMenuItem;
    ChangeToUnsigned: TMenuItem;
    ChangeToString: TMenuItem;
    UByte: TMenuItem;
    UWord: TMenuItem;
    UDword: TMenuItem;
    ChangeToSigned: TMenuItem;
    SByte: TMenuItem;
    SWord: TMenuItem;
    SDword: TMenuItem;
    SQword: TMenuItem;
    UQword: TMenuItem;
    StringPascal: TMenuItem;
    StringC: TMenuItem;
    StringPascalUniCode: TMenuItem;
    StringCUnicode: TMenuItem;
    ChangeToFloat: TMenuItem;
    FSingle: TMenuItem;
    FDouble: TMenuItem;
    FExtended: TMenuItem;
    AdvancedDataChange: TMenuItem;
    N6: TMenuItem;
    Insert1: TMenuItem;
    N7: TMenuItem;
    AdvancedDisassemble: TMenuItem;
    GotoBookmarks: TMenuItem;
    GBookmark0: TMenuItem;
    GBookmark9: TMenuItem;
    GBookmark8: TMenuItem;
    GBookmark7: TMenuItem;
    GBookmark6: TMenuItem;
    GBookmark5: TMenuItem;
    GBookmark4: TMenuItem;
    GBookmark3: TMenuItem;
    GBookmark2: TMenuItem;
    GBookmark1: TMenuItem;
    ToggleBookmarks: TMenuItem;
    TBookmark9: TMenuItem;
    TBookmark8: TMenuItem;
    TBookmark7: TMenuItem;
    TBookmark6: TMenuItem;
    TBookmark5: TMenuItem;
    TBookmark4: TMenuItem;
    TBookmark3: TMenuItem;
    TBookmark2: TMenuItem;
    TBookmark1: TMenuItem;
    TBookmark0: TMenuItem;
    N8: TMenuItem;
    RemoveLine: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    N9: TMenuItem;
    Tools1: TMenuItem;
    Calculator1: TMenuItem;
    Options1: TMenuItem;
    Image1: TImage;
    InsertComment: TMenuItem;
    EmptyLine: TMenuItem;
    HexEditor1: TMenuItem;
    MainActionList: TActionList;
    actGoToEntryPoint: TAction;
    OpenProjectOpenDialog: TOpenDialog;
    actFollowJump: TAction;
    actReturnJump: TAction;
    actGoToAddress: TAction;
    Panel1: TPanel;
    Gotoline1: TMenuItem;
    actGoToLine: TAction;
    actOpen: TAction;
    actClose: TAction;
    actDisassemble: TAction;
    actSave: TAction;
    actOpenProject: TAction;
    SaveProject1: TMenuItem;

    procedure DisassembleClick(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure OpenProjectClick(Sender: TObject);
    procedure ExportClick(Sender: TObject);
    procedure CloseFileClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);

//    function CloseMainFile: boolean;// Zavretie suboru

    procedure About1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);

    procedure FindText1Click(Sender: TObject);
    procedure Help2Click(Sender: TObject);

    function Translatable: TTranslatable;

    procedure FindDialog1Find(Sender: TObject);
    procedure MainPageControlChange(Sender: TObject);
    procedure actGotoEntrypointExecute(Sender: TObject);
    procedure actGotoAddressExecute(Sender: TObject);
    procedure actFollowJUMPCALLExecute(Sender: TObject);
    procedure actReturnfromJUMPCALLExecute(Sender: TObject);
    procedure Calculator1Click(Sender: TObject);
    procedure Options1Click(Sender: TObject);

    procedure AdvancedDataChangeClick(Sender: TObject);
    procedure AdvancedDisassemble1Click(Sender: TObject);
    procedure Insert1Click(Sender: TObject);
    procedure TBookmarkClick(Sender: TObject);
    procedure GBookmarkClick(Sender: TObject);
    procedure RemoveLineClick(Sender: TObject);
    procedure ChangeStringClick(Sender: TObject);
    procedure Disassemble2Click(Sender: TObject);
    procedure ChangeUDataClick(Sender: TObject);
    procedure ChangeSDataClick(Sender: TObject);
    procedure ChangeFDataClick(Sender: TObject);
    procedure HexEditor1Click(Sender: TObject);
    procedure ProjectModified(Sender: TObject);
    procedure actGoToLineExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LanguageMenuItemClick(Sender: TObject);
    procedure actOpenExecute(Sender: TObject);
    procedure actOpenProjectExecute(Sender: TObject);
    procedure SaveProjectClick(Sender: TObject);

  private
    fopenfilepath: string;
    fOpenProjectPath: string;
    fSaveProjectPath: string;
    fModified: Boolean;

    function GetActivePageType: TPageType; // remove candidate
    function GetActiveFrame: TTabFrameTemplate;
    procedure SetModified(AModified: Boolean);
    procedure DoOpenFile(AFileName: string);
    procedure DoCloseProject;
    function DoSaveProject: Boolean;
    function AskToSaveAndCloseProject: Boolean;
  public
    ExecFile: TExecutableFile;

    OpenMyButton: TSpeedButton;
    DisassembleMyButton: TSpeedButton;
    ProjectMyButton: TSpeedButton;
    SaveMyButton: TSpeedButton;
//    HelpMyButton: TSpeedButton;

    sINI: TMemINIFile;

//    ActivePage: TTabSheetTemplate;

    function GetSectionsTabSheet(ASection: TSection): TTabSheetTemplate;

    procedure SetOpenFilePath(Path: string);
    procedure SetOpenProjectPath(Path: string);
    procedure SetSaveProjectPath(Path: string);

    procedure GuiProcessException(Sender: TObject; E: Exception);

    property OpenFilePath: string read fOpenFilePath write SetOpenFilePath;
    property OpenProjectPath: string read fOpenProjectPath write SetOpenProjectPath;
    property SaveProjectPath: string read fSaveProjectPath write SetSaveProjectPath;


    property ActivePageType: TPageType read GetActivePageType;
    property ActiveFrame: TTabFrameTemplate read GetActiveFrame;
    property Modified: Boolean read fModified write SetModified;
  end;


var
  MainForm: TMainForm;


implementation


uses
  StringUtilities,
  AboutBoxUnit,
  SaveOptionsFormUnit,
  CalculatorUnit,
  OptionsFormUnit,
  ProgressFormUnit,
  UnknownFileFormUnit,
  MessageFormUnit,
  TranslatorUnit;

{$R *.lfm}



procedure TMainForm.actOpenExecute(Sender: TObject);
begin
  OpenFileOpenDialog.FileName := '';
  OpenFileOpenDialog.InitialDir := OpenFilePath;
  if OpenFileOpenDialog.Execute then begin
    OpenFilePath := ExtractFilePath(OpenFileOpenDialog.FileName);
    DoOpenFile(OpenFileOpenDialog.FileName);
  end;
end;



procedure TMainForm.DoOpenFile(AFileName: string);
var
  i: Integer;
begin
  Application.ProcessMessages;

  if not AskToSaveAndCloseProject then
    Exit;

  try
    ExecFile := ExecFileManager.CreateNewExecFile(AFileName);
  except
    on E: EFileCorrupted do
      DisplayMessage(E.Message, mtError, [mbOK]);
    else
      raise;
  end;

  // Unknown file format - custom file format offer
  if ExecFile = nil then begin
    UnknownFileFormatForm.FileName := ExtractFileName(AFileName);
    UnknownFileFormatForm.FileSize := GetFileSize(AFileName);
    if UnknownFileFormatForm.ShowModal = mrOK then
      ExecFile := ExecFileManager.CreateNewCustomExecFile(AFileName, UnknownFileFormatForm.Parameters)
    else
      Exit;
  end;

  Modified := False;

  // Create section tabs and frames
  TTabSheetTemplate.CreateFileTab(ExecFile);
  for i := 0 to ExecFile.Sections.Count - 1 do
    if ExecFile.Sections[i].Typ <> stCode then
      TTabSheetTemplate.Create(ExecFile.Sections[i]);
  MainPageControl.ActivePageIndex := 0;
  MainPageControlChange(nil);

  HexEditor1.Enabled := True;
  CloseFile1.Enabled := True;
  DisassembleMyButton.Enabled := True;
  Disassemble1.Enabled := True;
  Caption := TatraDASFullNameVersion + ' - ' + ExecFile.FileName;
end;



procedure TMainForm.DisassembleClick(Sender: TObject);
var
  PageIndex, SectionIndex: Integer;
  DisassembleThread: TProgressThread;
begin
  // Set TatraDAS to non-disassembled state
  SaveMyButton.Enabled := False;
  SaveProject1.Enabled := False;
  Export1.Enabled := False;
  // Clear code tabs
  for PageIndex := MainPageControl.PageCount - 1 downto 0 do
    if (MainPageControl.Pages[PageIndex] as TTabSheetTemplate).PageType = ttCode then
      MainPageControl.Pages[PageIndex].Free;

  // Disassemble
  DisassembleThread := TDisassembleThread.Create(ExecFile);
  try
    ProgressForm.Execute(DisassembleThread);
  finally
    FreeAndNil(DisassembleThread);
  end;

  //ExecFile.Disassemble; //= non-thread way

  SaveMyButton.Enabled := True;
  SaveProject1.Enabled := True;
  Export1.Enabled := True;
  Modified := True;

  // Create section tabs and frames
  for SectionIndex := 0 to ExecFile.Sections.Count - 1 do
    if ExecFile.Sections[SectionIndex].Typ = stCode then
      if (ExecFile.Sections[SectionIndex] as TCodeSection).IsDisassembled then begin
        TTabSheetTemplate.Create(ExecFile.Sections[SectionIndex]);
        ((MainPageControl.Pages[MainPageControl.PageCount - 1] as TTabSheetTemplate).Frame as TCodeTabFrame).OnChangeDisassembled := ProjectModified;
      end;
end;



procedure TMainForm.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  if Length(FileNames) = 1 then
    DoOpenFile(FileNames[0]);
end;



procedure TMainForm.ExportClick(Sender: TObject);
var
  ExportThread: TProgressThread;
begin
  if SaveOptionsForm.ShowModal = mrOK then begin
    SaveProjectSaveDialog.FileName := '';
    SaveProjectSaveDialog.InitialDir := SaveProjectPath;
    case SaveOptionsForm.ExportOption of
      eoDAS: SaveProjectSaveDialog.Filter := SaveDisassemblyFilterStr + '(*.DAS)|*.DAS';
      eoNASM: SaveProjectSaveDialog.Filter := '(*.ASM)|*.ASM';
      else
        SaveProjectSaveDialog.Filter := '';
    end;
    if SaveProjectSaveDialog.Execute then begin
      SaveProjectPath := ExtractFilePath(SaveProjectSaveDialog.FileName);
      ExportThread := TExportThread.Create(ExecFileManager, ExecFile, SaveProjectSaveDialog.FileName, SaveOptionsForm.ExportOption, SaveOptionsForm.ExportCustomDASOptions);
      try
        ProgressForm.Execute(ExportThread);
      finally
        FreeAndNil(ExportThread);
      end;
    end;
  end;
end;



procedure TMainForm.SaveProjectClick(Sender: TObject);
begin
  DoSaveProject;
end;


{
  Returns True after successful saving, False if user did not choose target file. 
}
function TMainForm.DoSaveProject: Boolean;
var
  SaveThread: TProgressThread;
begin
  SaveProjectSaveDialog.Filter := ProjectFilterStr + '(*.DHF)|*.DHF';
  if SaveProjectSaveDialog.Execute then begin
    SaveProjectPath := ExtractFilePath(SaveProjectSaveDialog.FileName);
    SaveThread := TSaveThread.Create(ExecFileManager, ExecFile, SaveProjectSaveDialog.FileName);
    try
      ProgressForm.Execute(SaveThread);
    finally
      FreeAndNil(SaveThread);
    end;

    Modified := False;
    Result := True;
  end
  else
    Result := False;
end;



procedure TMainForm.actOpenProjectExecute(Sender: TObject);
begin
  OpenProjectClick(Sender);
end;



procedure TMainForm.OpenProjectClick(Sender: TObject);
var
  LoadThread: TProgressThread;
  i: Integer;
begin
  OpenProjectOpenDialog.Filename := '';
  OpenProjectOpenDialog.InitialDir := OpenProjectPath;
  if not OpenProjectOpenDialog.Execute then
    Exit;
  OpenProjectPath := ExtractFilePath(OpenProjectOpenDialog.FileName);

  if not AskToSaveAndCloseProject then
    Exit;


  LoadThread := TLoadThread.Create(ExecFileManager, OpenProjectOpenDialog.FileName);
  try
    ProgressForm.Execute(LoadThread);
  finally
    FreeAndNil(LoadThread);
  end;

  ExecFile := ProgressData.Result;

  // Create file and section tabs and frames
  TTabSheetTemplate.CreateFileTab(ExecFile);
  // Load non-code sections
  for i := 0 to ExecFile.Sections.Count - 1 do
    if ExecFile.Sections[i].typ <> stCode then
      TTabSheetTemplate.Create(ExecFile.Sections[i]);
  // Load code sections
  for i := 0 to ExecFile.Sections.Count - 1 do
    if ExecFile.Sections[i].typ = stCode then
      if (ExecFile.Sections[i] as TCodeSection).IsDisassembled then begin
        TTabSheetTemplate.Create(ExecFile.Sections[i]);
        ((MainPageControl.Pages[MainPageControl.PageCount - 1] as TTabSheetTemplate).Frame as TCodeTabFrame).OnChangeDisassembled := ProjectModified;
      end;

  Modified := False;

  DisassembleMyButton.Enabled := False;
  SaveMyButton.Enabled := True;
  SaveProject1.Enabled := True;
  Export1.Enabled := True;
  CloseFile1.Enabled := True;
end;


// Zatvorenie otvoreneho suboru/projektu - uvolnenie prislusnych objektov
//a vycistenie uzivatelskeho rozhrania (zatvorenie tabov, nastavenie Enabled na tlacidlach, atd.) 
procedure TMainForm.DoCloseProject;
begin
  if ExecFile = nil then
    Exit;

  // Close tabs
  while MainPageControl.PageCount > 0 do
    MainPageControl.Pages[0].Free;

  // Buttons and menu items
  DisassembleMyButton.Enabled := False;
  Disassemble1.Enabled := False;
  CloseFile1.Enabled := False;
  SaveProject1.Enabled := False;
  Export1.Enabled := False;
  SaveMyButton.Enabled := False;
  HexEditor1.Enabled := False;

  // Other stuff
//  HexEditForm.Close;
  Modified := False;
  Caption := TatraDASFullNameVersion;

  FreeAndNil(ExecFile);
end;


{
//If project is modified, then ask user whether to save project.
Return False, if project is modified, and
1) user clicks cancel
2) user wants to save a cancels savings during file selection.
Otherwise return True (or exception).
}
function TMainForm.AskToSaveAndCloseProject: Boolean;
begin
  if Modified then begin
    case DisplayMessage(InjectStr(ProjectModifiedStr, ['''' + ExecFile.FileName + '''']), mtConfirmation, mbYesNoCancel) of
      mrYes: Result := DoSaveProject;
      mrNo: Result := True;
      mrCancel: Result := False;
      else
        Result := False; // Cannot happen
    end;
  end
  else
    Result := True;

  if Result then
    DoCloseProject;
end;


{
function TMainForm.CloseMainFile: boolean;
begin
  // nothing to close
  if ExecFile = nil then begin
    result := true;
    Exit;
  end;

  result := false;
  if Modified then begin
    case DisplayMessage(InjectStr(ProjectModifiedStr, ['''' + ExecFile.FileName + '''']), mtConfirmation, mbYesNoCancel) of
      mrYes: SaveProjectClick(nil);
      mrNo: ;
      mrCancel: Exit;
    end;
  end;
  DoCloseProject;
  result:= true;
end;
}



procedure TMainForm.CloseFileClick(Sender: TObject);
begin
  AskToSaveAndCloseProject;
end;



procedure TMainForm.ExitClick(Sender: TObject);
begin
  Close;
end;



procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if not AskToSaveAndCloseProject then begin
    Action := caNone;
    Exit;
  end;
  sINI.WriteInteger('Settings', 'Top', Top);
  sINI.WriteInteger('Settings', 'Left', Left);
  sINI.WriteInteger('Settings', 'Height', Height);
  sINI.WriteInteger('Settings', 'Width', Width);
  sINI.WriteString('Settings', 'Language', Translator.ActiveLanguage.ShortCut);

  OptionsForm.SaveSettings(sINI);

  sINI.UpdateFile;
end;



procedure TMainForm.FormCreate(Sender: TObject);

  procedure CreateLanguagesMenuItems;
  var
    i: Integer;
    LanguageInfo: TLanguageInfo;
    MenuItem: TMenuItem;
  begin
    for i := 0 to Translator.AvailableLanguagesCount - 1 do begin
      LanguageInfo := Translator.AvailableLanguages[i];
      MenuItem := TMenuItem.Create(LanguagesMenuItem);
      MenuItem.Caption := LanguageInfo.Name;
      MenuItem.Hint := LanguageInfo.Hint;
      MenuItem.ImageIndex := MenuImageList.AddIcon(LanguageInfo.Icon);
      MenuItem.OnClick := LanguageMenuItemClick;
      LanguagesMenuItem.Add(MenuItem);
    end;
  end;

  function CreateSpeedButton(ButtonIndex: Integer; Action: TBasicAction; ResourceName: string): TSpeedButton;
  const
    Size: Integer = 72;
  begin
    Result := TSpeedButton.Create(self);
    Result.Parent := self;
    Result.Left := ButtonIndex * Size;
    Result.Width := Size;
    Result.Height := Size;
    Result.Action := Action;
    Result.Layout := blGlyphTop;
    Result.Flat := True;
    Result.Glyph.LoadFromResourceName(hinstance, ResourceName);
  end;

begin
  DoubleBuffered := True;

  Image1.Picture.Bitmap.LoadFromResourceName(hinstance, 'buttons_background');

  OpenMyButton := CreateSpeedButton(0, actOpen, 'open1');

  DisassembleMyButton := CreateSpeedButton(1, nil, 'disassemble1');
  DisassembleMyButton.OnClick := DisassembleClick;
  DisassembleMyButton.Enabled := False;

  ProjectMyButton := CreateSpeedButton(2, actOpenProject, 'project1');
  ProjectMyButton.Enabled := False;

  SaveMyButton := CreateSpeedButton(3, nil, 'save1');
  SaveMyButton.Enabled := False;
  SaveMyButton.OnClick := SaveProjectClick;

{ Keep commented until english documentation becomes available
  HelpMyButton := CreateSpeedButton(4, actOpen, 'help1');
  HelpMyButton.OnClick := Help2Click; }

  Caption := TatraDASFullNameVersion;
  StatusBar2.Panels[1].Text := TatraDASFullNameVersion;
///  ProcessLabel.Caption:='';

  sINI := TMemINIFile.Create(ExtractFilePath(Application.ExeName) + 'TatraDAS.ini');
  Top := sINI.ReadInteger('Settings', 'Top', 10);
  Left := sINI.ReadInteger('Settings', 'Left', 10);
//  Height:= INI.ReadInteger('Settings','Height',200);
//  Width:= INI.ReadInteger('Settings','Width',500);

  CreateLanguagesMenuItems;

  fOpenFilePath := sINI.ReadString('Paths', 'OpenFile', '');
  fOpenProjectPath := sINI.ReadString('Paths', 'OpenProject', '');
  fSaveProjectPath := sINI.ReadString('Paths', 'SaveProject', '');

//  OptionsForm.LoadSettings(sINI); - treba volat ked uz je OptionsForm vytvoreny

  Logger.Info('----- START -----');
end;



procedure TMainForm.About1Click(Sender: TObject);
begin
  AboutBox.ShowModal;
end;



procedure TMainForm.FormDestroy(Sender: TObject);
begin
  sINI.Free;
end;


//==============================================================================
//==============================================================================

procedure TMainForm.Help2Click(Sender: TObject);      // Spustenie helpu
begin
//  ShellExecute(Application.Handle, Pchar('open'), Pchar('hh.exe'), Pchar(ExtractFilePath(Application.ExeName) + 'doc' + PathDelim + 'TatraDAS.chm'), nil, sw_show);
end;



procedure TMainForm.LanguageMenuItemClick(Sender: TObject);
begin
  if not Translator.ChangeLanguage((Sender as TMenuItem).MenuIndex) then
    DisplayMessage(UnableToChangeLanguageStr, mtError, [mbOK]);
end;



function TMainForm.Translatable: TTranslatable;
var
  BookMarkIndex: Integer;
begin
  Result := TTranslatableGroups.Create([
    TTranslatableGroup.Create(
      'ButtonCaption',
      [
        TTranslatableCaption.Create(OpenMyButton, 'OpenFile'),
        TTranslatableCaption.Create(ProjectMyButton, 'OpenDisasm'),
        TTranslatableCaption.Create(DisassembleMyButton, 'Disassemble'),
        TTranslatableCaption.Create(SaveMyButton, 'SaveDisasm')
      ]
    ),
    TTranslatableGroup.Create(
      'ButtonHint',
      [
        TTranslatableHint.Create(OpenMyButton, 'OpenFile'),
        TTranslatableHint.Create(ProjectMyButton, 'OpenDisasm'),
        TTranslatableHint.Create(DisassembleMyButton, 'Disassemble'),
        TTranslatableHint.Create(SaveMyButton, 'SaveDisasm')
      ]
    ),
    TTranslatableGroup.Create(
      'MenuCaption',
      [
        TTranslatableMenuItem.Create(File1, 'file'),
        TTranslatableMenuItem.Create(Edit1, 'edit'),
        TTranslatableMenuItem.Create(Search1, 'search'),
        TTranslatableMenuItem.Create(Goto1, 'goto'),
        TTranslatableMenuItem.Create(Tools1, 'tools'),
        TTranslatableMenuItem.Create(Settings1, 'settings'),
        TTranslatableMenuItem.Create(Help1, 'help'),
        TTranslatableMenuItem.Create(OpenFile1, 'OpenFile'),
        TTranslatableMenuItem.Create(OpenProject1, 'OpenProject'),
        TTranslatableMenuItem.Create(Disassemble1, 'Disassemble'),
        TTranslatableMenuItem.Create(SaveProject1, 'SaveProject'),
        TTranslatableMenuItem.Create(Export1, 'Export'),
        TTranslatableMenuItem.Create(Closefile1, 'CloseFile'),
        TTranslatableMenuItem.Create(Exit1, 'Exit'),
        TTranslatableMenuItem.Create(FindText1, 'FindText'),
        TTranslatableMenuItem.Create(SearchAgain1, 'SearchAgain'),
        TTranslatableMenuItem.Create(Calculator1, 'Calculator'),
        TTranslatableMenuItem.Create(LanguagesMenuItem, 'Language'),
        TTranslatableMenuItem.Create(Help2, 'HelpTopic'),
        TTranslatableMenuItem.Create(About1, 'About')
      ]
    ),
    TTranslatableGroup.Create(
      'MenuHint',
      [
        TTranslatableMenuItemHint.Create(OpenFile1, 'OpenFile'),
        TTranslatableMenuItemHint.Create(OpenProject1, 'OpenProject'),
        TTranslatableMenuItemHint.Create(Disassemble1, 'Disassemble'),
        TTranslatableMenuItemHint.Create(SaveProject1, 'SaveProject'),
        TTranslatableMenuItemHint.Create(Export1, 'Export'),
        TTranslatableMenuItemHint.Create(Closefile1, 'CloseFile'),
        TTranslatableMenuItemHint.Create(Exit1, 'Exit'),
        TTranslatableMenuItemHint.Create(FindText1, 'FindText'),
        TTranslatableMenuItemHint.Create(SearchAgain1, 'SearchAgain'),
        TTranslatableMenuItemHint.Create(Calculator1, 'Calculator'),
        TTranslatableMenuItemHint.Create(HexEditor1, 'HexEditor'),
        TTranslatableMenuItemHint.Create(LanguagesMenuItem, 'Language'),
        TTranslatableMenuItemHint.Create(Help2, 'HelpTopic'),
        TTranslatableMenuItemHint.Create(About1, 'About')
      ]
    ),
    TTranslatableGroup.Create(
      'Code',
      [
        TTranslatableAction.Create(actGoToEntryPoint, 'EntrypointButton'),
        TTranslatableAction.Create(actGoToAddress, 'GotoAddressButton'),
        TTranslatableAction.Create(actGoToLine, 'GotoLineButton'),
        TTranslatableAction.Create(actReturnJump, 'ReturnButton'),
        TTranslatableAction.Create(actFollowJump, 'FollowButton'),
        TTranslatableMenuItem.Create(ChangeToUnsigned, 'ChangeToUnsigned'),
        TTranslatableMenuItem.Create(UByte, 'ChangeByte'),
        TTranslatableMenuItem.Create(UWord, 'ChangeWord'),
        TTranslatableMenuItem.Create(UDword, 'ChangeDword'),
        TTranslatableMenuItem.Create(UQword, 'ChangeQword'),
        TTranslatableMenuItem.Create(ChangeToSigned, 'ChangeToSigned'),
        TTranslatableMenuItem.Create(SByte, 'ChangeByte'),
        TTranslatableMenuItem.Create(SWord, 'ChangeWord'),
        TTranslatableMenuItem.Create(SDword, 'ChangeDword'),
        TTranslatableMenuItem.Create(SQword, 'ChangeQword'),
        TTranslatableMenuItem.Create(ChangeToFloat, 'ChangeToFloat'),
        TTranslatableMenuItem.Create(FSingle, 'ChangeSingle'),
        TTranslatableMenuItem.Create(FDouble, 'ChangeDouble'),
        TTranslatableMenuItem.Create(FExtended, 'ChangeExtended'),
        TTranslatableMenuItem.Create(ChangeToString, 'ChangeToString'),
        TTranslatableMenuItem.Create(StringPascal, 'ChangePascal'),
        TTranslatableMenuItem.Create(StringC, 'ChangeC'),
        TTranslatableMenuItem.Create(AdvancedDataChange, 'AdvancedDataChange'),
        TTranslatableMenuItem.Create(Disassemble2, 'Disassemble'),
        TTranslatableMenuItem.Create(AdvancedDisassemble, 'AdvancedDisassemble'),
        TTranslatableMenuItem.Create(Insert1, 'Insert'),
        TTranslatableMenuItem.Create(InsertComment, 'InsertComment'),
        TTranslatableMenuItem.Create(EmptyLine, 'InsertEmpty'),
        TTranslatableMenuItem.Create(RemoveLine, 'Remove'),
        TTranslatableMenuItem.Create(GotoEntrypoint1, 'EntrypointButtonHint'),
        TTranslatableMenuItem.Create(GotoAddress1, 'GotoAddressButtonHint'),
        TTranslatableMenuItem.Create(FollowJUMPCALL1, 'FollowButtonHint'),
        TTranslatableMenuItem.Create(ReturnFromJUMPCALL1, 'ReturnButtonHint')
      ]
    ),
    TTranslatableProcessText.Create('LabelCaption', ProcessText),
    TTranslatableBookmarks.Create('Code', ToggleBookmarks, 'Toggle'),
    TTranslatableBookmarks.Create('Code', GotoBookmarks, 'Goto')
  ]);

  // HelpMyButton.Hint:= Translator.TranslateControl('ButtonHint','Help');
  // StopMyButton.Hint:= Translator.TranslateControl('ButtonHint','StopDisasm');
end;



procedure TMainForm.FindText1Click(Sender: TObject);  // Otvorenie vyhladavacieho dialogu
begin
  if ActivePageType = ttCode then
    FindDialog1.Execute;
end;



procedure TMainForm.FindDialog1Find(Sender: TObject); // Vyhladavanie retazca
begin
  ((MainPageControl.ActivePage as TTabSheetTemplate).Frame as TCodeTabFrame).FindString(FindDialog1.FindText, FindDialog1.Options);
  FindDialog1.CloseDialog;
end;



procedure TMainForm.MainPageControlChange(Sender: TObject);
begin
  if ActivePageType = ttCode then begin
    Goto1.Enabled := True;
    Search1.Enabled := True;
    Edit1.Enabled := True;
    with (ActiveFrame as TCodeTabFrame) do begin
      UpdateActions;
    end;
  end
  else begin
    Goto1.Enabled := False;
    Search1.Enabled := False;
    Edit1.Enabled := False;
  end;
end;



function TMainForm.GetActiveFrame: TTabFrameTemplate;
begin
  Result := (MainPageControl.ActivePage as TTabSheetTemplate).Frame;
end;



procedure TMainForm.actGotoEntrypointExecute(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).GotoEntryPointButtonClick(self);
end;



procedure TMainForm.actGotoAddressExecute(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).GotoAddressButtonClick(self);
end;



procedure TMainForm.actGoToLineExecute(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).GotoLineClick(self);
end;



procedure TMainForm.actFollowJUMPCALLExecute(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).FollowButtonClick(self);
end;



procedure TMainForm.actReturnfromJUMPCALLExecute(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).ReturnButtonClick(self);
end;



procedure TMainForm.SetOpenFilePath(Path: string);
begin
  sINI.WriteString('Paths', 'OpenFile', Path);
  sINI.UpdateFile;
  fOpenFilePath := Path;
end;



procedure TMainForm.SetOpenProjectPath(Path: string);
begin
  sINI.WriteString('Paths', 'OpenProject', Path);
  sINI.UpdateFile;
  fOpenProjectPath := Path;
end;



procedure TMainForm.SetSaveProjectPath(Path: string);
begin
  sINI.WriteString('Paths', 'SaveProject', Path);
  sINI.UpdateFile;
  fSaveProjectPath := Path;
end;



procedure TMainForm.AdvancedDataChangeClick(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).AdvancedChangeToDataClick(Sender);
end;



procedure TMainForm.AdvancedDisassemble1Click(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).AdvancedDisassembleClick(Sender);
end;



procedure TMainForm.Insert1Click(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).InsertCommentClick(Sender);
end;



procedure TMainForm.RemoveLineClick(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).RemoveLineClick(Sender);
end;



procedure TMainForm.TBookmarkClick(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).ToggleBookmarkClick(Sender);
end;



procedure TMainForm.GBookmarkClick(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).GotoBookmarkClick(Sender);
end;



procedure TMainForm.ChangeUDataClick(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).ChangeToUnsignedDataClick(Sender);
end;



procedure TMainForm.ChangeSDataClick(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).ChangeToSignedDataClick(Sender);
end;



procedure TMainForm.ChangeFDataClick(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).ChangeToFloatDataClick(Sender);
end;



procedure TMainForm.ChangeStringClick(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).ChangeToStringDataClick(Sender);
end;



procedure TMainForm.Disassemble2Click(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).NormalDisassembleClick(Sender);
end;



procedure TMainForm.Calculator1Click(Sender: TObject);
begin
  Calculator.Show;
end;



procedure TMainForm.Options1Click(Sender: TObject);
begin
  OptionsForm.ShowModal;
end;



procedure TMainForm.HexEditor1Click(Sender: TObject);
begin
//  HexEditForm.OpenAndLoad(MainForm.ExecFile.FullPath);
end;



function TMainForm.GetActivePageType: TPageType;
begin
  Result := (MainPageControl.ActivePage as TTabSheetTemplate).PageType;
end;



function TMainForm.GetSectionsTabSheet(ASection: TSection): TTabSheetTemplate;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to MainPageControl.PageCount - 1 do
    if (MainPageControl.Pages[i] as TTabSheetTemplate).IsHavingSection(ASection) then
      Result := MainPageControl.Pages[i] as TTabSheetTemplate;
end;



procedure TMainForm.ProjectModified(Sender: TObject);
begin
  Modified := True;
end;



procedure TMainForm.SetModified(AModified: Boolean);
begin
  fModified := AModified;
  if ExecFile <> nil then begin
    if fModified then
      Caption := TatraDASFullNameVersion + ' - ' + ExecFile.FileName + '*'
    else
      Caption := TatraDASFullNameVersion + ' - ' + ExecFile.FileName;
  end
  else
    Caption := TatraDASFullNameVersion;
end;



procedure TMainForm.GuiProcessException(Sender: TObject; E: Exception);
begin
  ProcessException(E, ShowMessage);
end;



initialization
  ProcessText.Disassembling := 'Disassembling...';
  ProcessText.PreparingOutput := 'Preparing output...';
  ProcessText.LoadingDAS := 'Loading DAS file - Code Section #';
  ProcessText.LoadingDHF := 'Loading DHF file - Code Section #';
  ProcessText.SavingDAS := 'Saving DAS file - Code Section #';
  ProcessText.SavingDHF := 'Saving DHF file - Code Section #';

end.
