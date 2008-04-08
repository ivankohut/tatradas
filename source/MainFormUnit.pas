unit MainFormUnit;

{$INCLUDE 'delver.inc'}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
  Controls, Forms, Dialogs, ShellAPI,  ActnList,
  Graphics, StdCtrls, ComCtrls, ImgList, Menus, ExtCtrls, Grids,
{$ENDIF}
{$IFDEF LINUX}
  QDialogs, QImgList, QMenus, QTypes, QGraphics,
  QExtCtrls, QComCtrls, QGrids, QStdCtrls, QControls,
{$ENDIF}
  SysUtils,
  Classes,
  IniFiles,
  Contnrs,
  StrUtils,

  SynEdit,

  ButtonsX,
  TranslatorUnit,
  StringRes,
  procmat,
  SectionUnit,
  TabFrameTemplateUnit,
  FileTabFrameUnit,
  CodeTabFrameUnit,
  ExecFileManagerUnit,
  ExecFileUnit,
  CodeSectionUnit,
  HexEditFormUnit,
  LoggerUnit,
  ProgressThreads;


type

  TMainForm = class(TForm, ITranslatable)
    OpenFileOpenDialog: TOpenDialog;
    MainPageControl: TPageControl;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    OpenFile1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Save1: TMenuItem;
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
    Language1: TMenuItem;
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

    procedure OpenClick(Sender: TObject);
    procedure DisassembleClick(Sender: TObject);
    procedure OpenProjectClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure CloseFileClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);

    function CloseMainFile: boolean;// Zavretie suboru

    procedure About1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);

    procedure FindText1Click(Sender: TObject);
    procedure Help2Click(Sender: TObject);

    procedure Translate;

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

  private
    fopenfilepath: string;
    fOpenProjectPath: string;
    fSaveProjectPath: string;
    fModified: boolean;

    function GetActivePageType: TPageType; // remove candidate
    function GetActiveFrame: TTabFrameTemplate;
    procedure SetModified(AModified: boolean);
  public
    ExecFile: TExecutableFile;

  public
    OpenMyButton: TIvanSpeedButton;
    DisassembleMyButton: TIvanSpeedButton;
    ProjectMyButton: TIvanSpeedButton;
    SaveMyButton: TIvanSpeedButton;
    //HelpMyButton: TIvanSpeedButton;

    sINI: TMemINIFile;

//    ActivePage: TTabSheetTemplate;

    function GetSectionsTabSheet(ASection: TSection): TTabSheetTemplate;

    procedure SetOpenFilePath(path: string);
    procedure SetOpenProjectPath(path: string);
    procedure SetSaveProjectPath(path: string);

    property OpenFilePath:string read fOpenFilePath write SetOpenFilePath;
    property OpenProjectPath:string read fOpenProjectPath write SetOpenProjectPath;
    property SaveProjectPath:string read fSaveProjectPath write SetSaveProjectPath;


    property ActivePageType: TPageType read GetActivePageType;
    property ActiveFrame: TTabFrameTemplate read GetActiveFrame;
    property Modified: boolean read fModified write SetModified;
  end;


var
  MainForm: TMainForm;
  ExecFileManager: TExecFileManager;


implementation


uses
  AboutBoxUnit,
  SaveOptionsFormUnit,
  CalculatorUnit,
  OptionsFormUnit,
  ProgressFormUnit;

{$R *.DFM}



procedure TMainForm.OpenClick(Sender: TObject);       // Otvorenie suboru
var
  ErrorMessage: string;
  i: integer;
begin
  OpenFileOpenDialog.FileName:= '';
  OpenFileOpenDialog.InitialDir:= OpenFilePath;
  if not OpenFileOpenDialog.Execute then Exit;
  OpenFilePath:= ExtractFilePath(OpenFileOpenDialog.FileName);

  Application.ProcessMessages;

  if ExecFile <> nil then CloseFileClick(nil);

  ExecFile:= ExecFileManager.CreateNewExecFile(OpenFileOpenDialog.FileName);
  if ProgressData.ErrorStatus <> errNone then begin
    ExecFile.Free;
    case ProgressData.ErrorStatus of
      errCanceled: begin
        Exit;
      end;

      errOpen: begin
        ErrorMessage:=CouldNotOpenFileStr;
      end;
      errBadFormat: begin
        ErrorMessage:='File is corrupted: ';
      end;
      // Neznamy format, nemalo by nastat
      errUnknownFormat: begin
        ErrorMessage:='Unknown file format: ';
        raise Exception.Create('This should not occur !');
      end;
    end;
    MessageDlg(ErrorMessage + '"' + OpenFileOpenDialog.FileName + '"', mtError, [mbOK], 0);
    Exit;
  end;

  Modified := false;

  // Create section tabs and frames
  TTabSheetTemplate.CreateFileTab(ExecFile);
  for i := 0 to ExecFile.Sections.Count - 1 do
    if ExecFile.Sections[i].Typ <> stCode then
      TTabSheetTemplate.Create(ExecFile.Sections[i]);
  MainPageControl.ActivePageIndex:= 0;
  MainPageControlChange(nil);

  HexEditor1.Enabled:= true;
  CloseFile1.Enabled:= true;
  DisassembleMyButton.Enabled:= true;
  Disassemble1.Enabled:= true;
  Caption:= TatraDASFullNameVersion + ' - ' + ExecFile.FileName;
end;



procedure TMainForm.DisassembleClick(Sender: TObject);
var
  PageIndex, SectionIndex: integer;
begin
//  ProgressForm.Execute(TDisassembleThread.Create(ExecFile));
  ExecFile.Disassemble; //= non-thread way

  if ProgressData.ErrorStatus = errNone then begin
    SaveMyButton.Enabled:= true;
    Save1.Enabled:= true;
    Modified:= true;

    // Clear old code tabs
    for PageIndex := MainPageControl.PageCount - 1 downto 0 do
      if (MainPageControl.Pages[PageIndex] as TTabSheetTemplate).PageType = ttCode then
        MainPageControl.Pages[PageIndex].Free;

    // Create section tabs and frames
    for SectionIndex:= 0 to ExecFile.Sections.Count - 1 do
      if ExecFile.Sections[SectionIndex].Typ = stCode then
        if (ExecFile.Sections[SectionIndex] as TCodeSection).IsDisassembled then begin
          TTabSheetTemplate.Create(ExecFile.Sections[SectionIndex]);
          ((MainPageControl.Pages[MainPageControl.PageCount-1] as TTabSheetTemplate).Frame as TCodeTabFrame).OnChangeDisassembled:= ProjectModified;
        end;
  end
  else begin
    SaveMyButton.Enabled:= false;
    Save1.Enabled:= false;
    case ProgressData.ErrorStatus of
      errUserTerminated: ;
    end
  end;
end;



procedure TMainForm.SaveClick(Sender: TObject);    // Ulozenie projektu
begin
  if SaveOptionsForm.ShowModal = mrOK then begin
    SaveProjectSaveDialog.FileName:= '';
    SaveProjectSaveDialog.InitialDir:= SaveProjectPath;
    if soProject in SaveOptionsForm.SaveOptions then
      SaveProjectSaveDialog.Filter:=ProjectFilterStr + '(*.DHF)|*.DHF'
    else
      if soDisassembly in SaveOptionsForm.SaveOptions then
        SaveProjectSaveDialog.Filter:= SaveDisassemblyFilterStr + '(*.DAS)|*.DAS'
      else
        SaveProjectSaveDialog.Filter:= '';
    if not SaveProjectSaveDialog.Execute then
      Exit;

    ProgressForm.Execute(TSaveThread.Create(ExecFileManager, ExecFile, SaveProjectSaveDialog.FileName, SaveOptionsForm.SaveOptions));
    if ProgressData.ErrorStatus <> errNone then begin
      case ProgressData.ErrorStatus of
        errUserTerminated: ;
      end;
      Exit;
    end;
    Modified:= false;
    SaveProjectPath:= ExtractFilePath(SaveProjectSaveDialog.FileName);
  end;
end;



procedure TMainForm.OpenProjectClick(Sender: TObject);
var
  ErrorMessage: string;
  i: integer;
begin
  OpenProjectOpenDialog.Filename:= '';
  OpenProjectOpenDialog.InitialDir:= OpenProjectPath;
  if not OpenProjectOpenDialog.Execute then Exit;
  OpenProjectPath:= ExtractFilePath(OpenProjectOpenDialog.FileName);

  if ExecFile <> nil then
    CloseFileClick(nil);


  ProgressForm.Execute(TLoadThread.Create(ExecFileManager, OpenProjectOpenDialog.FileName));
  ExecFile:= ProgressData.Result;

  if ProgressData.ErrorStatus <> ErrNone then begin
    ExecFile.Free;
    case ProgressData.ErrorStatus of
      errCanceled: begin
        Exit;
      end;

      errDASNotFound:
        ErrorMessage:=CouldNotFindDASFileStr;
      errBadProjectVersion:
        ErrorMessage:= InCompatibleProjectVersion + '.' + #13 + CurrentVersion + ' ' + IntToHex(TatraDASProjectVersion,8) + '.';
      errOpen: begin
        ErrorMessage:=CouldNotOpenFileStr;
      end;
      errBadFormat: begin
        ErrorMessage:= 'File is corrupted: ';
      end;
      errUnspecified:
        ErrorMessage:= 'An error occured. Process stopped.';
    end;
    MessageDlg(ErrorMessage + '"' + OpenProjectOpenDialog.FileName + '"', mtError, [mbOK], 0);
    Exit;
  end;

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
        ((MainPageControl.Pages[MainPageControl.PageCount-1] as TTabSheetTemplate).Frame as TCodeTabFrame).OnChangeDisassembled:= ProjectModified;
      end;

  Modified:= false;

  DisassembleMyButton.Enabled:= false;
  SaveMyButton.Enabled:= true;
  Save1.Enabled:= true;
  CloseFile1.Enabled:= true;
end;



function TMainForm.CloseMainFile: boolean;
begin
  // nothing to close
  if ExecFile = nil then begin
    result := true;
    Exit;
  end;

  result := false;
  if Modified then begin
    case MessageDlg(AnsiReplaceStr(ProjectModifiedStr, '<project>', '''' + ExecFile.FileName + ''''), mtConfirmation, mbYesNoCancel, 0) of
      mrYes: SaveClick(nil);
      mrNo: ;
      mrCancel: Exit;
    end;
  end;

  while MainPageControl.PageCount > 0 do
    MainPageControl.Pages[0].Free;

  ExecFile.Free;
  ExecFile:= nil;
  Modified:= false;
  DisassembleMyButton.Enabled:= false;
  Disassemble1.Enabled:= false;
  CloseFile1.Enabled:= false;
  Save1.Enabled:= false;
  SaveMyButton.Enabled:= false;
  HexEditor1.Enabled:= false;
  HexEditForm.Close;
  Caption:= TatraDASFullNameVersion;
  result:= true;
end;



procedure TMainForm.ExitClick(Sender: TObject);
begin
  Close;
end;



procedure TMainForm.CloseFileClick(Sender: TObject);
begin
  CloseMainFile;
end;



procedure TMainForm.About1Click(Sender: TObject);
begin
  AboutBox.ShowModal;
end;



procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction); // Uzavretie MainForm
begin
  if not CloseMainFile then begin // Vycistenie ExecFile
    Action:=caNone;
    Exit;
  end;
  sINI.WriteInteger('Settings','Top',Top);
  sINI.WriteInteger('Settings','Left',Left);
  sINI.WriteInteger('Settings','Height',Height);
  sINI.WriteInteger('Settings','Width',Width);
  sINI.WriteString('Settings','Language', Translator.ShortCut);

  OptionsForm.SaveSettings(sINI);

  sINI.UpdateFile;
  sINI.Free;
end;



procedure TMainForm.FormCreate(Sender: TObject);      // Inicializacia
begin
  DoubleBuffered:= true;

  Image1.Picture.Bitmap.LoadFromResourceName(hinstance,'buttons_background');

  OpenMyButton:= TIvanSpeedButton.Create(self);
  OpenMyButton.Parent:=self;
  OpenMyButton.ObrMimo.LoadFromResourceName(hinstance,'open1');
  OpenMyButton.ObrNad.LoadFromResourceName(hinstance,'open2');
  OpenMyButton.Left:=0;
  OpenMyButton.OnClick:=OpenClick;
  OpenMyButton.Glyph:=OpenMyButton.ObrMimo;

  DisassembleMyButton:= TIvanSpeedButton.Create(self);
  DisassembleMyButton.Parent:=self;
  DisassembleMyButton.ObrMimo.LoadFromResourceName(hinstance,'disassemble1');
  DisassembleMyButton.ObrNad.LoadFromResourceName(hinstance,'disassemble2');
  DisassembleMyButton.Left:=72;
  DisassembleMyButton.OnClick:=DisassembleClick;
  DisassembleMyButton.Glyph:=DisassembleMyButton.ObrMimo;
  DisassembleMyButton.Enabled:=false;


  ProjectMyButton:= TIvanSpeedButton.Create(self);
  ProjectMyButton.Parent:=self;
  ProjectMyButton.ObrMimo.LoadFromResourceName(hinstance,'project1');
  ProjectMyButton.ObrNad.LoadFromResourceName(hinstance,'project2');
  ProjectMyButton.Left:=220;
  ProjectMyButton.OnClick:= OpenProjectClick;
  ProjectMyButton.Glyph:=ProjectMyButton.ObrMimo;

  SaveMyButton:= TIvanSpeedButton.Create(self);
  SaveMyButton.Parent:=self;
  SaveMyButton.ObrMimo.LoadFromResourceName(hinstance,'save1');
  SaveMyButton.ObrNad.LoadFromResourceName(hinstance,'save2');
  SaveMyButton.Left:=295;
  SaveMyButton.OnClick:=SaveClick;
  SaveMyButton.Glyph:=SaveMyButton.ObrMimo;
  SaveMyButton.Enabled:=false;

{ Keep commented until english documentation becomes available
  HelpMyButton:=TIvanSpeedButton.Create(self);
  HelpMyButton.Parent:=self;
  HelpMyButton.ObrMimo.LoadFromResourceName(hinstance,'help1');
  HelpMyButton.ObrNad.LoadFromResourceName(hinstance,'help2');
  HelpMyButton.Left:=400;
  HelpMyButton.OnClick:=Help2Click;
  HelpMyButton.Glyph:=HelpMyButton.ObrMimo;
}
  Caption:=TatraDASFullNameVersion;
  StatusBar2.Panels[1].Text:=TatraDASFullNameVersion;
///  ProcessLabel.Caption:='';

  sINI:= TMemINIFile.Create(ExtractFilePath(Application.ExeName)+'TatraDAS.ini');
  Top:= sINI.ReadInteger('Settings','Top',10);
  Left:= sINI.ReadInteger('Settings','Left',10);
//  Height:= INI.ReadInteger('Settings','Height',200);
//  Width:= INI.ReadInteger('Settings','Width',500);
//  CurrentLanguage:= sINI.ReadString('Settings','Language','eng');
// osetrit ak sa podari zmenit jazyk
  Translator:= TTranslator.Create(ExtractFilePath(Application.ExeName) + sINI.ReadString('Settings', 'LanguageFolder', PathDelim + 'languages'), Language1, MenuImageList);
  if not Translator.ChangeLanguage(sINI.ReadString('Settings','Language','en')) then
    if not Translator.ChangeLanguage('en') then begin
       ShowMessage(NoLanguageFilesStr);
       Application.Terminate;
    end;

  fOpenFilePath:= sINI.ReadString('Paths','OpenFile','');
  fOpenProjectPath:= sINI.ReadString('Paths','OpenProject','');
  fSaveProjectPath:= sINI.ReadString('Paths','SaveProject','');

//  OptionsForm.LoadSettings(sINI); - treba volat ked uz je OptionsForm vytvoreny

  ExecFileManager:=TExecFileManager.Create;

  Logger.AddListener(TTextFileLoggerListener.Create('disasm.log'));
  Logger.Info('----- START -----');
end;

//==============================================================================
//==============================================================================

procedure TMainForm.Help2Click(Sender: TObject);      // Spustenie helpu
begin
  ShellExecute(Application.Handle, Pchar('open'), Pchar('hh.exe'), Pchar(ExtractFilePath(Application.ExeName) + 'doc' + PathDelim + 'TatraDAS.chm'), nil, sw_show);
end;


procedure TMainForm.Translate;    // Zmena jazyka prostredia
var
  BookMarkIndex: integer;
begin
// Zmena popisov komponent

  //  [ButtonCaption]
  OpenMyButton.ChangeCaption(Translator.TranslateControl('ButtonCaption', 'OpenFile'));
  ProjectMyButton.ChangeCaption(Translator.TranslateControl('ButtonCaption','OpenDisasm'));
  DisassembleMyButton.ChangeCaption(Translator.TranslateControl('ButtonCaption','Disassemble'));
  SaveMyButton.ChangeCaption(Translator.TranslateControl('ButtonCaption','SaveDisasm'));
  //HelpMyButton.ChangeCaption(Translator.TranslateControl('ButtonCaption','Help'));

  // [MenuCaption]
  File1.Caption:= Translator.TranslateControl('MenuCaption','file');
  Edit1.Caption:= Translator.TranslateControl('MenuCaption','edit');
  Search1.Caption:= Translator.TranslateControl('MenuCaption','search');
  Goto1.Caption:= Translator.TranslateControl('MenuCaption','goto');
  Tools1.Caption:= Translator.TranslateControl('MenuCaption','tools');
  Settings1.Caption:= Translator.TranslateControl('MenuCaption','settings');
  Help1.Caption:= Translator.TranslateControl('MenuCaption','help');

  OpenFile1.Caption:= Translator.TranslateControl('MenuCaption','Openfile');
  OpenProject1.Caption:= Translator.TranslateControl('MenuCaption','OpenProject');
  Disassemble1.Caption:= Translator.TranslateControl('MenuCaption','Disassemble');
  Save1.Caption:= Translator.TranslateControl('MenuCaption','Save');
  Closefile1.Caption:= Translator.TranslateControl('MenuCaption','Closefile');
  Exit1.Caption:= Translator.TranslateControl('MenuCaption','Exit');
  FindText1.Caption:= Translator.TranslateControl('MenuCaption','Findtext');
  SearchAgain1.Caption:= Translator.TranslateControl('MenuCaption','SearchAgain');
  Calculator1.Caption:= Translator.TranslateControl('MenuCaption','Calculator');
  Language1.Caption:= Translator.TranslateControl('MenuCaption','Language');
  Options1.Caption:= Translator.TranslateControl('MenuCaption','Options');
  Help2.Caption:= Translator.TranslateControl('MenuCaption','HelpTopic');
  About1.Caption:= Translator.TranslateControl('MenuCaption','About');

  ToggleBookmarks.Caption:= Translator.TranslateControl('Code','ToggleBookmark');
  ToggleBookmarks.Hint:= Translator.TranslateControl('Code','MainToggleBookmarkHint');
  for BookMarkIndex:= 0 to 9 do begin
    ToggleBookmarks.Items[BookMarkIndex].Caption:= Translator.TranslateControl('Code', 'Bookmark') + ' ' + IntToStr(BookMarkIndex);
    ToggleBookmarks.Items[BookMarkIndex].Hint:= Translator.TranslateControl('Code', 'ToggleBookmarkHint') + ' ' + IntToStr(BookMarkIndex);
  end;
  GotoBookmarks.Caption:= Translator.TranslateControl('Code', 'GotoBookmark');
  GotoBookmarks.Hint:= Translator.TranslateControl('Code', 'MainGotoBookmarkHint');
  for BookMarkIndex:= 0 to 9 do begin
    GotoBookmarks.Items[BookMarkIndex].Caption:= Translator.TranslateControl('Code', 'Bookmark') + ' ' + IntToStr(BookMarkIndex);
    GotoBookmarks.Items[BookMarkIndex].Hint:= Translator.TranslateControl('Code', 'GotoBookmarkHint') + ' ' + IntToStr(BookMarkIndex);
  end;

  ChangeToUnsigned.Caption:= Translator.TranslateControl('Code','ChangeToUnsigned');
  UByte.Caption:= Translator.TranslateControl('Code','ChangeByte');
  UWord.Caption:= Translator.TranslateControl('Code','ChangeWord');
  UDword.Caption:= Translator.TranslateControl('Code','ChangeDword');
  UQword.Caption:= Translator.TranslateControl('Code','ChangeQword');
  ChangeToSigned.Caption:= Translator.TranslateControl('Code','ChangeToSigned');
  SByte.Caption:= Translator.TranslateControl('Code','ChangeByte');
  SWord.Caption:= Translator.TranslateControl('Code','ChangeWord');
  SDword.Caption:= Translator.TranslateControl('Code','ChangeDword');
  SQword.Caption:= Translator.TranslateControl('Code','ChangeQword');
  ChangeToFloat.Caption:= Translator.TranslateControl('Code','ChangeToFloat');
  FSingle.Caption:= Translator.TranslateControl('Code','ChangeSingle');
  FDouble.Caption:= Translator.TranslateControl('Code','ChangeDouble');
  FExtended.Caption:= Translator.TranslateControl('Code','ChangeExtended');
  ChangeToString.Caption:= Translator.TranslateControl('Code','ChangeToString');
  StringPascal.Caption:= Translator.TranslateControl('Code','ChangePascal');
  StringC.Caption:= Translator.TranslateControl('Code','ChangeC');
  AdvancedDataChange.Caption:= Translator.TranslateControl('Code','AdvancedDataChange');
  Disassemble2.Caption:= Translator.TranslateControl('Code','Disassemble');
  AdvancedDisassemble.Caption:= Translator.TranslateControl('Code','AdvancedDisassemble');
  Insert1.Caption:= Translator.TranslateControl('Code','Insert');
  InsertComment.Caption:= Translator.TranslateControl('Code','InsertComment');
  EmptyLine.Caption:= Translator.TranslateControl('Code','InsertEmpty');
  RemoveLine.Caption:= Translator.TranslateControl('Code','Remove');



  // [LabelCaption]

  ProcessText.Disassembling:= Translator.TranslateControl('LabelCaption','Disassembling');
  ProcessText.PreparingOutput:= Translator.TranslateControl('LabelCaption','Preparingoutput');
  ProcessText.LoadingDAS:= Translator.TranslateControl('LabelCaption','LoadingDAS');
  ProcessText.LoadingDHF:= Translator.TranslateControl('LabelCaption','LoadingDHF');
  ProcessText.SavingDAS:= Translator.TranslateControl('LabelCaption','SavingDAS');
  ProcessText.SavingDHF:= Translator.TranslateControl('LabelCaption','SavingDHF');

  GotoEntryPoint1.Caption:= Translator.TranslateControl('Code','EntrypointButton');
  GotoAddress1.Caption:= Translator.TranslateControl('Code','GotoAddressButton');
  FollowJUMPCALL1.Caption:= Translator.TranslateControl('Code','FollowButton');
  ReturnfromJUMPCALL1.Caption:= Translator.TranslateControl('Code','ReturnButton');

// Zmena hintov

//[ButtonHint]
  OpenMyButton.Hint:= Translator.TranslateControl('ButtonHint','OpenFile');
  ProjectMyButton.Hint:= Translator.TranslateControl('ButtonHint','OpenDisasm');
  DisassembleMyButton.Hint:= Translator.TranslateControl('ButtonHint','Disassemble');
  SaveMyButton.Hint:= Translator.TranslateControl('ButtonHint','SaveDisasm');
  //HelpMyButton.Hint:= Translator.TranslateControl('ButtonHint','Help');
//  StopMyButton.Hint:= Translator.TranslateControl('ButtonHint','StopDisasm');

//[MenuHint]
  OpenFile1.Hint:= Translator.TranslateControl('MenuHint','Openfile');
  OpenProject1.Hint:= Translator.TranslateControl('MenuHint','Opendisassembled');
  Disassemble1.Hint:= Translator.TranslateControl('MenuHint','Disassemble');
  Save1.Hint:= Translator.TranslateControl('MenuHint','Savedisassembled');
  Closefile1.Hint:= Translator.TranslateControl('MenuHint','Closefile');
  Exit1.Hint:= Translator.TranslateControl('MenuHint','Exit');

  FindText1.Hint:= Translator.TranslateControl('MenuHint','Findtext');
  SearchAgain1.Hint:= Translator.TranslateControl('MenuHint','SearchAgain');

//  Language1.Hint:= Translator.TranslateControl('MenuHint','Language');
//  Language1.Hint:= Translator.TranslateControl('MenuHint','Language');

  GotoEntrypoint1.Hint:= Translator.TranslateControl('Code','EntrypointButtonHint');
  GotoAddress1.Hint:= Translator.TranslateControl('Code','GotoAddressButtonHint');
  FollowJUMPCALL1.Hint:= Translator.TranslateControl('Code','FollowButtonHint');
  ReturnFromJUMPCALL1.Hint:= Translator.TranslateControl('Code','ReturnButtonHint');

  Calculator1.Hint:= Translator.TranslateControl('MenuHint','Calculator');
  HexEditor1.Hint:= Translator.TranslateControl('MenuHint','HexEditor');

  Language1.Hint:= Translator.TranslateControl('MenuHint','Language');
//  Options1.Hint:= Translator.TranslateControl('MenuHint','Options');

  Help2.Hint:= Translator.TranslateControl('MenuHint','Help');
  About1.Hint:= Translator.TranslateControl('MenuHint','About');

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
    Goto1.Enabled:= True;
    Search1.Enabled:= True;
    Edit1.Enabled:= True;
    with (ActiveFrame as TCodeTabFrame) do begin
      UpdateActions;
    end;
  end
  else begin
    Goto1.Enabled:= False;
    Search1.Enabled:= False;
    Edit1.Enabled:= False;
  end;
end;



function TMainForm.GetActiveFrame: TTabFrameTemplate;
begin
  result:= (MainPageControl.ActivePage as TTabSheetTemplate).Frame;
end;



procedure TMainForm.actGotoEntrypointExecute(Sender: TObject);
begin
 (ActiveFrame as TCodeTabFrame).GotoEntryPointButtonClick(self);
end;



procedure TMainForm.actGotoAddressExecute(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).GotoAddressButtonClick(self);
end;



procedure TMainForm.actFollowJUMPCALLExecute(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).FollowButtonClick(self);
end;



procedure TMainForm.actReturnfromJUMPCALLExecute(Sender: TObject);
begin
  (ActiveFrame as TCodeTabFrame).ReturnButtonClick(self);
end;



procedure TMainform.SetOpenFilePath(path: string);
begin
  sINI.WriteString('Paths','OpenFile',path);
  sINI.UpdateFile;
  fOpenFilePath:=path;
end;

procedure TMainform.SetOpenProjectPath(path: string);
begin
  sINI.WriteString('Paths','OpenProject',path);
  sINI.UpdateFile;
  fOpenProjectPath:=path;
end;

procedure TMainform.SetSaveProjectPath(path: string);
begin
  sINI.WriteString('Paths','SaveProject',path);
  sINI.UpdateFile;
  fSaveProjectPath:=path;
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
  HexEditForm.OpenAndLoad(MainForm.ExecFile.FullPath);
end;



function TMainForm.GetActivePageType: TPageType;
begin
  result:=(MainPageControl.ActivePage as TTabSheetTemplate).PageType;
end;


function TMainForm.GetSectionsTabSheet(ASection: TSection): TTabSheetTemplate;
var
  i: integer;
begin
  result:= nil;
  for i:=0 to MainPageControl.PageCount - 1 do
    if (MainPageControl.Pages[i] as TTabSheetTemplate).IsHavingSection(ASection) then
      result:= MainPageControl.Pages[i] as TTabSheetTemplate;
end;



procedure TMainForm.ProjectModified(Sender: TObject);
begin
  Modified:= true;
end;



procedure TMainForm.SetModified(AModified: boolean);
begin
  fModified:= AModified;
  if ExecFile <> nil then begin
    if fModified then
      Caption:= TatraDASFullNameVersion + ' - ' + ExecFile.FileName + '*'
    else
      Caption:= TatraDASFullNameVersion + ' - ' + ExecFile.FileName;
  end
  else
    Caption:= TatraDASFullNameVersion;
end;



initialization
  ProcessText.Disassembling:= 'Disassembling...';
  ProcessText.PreparingOutput:= 'Preparing output...';
  ProcessText.LoadingDAS:= 'Loading DAS file - Code Section #';
  ProcessText.LoadingDHF:= 'Loading DHF file - Code Section #';
  ProcessText.SavingDAS:= 'Saving DAS file - Code Section #';
  ProcessText.SavingDHF:= 'Saving DHF file - Code Section #';


end.
