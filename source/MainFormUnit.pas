unit MainFormUnit;

{$INCLUDE 'delver.inc'}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
  Controls, Forms, Dialogs, ShellAPI,
  StdCtrls, ComCtrls, ImgList, Menus, ExtCtrls, Grids,
{$ENDIF}
{$IFDEF LINUX}
  QDialogs, QImgList, QMenus, QTypes, QGraphics,
  QExtCtrls, QComCtrls, QGrids, QStdCtrls, QControls,
{$ENDIF}
  SysUtils,
  Classes,
  IniFiles,
  Contnrs,

  TatraDASFormUnit,
  ButtonsX,
  Languages,
  StringRes,
  procmat,
  SynEdit,
  SectionUnit,
  TabFrameTemplateUnit,
  FileTabFrameUnit,
  CodeTabFrameUnit,
  ExecFileManagerUnit,
  ExecFileUnit,
  CodeSectionUnit,
  HexEditFormUnit,
  ProgressThreads;


type

  TMainForm = class(TTatraDASForm)
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
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
    ImageList1: TImageList;
    OpenProject1: TMenuItem;
    CloseFile1: TMenuItem;
    SaveDialog1: TSaveDialog;
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

    procedure OpenClick(Sender: TObject);
    procedure DisassembleClick(Sender: TObject);
    procedure ProjectClick(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure CloseFileClick(Sender: TObject);
    procedure ExitClick(Sender: TObject);

    function CloseMainFile: boolean;// Zavretie suboru

    procedure About1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);

    procedure FindText1Click(Sender: TObject);
    procedure Help2Click(Sender: TObject);

    procedure Translate(ini:TMemINIFile); override;   // Zmena jazyka prostredia

    procedure FindDialog1Find(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure GotoEntrypoint1Click(Sender: TObject);
    procedure Gotoaddress1Click(Sender: TObject);
    procedure FollowJUMPCALL1Click(Sender: TObject);
    procedure ReturnfromJUMPCALL1Click(Sender: TObject);
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

private
    fopenfilepath:string;
    fOpenProjectPath: string;
    fSaveProjectPath: string;

{toto bolo v 2.9.8-pokus odstranene }
 ActivePageIndex: integer;
  function GetActivePageType: TPageType;
{ -- }

public
    Modified: boolean;
    ExecFile: TExecutableFile;
    procedure CreateSection(ASection: TSection);

public
    OpenMyButton: TIvanSpeedButton;
    DisassembleMyButton: TIvanSpeedButton;
    ProjectMyButton: TIvanSpeedButton;
    SaveMyButton: TIvanSpeedButton;
    HelpMyButton: TIvanSpeedButton;

    sINI: TMemINIFile;

//    ActivePage: TTabSheetTemplate;
    ActiveFrame: TTabFrameTemplate;

    function GetSectionsTabSheet(ASection: TSection): TTabSheetTemplate;

    procedure SetOpenFilePath(path: string);
    procedure SetOpenProjectPath(path: string);
    procedure SetSaveProjectPath(path: string);

    property OpenFilePath:string read fOpenFilePath write SetOpenFilePath;
    property OpenProjectPath:string read fOpenProjectPath write SetOpenProjectPath;
    property SaveProjectPath:string read fSaveProjectPath write SetSaveProjectPath;


    property ActivePageType: TPageType read GetActivePageType;

  end;



var
//  CurrentTab: integer;

  MainForm: TMainForm;

  OpenMode: boolean;
  PocetVyskytov: integer;
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
  OpenDialog1.Filter:= 'EXE, COM and DLL files(*.EXE;*.COM;*.DLL)|*.exe;*.com;*.dll;|All files(*.*)|*.*';
  OpenDialog1.FileName:= '';
  OpenDialog1.InitialDir:= OpenFilePath;
  if not OpenDialog1.Execute then Exit;
  OpenFilePath:= ExtractFilePath(OpenDialog1.FileName);

  Application.ProcessMessages;

  if ExecFile <> nil then CloseFileClick(nil);

  ExecFile:= ExecFileManager.CreateNewExecFile(OpenDialog1.FileName);
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
    MessageDlg(ErrorMessage + '"' + Opendialog1.FileName + '"', mtError, [mbOK], 0);
    Exit;
  end;

  Modified:=false;

  // Create section tabs and frames
  TTabSheetTemplate.CreateFileTab(ExecFile);
  for i := 0 to ExecFile.Sections.Count - 1 do
    if ExecFile.Sections[i].Typ <> stCode then
      TTabSheetTemplate.Create(ExecFile.Sections[i]);
  PageControl1.ActivePageIndex:= 0;
  PageControl1Change(nil);

  HexEditor1.Enabled:= true;
  CloseFile1.Enabled:= true;
  DisassembleMyButton.Enabled:= true;
  Disassemble1.Enabled:= true;
  Caption:= TatraDASFullNameVersion + ' - ' + ExecFile.FileName;
end;



procedure TMainForm.DisassembleClick(Sender: TObject);
var
  i: integer;
begin
  ProgressForm.Execute(TDisassembleThread.Create(ExecFile));
  if ProgressData.ErrorStatus = errNone then begin
    SaveMyButton.Enabled:= true;
    Save1.Enabled:= true;
    // Create section tabs and frames
    for i := 0 to ExecFile.Sections.Count - 1 do
      if ExecFile.Sections[i].Typ = stCode then
        if (ExecFile.Sections[i] as TCodeSection).IsDisassembled then
          TTabSheetTemplate.Create(ExecFile.Sections[i]);
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
    SaveDialog1.FileName:= '';
    SaveDialog1.InitialDir:= SaveProjectPath;
    if soProject in SaveOptionsForm.SaveOptions then
      SaveDialog1.Filter:=ProjectFilterStr + '(*.DHF)|*.DHF'
    else
      if soDisassembly in SaveOptionsForm.SaveOptions then
        SaveDialog1.Filter:= SaveDisassemblyFilterStr + '(*.DAS)|*.DAS'
      else
        SaveDialog1.Filter:= '';
    if not SaveDialog1.Execute then
      Exit;

    ProgressForm.Execute(TSaveThread.Create(ExecFileManager, ExecFile, SaveDialog1.FileName, SaveOptionsForm.SaveOptions));
    if ProgressData.ErrorStatus <> errNone then begin
      case ProgressData.ErrorStatus of
        errUserTerminated: ; 
      end;
      Exit;
    end;
    Modified:= false;
    SaveProjectPath:= ExtractFilePath(SaveDialog1.FileName);
  end;
end;



procedure TMainForm.ProjectClick(Sender: TObject);     
var
  ErrorMessage: string;
  i: integer;
begin
  OpenDialog1.Filter:= ProjectFilterStr + ' (*.dhf)|*.DHF';
  OpenDialog1.Filename:= '';
  OpenDialog1.InitialDir:= OpenProjectPath;
  if not OpenDialog1.Execute then Exit;
  OpenProjectPath:= ExtractFilePath(OpenDialog1.FileName);

  if ExecFile <> nil then
    CloseFileClick(nil);


  ProgressForm.Execute(TLoadThread.Create(ExecFileManager, OpenDialog1.FileName));
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
    end;
    MessageDlg(ErrorMessage + '"' + Opendialog1.FileName + '"', mtError, [mbOK], 0);
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
      if (ExecFile.Sections[i] as TCodeSection).IsDisassembled then
        TTabSheetTemplate.Create(ExecFile.Sections[i]);

  DisassembleMyButton.Enabled:= false;
  SaveMyButton.Enabled:= true;
  Save1.Enabled:= true;
  CloseFile1.Enabled:= true;

  Caption:= TatraDASFullNameVersion + ' - ' + ExecFile.FileName;
end;



function TMainForm.CloseMainFile: boolean;
begin
  // nothing to close
  if ExecFile = nil then begin
    result:=true;
    Exit;
  end;

  result:=false;
  if Modified then begin
    case MessageDlg(FileModifiedStr,mtConfirmation,mbYesNoCancel,0) of
      mrYes: SaveClick(nil);
      mrNo: ;
      mrCancel: Exit;
    end;
  end;

  while PageControl1.PageCount > 0 do
    PageControl1.Pages[0].Free;

  ExecFile.Free;
  ExecFile:=nil;

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
  sINI.WriteString('Settings','Language',Langs.ShortCut);

  OptionsForm.SaveSettings(sINI);

  sINI.UpdateFile;
  sINI.Free;
end;



procedure TMainForm.FormCreate(Sender: TObject);      // Inicializacia
begin
  DoubleBuffered:=true;

  Image1.Picture.Bitmap.LoadFromResourceName(hinstance,'background');

  OpenMyButton:=TIvanSpeedButton.Create(self);
  OpenMyButton.Parent:=self;
  OpenMyButton.ObrMimo.LoadFromResourceName(hinstance,'open1');
  OpenMyButton.ObrNad.LoadFromResourceName(hinstance,'open2');
  OpenMyButton.Left:=0;
  OpenMyButton.OnClick:=OpenClick;
  OpenMyButton.Glyph:=OpenMyButton.ObrMimo;

  DisassembleMyButton:=TIvanSpeedButton.Create(self);
  DisassembleMyButton.Parent:=self;
  DisassembleMyButton.ObrMimo.LoadFromResourceName(hinstance,'disassemble1');
  DisassembleMyButton.ObrNad.LoadFromResourceName(hinstance,'disassemble2');
  DisassembleMyButton.Left:=72;
  DisassembleMyButton.OnClick:=DisassembleClick;
  DisassembleMyButton.Glyph:=DisassembleMyButton.ObrMimo;
  DisassembleMyButton.Enabled:=false;

  ProjectMyButton:=TIvanSpeedButton.Create(self);
  ProjectMyButton.Parent:=self;
  ProjectMyButton.ObrMimo.LoadFromResourceName(hinstance,'project1');
  ProjectMyButton.ObrNad.LoadFromResourceName(hinstance,'project2');
  ProjectMyButton.Left:=220;
  ProjectMyButton.OnClick:=ProjectClick;
  ProjectMyButton.Glyph:=ProjectMyButton.ObrMimo;

  SaveMyButton:=TIvanSpeedButton.Create(self);
  SaveMyButton.Parent:=self;
  SaveMyButton.ObrMimo.LoadFromResourceName(hinstance,'save1');
  SaveMyButton.ObrNad.LoadFromResourceName(hinstance,'save2');
  SaveMyButton.Left:=295;
  SaveMyButton.OnClick:=SaveClick;
  SaveMyButton.Glyph:=SaveMyButton.ObrMimo;
  SaveMyButton.Enabled:=false;

  HelpMyButton:=TIvanSpeedButton.Create(self);
  HelpMyButton.Parent:=self;
  HelpMyButton.ObrMimo.LoadFromResourceName(hinstance,'help1');
  HelpMyButton.ObrNad.LoadFromResourceName(hinstance,'help2');
  HelpMyButton.Left:=400;
  HelpMyButton.OnClick:=Help2Click;
  HelpMyButton.Glyph:=HelpMyButton.ObrMimo;

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
  Langs:=TTatraDASLanguages.Create(ExtractFilePath(Application.ExeName)+sINI.ReadString('Settings','LanguageFolder','\languages'),Language1, ImageList1);
  if not Langs.ChangeLanguage(sINI.ReadString('Settings','Language','en')) then
    if not Langs.ChangeLanguage('en') then begin
       ShowMessage(NoLanguageFilesStr);
       Application.Terminate;
    end;
  
  fOpenFilePath:= sINI.ReadString('Paths','OpenFile','');
  fOpenProjectPath:= sINI.ReadString('Paths','OpenProject','');
  fSaveProjectPath:= sINI.ReadString('Paths','SaveProject','');

//  OptionsForm.LoadSettings(sINI); - treba volat ked uz je OptionsForm vytvoreny

  OnExecFileCreateSection:= CreateSection;
  ExecFileManager:=TExecFileManager.Create;

end;

//==============================================================================
//==============================================================================

procedure TMainForm.Help2Click(Sender: TObject);      // Spustenie helpu
begin
  ShellExecute(Application.Handle, Pchar('open'), Pchar('hh.exe'), Pchar(ExtractFilePath(Application.ExeName) + 'TatraDAS.chm'), nil, sw_show);
end;

procedure TMainForm.Translate(ini:TMemINIFile);    // Zmena jazyka prostredia
var i:integer;
begin
// Zmena popisov komponent

  //  [ButtonCaption]
  OpenMyButton.ChangeCaption(ini.ReadString('ButtonCaption', 'OpenFile', TranslateErrorStr));
  ProjectMyButton.ChangeCaption(ini.ReadString('ButtonCaption','OpenDisasm',TranslateErrorStr));
  DisassembleMyButton.ChangeCaption(ini.ReadString('ButtonCaption','Disassemble',TranslateErrorStr));
  SaveMyButton.ChangeCaption(ini.ReadString('ButtonCaption','SaveDisasm',TranslateErrorStr));
  HelpMyButton.ChangeCaption(ini.ReadString('ButtonCaption','Help',TranslateErrorStr));

  // [MenuCaption]
  File1.Caption:=ini.ReadString('MenuCaption','file',TranslateErrorStr);
  Edit1.Caption:=ini.ReadString('MenuCaption','edit',TranslateErrorStr);
  Search1.Caption:=ini.ReadString('MenuCaption','search',TranslateErrorStr);
  Goto1.Caption:=ini.ReadString('MenuCaption','goto',TranslateErrorStr);
  Tools1.Caption:=ini.ReadString('MenuCaption','tools',TranslateErrorStr);
  Settings1.Caption:=ini.ReadString('MenuCaption','settings',TranslateErrorStr);
  Help1.Caption:=ini.ReadString('MenuCaption','help',TranslateErrorStr);

  OpenFile1.Caption:=ini.ReadString('MenuCaption','Openfile',TranslateErrorStr);
  OpenProject1.Caption:=ini.ReadString('MenuCaption','OpenProject',TranslateErrorStr);
  Disassemble1.Caption:=ini.ReadString('MenuCaption','Disassemble',TranslateErrorStr);
  Save1.Caption:=ini.ReadString('MenuCaption','Save',TranslateErrorStr);
  Closefile1.Caption:=ini.ReadString('MenuCaption','Closefile',TranslateErrorStr);
  Exit1.Caption:=ini.ReadString('MenuCaption','Exit',TranslateErrorStr);
  FindText1.Caption:=ini.ReadString('MenuCaption','Findtext',TranslateErrorStr);
  SearchAgain1.Caption:=ini.ReadString('MenuCaption','SearchAgain',TranslateErrorStr);
  Calculator1.Caption:=ini.ReadString('MenuCaption','Calculator',TranslateErrorStr);
  Language1.Caption:=ini.ReadString('MenuCaption','Language',TranslateErrorStr);
  Options1.Caption:=ini.ReadString('MenuCaption','Options',TranslateErrorStr);
  Help2.Caption:=ini.ReadString('MenuCaption','HelpTopic',TranslateErrorStr);
  About1.Caption:=ini.ReadString('MenuCaption','About',TranslateErrorStr);

  ToggleBookmarks.Caption:=ini.ReadString('Code','ToggleBookmark',TranslateErrorStr);
  ToggleBookmarks.Hint:=ini.ReadString('Code','MainToggleBookmarkHint',TranslateErrorStr);
  for i:=0 to 9 do begin
    ToggleBookmarks.Items[i].Caption:=ini.ReadString('Code','Bookmark',TranslateErrorStr)+' '+IntToStr(i);
    ToggleBookmarks.Items[i].Hint:=ini.ReadString('Code','ToggleBookmarkHint',TranslateErrorStr)+' '+IntToStr(i);
  end;
  GotoBookmarks.Caption:=ini.ReadString('Code','GotoBookmark',TranslateErrorStr);
  GotoBookmarks.Hint:=ini.ReadString('Code','MainGotoBookmarkHint',TranslateErrorStr);
  for i:=0 to 9 do begin
    GotoBookmarks.Items[i].Caption:=ini.ReadString('Code','Bookmark',TranslateErrorStr)+' '+IntToStr(i);
    GotoBookmarks.Items[i].Hint:=ini.ReadString('Code','GotoBookmarkHint',TranslateErrorStr)+' '+IntToStr(i);
  end;

  ChangeToUnsigned.Caption:=ini.ReadString('Code','ChangeToUnsigned',TranslateErrorStr);
  UByte.Caption:=ini.ReadString('Code','ChangeByte',TranslateErrorStr);
  UWord.Caption:=ini.ReadString('Code','ChangeWord',TranslateErrorStr);
  UDword.Caption:=ini.ReadString('Code','ChangeDword',TranslateErrorStr);
  UQword.Caption:=ini.ReadString('Code','ChangeQword',TranslateErrorStr);
  ChangeToSigned.Caption:=ini.ReadString('Code','ChangeToSigned',TranslateErrorStr);
  SByte.Caption:=ini.ReadString('Code','ChangeByte',TranslateErrorStr);
  SWord.Caption:=ini.ReadString('Code','ChangeWord',TranslateErrorStr);
  SDword.Caption:=ini.ReadString('Code','ChangeDword',TranslateErrorStr);
  SQword.Caption:=ini.ReadString('Code','ChangeQword',TranslateErrorStr);
  ChangeToFloat.Caption:=ini.ReadString('Code','ChangeToFloat',TranslateErrorStr);
  FSingle.Caption:=ini.ReadString('Code','ChangeSingle',TranslateErrorStr);
  FDouble.Caption:=ini.ReadString('Code','ChangeDouble',TranslateErrorStr);
  FExtended.Caption:=ini.ReadString('Code','ChangeExtended',TranslateErrorStr);
  ChangeToString.Caption:=ini.ReadString('Code','ChangeToString',TranslateErrorStr);
  StringPascal.Caption:=ini.ReadString('Code','ChangePascal',TranslateErrorStr);
  StringC.Caption:=ini.ReadString('Code','ChangeC',TranslateErrorStr);
  AdvancedDataChange.Caption:=ini.ReadString('Code','AdvancedDataChange',TranslateErrorStr);
  Disassemble2.Caption:=ini.ReadString('Code','Disassemble',TranslateErrorStr);
  AdvancedDisassemble.Caption:=ini.ReadString('Code','AdvancedDisassemble',TranslateErrorStr);
  Insert1.Caption:=ini.ReadString('Code','Insert',TranslateErrorStr);
  InsertComment.Caption:=ini.ReadString('Code','InsertComment',TranslateErrorStr);
  EmptyLine.Caption:=ini.ReadString('Code','InsertEmpty',TranslateErrorStr);
  RemoveLine.Caption:=ini.ReadString('Code','Remove',TranslateErrorStr);



  // [LabelCaption]

  ProcessText.Disassemblying:=ini.ReadString('LabelCaption','Disassemblying',TranslateErrorStr);
  ProcessText.Indentifying:=ini.ReadString('LabelCaption','Identifyingjumps',TranslateErrorStr);
  ProcessText.PreparingOutput:=ini.ReadString('LabelCaption','Preparingoutput',TranslateErrorStr);
  ProcessText.LoadingDAS:=ini.ReadString('LabelCaption','LoadingDAS',TranslateErrorStr);
  ProcessText.LoadingDHF:=ini.ReadString('LabelCaption','LoadingDHF',TranslateErrorStr);
  ProcessText.SavingDAS:=ini.ReadString('LabelCaption','SavingDAS',TranslateErrorStr);
  ProcessText.SavingDHF:=ini.ReadString('LabelCaption','SavingDHF',TranslateErrorStr);

  GotoEntryPoint1.Caption:=ini.ReadString('Code','EntrypointButton',TranslateErrorStr);
  GotoAddress1.Caption:=ini.ReadString('Code','GotoAddressButton',TranslateErrorStr);
  FollowJUMPCALL1.Caption:=ini.ReadString('Code','FollowButton',TranslateErrorStr);
  ReturnfromJUMPCALL1.Caption:=ini.ReadString('Code','ReturnButton',TranslateErrorStr);

// Zmena hintov

//[ButtonHint]
  OpenMyButton.Hint:=ini.ReadString('ButtonHint','OpenFile',TranslateErrorStr);
  ProjectMyButton.Hint:=ini.ReadString('ButtonHint','OpenDisasm',TranslateErrorStr);
  DisassembleMyButton.Hint:=ini.ReadString('ButtonHint','Disassemble',TranslateErrorStr);
  SaveMyButton.Hint:=ini.ReadString('ButtonHint','SaveDisasm',TranslateErrorStr);
  HelpMyButton.Hint:=ini.ReadString('ButtonHint','Help',TranslateErrorStr);
//  StopMyButton.Hint:=ini.ReadString('ButtonHint','StopDisasm',TranslateErrorStr);

//[MenuHint]
  OpenFile1.Hint:=ini.ReadString('MenuHint','Openfile',TranslateErrorStr);
  OpenProject1.Hint:=ini.ReadString('MenuHint','Opendisassembled',TranslateErrorStr);
  Disassemble1.Hint:=ini.ReadString('MenuHint','Disassemble',TranslateErrorStr);
  Save1.Hint:=ini.ReadString('MenuHint','Savedisassembled',TranslateErrorStr);
  Closefile1.Hint:=ini.ReadString('MenuHint','Closefile',TranslateErrorStr);
  Exit1.Hint:=ini.ReadString('MenuHint','Exit',TranslateErrorStr);

  FindText1.Hint:=ini.ReadString('MenuHint','Findtext',TranslateErrorStr);
  SearchAgain1.Hint:=ini.ReadString('MenuHint','SearchAgain',TranslateErrorStr);

//  Language1.Hint:=ini.ReadString('MenuHint','Language',TranslateErrorStr);
//  Language1.Hint:=ini.ReadString('MenuHint','Language',TranslateErrorStr);

  GotoEntrypoint1.Hint:=ini.ReadString('Code','EntrypointButtonHint',TranslateErrorStr);
  GotoAddress1.Hint:=ini.ReadString('Code','GotoAddressButtonHint',TranslateErrorStr);
  FollowJUMPCALL1.Hint:=ini.ReadString('Code','FollowButtonHint',TranslateErrorStr);
  ReturnFromJUMPCALL1.Hint:=ini.ReadString('Code','ReturnButtonHint',TranslateErrorStr);

  Calculator1.Hint:=ini.ReadString('MenuHint','Calculator',TranslateErrorStr);
  HexEditor1.Hint:=ini.ReadString('MenuHint','HexEditor',TranslateErrorStr);

  Language1.Hint:=ini.ReadString('MenuHint','Language',TranslateErrorStr);
//  Options1.Hint:=ini.ReadString('MenuHint','Options',TranslateErrorStr);

  Help2.Hint:=ini.ReadString('MenuHint','Help',TranslateErrorStr);
  About1.Hint:=ini.ReadString('MenuHint','About',TranslateErrorStr);

end;



procedure TMainForm.FindText1Click(Sender: TObject);  // Otvorenie vyhladavacieho dialogu
begin
  if ActivePageType = ttCode then
    FindDialog1.Execute;
end;



procedure TMainForm.FindDialog1Find(Sender: TObject); // Vyhladavanie retazca
begin
  ((PageControl1.ActivePage as TTabSheetTemplate).Frame as TCodeTabFrame).FindString(FindDialog1.FindText, FindDialog1.Options);
  FindDialog1.CloseDialog;
end;



procedure TMainForm.PageControl1Change(Sender: TObject);
begin
  if ActivePageType = ttCode then begin
    Goto1.Enabled:=True;
    Search1.Enabled:=True;
    Edit1.Enabled:=True;
  end
  else begin
    Goto1.Enabled:=False;
    Search1.Enabled:=False;
    Edit1.Enabled:=False;
  end;
  ActiveFrame:=(PageControl1.ActivePage as TTabSheetTemplate).Frame;
end;

procedure TMainForm.GotoEntrypoint1Click(Sender: TObject);
begin
  with ActiveFrame as TCodeTabFrame do begin
    if GotoEntryPointButton.Enabled then
      GotoEntryPointButtonClick(self);
  end;
end;

procedure TMainForm.Gotoaddress1Click(Sender: TObject);
begin
  with ActiveFrame as TCodeTabFrame do begin
    if GotoAddressButton.Enabled then
      GotoAddressButtonClick(self);
  end;
end;

procedure TMainForm.FollowJUMPCALL1Click(Sender: TObject);
begin
  with ActiveFrame as TCodeTabFrame do begin
    if FollowButton.Enabled then
      FollowButtonClick(self);
  end;
end;

procedure TMainForm.ReturnfromJUMPCALL1Click(Sender: TObject);
begin
  with ActiveFrame as TCodeTabFrame do begin
    if ReturnButton.Enabled then
      ReturnButtonClick(self);
  end;
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
var ff:integer;
begin
  ff:=FileOpen(ExecFile.Fullpath,fmOpenReadWrite);
  if ff = -1 then begin
    ShowMessage(CouldNotOpenReadWriteFileStr);
    Exit;
  end;
  FileClose(ff);
  HexEditForm.Show;
end;



procedure TMainForm.CreateSection(ASection: TSection);
begin
//  TTabSheetTemplate.Create(aSection);
end;

function TMainForm.GetActivePageType: TPageType;
begin
  result:=(PageControl1.ActivePage as TTabSheetTemplate).PageType;
end;


function TMainForm.GetSectionsTabSheet(ASection: TSection): TTabSheetTemplate;
var
  i: integer;
begin
  for i:=0 to PageControl1.PageCount - 1 do
    if (PageControl1.Pages[i] as TTabSheetTemplate).IsHavingSection(ASection) then
      result:= PageControl1.Pages[i] as TTabSheetTemplate;
end;



initialization
  ProcessText.Disassemblying:='Disassemblying...';
  ProcessText.Indentifying:='Identifying jumps and calls...';
  ProcessText.PreparingOutput:='Preparing output...';
  ProcessText.LoadingDAS:='Loading DAS file - Code Section #';
  ProcessText.LoadingDHF:='Loading DHF file - Code Section #';
  ProcessText.SavingDAS:='Saving DAS file - Code Section #';
  ProcessText.SavingDHF:='Saving DHF file - Code Section #';
end.                                        // KONIEC 'UNIT1'
