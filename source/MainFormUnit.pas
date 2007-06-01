{ TODO:
  procedure TMainForm.ProjectClick(Sender: TObject);  - co v pripade neuspesneho otvorenia
  vynulovanie progressbaru a processlabel by mal kazdy robit sam
  niekedy pada pri zastaveni progresu

  DONE:
}
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
  QDialogs, QImgList, QMenus, QTypes,
  QExtCtrls, QComCtrls, QGrids, QStdCtrls, QControls,
{$ENDIF}
  SysUtils,
  Classes,
  Graphics,
  IniFiles,
  Contnrs,

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
  HexEditFormUnit,
//  jpeg,
  ProgressFormUnit,
  UProgressThread;


type

  TMainForm = class(TForm)
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
    Findtext1: TMenuItem;
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

    procedure Translate(ini:TMemINIFile; error:string);    // Zmena jazyka prostredia

    procedure FindDialog1Find(Sender: TObject);
    procedure StopClick(Sender: TObject);
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
 procedure BeforeProgress;
 procedure AfterProgress;
  function GetActivePageType: TPageType;
{ -- }

public
    Modified: boolean; 
    Status: TTatraDASStatus; 


    ExecFile: TExecutableFile;
    procedure CreateSection(ASection: TSection);



public
    OpenMyButton: TIvanSpeedButton;
    DisassembleMyButton: TIvanSpeedButton;
    StopMyButton: TIvanSpeedButton;
    ProjectMyButton: TIvanSpeedButton;
    SaveMyButton: TIvanSpeedButton;
    HelpMyButton: TIvanSpeedButton;

    sINI: TMemINIFile;
//    Ctrls: TCtrls;

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


  function ProgressFunction(a,b:cardinal; c: string): boolean;

var
//  CurrentTab: integer;

  MainForm: TMainForm;

  OpenMode: boolean;
  PocetVyskytov: integer;
  Stopped: boolean;
  ExecFileManager: TExecFileManager;

implementation

uses
  AboutBoxUnit, GotoAddressFormUnit, UnknownFileFormUnit, SaveOptionsFormUnit,
  AdvancedDisassembleFormUnit, AdvancedChangingToDataFormUnit, CalculatorUnit,
  OptionsFormUnit, CodeSectionUnit, InsertCommentFormUnit;

{$R *.DFM}



procedure TMainForm.OpenClick(Sender: TObject);       // Otvorenie suboru
var
  ErrorMessage: string;
begin
  OpenDialog1.Filter:='EXE, COM and DLL files(*.EXE;*.COM;*.DLL)|*.exe;*.com;*.dll;|All files(*.*)|*.*';
  OpenDialog1.FileName:='';
  OpenDialog1.InitialDir:=OpenFilePath;
  if not OpenDialog1.Execute then Exit;
  OpenFilePath:=ExtractFilePath(OpenDialog1.FileName);

  Application.ProcessMessages;

  if ExecFile <> nil then CloseFileClick(nil);

//  ctrls.INIFile:=Langs.INI;
//  MainFile:=TFileEx.Create(ErrNo,OpenDialog1.FileName,ctrls);                       // Otvorenie suboru
  ExecFile:=ExecFileManager.CreateNewExecFile(OpenDialog1.FileName);
  if ExecFileManager.Error <> ErrNone then begin
    ExecFile.Free;
    case ExecFileManager.Error of
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

  //  Status:=tsOpened;  // do MainForm
  Modified:=false;

  TTabSheetTemplate.CreateFileTab(ExecFile);

  HexEditor1.Enabled:=true;
  CloseFile1.Enabled:=true;
  DisassembleMyButton.Enabled:=true;
  Disassemble1.Enabled:=true;
  Caption:=TatraDASFullNameVersion+' - '+ExecFile.FileName;
end;



procedure TMainForm.DisassembleClick(Sender: TObject);    // Spustenie disassemblovania
begin
  ProgressForm.Show;
  BeforeProgress;
  if not ExecFile.Disassemble() then begin
  // hodila by sa nejaka sprava o pricine neuspechu Disassembluj
    AfterProgress;
    DisassembleMyButton.Enabled:=true;
    SaveMyButton.Enabled:=false;
    Save1.Enabled:=false;
    Exit;
  end;
  AfterProgress;
  DisassembleMyButton.Enabled:=true;
  SaveMyButton.Enabled:=true;
  Save1.Enabled:=true;

  PageControl1.ActivePageIndex:=ActivePageIndex;      // vratime sa na povodnu kartu
  PageControl1Change(nil);
end;



procedure TMainForm.SaveClick(Sender: TObject);    // Ulozenie projektu
begin
  if SaveOptionsForm.ShowModal = mrOK then begin
    SaveDialog1.FileName:='';
    SaveDialog1.InitialDir:=SaveProjectPath;
    if soProject in SaveOptionsForm.SaveOptions then SaveDialog1.Filter:=ProjectFilterStr + '(*.DHF)|*.DHF'
    else if soDisassembly in SaveOptionsForm.SaveOptions then SaveDialog1.Filter:=SaveDisassemblyFilterStr + '(*.DAS)|*.DAS'
    else SaveDialog1.Filter:='';
    if not SaveDialog1.Execute then Exit;
// Osetrit nejakou spravou v pripade zlyhania ukladania, napr. disk full
    BeforeProgress;
//    ExecFile.SaveToFile(SaveDialog1.FileName, SaveOptionsForm.SaveOptions);
    AfterProgress;

    Modified:=false;
    if ExecFile.IsDisassembled then
      DisassembleMyButton.Enabled:=true;
    SaveMyButton.Enabled:=true;
    PageControl1.ActivePageIndex:=ActivePageIndex;  // vratime sa na povodnu kartu
    PageControl1Change(nil);

    SaveProjectPath:=ExtractFilePath(SaveDialog1.FileName);
  end;
end;



procedure TMainForm.ProjectClick(Sender: TObject);     // Otvorenie projektu
var DHF,DAS:string;
    Success:boolean;
    vlakno: TExecFileManager_LoadExecFileFromFile;
begin
  OpenDialog1.Filter:=ProjectFilterStr + ' (*.dhf)|*.DHF';
  OpenDialog1.Filename:='';
  OpenDialog1.InitialDir:=OpenProjectPath;
  if not OpenDialog1.Execute then Exit;
  OpenProjectPath:=ExtractFilePath(OpenDialog1.FileName);



  if ExecFile<>nil then CloseFileClick(nil);                                // Odstranenie MainFile a vycistenie MainForm

  BeforeProgress;
//  ctrls.INIFile:=Langs.INI;

  //  MainFile:=TFileEx.Create(Success,DHF,DAS,ctrls);                          // Vyvorenie MainFile
{
  ProgressFinished:=false;
  vlakno:=TExecFileManager_LoadExecFileFromFile.Create(OpenDialog1.FileName);
  ProgressForm.Execute;
  ExecFile:=vlakno.TheResult;
  vlakno.Free;
}
//  ExecFile:=ExecFileManager.LoadExecFileFromFile(OpenDialog1.FileName);
  ExecFile:=ExecFileManager.NewLoadExecFileFromFile(OpenDialog1.FileName);
{
  if not Success then begin
    MessageDlg(CouldNotOpenProjectStr,mtError,[mbOK],0);
    ExecFile.Destroy;
    ExecFile:=nil;
    AfterProgress;
    DisassembleMyButton.Enabled:=false;
    SaveMyButton.Enabled:=false;
    Disassemble1.Enabled:=false;
    Save1.Enabled:=false;
    CloseFile1.Enabled:=false;
    Exit;
  end;
}
  AfterProgress;

  TTabSheetTemplate.CreateFileTab(ExecFile);

  DisassembleMyButton.Enabled:=false;
  SaveMyButton.Enabled:=true;
  Save1.Enabled:=true;
  CloseFile1.Enabled:=true;
{
   Nasledujuce riadky by mali byt v konkretnych execfiloch
}

// - vytvorit file tabsheet
//  FileNameEdit.Text:=Mainfile.Filename;
//  FileSizeEdit.Text:=IntToStr(MainFile.size);
  Caption:=TatraDASFullNameVersion+' - '+ExecFile.FileName;
end;



function TMainForm.CloseMainFile: boolean;// Zavretie suboru
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

  DisassembleMyButton.Enabled:=false;
  Disassemble1.Enabled:=false;
  CloseFile1.Enabled:=false;
  Save1.Enabled:=false;
  SaveMyButton.Enabled:=false;
  HexEditor1.Enabled:=false;
  HexEditForm.Close;
  Caption:=TatraDASFullNameVersion;
  result:=true;
end;



procedure TMainForm.ExitClick(Sender: TObject);                // Ukoncenie programu
begin
  Close;
end;



procedure TMainForm.CloseFileClick(Sender: TObject); // Zavretie suboru
begin
  CloseMainFile;
end;



procedure TMainForm.About1Click(Sender: TObject);     // Otvorenie okna 'About...'
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

  StopMyButton:=TIvanSpeedButton.Create(self);
  StopMyButton.Parent:=self;
  StopMyButton.ObrMimo.LoadFromResourceName(hinstance,'stop1');
  StopMyButton.ObrNad.LoadFromResourceName(hinstance,'stop2');
  StopMyButton.Left:=145;
  StopMyButton.OnClick:=StopClick;
  StopMyButton.Glyph:=StopMyButton.ObrMimo;
  StopMyButton.Enabled:=false;

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

{ ///
  ctrls.PageControl:=PageControl1;
//  ctrls.INIFile:=lINI; - toto sa vzdy nastavi nanovo
  ctrls.InfoMemo:=InfoMemo;
  ctrls.ProgressFunction:=ProgressFunction;
  ctrls.InfoFormatEdit:=FileFormatEdit;
  ctrls.InfoGrid:=StringGrid1;
  ctrls.InfoListView:=ObjectListView;
}

  OnExecFileCreateSection:=CreateSection;
  ExecFileManager:=TExecFileManager.Create;
end;

//==============================================================================
//==============================================================================

procedure TMainForm.Help2Click(Sender: TObject);      // Spustenie helpu
begin
  ShellExecute(Application.Handle,Pchar('open'),Pchar('hh.exe'),Pchar(ExtractFilePath(Application.ExeName)+'TatraDAS.chm'),nil,sw_show);
end;

procedure TMainForm.Translate(ini:TMemINIFile; error: string);    // Zmena jazyka prostredia
var i:integer;
begin
// Zmena popisov komponent

  //  [ButtonCaption]
  OpenMyButton.ChangeCaption(ini.ReadString('ButtonCaption','OpenFile',error));
  ProjectMyButton.ChangeCaption(ini.ReadString('ButtonCaption','OpenDisasm',error));
  DisassembleMyButton.ChangeCaption(ini.ReadString('ButtonCaption','Disassemble',error));
  SaveMyButton.ChangeCaption(ini.ReadString('ButtonCaption','SaveDisasm',error));
  HelpMyButton.ChangeCaption(ini.ReadString('ButtonCaption','Help',error));

  // [MenuCaption]
  File1.Caption:=ini.ReadString('MenuCaption','file',error);
  Edit1.Caption:=ini.ReadString('MenuCaption','edit',error);
  Search1.Caption:=ini.ReadString('MenuCaption','search',error);
  Goto1.Caption:=ini.ReadString('MenuCaption','goto',error);
  Tools1.Caption:=ini.ReadString('MenuCaption','tools',error);
  Settings1.Caption:=ini.ReadString('MenuCaption','settings',error);
  Help1.Caption:=ini.ReadString('MenuCaption','help',error);

  OpenFile1.Caption:=ini.ReadString('MenuCaption','Openfile',error);
  OpenProject1.Caption:=ini.ReadString('MenuCaption','OpenProject',error);
  Disassemble1.Caption:=ini.ReadString('MenuCaption','Disassemble',error);
  Save1.Caption:=ini.ReadString('MenuCaption','Save',error);
  Closefile1.Caption:=ini.ReadString('MenuCaption','Closefile',error);
  Exit1.Caption:=ini.ReadString('MenuCaption','Exit',error);
  FindText1.Caption:=ini.ReadString('MenuCaption','Findtext',error);
  SearchAgain1.Caption:=ini.ReadString('MenuCaption','SearchAgain',error);
  Calculator1.Caption:=ini.ReadString('MenuCaption','Calculator',error);
  Language1.Caption:=ini.ReadString('MenuCaption','Language',error);
  Options1.Caption:=ini.ReadString('MenuCaption','Options',error);
  Help2.Caption:=ini.ReadString('MenuCaption','HelpTopic',error);
  About1.Caption:=ini.ReadString('MenuCaption','About',error);

  ToggleBookmarks.Caption:=ini.ReadString('Code','ToggleBookmark',error);
  ToggleBookmarks.Hint:=ini.ReadString('Code','MainToggleBookmarkHint',error);
  for i:=0 to 9 do begin
    ToggleBookmarks.Items[i].Caption:=ini.ReadString('Code','Bookmark',error)+' '+IntToStr(i);
    ToggleBookmarks.Items[i].Hint:=ini.ReadString('Code','ToggleBookmarkHint',error)+' '+IntToStr(i);
  end;
  GotoBookmarks.Caption:=ini.ReadString('Code','GotoBookmark',error);
  GotoBookmarks.Hint:=ini.ReadString('Code','MainGotoBookmarkHint',error);
  for i:=0 to 9 do begin
    GotoBookmarks.Items[i].Caption:=ini.ReadString('Code','Bookmark',error)+' '+IntToStr(i);
    GotoBookmarks.Items[i].Hint:=ini.ReadString('Code','GotoBookmarkHint',error)+' '+IntToStr(i);
  end;

  ChangeToUnsigned.Caption:=ini.ReadString('Code','ChangeToUnsigned',error);
  UByte.Caption:=ini.ReadString('Code','ChangeByte',error);
  UWord.Caption:=ini.ReadString('Code','ChangeWord',error);
  UDword.Caption:=ini.ReadString('Code','ChangeDword',error);
  UQword.Caption:=ini.ReadString('Code','ChangeQword',error);
  ChangeToSigned.Caption:=ini.ReadString('Code','ChangeToSigned',error);
  SByte.Caption:=ini.ReadString('Code','ChangeByte',error);
  SWord.Caption:=ini.ReadString('Code','ChangeWord',error);
  SDword.Caption:=ini.ReadString('Code','ChangeDword',error);
  SQword.Caption:=ini.ReadString('Code','ChangeQword',error);
  ChangeToFloat.Caption:=ini.ReadString('Code','ChangeToFloat',error);
  FSingle.Caption:=ini.ReadString('Code','ChangeSingle',error);
  FDouble.Caption:=ini.ReadString('Code','ChangeDouble',error);
  FExtended.Caption:=ini.ReadString('Code','ChangeExtended',error);
  ChangeToString.Caption:=ini.ReadString('Code','ChangeToString',error);
  StringPascal.Caption:=ini.ReadString('Code','ChangePascal',error);
  StringC.Caption:=ini.ReadString('Code','ChangeC',error);
  AdvancedDataChange.Caption:=ini.ReadString('Code','AdvancedDataChange',error);
  Disassemble2.Caption:=ini.ReadString('Code','Disassemble',error);
  AdvancedDisassemble.Caption:=ini.ReadString('Code','AdvancedDisassemble',error);
  Insert1.Caption:=ini.ReadString('Code','Insert',error);
  InsertComment.Caption:=ini.ReadString('Code','InsertComment',error);
  EmptyLine.Caption:=ini.ReadString('Code','InsertEmpty',error);
  RemoveLine.Caption:=ini.ReadString('Code','Remove',error);



  // [LabelCaption]

  ProcessText.Disassemblying:=ini.ReadString('LabelCaption','Disassemblying',error);
  ProcessText.Indentifying:=ini.ReadString('LabelCaption','Identifyingjumps',error);
  ProcessText.PreparingOutput:=ini.ReadString('LabelCaption','Preparingoutput',error);
  ProcessText.LoadingDAS:=ini.ReadString('LabelCaption','LoadingDAS',error);
  ProcessText.LoadingDHF:=ini.ReadString('LabelCaption','LoadingDHF',error);
  ProcessText.SavingDAS:=ini.ReadString('LabelCaption','SavingDAS',error);
  ProcessText.SavingDHF:=ini.ReadString('LabelCaption','SavingDHF',error);


  GotoEntryPoint1.Caption:=ini.ReadString('Code','EntrypointButton',error);
  GotoAddress1.Caption:=ini.ReadString('Code','GotoAddressButton',error);
  FollowJUMPCALL1.Caption:=ini.ReadString('Code','FollowButton',error);
  ReturnfromJUMPCALL1.Caption:=ini.ReadString('Code','ReturnButton',error);

// Zmena hintov

//[ButtonHint]
  OpenMyButton.Hint:=ini.ReadString('ButtonHint','OpenFile',error);
  ProjectMyButton.Hint:=ini.ReadString('ButtonHint','OpenDisasm',error);
  DisassembleMyButton.Hint:=ini.ReadString('ButtonHint','Disassemble',error);
  SaveMyButton.Hint:=ini.ReadString('ButtonHint','SaveDisasm',error);
  HelpMyButton.Hint:=ini.ReadString('ButtonHint','Help',error);
  StopMyButton.Hint:=ini.ReadString('ButtonHint','StopDisasm',error);

//[MenuHint]
  OpenFile1.Hint:=ini.ReadString('MenuHint','Openfile',error);
  OpenProject1.Hint:=ini.ReadString('MenuHint','Opendisassembled',error);
  Disassemble1.Hint:=ini.ReadString('MenuHint','Disassemble',error);
  Save1.Hint:=ini.ReadString('MenuHint','Savedisassembled',error);
  Closefile1.Hint:=ini.ReadString('MenuHint','Closefile',error);
  Exit1.Hint:=ini.ReadString('MenuHint','Exit',error);

  FindText1.Hint:=ini.ReadString('MenuHint','Findtext',error);
  SearchAgain1.Hint:=ini.ReadString('MenuHint','SearchAgain',error);

//  Language1.Hint:=ini.ReadString('MenuHint','Language',error);
//  Language1.Hint:=ini.ReadString('MenuHint','Language',error);

  GotoEntrypoint1.Hint:=ini.ReadString('Code','EntrypointButtonHint',error);
  GotoAddress1.Hint:=ini.ReadString('Code','GotoAddressButtonHint',error);
  FollowJUMPCALL1.Hint:=ini.ReadString('Code','FollowButtonHint',error);
  ReturnFromJUMPCALL1.Hint:=ini.ReadString('Code','ReturnButtonHint',error);

  Calculator1.Hint:=ini.ReadString('MenuHint','Calculator',error);
  HexEditor1.Hint:=ini.ReadString('MenuHint','HexEditor',error);

  Language1.Hint:=ini.ReadString('MenuHint','Language',error);
//  Options1.Hint:=ini.ReadString('MenuHint','Options',error);

  Help2.Hint:=ini.ReadString('MenuHint','Help',error);
  About1.Hint:=ini.ReadString('MenuHint','About',error);
end;

procedure TMainForm.Findtext1Click(Sender: TObject);  // Otvorenie vyhladavacieho dialogu
begin
  if ActivePageType = ttCode then FindDialog1.Execute;
end;

procedure TMainForm.FindDialog1Find(Sender: TObject); // Vyhladavanie retazca
begin
  ((PageControl1.ActivePage as TTabSheetTemplate).Frame as TCodeTabFrame).FindString(FindDialog1.FindText,FindDialog1.Options);
  FindDialog1.CloseDialog;
end;

procedure TMainForm.StopClick(Sender: TObject);
begin
  Stopped:=true;
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
  with ActiveFrame as TCodeTabFrame do begin
    if AdvancedChangingToDataForm.ShowModal = mrOK then
      ChangeToData(AdvancedChangingToDataForm.Options);
  end;
end;

procedure TMainForm.AdvancedDisassemble1Click(Sender: TObject);
begin
  if AdvancedDisassembleForm.ShowModal=mrOK then
    (ActiveFrame as TCodeTabFrame).Disassemble(AdvancedDisassembleForm.Options);
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
var Options: TDataChangeOptions;
begin
  Options.signed:=false;
  Options.option:=dcItems;
  Options.value:=1;
  case (Sender as TMenuItem).MenuIndex of
    0: Options.datatype:=dtByte;
    1: Options.datatype:=dtWord;
    2: Options.datatype:=dtDword;
    3: Options.datatype:=dtQword;
  end;
  (ActiveFrame as TCodeTabFrame).ChangeToData(Options);
end;

procedure TMainForm.ChangeSDataClick(Sender: TObject);
var Options: TDataChangeOptions;
begin
  Options.signed:=true;
  Options.option:=dcItems;
  Options.value:=1;
  case (Sender as TMenuItem).MenuIndex of
    0: Options.datatype:=dtByte;
    1: Options.datatype:=dtWord;
    2: Options.datatype:=dtDword;
    3: Options.datatype:=dtQword;
  end;
  (ActiveFrame as TCodeTabFrame).ChangeToData(Options);
end;

procedure TMainForm.ChangeFDataClick(Sender: TObject);
var Options: TDataChangeOptions;
begin
  Options.signed:=false;
  Options.option:=dcItems;
  Options.value:=1;
  case (Sender as TMenuItem).MenuIndex of
    0: Options.datatype:=dtSingle;
    1: Options.datatype:=dtDouble;
    2: Options.datatype:=dtDoubleEx;
  end;
  (ActiveFrame as TCodeTabFrame).ChangeToData(Options);
end;

procedure TMainForm.ChangeStringClick(Sender: TObject);
var Options: TDataChangeOptions;
begin
  Options.signed:=false;
  Options.option:=dcItems;
  Options.value:=1;
  case (Sender as TMenuItem).MenuIndex of
    0: Options.datatype:=dtPascalStr;
    1: Options.datatype:=dtCStr;
    2: Options.datatype:=dtPascalUniCodeStr;
    3: Options.datatype:=dtCUniCodeStr;
  end;
  (ActiveFrame as TCodeTabFrame).ChangeToStringData(Options);
end;

procedure TMainForm.Disassemble2Click(Sender: TObject);
var Options: TDisassembleFormOptions;
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

function ProgressFunction(a,b:cardinal; c: string): boolean;
begin
  if a = 0 then begin
///    Mainform.ProgressBar1.Max:=round(b/1000);
///    MainForm.ProcessLabel.Caption:=c;
  end;
///  Mainform.ProgressBar1.Position:=round(a/1000);
  Application.ProcessMessages;
  result:=not Stopped;
  if stopped then
  Stopped:=false;
end;



procedure TMainForm.BeforeProgress;
begin
  OpenMyButton.Enabled:=false;
  DisassembleMyButton.Enabled:=false;
  StopMyButton.Enabled:=true;
  ProjectMyButton.Enabled:=false;
  SaveMyButton.Enabled:=false;
  File1.Enabled:=false;
  Settings1.Enabled:=false;
end;

procedure TMainForm.AfterProgress;
begin
//  ProcessLabel.Caption:='';
//  ProgressBar1.Position:=0;
  OpenMyButton.Enabled:=true;
  StopMyButton.Enabled:=false;
  ProjectMyButton.Enabled:=true;
  File1.Enabled:=true;
  Settings1.Enabled:=true;
end;



procedure TMainForm.CreateSection(ASection: TSection);
begin
  TTabSheetTemplate.Create(aSection);
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
