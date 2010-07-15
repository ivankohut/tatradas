unit TabFrameTemplateUnit;

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  ComCtrls,
  IniFiles,

  procmat,
  TranslatorUnit,
  SectionUnit,
  CodeSectionUnit,
  ImportSectionUnit,
  ExecFileUnit;

type

  TPageType = (ttFile, ttCode, ttImport, ttExport, ttRelocation, ttResource);

  TTabFrameTemplate = class(TFrame, ITranslatable)
    Panel: TPanel;
  private
    fTypeName: string;
    function GetPageType: TPageType;
  protected
    function GetSection: TSection; virtual; abstract;
    property Section: TSection read GetSection;
  public
    constructor Create(AOwner: TComponent; ASection: TSection); overload; virtual; //abstract;
    procedure Translate; virtual; abstract;
//    procedure SaveToStream(AStream: TStream); virtual; abstract;
    property TypeName: string read fTypeName;
    property PageType: TPageType read GetPageType;
  end;

  TTabFrameTemplateClass = class of TTabFrameTemplate;

  TTabSheetTemplate = class(TTabSheet)
   private
     function GetPageType: TPageType;
   public
    Frame: TTabFrameTemplate;
    constructor CreateFileTab(aExecFile: TExecutableFile); 
    constructor Create(ASection: TSection); reintroduce; overload;
    destructor Destroy; override;
    procedure Translate;
    function IsHavingSection(ASection: TSection): boolean; 
    property PageType: TPageType read GetPageType;
  end;

implementation

{$R *.dfm}

uses
  MainFormUnit,
  FileTabFrameUnit,
  ImportTabFrameUnit,
  ExportTabFrameUnit,
  CodeTabFrameUnit, ExceptionsUnit;


//******************************************************************************
// TTabSheetTemplate class
//******************************************************************************


constructor TTabSheetTemplate.Create(ASection: TSection);
begin
  if aSection = nil then
    raise EIllegalState.Create('TTabSheetTemplate.Create: Section = nil');
  inherited Create(MainForm);
  PageControl := MainForm.MainPageControl;
  PageIndex := PageControl.PageCount - 1;
  case aSection.typ of
    stCode: Frame := TCodeTabFrame.Create(MainForm, ASection);
    stExport: Frame := TExportTabFrame.Create(MainForm, ASection);
    stImport: Frame := TImportTabFrame.Create(MainForm, ASection);
//    stResource: Frame := TResourceTabFrame.Create(MainForm, ASection);
    else
      Exit;
  end;
  Frame.Parent := self;
  Caption := Frame.Caption;
  Frame.Translate;
end;



constructor TTabSheetTemplate.CreateFileTab(aExecFile: TExecutableFile);
begin
  inherited Create(MainForm);
  PageControl := MainForm.MainPageControl;
//  PageIndex := 0; - Lazarus BUG
  case aExecFile.ExeFormat of
    ffCOM: Frame:= TCOMFileTabFrame.Create(MainForm, aExecFile);
    ffMZ: Frame:= TMZFileTabFrame.Create(MainForm, aExecFile);
    ffNE: Frame:= TNEFileTabFrame.Create(MainForm, aExecFile);
    ffPE: Frame:= TPEFileTabFrame.Create(MainForm, aExecFile);
    ffELF: Frame:= TELFFileTabFrame.Create(MainForm, aExecFile);
    ffCustom: Frame:= TCustomFileTabFrame.Create(MainForm, aExecFile);
    else
      Frame:= TFileTabFrame.Create(MainForm, aExecFile);
  end;
  Frame.Parent := self;
  Caption := Frame.Caption;
  Frame.Translate;
end;



destructor TTabSheetTemplate.Destroy;
begin
  Frame.Free;
  inherited;
end;



function TTabSheetTemplate.GetPageType: TPageType;
begin
  Result := Frame.PageType;
end;



procedure TTabSheetTemplate.Translate;
begin
  Caption := Translator.TranslateControl(Frame.TypeName, 'Caption');
  Frame.Translate;
end;



function TTabSheetTemplate.IsHavingSection(ASection: TSection): boolean;
begin
  result:= (Frame.Section = ASection);
end;


//******************************************************************************
// TTabFrameTemplate class
//******************************************************************************


constructor TTabFrameTemplate.Create(AOwner: TComponent; ASection: TSection);
begin
  inherited Create(AOwner);
end;



function TTabFrameTemplate.GetPageType: TPageType;
begin
  if self is TFileTabFrame then
    Result := ttFile
  else begin
    if Section = nil then
      raise EIllegalState.Create('TTabSheetTemplate.Create: Section = nil');
    case Section.typ of
      stCode: Result := ttCode;
      stImport: Result := ttImport;
      stExport: Result := ttExport;
      stRelocation: Result := ttRelocation;
      stResource: Result := ttResource;
      else
        raise EIllegalState.Create('Bad section type');
    end;
  end;  
end;



end.
