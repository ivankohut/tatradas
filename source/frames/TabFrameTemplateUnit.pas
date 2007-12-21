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
  ResourceTabFrameUnit,
  CodeTabFrameUnit;


//******************************************************************************
// TTabSheetTemplate class
//******************************************************************************


constructor TTabSheetTemplate.Create(ASection: TSection);
begin
  if aSection = nil then
    raise Exception.Create('TTabSheetTemplate.Create: Section = nil');
  inherited Create(MainForm.PageControl1);
  PageControl:=MainForm.PageControl1;
  PageIndex:=PageControl.PageCount-1;
  case aSection.typ of
    stCode: Frame:= TCodeTabFrame.Create(self, ASection);
    stExport: Frame:= TExportTabFrame.Create(self, ASection);
    stImport: Frame:= TImportTabFrame.Create(self, ASection);
    stResource: Frame:= TResourceTabFrame.Create(self, ASection);
    else
      Exit;
  end;
  Frame.Parent:=self;
  Caption:=Frame.Caption;
  Frame.Translate;
end;



constructor TTabSheetTemplate.CreateFileTab(aExecFile: TExecutableFile);
begin
  inherited Create(MainForm.PageControl1);
  PageControl:=MainForm.PageControl1;
  PageIndex:=0;
  case aExecFile.ExeFormat of
    ffCOM: Frame:= TCOMFileTabFrame.Create(self, aExecFile);
    ffMZ: Frame:= TMZFileTabFrame.Create(self, aExecFile);
    ffNE: Frame:= TNEFileTabFrame.Create(self, aExecFile);
    ffPE: Frame:= TPEFileTabFrame.Create(self, aExecFile);
    ffELF: Frame:= TELFFileTabFrame.Create(self, aExecFile);
    ffCustom: Frame:= TCustomFileTabFrame.Create(self, aExecFile);
    else
      Frame:= TFileTabFrame.Create(self, aExecFile);
  end;
  Frame.Parent:= self;
  Caption:=Frame.Caption;
  Frame.Translate;
end;



destructor TTabSheetTemplate.Destroy;
begin
  Frame.Free;
  inherited;
end;



function TTabSheetTemplate.GetPageType: TPageType;
begin
  result:=Frame.PageType;
end;



procedure TTabSheetTemplate.Translate;
begin
  Caption:=Translator.TranslateControl(Frame.TypeName, 'Caption');
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
    result:= ttFile
  else begin
    if Section = nil then
      raise Exception.Create('TTabSheetTemplate.Create: Section = nil');
    case Section.typ of
      stCode: result:=ttCode;
      stImport: result:=ttImport;
      stExport: result:=ttExport;
      stRelocation: result:=ttRelocation;
      stResource: result:=ttResource;
    end;
  end;  
end;



end.
