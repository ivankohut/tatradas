unit ResourceTabFrameUnit;

interface

uses
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,

  SectionUnit,
  ResourceSectionUnit,
  TabFrameTemplateUnit;

type
  TResourceTabFrame = class(TTabFrameTemplate)
  protected
    function GetSection: TSection; override;

  private
    fSection: TResourceSection;
  public
    constructor Create(AOwner: TComponent; ASection: TSection); overload; override;
  end;

var
  ResourceTabFrame: TResourceTabFrame;

implementation

{$R *.dfm}

constructor TResourceTabFrame.Create(AOwner: TComponent; ASection: TSection);
begin
  inherited;
  fSection:=aSection as TResourceSection;
end;

function TResourceTabFrame.GetSection: TSection;
begin
  result:=fSection;
end;



end.
