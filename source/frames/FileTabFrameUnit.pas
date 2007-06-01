unit FileTabFrameUnit;

interface

uses
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Grids,
  StdCtrls,
  ComCtrls,
  ExtCtrls,

  SectionUnit,
  TabFrameTemplateUnit,
  Languages,
  ExecFileUnit;

type

  TFileTabFrame = class(TTabFrameTemplate)
    FileOverviewGroupBox: TGroupBox;
    FileNameLabel: TLabel;
    FullPathLabel: TLabel;
    FileSizeLabel: TLabel;
    FileFormatLabel: TLabel;
    BytesLabel: TLabel;
    FileNameEdit: TEdit;
    FullpathEdit: TEdit;
    FileSizeEdit: TEdit;
    FileFormatEdit: TEdit;
    ObjectListView: TListView;
    InfoMemo: TMemo;
    AdvancedInfoGrid: TStringGrid;

  private
    ExecFile: TExecutableFile;

  protected
    function GetSection: TSection; override;

  public
    constructor Create(AOwner: TComponent; aExecFile: TExecutableFile);
    procedure Translate(Translator: TTatraDASLanguages); override;
  end;

implementation

{$R *.dfm}

constructor TFileTabFrame.Create(AOwner: TComponent; aExecFile: TExecutableFile);
var i: integer;
    ListItem: TListItem;
//    AdvancedInfo: TExecFileAdvancedInfo;
//    AdvancedInfoCount: integer;
begin
  inherited Create(AOwner);
  ExecFile:=aExecFile;
  Caption:= 'File info';

  // Fill FileOverviewGroupBox subitems
  FileNameEdit.Text:=ExecFile.filename;
  FullPathEdit.Text:=ExecFile.fullpath;
  FileSizeEdit.Text:=IntToStr(ExecFile.FileSize);
  FileFormatEdit.Text:=ExecFile.FormatDescription;

{
  // Fill ObjectListView
  for i:=0 to ExecFile.SectionCount - 1 do begin
    ListItem:=ObjectListView.Items.Add;
    ListItem.Caption:=IntToStr(i+1)+'.';
    with ExecFile.Sections[i] do begin
      ListItem.SubItems.Add(Name);
      ListItem.SubItems.Add(IntToHex(FileOffset,8));
      ListItem.SubItems.Add(IntToHex(FileSize,8));
      ListItem.SubItems.Add(IntToHex(MemOffset,8));
      ListItem.SubItems.Add(IntToHex(MemSize,8));
//      ListItem.SubItems.Add(IntToHex(Flags,8));
    end;
  end;
}


{
  // Fill AdvancedInfoStringGrid
  AdvancedInfo:=ExecFile.GetAdvancedInfo;
  AdvancedInfoCount:=AdvancedInfo.Count;
  if AdvancedInfoCount > 0 then begin
    AdvancedInfoGrid.RowCount:=AdvancedInfoCount;
    for i:=0 to AdvancedInfoCount-1 do begin
      AdvancedInfoGrid.Cells[0,i]:=AdvancedInfo[i].Description;
      AdvancedInfoGrid.Cells[1,i]:=AdvancedInfo[i].Content;
    end;
    AdvancedInfoGrid.Visible:=true;
  end;
  AdvancedInfo.Free;
}

  {
   Pridat "case" na rozne ExecFily a zobrazit ich specifika
  }
end;



procedure TFileTabFrame.Translate(Translator: TTatraDASLanguages);
begin
  // Translate FileOverviewGroupBox and its subitems
  FileOverviewGroupBox.Caption:=Translator.TranslateControl('File','FileOverview');
  FilenameLabel.Caption:=Translator.TranslateControl('File','Filename');
  FilesizeLabel.Caption:=Translator.TranslateControl('File','Filesize');
  FileformatLabel.Caption:=Translator.TranslateControl('File','Fileformat');
  FullpathLabel.Caption:=Translator.TranslateControl('File','Fullpath');
  BytesLabel.Caption:=Translator.TranslateControl('File','Bytes');

  // Translate ObjectListView column captions
  ObjectListView.Columns[1].Caption:=Translator.TranslateControl('File','SectionName');
  ObjectListView.Columns[2].Caption:=Translator.TranslateControl('File','SectionFileOffset');
  ObjectListView.Columns[3].Caption:=Translator.TranslateControl('File','SectionFileSize');
  ObjectListView.Columns[4].Caption:=Translator.TranslateControl('File','SectionMemAddress');
  ObjectListView.Columns[5].Caption:=Translator.TranslateControl('File','SectionMemSize');
  ObjectListView.Columns[6].Caption:=Translator.TranslateControl('File','SectionFlags');
end;


function TFileTabFrame.GetSection: TSection; 
begin
  result:=nil;
end;

end.
