{ TODO:
  - zmrzne pri DI.EXE
  - StringBox - nejako dorobit scrollbar pre dlhe retazce
}
unit ResourceSectionUnit;

{$INCLUDE 'delver.inc'}

interface

uses
{$IFDEF GUI_B}
  {$IFDEF MSWINDOWS}
     ComCtrls, StdCtrls, Controls, ExtCtrls, ImgList,
  {$ENDIF}
  {$IFDEF LINUX}
     QComCtrls, QStdCtrls, QControls, QExtCtrls, QImgList,
  {$ENDIF}
  Graphics,
{$ENDIF}
     classes, INIFiles, SysUtils,
     procmat,
     SectionUnit;

type

{$IFDEF GUI_B}
  TResourceTabSheet = class;
{$ENDIF}
  TResourceType = (
    RT_CURSOR=1,
    RT_BITMAP=2,
    RT_ICON=3,
    RT_MENU=4,
    RT_DIALOG=5,
    RT_STRING=6,
    RT_FONTDIR=7,
    RT_FONT=8,
    RT_ACCELERATOR=9,
    RT_RCDATA=10,
    RT_GROUP_CURSOR=12,
    RT_GROUP_ICON=14,

    RT_MESSAGETABLE=11,
    RT_VERSION=16,
    RT_DLGINCLUDE=17,
    RT_PLUGPLAY=19,
    RT_VXD=20,
    RT_ANICURSOR=21
  );

  TResourceDirectoryTable = record
    flags:cardinal;
    TimeDateStamp:cardinal;
    MajorVersion,MinorVersion:word;
    NamedEntriesCount, IDEntriesCount:word;
  end;

  TResourceDirectoryEntry = record
    NameRVA: cardinal;      // alebo Resource ID
    DataEntryRVA: cardinal; // alebo SubDirRVA, bit 31 (t.j. 32.) je 0 pri DataEntryRVA, 1 pri SubDirRVA
  end;

  TResourceDataEntry = record
    DataRVA: cardinal;
    Size: cardinal;
    CodePage: cardinal;
    Reserved: cardinal;
  end;

  PTResourceData = ^TResourceData;

  TResourceData = record
    DataRVA: cardinal;
    Size: cardinal;
    CodePage: cardinal;
    Reserved: cardinal;
    NamePresent: boolean;
    name: string; // ak je resource identifikovany menom
    id: cardinal; // ak je resource identifikovany ID-ckom
    typ: TResourceType;
    Data: TObject;
{
    StringTable: TStrings;
    Icon: TIcon;
    Obr: TBitmap;
}
  end;

  PTResourceDirectory = ^TResourceDirectory;

  TResourceDirectory = record
    flags:cardinal;
    TimeDateStamp:cardinal;
    MajorVersion,MinorVersion:word;
    NamedEntriesCount, IDEntriesCount:word;
    NamePresent: boolean;
    name: WideString;
    ID: cardinal;
    typ: TResourceType;
    Data: array of TResourceData;
    Dir: array of PTResourceDirectory;
  end;
{
  TLanguageDir = record
    flags:cardinal;
    TimeDateStamp:cardinal;
    MajorVersion,MinorVersion:word;
    NamedEntriesCount, IDEntriesCount:word;
    Dir: array of TResourceDirectoryTable;         // ak vsetko OK a standardne, tak toto pole je vdzy prazdne
    Data: array of TResourceDataEntry; //TResourceDirectoryEntry;
  end;


  TNameDir = record
    flags:cardinal;
    TimeDateStamp:cardinal;
    MajorVersion,MinorVersion:word;
    NamedEntriesCount, IDEntriesCount:word;
    Dir: array of TLanguageDir;
    Data: array of TResourceDataEntry;//TResourceDirectoryEntry;
  end;

  TTypeDir = record
    flags:cardinal;
    TimeDateStamp:cardinal;
    MajorVersion,MinorVersion:word;
    NamedEntriesCount, IDEntriesCount:word;
    Dir: array of TNameDir;
    Data: array of TResourceDataEntry; //TResourceDirectoryEntry;
  end;
}
  TResourceSection = class(TSection)
{$IFDEF GUI_B}
///    tab: TResourceTabSheet;
{$ENDIF}
    MemOffset: cardinal;
    Size, LogSize: cardinal;
    TypeDir: TResourceDirectory;
//    constructor Create(Input: TStream; A,ss,RVA: cardinal; ResourceTableRVA: cardinal; efile:TObject); overload;
    constructor Create(InputStream: TStream; aName: string; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aSectionIndex: integer; ResourceTableRVA: cardinal; aExecFile:TObject); overload;
    constructor Create(efile:TObject); overload;
    destructor Destroy; override;
    procedure SaveToFile(DHF: TStream; var DAS: TextFile); override;
    procedure LoadFromFile(DHF: TStream; var DAS: TextFile); overload; override;
  end;

{$IFDEF GUI_B}
  TNodeData = record
    typ: TResourceType;
    Resource: PTResourceData;
  end;

  TResourceTabSheet = class(TTabSheet)
    Panel: TPanel;
    Tree: TTreeView;
//    Memo: TMemo;
    StringBox: TListBox;
    Image: TImage;
    ResLabel, StringLabel: TLabel;
    ImageList: TImageList;
    Section: TResourceSection;
    constructor Create(AOwner:TComponent; ssection: TResourceSection);
    destructor Destroy; override;
    procedure TreeChange(Sender: TObject; Node: TTreeNode);
    procedure Translate(ini: TMemINIFile; error: string);
  end;
{$ENDIF}

implementation

//uses Unit1;

destructor TResourceSection.Destroy;

  procedure FreeDirectory(Directory: PTResourceDirectory);
  var i:integer;
  begin
    for i:=0 to Length(Directory^.Dir)-1 do FreeDirectory(Directory^.Dir[i]);
    for i:=0 to Length(Directory^.Data)-1 do Directory^.Data[i].Data.Free;
  end;

begin
  FreeDirectory(@TypeDir);
{$IFDEF GUI_B}
{
  tab.Free;
  tab:=nil;
}
{$ENDIF}
  inherited;
end;

constructor TResourceSection.Create(InputStream: TStream; aName: string; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aSectionIndex: integer; ResourceTableRVA: cardinal; aExecFile:TObject);
//constructor TResourceSection.Create(a:TStream; oo,ss,RVA: cardinal; ResourceTableRVA: cardinal; efile:TObject);
var b: TMemoryStream;
    i: integer;

procedure LoadResource(var Resource: TResourceData; address: cardinal);
var StringLength:word;
    wString: WideString;
    i: integer;
    StringAddress: cardinal;
    temp:int64;
//    temp:integer;
begin
  b.Seek(address,0);
  b.Read(Resource,sizeof(TResourceDataEntry));
  case Resource.typ of
    rt_String: begin
//      StringAddress:=Resource.DataRVA-MemOffset;
      temp:=integer(Resource.DataRVA-MemOffset);  // toto vychadza niekedy zaporne a potom to blbne
//      if temp > maxint then showmessage('asdas');
      if temp > 0 then StringAddress:=temp;
      b.Seek(StringAddress,0);
      Resource.Data:=TStringList.Create;

      // Tu prejdem cez nuly, ktore sa niekedy nachadzaju na zaciatku (neviem preco), treba to nejak rozumnejsie osetrit
      i:=0;
      StringLength:=0;
      while true do begin
        while (StringLength=0) and (b.Position+MemOffset < Resource.Size+Resource.DataRVA) do b.Read(StringLength,2);
        if (b.Position+MemOffset < Resource.Size+Resource.DataRVA) then begin
          StringAddress:=b.Position-2;
          inc(i);
          SetLength(WString,StringLength);
          b.Read(WString[1],StringLength*2);
          (Resource.Data as TStringList).Add('String '+IntToStr(i)+', RVA: '+IntToHex(MemOffset+StringAddress,8)+': '+wString);
          StringLength:=0;
        end
        else break;
      end;
    end;
    rt_Icon: begin
{
      Resource.Icon:=TIcon.Create;
      b.Seek(Resource.DataRVA-MemOffset,0);
      Resource.Icon.LoadFromStream(b);
} ;
    end;
    rt_Bitmap: begin
{
      Resource.Obr:=TBitmap.Create;
      b.Seek(Resource.DataRVA-MemOffset,0);
      Resource.Obr.LoadFromStream(b);
} ;
    end;
  end;
end;

procedure LoadDirectory(Directory: PTResourceDirectory; address: cardinal);
var i,iDir,iData: integer;
    ResEntry: TResourceDirectoryEntry;
    NameLength: word;
begin
  iDir:=0; iData:=0;
  b.Seek(address,0);
  b.Read(Directory^,sizeof(TResourceDirectoryTable));
  SetLength(Directory^.Dir,Directory^.NamedEntriesCount+Directory^.IDEntriesCount);
  SetLength(Directory^.Data,Directory^.NamedEntriesCount+Directory^.IDEntriesCount);
  for i:=0 to Directory^.NamedEntriesCount-1 do begin
    b.Seek(address+SizeOf(TResourceDirectoryTable)+i*SizeOf(TResourceDirectoryEntry),0);
    b.Read(ResEntry,SizeOf(TResourceDirectoryEntry));
    if (ResEntry.DataEntryRVA and $80000000)<>0 then begin// SubDirectory
      New(Directory^.Dir[iDir]);
      Directory^.Dir[iDir]^.typ:=Directory^.typ;
      Directory^.Dir[iDir]^.NamePresent:=true;
      b.Seek(ResEntry.NameRVA and $7FFFFFFF,0);
      b.Read(NameLength,2);
      SetLength(Directory^.Dir[iDir]^.name,NameLength);
      b.Read(Directory^.Dir[iDir]^.name[1],NameLength);
      LoadDirectory(Directory^.Dir[iDir], (ResEntry.DataEntryRVA and $7FFFFFFF){-MemOffset});
      inc(iDir);
    end
    else begin // Data
      Directory^.Data[iData].typ:=Directory^.typ;
      b.Seek(ResEntry.DataEntryRVA,0);
      Directory^.Data[iData].NamePresent:=true;
      b.Read(NameLength,2);
      SetLength(Directory^.Data[iData].name,NameLength);
      b.Read(Directory^.Data[iData].name[1],NameLength);
      LoadResource(Directory^.Data[iData], (ResEntry.DataEntryRVA and $7FFFFFFF){-MemOffset});
      inc(iData);
    end;
  end;

  for i:= Directory^.NamedEntriesCount to Directory^.NamedEntriesCount+Directory^.IDEntriesCount-1 do begin
    b.Seek(address+SizeOf(TResourceDirectoryTable)+i*SizeOf(TResourceDirectoryEntry),0);
    b.Read(ResEntry,SizeOf(TResourceDirectoryEntry));
    if (ResEntry.DataEntryRVA and $80000000)<>0 then begin// SubDirectory
      New(Directory^.Dir[iDir]);
      if Directory=@TypeDir then Directory^.Dir[iDir]^.typ:=TResourceType(ResEntry.NameRVA)
      else Directory^.Dir[iDir]^.typ:=Directory^.typ;
      Directory^.Dir[iDir]^.NamePresent:=false;
      Directory^.Dir[iDir]^.ID:=ResEntry.NameRVA and $7FFFFFFF;
      LoadDirectory(Directory^.Dir[iDir], (ResEntry.DataEntryRVA and $7FFFFFFF){-MemOffset});
      inc(iDir);
    end
    else begin // Data
      Directory^.Data[iData].NamePresent:=false;
      Directory^.Data[iData].ID:=ResEntry.NameRVA and $7FFFFFFF;
      Directory^.Data[iData].typ:=Directory^.typ;
      LoadResource(Directory^.Data[iData], (ResEntry.DataEntryRVA and $7FFFFFFF){-MemOffset});
//      Directory^.Data[iData].NamePresent:=true;
//      Directory^.Data[iData].Name:='RVA: '+IntToHex(Directory^.Data[iData].DataRVA,8)+' Size: '+Inttohex(Directory^.Data[iData].Size,8);
      inc(iData);
    end;
  end;

  SetLength(Directory^.Dir,iDir);
  SetLength(Directory^.Data,iData);

end;

begin
{
  inherited Create(aName, aSectionIndex, aExecFile);
  fTyp:=stResource;

  b:=TMemoryStream.Create;
  InputStream.Seek(FileOffset,0);
  b.CopyFrom(InputStream,FileSize);
  TypeDir.typ:=TResourceType(0);
  LoadDirectory(@TypeDir,ResourceTableRVA-MemOffset);
  For i:=0 to Length(TypeDir.Dir)-1 do
    if not TypeDir.Dir[i]^.NamePresent then begin
      case TypeDir.Dir[i]^.ID of
        1: TypeDir.Dir[i]^.Name:='RT_CURSOR';
        2: TypeDir.Dir[i]^.Name:='RT_BITMAP';
        3: TypeDir.Dir[i]^.Name:='RT_ICON';
        4: TypeDir.Dir[i]^.Name:='RT_MENU';
        5: TypeDir.Dir[i]^.Name:='RT_DIALOG';
        6: TypeDir.Dir[i]^.Name:='RT_STRING';
        7: TypeDir.Dir[i]^.Name:='RT_FONTDIR';
        8: TypeDir.Dir[i]^.Name:='RT_FONT';
        9: TypeDir.Dir[i]^.Name:='RT_ACCELERATOR';
        10: TypeDir.Dir[i]^.Name:='RT_RCDATA';
        12: TypeDir.Dir[i]^.Name:='RT_GROUP_CURSOR';
        14: TypeDir.Dir[i]^.Name:='RT_GROUP_ICON';

        11: TypeDir.Dir[i]^.Name:='RT_MESSAGETABLE';
        16: TypeDir.Dir[i]^.Name:='RT_VERSION';
        17: TypeDir.Dir[i]^.Name:='RT_DLGINCLUDE';
        19: TypeDir.Dir[i]^.Name:='RT_PLUGPLAY';
        20: TypeDir.Dir[i]^.Name:='RT_VXD';
        21: TypeDir.Dir[i]^.Name:='RT_ANICURSOR';
      end;
      TypeDir.Dir[i]^.Name:=TypeDir.Dir[i]^.Name + ' (' + IntToStr(TypeDir.Dir[i]^.ID) + ')';
      TypeDir.Dir[i]^.NamePresent:=true;
    end;
  b.Free;
}
end;

constructor TResourceSection.Create(efile:TObject);
begin
  fTyp:=stResource;
  fExecFile:=efile;
end;

procedure TResourceSection.SaveToFile(DHF: TStream; var DAS: TextFile);
begin
  inherited SaveToFile(DHF, DAS);
end;

procedure TResourceSection.LoadFromFile(DHF: TStream; var DAS: TextFile);
begin
  inherited LoadFromFile(DHF, DAS);
end;

//============================================================================================================
// TResourceTabSheet class
//============================================================================================================

{$IFDEF GUI_B}
constructor TResourceTabSheet.Create(AOwner:TComponent; ssection: TResourceSection);

procedure DisplayTree(Directory: TResourceDirectory; Node: TTreeNode);
var TreeNode: TTreeNode;
    i: integer;
begin
  for i:=0 to Length(Directory.Dir)-1 do begin
    if Directory.Dir[i]^.NamePresent then TreeNode:=Tree.Items.AddChild(Node,Directory.Dir[i]^.name)
    else TreeNode:=Tree.Items.AddChild(Node,IntToStr(Directory.Dir[i]^.ID));
//    TreeNode.ImageIndex:=0;
//    TreeNode.SelectedIndex:=1;
    DisplayTree(Directory.Dir[i]^, TreeNode)
  end;
  for i:=0 to Length(Directory.Data)-1 do begin
    if Directory.Data[i].NamePresent then TreeNode:= Tree.Items.AddChild(Node,Directory.Data[i].name)
    else TreeNode:= Tree.Items.AddChild(Node,IntToStr(Directory.Data[i].ID));
    TreeNode.Data:=@Directory.Data[i];
  end;
end;
   var iii:ticon;
begin
  inherited Create(AOwner);
  parent:=(AOwner as TPageControl);
  PageControl:=(AOwner as TPageControl);
  self.Section:=sSection;

  Panel:= TPanel.Create(self);
  Panel.Parent:=self;
  Panel.Left:=8;
  Panel.Top:=8;
  Panel.Width:=self.Width-16;
  Panel.Height:=self.Height-16;
  Panel.Anchors:=[akLeft,akRight,akTop,akBottom];
  Panel.BevelInner:=bvRaised;
  Panel.BevelOuter:=bvLowered;

  ImageList:=TImageList.Create(Panel);

//  ImageList.ResourceLoad(rtBitmap,'open1',clWhite);
  ImageList.GetResource(rtIcon,'dir2',16,[lrDefaultSize],clWhite);
//  iii:=ticon.Create;

  Tree:= TTreeView.Create(Panel);
  Tree.Parent:=Panel;
  Tree.Top:=16;
  Tree.Left:=16;
  Tree.Height:=Panel.Height-64;
  Tree.Width:=240;
  Tree.Anchors:=[akLeft,akRight,akTop,akBottom];
  Tree.OnChange:=TreeChange;
//  Tree.Images:=ImageList;
  DisplayTree(Section.TypeDir,Tree.Items.Add(nil,'Resources'));
//  Tree.Items[0].ImageIndex:=1;
{
  Memo:=TMemo.Create(Panel);
  Memo.Parent:=Panel;
  Memo.Left:=272;
  Memo.Top:=16;
  Memo.Width:=Panel.Width-256-16-16;
  Memo.Height:=Panel.Height-32;
  Memo.Anchors:=[akRight,akTop,akBottom];
  Memo.WordWrap:=false;
  Memo.ScrollBars:=ssBoth;
  Memo.Enabled:=false;
}
  StringBox:=TListBox.Create(Panel);
  StringBox.Parent:=Panel;
  StringBox.Left:=272;
  StringBox.Top:=16;
  StringBox.Width:=Panel.Width-256-16-16;
  StringBox.Height:=Panel.Height-32;
  StringBox.Anchors:=[akRight,akTop,akBottom];
//  StringBox.ScrollWidth:=500;
  StringBox.Visible:=false;
  StringBox.Enabled:=false;

  Image:= TImage.Create(Panel);
  Image.Parent:=Panel;
  Image.Left:=StringBox.Left;
  Image.Top:=StringBox.Top;
  Image.Visible:=false;

  ResLabel:=TLabel.Create(Panel);
  ResLabel.Parent:=Panel;
  ResLabel.Left:=16;
  ResLabel.Top:=Panel.Height-32;
  ResLabel.Anchors:=[akLeft,akBottom];
//  ResLabel
{
  StringLabel:=TLabel.Create(Panel);
  StringLabel.Parent:=Panel;
}
end;

destructor TResourceTabSheet.Destroy;
begin
//  StringLabel.Free;
  ResLabel.Free;
  StringBox.Free;
  ImageList.Free;
  Tree.Free;
  Panel.Free;
  inherited;
end;

procedure TResourceTabSheet.TreeChange(Sender: TObject; Node: TTreeNode);
begin
  if Node.HasChildren then Exit;
  with TResourceData(Node.Data^) do begin
    case Typ of
      rt_String: begin
        Image.Visible:=false;
        StringBox.Items:=Data as TStringList;
        StringBox.Visible:=true;

        StringBox.Enabled:=true;
      end;
      rt_Icon: begin
        StringBox.Visible:=false;
//        Image.Canvas.Draw(0,0,Icon);//:=Icon.Canvas;
        Image.Visible:=true;
      end;
      rt_Bitmap: begin
        StringBox.Visible:=false;
//        Image.Canvas.Draw(0,0,Obr);//:=Icon.Canvas;
        Image.Visible:=true;
      end;
    end;

    ResLabel.Caption:='RVA: '+IntToHex(DataRVA,8)+'       Size: '+IntToHex(Size,8);
  end;
end;

procedure TResourceTabSheet.Translate(ini: TMemINIFile; error: string);
begin
  Caption:=ini.ReadString('Resource','Caption',error);
end;

{$ENDIF}


end.
