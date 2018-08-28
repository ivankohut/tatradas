unit FilesUnit;



interface

uses
  Classes, SysUtils;

type

  { TFileNode }

  TFileNodeClass = class of TFileNode;

  TFileNode = class
  private
    fFileName: string;
    fPathToTopDir: string;
    function GetName: string;
  public
    constructor Create(const ARootDirPath, ARelativeFileName, APathTopDir: string); virtual;
    destructor Destroy; override;

    property Name: string read GetName;
    property FullPathName: string read fFileName;
    property PathToTopDir: string read fPathToTopDir;
  end;


  { TDirNode }

  TDirNode = class(TFileNode)
  private
    fFiles: array of TFileNode;
    fDirs: array of TDirNode;
    function GetFile(Index: Integer): TFileNode;
    function GerDir(Index: Integer): TDirNode;
    function GetFileCount: Integer;
    function GetDirCount: Integer;
  public
    constructor Create(const ARootDirName, ARelativeDirName: string; APathTopDir: string; AExtension: string; AFileNodeClass: TFileNodeClass); reintroduce; overload;
    destructor Destroy; override;

    property Files[Index: Integer]: TFileNode read GetFile;
    property Dirs[Index: Integer]: TDirNode read GerDir;
    property FileCount: Integer read GetFileCount;
    property DirCount: Integer read GetDirCount;
  end;


function AddPathDelimiter(const AFileName: string; UseOSDelimiter: Boolean = False): string;
function AppendToInternalPath(const AInternalPath, AFileName: string): string;
function InternalToOsPath(const AInternalPath: string): string;
function OsToInternalPath(const AOsPath: string): string;
function InternalChangeFileExt(const AInternalFileName, AExtension: string): string;
procedure SaveStringToFile(const AString, AFileName: string);
function LoadStringFromFile(const AFileName: string): string;
function GetFileSize(AFileName: string): Int64;

const
  InternalDirDelimiter = '/';


implementation


{ TFileNode }


function TFileNode.GetName: string;
var
  I: Integer;
begin
  I := LastDelimiter(InternalDirDelimiter, fFileName);
  Result := Copy(fFileName, I + 1, MaxInt);
end;



constructor TFileNode.Create(const ARootDirPath, ARelativeFileName, APathTopDir: string);
begin
  fFileName := OsToInternalPath(ARelativeFileName);
  fPathToTopDir := APathTopDir;
end;



destructor TFileNode.Destroy;
begin
  inherited Destroy;
end;


{ TDirNode }


function TDirNode.GetFile(Index: Integer): TFileNode;
begin
  Result := fFiles[Index];
end;



function TDirNode.GerDir(Index: Integer): TDirNode;
begin
  Result := fDirs[Index];
end;



function TDirNode.GetDirCount: Integer;
begin
  Result := Length(fDirs);
end;



function TDirNode.GetFileCount: Integer;
begin
  Result := Length(fFiles);
end;



constructor TDirNode.Create(const ARootDirName, ARelativeDirName: string; APathTopDir: string; AExtension: string; AFileNodeClass: TFileNodeClass);
var
  sr: TSearchRec;
  DirNames: TStringList;
  FileNames: TStringList;
  DirIndex: Integer;
  FileIndex: Integer;
  CurrentDirRelative: string;
  RootDirName: string;
begin
  fFileName := OsToInternalPath(ARelativeDirName);
  fPathToTopDir := APathTopDir;
  if ARelativeDirName <> '' then
    CurrentDirRelative := AddPathDelimiter(ARelativeDirName, True);
  RootDirName := AddPathDelimiter(ARootDirName, True);

  DirNames := TStringList.Create;
  FileNames := TStringList.Create;
  if FindFirst(RootDirName + CurrentDirRelative + '*', faAnyFile, sr) = 0 then begin
    repeat
      if (sr.Name = '.') or (sr.Name = '..') then
        Continue;
      if (sr.Attr and faDirectory) <> 0 then
        DirNames.Add(CurrentDirRelative + sr.Name)
      else if UpperCase(ExtractFileExt(sr.Name)) = UpperCase(AExtension) then
        FileNames.Add(CurrentDirRelative + sr.Name);
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;

  SetLength(fFiles, FileNames.Count);
  FileNames.Sort;
  for FileIndex := 0 to FileNames.Count - 1 do
    fFiles[FileIndex] := AFileNodeClass.Create(RootDirName, FileNames[FileIndex], '..' + InternalDirDelimiter + APathTopDir);

  SetLength(fDirs, DirNames.Count);
  DirNames.Sort;
  for DirIndex := 0 to DirNames.Count - 1 do
    fDirs[DirIndex] := TDirNode.Create(RootDirName, DirNames[DirIndex], '..' + InternalDirDelimiter + APathTopDir, AExtension, AFileNodeClass);

  DirNames.Free;
  FileNames.Free;
end;



destructor TDirNode.Destroy;
var
  DirIndex, FileIndex: Integer;
begin
  inherited Destroy;
  for DirIndex := 0 to DirCount - 1 do
    fDirs[DirIndex].Free;
  for FileIndex := 0 to FileCount - 1 do
    fFiles[FileIndex].Free;
end;


// Various utilities


function InternalChangeFileExt(const AInternalFileName, AExtension: string): string;
var
  i: LongInt;
begin
  I := Length(AInternalFileName);
  while (I > 0) and not (AInternalFileName[I] in [InternalDirDelimiter, '.']) do
    Dec(I);
  if (I = 0) or (AInternalFileName[I] <> '.') then
    I := Length(AInternalFileName) + 1;
  Result := Copy(AInternalFileName, 1, I - 1) + AExtension;
end;



function AddPathDelimiter(const AFileName: string; UseOSDelimiter: Boolean = False): string;
var
  Delimiter: Char;
begin
  if UseOSDelimiter then
    Delimiter := PathDelim
  else
    Delimiter := InternalDirDelimiter;

  if Length(AFileName) = 0 then
    Result := Delimiter
  else if AFileName[Length(AFileName)] = Delimiter then
    Result := AFileName
  else
    Result := AFileName + Delimiter;
end;



function AppendToInternalPath(const AInternalPath, AFileName: string): string;
begin
  Result := AddPathDelimiter(AInternalPath) + AFileName;
end;



function InternalToOsPath(const AInternalPath: string): string;
var
  i: Integer;
begin
  Result := AInternalPath;
  if InternalDirDelimiter <> PathDelim then
    for i := 1 to Length(AInternalPath) do
      if AInternalPath[i] = InternalDirDelimiter then
        Result[i] := PathDelim;
end;



function OsToInternalPath(const AOsPath: string): string;
var
  i: Integer;
begin
  Result := AOsPath;
  if InternalDirDelimiter <> PathDelim then
    for i := 1 to Length(AOsPath) do
      if AOsPath[i] = PathDelim then
        Result[i] := InternalDirDelimiter;
end;



procedure SaveStringToFile(const AString, AFileName: string);
var
  f: TextFile;
begin
  AssignFile(f, AFileName);
  Rewrite(f);
  Write(f, AString);
  CloseFile(f);
end;



function LoadStringFromFile(const AFileName: string): string;
var
  f: TFileStream;
begin
  f := TFileStream.Create(AFileName, fmOpenRead);
  SetLength(Result, f.Size);
  f.Read(Result[1], f.Size);
  f.Free;
end;



function GetFileSize(AFileName: string): Int64;
var
  Stream: TFileStream;
begin
  Stream := nil;
  try
    Stream := TFileStream.Create(AFileName, fmOpenRead);
    Result := Stream.Size;
  except
    Result := -1;
  end;
  Stream.Free;
end;

end.
