unit procmat;

{$INCLUDE 'delver.inc'}

{$DEFINE RangeChecking}

interface

uses 
  SysUtils,
  Classes,  
  Math,
  
  {$IFDEF GUI_B}
    SynEditTextBuffer
  {$ELSE}
    TatraDAS_SynEditStringList
  {$ENDIF}
  ;

const HEX_CIFRY : set of char = ['0'..'9','A'..'F'];
      maxByte=255;
      dtByte = 0;
      dtWord = 1;
      dtDword = 2;
      dtQword = 3;
      dtSingle = 4;
      dtDouble = 5;
      dtDoubleEx = 6;
      dtPascalStr = 7;
      dtCStr = 8;
      dtPascalUniCodeStr = 9;
      dtCUniCodeStr = 10;

      DataTypesCount = 11;
      DataTypeSizes:array [0..DataTypesCount-1] of Byte = ( (1),(2),(4),(8),(4),(8),(10),(1),(1),(1),(1) );

const CodeArrayReserve=20;
      MaxCardinal = $FFFFFFFF;

type

  TTatraDASStringList = TSynEditStringList;
  

  TProgressFunction = Function(a,b: cardinal; c: string): boolean;

  TDataChangeType = (dcItems, dcBytes, dcMaxAddress, dcEndSection, dcCode, dcNormal);

  TDisassembleType = TDataChangeType;

  TDataChangeOptions = record
    datatype: byte;
    datatypesize: byte;
    signed: boolean;
    option: TDataChangeType;
    value: cardinal;
  end;

  TDisassembleFormOptions = record
    option: TDisassembleType;
    value: cardinal;
    bit32: boolean;
    recursive: boolean;
  end;

  TTatraDASOptions = record
    autoformat: boolean;          // automatic file format selection
    procdetect: boolean;          // procedure detection
    stringdetect: boolean;        // string detection
    removejump: boolean;          
    removeimport: boolean;
    removeexport: boolean;
  end;

  TProcessText = record
    Disassemblying,Indentifying,PreparingOutput:string;
    LoadingDAS, LoadingDHF, SavingDAS, SavingDHF:string;
  end;

  TCardinalDynamicArray = array of cardinal;
  TByteDynamicArray = array of byte;
  TBooleanDynamicArray = array of boolean;
  TStringDynamicArray = array of string;

  TPByteDynamicArray = ^TByteDynamicArray;


  TSaveOption = (soProject, soDisassembly, soNASM, soAddress, soParsed, soDisassembled, soJump, soCall, soExport, soImport, soEntryPoint);
  TSaveOptions = set of TSaveOption;

  TLineType = (ltInstruction, ltComment, ltJumpRef, ltCallRef, ltLoopRef, ltImportRef, ltExportRef, ltEntryPointRef, ltEmpty);

  TExecFileFormat = (PE, MZ, COM, NE, LE, LX, ELF, ffError, ffCustom, ffUnknown);

  TCpuType = (_80386,_80486,Pentium);

  TTatraDASStatus = (tsEmpty, tsOpened, tsDisassembled, tsProject);

  TSectionInfoEntry = record
    offset,size:cardinal;
    logoffset,logsize:cardinal;
    bit32:boolean;
  end;

  TStatus = (opened,finished);

  TSectionInfo = array of TSectionInfoEntry;

  TMyMemoryStream = class(TMemoryStream)
    procedure SetMemory(Ptr:pointer; Size: LongInt);
  end;

(*
  TCtrls = record
{$IFDEF GUI_B}
    PageControl: TPageControl;
    INIFile: TMemINIFile;
    InfoMemo: TMemo;
    InfoGrid: TStringGrid;
    InfoListView: TListView;
    InfoFormatEdit: TEdit;
{$ENDIF}
    ProgressFunction: TProgressFunction;
  end;
*)


  TTextFileStream = class(TFileStream)
    function ReadLine: string;
    procedure WriteLine(const aLine: string);
  end;


const
  NewTatraDASProjectVersion = $00040000;
  TatraDASVersion:cardinal=$00029700;
  TatraDASDate:cardinal=$23122004;
  TatraDASProjectVersion=$00030001;
  ShortTatraDASVersion:string='2.9.7';
  TatraDASFullName:string='TatraDAS disassembler';
  TatraDASFullNameVersion:string='TatraDAS disassembler 2.9.8 alpha';

var ProcessText: TProcessText;


    ProgressPosition: cardinal;
    ProgressFinished: boolean;
    ProgressName: string;

    Modified: boolean; // stav otvoreneho zdisassemblovane suboru resp. projektu

procedure ReadStringFromStream(a:TStream; pos:cardinal; var retazec:string);
function Nezaporne(cislo:integer):cardinal;
function GetTargetAddress(s:string; var address:cardinal):boolean;

function IntToSignedHex(Value: integer; Digits: Integer): string;

function InsertStr(const Source:string; const Dest: string; index:integer):string;
function FirstCommaToPoint(AText: string):string;


function MyIsNan(const AValue: Single): Boolean; overload;
function MyIsNan(const AValue: Double): Boolean; overload;

function CarToStr(value: cardinal): string;
function CarToHex(Value: cardinal; Digits: integer): string;

function IsHexNumber(number: string): boolean;
function IsNumber(number: string): boolean;

{$IFDEF GUI_B}
procedure SM(msg: string);
{$ENDIF}

Implementation

{$IFDEF GUI_B}
uses dialogs;

procedure SM(msg: string);
begin
  ShowMessage(msg);
end;
{$ENDIF}



procedure TMyMemoryStream.SetMemory(Ptr: Pointer; Size: LongInt);
begin
  SetPointer(Ptr, Size);
end;

function Nezaporne(cislo:integer):cardinal;
begin
  if cislo<0 then result:=0 else result:=cislo;
end;

procedure ReadStringFromStream(a:TStream; pos:cardinal; var retazec:string);
var znak:char;
    r:string[255];
begin
  r:='';
  a.Seek(pos,0);
  a.Read(znak,1);
  // befunguje to tak ako ma, position a siye su stale 0
  while (znak<>#0) and (not (a.Position > a.size)) do begin
    r:=r + znak;
    a.read(znak,1);
  end;
  retazec:=r;
end;
{
procedure TExecutableFile.Init;
begin
end;
}
function GetTargetAddress(s:string; var address:cardinal):boolean;
//function GetTargetAddress(s:string; var address:cardinal):byte;
var i:cardinal;
begin
// Este to nie je spravne osetrene!!!
  if length(s)<43 then begin
    result:=false;
    exit;
  end;
    result:=true;
    address:=0;
    i:=34;
// JMP, Jxx
    if s[i]='J' then begin
      while s[i]<>' ' do inc(i);
      inc(i,3);
      if (s[i-1]='x') and (s[i-2]='0') and (s[i] in HEX_CIFRY)
        then address:=cardinal(StrToIntDef('$'+trim(copy(s,i,50)),-1)) else result:=false;
    end
// CALL
    else if copy(s,i,4)='CALL' then begin
      inc(i,7);
      if (s[i-1]='x') and (s[i-2]='0') and (s[i] in HEX_CIFRY)
        then address:=cardinal(StrToIntDef('$'+trim(copy(s,i,50)),-1))
      else result:=false;
    end
// LOOPxx
    else if copy(s,i,4)='LOOP' then begin
      while s[i]<>' ' do inc(i);
      inc(i,3);
      if (s[i-1]='x') and (s[i-2]='0') and (s[i] in HEX_CIFRY)
        then address:=cardinal(StrToIntDef('$'+trim(copy(s,i,50)),-1)) else result:=false;
    end
    else result:=false;
    if address=$FFFFFFFF then result:=false;
end;


function IntToSignedHex(Value: integer; Digits: Integer): string;
begin
  if Value >= 0 then result:=IntToHex(Value,Digits)
  else begin
    Value:=Abs(Value);
    result:='-' + IntToHex(Value,Digits);
  end;
end;


function InsertStr(const Source:string; const Dest: string; index:integer):string;
var s: string;
begin
  s:=dest;
  Insert(Source,s,index);
  result:=s;
end;

function FirstCommaToPoint(AText: string):string;
var i: integer;
begin
  for i:=1 to Length(AText) do
    if AText[i]=',' then begin
      AText[i]:='.';
      break;
    end;
  result:=AText;
end;

function MyIsNan(const AValue: Single): Boolean;
begin
  Result := ((PInteger(@AValue)^ and $7F800000)  = $7F800000) and
            ((PInteger(@AValue)^ and $007FFFFF) <> $00000000)
end;

function MyIsNan(const AValue: Double): Boolean;
begin
  Result := ((PInt64(@AValue)^ and $7FF0000000000000)  = $7FF0000000000000) and
            ((PInt64(@AValue)^ and $000FFFFFFFFFFFFF) <> $0000000000000000)
end;

function StringReverz(AString: string): string;
var i: integer;
begin
  result:='';
  for i:=Length(AString) downto 1 do result:=result + AString[i];
end;

function CarToStr(value: cardinal): string;
begin
  while value <> 0 do begin
    result:=result + Chr(value mod 10);
    value:=value div 10;
  end;
  result:=StringReverz(result);
end;

function CarToHex(Value: cardinal; Digits: integer): string;
begin
  while value <> 0 do begin
    result:=result + IntToHex(value mod 16,1);
    value:=value div 16;
  end;
  result:=StringReverz(result);
end;

function IsHexNumber(number: string): boolean;
var i: integer;
begin
  result:=false;
  for i:=1 to Length(Number) do if not (Number[i] in ['0'..'9','A'..'F','a'..'f']) then exit;
  result:=true;
end;

function IsNumber(number: string): boolean;
var Index,i: integer;
begin
  Index:=1;
  result:=false;
  if Number = '' then Exit;
  if Number = '0' then begin
    result:=true;
    exit;
  end;
  if Number[1] = '-' then Inc(index);
  if Length(Number) < Index then Exit;
  if Number[Index] = '0' then Exit;
  for i:=Index to Length(Number) do if not (Number[i] in ['0'..'9']) then exit;
  result:=true;
end;


{ TTextFileStream }

function TTextFileStream.ReadLine: string;
var Character,SecondCharacter: Char;
begin
  result:='';
  if Position = Size then Exit;
  Read(Character,1);

  while (Character <> #10) and (Character <> #13) do begin
    result:=result + Character;
    if Position = Size then Exit;
    Read(Character,1);
  end;

  if Position = Size then Exit;
  Position:=Position + 1;
  Read(SecondCharacter,1);
  if not ((Character = #10) and (SecondCharacter = #13)) or ((Character <> #13) and (SecondCharacter <> #13)) then
    Position:=Position - 1;
end;


procedure TTextFileStream.WriteLine(const aLine: string);
var EndOfLine: Char;
begin
  EndOfLine:=#10;
  Write(aLine,Length(aLine));
  Write(EndOfLine,1);
end;


end.
