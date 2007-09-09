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


const

  HEX_CIFRY : set of char = ['0'..'9', 'A'..'F'];
  MaxByte = 255;
  MaxCardinal = $FFFFFFFF;

  // Data types
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
  DataTypeSizes: array [0..DataTypesCount-1] of Byte = ( (1),(2),(4),(8),(4),(8),(10),(1),(1),(1),(1) );

  // Instruction line constants
  ilMaxAddressLength = 8;
  ilMaxParsedLength = 24;
  ilInstructionMnemonicIndex = ilMaxAddressLength + 1 + ilMaxParsedLength + 1 + 1; // 1 based index

  // Exe format descriptions
  fdCOM = 'COM (16-bit)';
  fdMZ = 'MZ - DOS executable (16-bit)';
  fdNE = 'NE - New Executable (16-bit)';
  fdPE = 'PE - Portable Executable (32-bit)';
  fdELF = 'ELF - Executable and Linkable Format';
  fdCustom = 'Custom file format';

  // TatraDAS version constants
  TatraDASVersion: cardinal = $00029700;
  TatraDASDate: string = '02. 09. 2007';
  TatraDASProjectVersion = $00030002;
  ShortTatraDASVersion: string = '2.9.8';
  TatraDASFullName: string = 'TatraDAS disassembler';
  TatraDASFullNameVersion: string = 'TatraDAS disassembler 2.9.8 alpha';


  TranslateErrorStr = 'TRANS ERROR';
  CodeArrayReserve = 20;
  MaxProgressNameLength = 25;


type

  TTatraDASStringList = TSynEditStringList;

  TDataChangeType = (dcItems, dcBytes, dcMaxAddress, dcEndSection, dcCode, dcNormal);

  TDisassembleType = (dtBytes, dtMaxAddress, dtNormal);

  TDataChangeOptions = record
    DataType: byte;
    DatatypeSize: byte;
    Signed: boolean;
    Option: TDataChangeType;
    Value: cardinal;
  end;

  TDisassembleFormOptions = record
    Option: TDisassembleType;
    Value: cardinal;
    Bit32: boolean;
    Recursive: boolean;
  end;

  TTatraDASOptions = record
    AutoFormat: boolean;          // automatic file format selection
    ProcDetect: boolean;          // procedure detection
    StringDetect: boolean;        // string detection
    RemoveJump: boolean;
    RemoveImport: boolean;
    RemoveExport: boolean;
  end;

  TCardinalDynamicArray = array of cardinal;
  TByteDynamicArray = array of byte;
  TBooleanDynamicArray = array of boolean;
  TStringDynamicArray = array of string;

  TPByteDynamicArray = ^TByteDynamicArray;

  TSaveOption = (soProject, soDisassembly, soNASM, soAddress, soParsed, soDisassembled, soJump, soCall, soExport, soImport, soEntryPoint);
  TSaveOptions = set of TSaveOption;

  TLineType = (ltInstruction, ltComment, ltJumpRef, ltCallRef, ltLoopRef, ltImportRef, ltExportRef, ltEntryPointRef, ltEmpty);

  TExecFileFormat = (ffError, ffUnknown, ffCustom, ffPE, ffMZ, ffCOM, ffNE, LE, LX, ffELF);

  TCPUType = (_80386, _80486, Pentium);


  TEntryTableEntryType = (etEmpty,etFixed,etMovable);


  TFixedBundle = packed record
    Flag: byte;
    Offset: word;
  end;

  TMovableBundle = packed record
    Flag: byte;
    Int3F: word;
    Segment: byte;
    Offset: word;
  end;

  TEntryTableEntry = record
    count: byte;
    typ: TEntryTableEntryType;
    Fixed: array of TFixedBundle;
    Movable: array of TMovableBundle;
  end;




  TMyMemoryStream = class(TMemoryStream)
    procedure SetMemory(Ptr: pointer; Size: LongInt);
  end;

  // Progress types
  TProcessText = record
    Disassemblying, Indentifying, PreparingOutput: string;
    LoadingDAS, LoadingDHF, SavingDAS, SavingDHF: string;
  end;

  TProgressError = (errNone, errOpen, errUnknownFormat, errBadFormat, errDASNotFound, errBadProjectVersion, errSave, errCanceled, errUserTerminated);

  TProgressData = record
    Name: string;
    Position: cardinal;
    Maximum: cardinal;
    Finished: boolean;
    ErrorStatus: TProgressError;
    Result: Pointer;
  end;

  ITranslatable = interface
    ['{E293B4CE-B91A-42FE-884A-27F54EEAD8DD}']
    procedure Translate;
  end;

var
  ProcessText: TProcessText;
  ProgressData: TProgressData;


procedure ReadStringFromStream(a:TStream; pos:cardinal; var retazec:string);
function StreamReadAnsiString (AStream: TStream): string;
Procedure StreamWriteAnsiString (AStream: TStream; AString: String);

function NonNegative(Number: integer): cardinal;

function MyIsNan(const AValue: Single): Boolean; overload;
function MyIsNan(const AValue: Double): Boolean; overload;
{$IFDEF GUI_B}
procedure SM(msg: string);
{$ENDIF}


Implementation


{$IFDEF GUI_B}
uses
  Dialogs;

procedure SM(msg: string);
begin
  ShowMessage(msg);
end;
{$ENDIF}



procedure TMyMemoryStream.SetMemory(Ptr: Pointer; Size: LongInt);
begin
  SetPointer(Ptr, Size);
end;



procedure ReadStringFromStream(a: TStream; pos:cardinal; var retazec:string);
var
  StartPosition, StrLen: cardinal;
  TheChar: Char;
begin

  a.Seek(Pos, 0);
  StartPosition:= a.Position;
  try
    repeat
      a.Read(TheChar, 1);
    until TheChar = #0;
  except
  end;
  StrLen:= a.Position - StartPosition - 1; // minus #0 char.
  if StrLen > 0 then begin
    SetLength(retazec, StrLen);
    a.Seek(Pos, 0);
    a.Read(retazec[1], StrLen);
  end;
end;



// From FPC
Function StreamReadAnsiString(AStream: TStream): String;
Type
  PByte = ^Byte;
Var
  TheSize : Longint;
  P : PByte ;
begin
  AStream.ReadBuffer (TheSize, SizeOf(TheSize));
  SetLength(Result, TheSize);
  if TheSize > 0 then
   begin
     AStream.ReadBuffer (Pointer(Result)^,TheSize);
     {$IFDEF FPC}
     P:=Pointer(Result) + TheSize;
     {$ELSE}
     P:=Ptr(LongInt(Pointer(Result)) + TheSize);
     {$ENDIF}
     p^:=0;
   end;
 end;



// From FPC
Procedure StreamWriteAnsiString (AStream: TStream; AString: String);
Var L : Longint;
begin
  L:=Length(AString);
  AStream.WriteBuffer (L,SizeOf(L));
  AStream.WriteBuffer (Pointer(AString)^,L);
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



function NonNegative(Number: integer): cardinal;
begin
  if Number < 0 then
    result:= 0
  else
    result:= Number;
end;



end.
