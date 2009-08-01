unit procmat;

{$INCLUDE 'delver.inc'}

{$DEFINE RangeChecking}

interface

uses
  SysUtils,
  Classes,
  Math,
  ProgressManagerUnit,
  {$IFDEF GUI_B}
    SynEditTextBuffer,
  {$ELSE}
    TatraDAS_SynEditStringList,
  {$ENDIF}
  LoggerUnit,
  StringRes
  ;


const
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
  ilAddressLength = 8;
  ilMaxParsedLength = 24;

    // 1 based (string) indices
  ilParsedIndex = ilAddressLength + 2;
  ilInstructionMnemonicIndex = ilAddressLength + 1 + ilMaxParsedLength + 1 + 1;

  // Exe format descriptions
  fdCOM = 'COM (16-bit)';
  fdMZ = 'MZ - DOS executable (16-bit)';
  fdNE = 'NE - New Executable (16-bit)';
  fdPE = 'PE - Portable Executable (32-bit)';
  fdELF = 'ELF - Executable and Linkable Format';
  fdCustom = 'Custom file format';

  // TatraDAS version constants
  TatraDASVersion: cardinal = $00029900;
  TatraDASDate = '1. 8. 2009';
  TatraDASProjectVersion = $00030002;
  ShortTatraDASVersion = '2.9.9';
  TatraDASFullName = 'TatraDAS disassembler';
  TatraDASFullNameVersion = TatraDASFullName + ' ' + ShortTatraDASVersion;
  CopyrightStr = 'Ivan Kohut (c) 2009';
  DASFileFirstLine = ';DisASsembled file, Original file: %s  ' + TatraDASFullNameVersion + ', ' + CopyrightStr;
  DASFileExtension = '.das';

  CodeArrayReserveSize = 20;
  MaxProgressNameLength = 25;


type

  TTatraDASStringList = TSynEditStringList;

  TDataChangeType = (dcItems, dcBytes, dcMaxAddress, dcEndSection, dcCode, dcNormal);

  TDisassembleType = (dtBytes, dtMaxAddress, dtNormal);

  TDataChangeOptions = record
    DataType: Byte;
    Signed: Boolean;
    Option: TDataChangeType;
    Value: Cardinal;
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

  TExportOption = (eoDAS, eoCustomDAS, eoNASM);
  TExportCustomDASOption = (soAddress, soParsed, soDisassembled, soJump, soCall, soExport, soImport, soEntryPoint);
  TExportCustomDASOptions = set of TExportCustomDASOption;

  TLineType = (ltInstruction, ltComment, ltJumpRef, ltCallRef, ltLoopRef, ltImportRef, ltExportRef, ltEntryPointRef, ltEmpty);




  // NE file format specific

  TEntryTableEntryType = (etEmpty, etFixed, etMovable);

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

  // Exceptions - general exceptions which can be used everywhere in TatraDAS

  ETatraDASException = class (Exception);

  // Raise in case of unexpected state = indicates bug
  // Messages in English only
  EIllegalState = class (ETatraDASException);

  // User exceptions,
  // Should be translated
  // User terminated a process
//  EUserTerminatedProcess = class (ETatraDASException);
  EFileCorrupted = class (ETatraDASException); // FileCorruptedStr // TODO


  TMyMemoryStream = class(TMemoryStream)
    procedure SetMemory(Ptr: pointer; Size: LongInt);
  end;

  // Progress types
  TProcessText = record
    Disassembling, PreparingOutput: string;
    LoadingDAS, LoadingDHF, SavingDAS, SavingDHF: string;
  end;

  TPhaseFinishedProc = procedure;

  TProgressData = record
    AbortExecution: Boolean;
    Result: Pointer;
  end;


  ITranslatable = interface
    ['{E293B4CE-B91A-42FE-884A-27F54EEAD8DD}']
    procedure Translate;
  end;


var
  ProcessText: TProcessText;
  ProgressData: TProgressData;
  ProgressManager: TProgressManager;


procedure ReadStringFromStream(a:TStream; pos:cardinal; var retazec:string);
function StreamReadAnsiString (AStream: TStream): string;
Procedure StreamWriteAnsiString (AStream: TStream; AString: String);

function NonNegative(Number: integer): cardinal;

function MyIsNan(const AValue: Single): Boolean; overload;
function MyIsNan(const AValue: Double): Boolean; overload;

function CompareValue(n1, n2: cardinal): integer;

procedure WriteLnToStream(AStream: TStream); overload;
procedure WriteLnToStream(AStream: TStream; const ALine: string); overload;

{$IFDEF GUI_B}
procedure SM(msg: string);
{$ENDIF}

type
  TDisplayMessageProc = procedure (const AMessage: string);

procedure ProcessException(E: Exception; DisplayMessageProc: TDisplayMessageProc);


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


function CompareValue(n1, n2: cardinal): integer;
begin
  if n1 = n2 then
    result:= 0
  else if n1 < n2 then
    result:= -1
  else
    result:= 1;
end;



procedure WriteLnToStream(AStream: TStream; const ALine: string);
begin
  AStream.Write((PChar(ALine + #13#10))^, Length(ALine) + 2);
end;



procedure WriteLnToStream(AStream: TStream);
begin
  WriteLnToStream(AStream, '');
end;



procedure ProcessException(E: Exception; DisplayMessageProc: TDisplayMessageProc);
var
  ErrorMessage: string;
begin
  // Note: Logging is in english.
  if E is EIllegalState then begin
    ErrorMessage := IllegalStateStr + ' ' + E.Message;
    Logger.Fatal(ErrorMessage);
  end
  else if E is ETatraDASException then begin
    ErrorMessage := E.Message;
    Logger.Fatal(E.ClassName + ': ' + E.Message);
  end
  else if E is EAbort then begin
    ErrorMessage := '';
    Logger.Info('Execution aborted by user.');
  end
  else begin
    ErrorMessage := ErrorStr + ': ' + E.Message;
    Logger.Fatal('Exception ' + E.ClassName + ': ' + E.Message);
  end;

  if ErrorMessage <> '' then
    DisplayMessageProc(ErrorMessage);
end;


initialization
{
  Logger.AddListener(TTextFileLoggerListener.Create(ExpandFileName('disasm.log')));
  Logger.LogLevel := llDebug;
}

end.
