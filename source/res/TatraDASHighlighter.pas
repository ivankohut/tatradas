{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Code template generated with SynGen.
The original code is: TatraDASHighlighter.pas, released 2009-08-01.
Description: Syntax Parser/Highlighter
The initial author of this file is Ivan.
Copyright (c) 2009, all rights reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

-------------------------------------------------------------------------------}

unit TatraDASHighlighter;

{$I SynEdit.inc}

interface

uses
{$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
{$ELSE}
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (
    tkCalls,
    tkComment,
    tkEntryPoint,
    tkIdentifier,
    tkInstructions,
    tkJumps,
    tkKey,
    tkKlucSlova,
    tkLoops,
    tkNull,
    tkReturns,
    tkSpace,
    tkString,
    tkTest,
    tkUnknown);

  TRangeState = (rsUnKnown, rsPascalComment, rsAsmComment, rsString);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

const
  MaxKey = 136;

type
  TSynTatraDASSyn = class(TSynCustomHighlighter)
  private
    fLineRef: string;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    fRange: TRangeState;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0 .. MaxKey] of TIdentFuncTableFunc;
    fCallsAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fEntryPointAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fInstructionsAttri: TSynHighlighterAttributes;
    fJumpsAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fKlucSlovaAttri: TSynHighlighterAttributes;
    fLoopsAttri: TSynHighlighterAttributes;
    fReturnsAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fTestAttri: TSynHighlighterAttributes;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func3: TtkTokenKind;
    function Func6: TtkTokenKind;
    function Func8: TtkTokenKind;
    function Func9: TtkTokenKind;
    function Func11: TtkTokenKind;
    function Func12: TtkTokenKind;
    function Func13: TtkTokenKind;
    function Func15: TtkTokenKind;
    function Func16: TtkTokenKind;
    function Func17: TtkTokenKind;
    function Func18: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func22: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func24: TtkTokenKind;
    function Func25: TtkTokenKind;
    function Func26: TtkTokenKind;
    function Func27: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func29: TtkTokenKind;
    function Func30: TtkTokenKind;
    function Func31: TtkTokenKind;
    function Func32: TtkTokenKind;
    function Func33: TtkTokenKind;
    function Func34: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func36: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func38: TtkTokenKind;
    function Func39: TtkTokenKind;
    function Func40: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func42: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func44: TtkTokenKind;
    function Func45: TtkTokenKind;
    function Func46: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func48: TtkTokenKind;
    function Func49: TtkTokenKind;
    function Func50: TtkTokenKind;
    function Func51: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func53: TtkTokenKind;
    function Func54: TtkTokenKind;
    function Func55: TtkTokenKind;
    function Func56: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func58: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func61: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func63: TtkTokenKind;
    function Func64: TtkTokenKind;
    function Func65: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func67: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func69: TtkTokenKind;
    function Func70: TtkTokenKind;
    function Func71: TtkTokenKind;
    function Func72: TtkTokenKind;
    function Func73: TtkTokenKind;
    function Func74: TtkTokenKind;
    function Func75: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func77: TtkTokenKind;
    function Func78: TtkTokenKind;
    function Func79: TtkTokenKind;
    function Func80: TtkTokenKind;
    function Func81: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func83: TtkTokenKind;
    function Func84: TtkTokenKind;
    function Func85: TtkTokenKind;
    function Func86: TtkTokenKind;
    function Func87: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func89: TtkTokenKind;
    function Func90: TtkTokenKind;
    function Func91: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func93: TtkTokenKind;
    function Func94: TtkTokenKind;
    function Func96: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func98: TtkTokenKind;
    function Func99: TtkTokenKind;
    function Func100: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func104: TtkTokenKind;
    function Func105: TtkTokenKind;
    function Func106: TtkTokenKind;
    function Func107: TtkTokenKind;
    function Func108: TtkTokenKind;
    function Func109: TtkTokenKind;
    function Func110: TtkTokenKind;
    function Func111: TtkTokenKind;
    function Func112: TtkTokenKind;
    function Func113: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func116: TtkTokenKind;
    function Func118: TtkTokenKind;
    function Func119: TtkTokenKind;
    function Func120: TtkTokenKind;
    function Func121: TtkTokenKind;
    function Func125: TtkTokenKind;
    function Func127: TtkTokenKind;
    function Func128: TtkTokenKind;
    function Func130: TtkTokenKind;
    function Func131: TtkTokenKind;
    function Func136: TtkTokenKind;
    procedure IdentProc;
    procedure UnknownProc;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure PascalCommentOpenProc;
    procedure PascalCommentProc;
    procedure AsmCommentOpenProc;
    procedure AsmCommentProc;
    procedure StringOpenProc;
    procedure StringProc;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}
    function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetKeyWords: string;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: String; LineNumber: Integer); override;
    function GetToken: String; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  published
    property CallsAttri: TSynHighlighterAttributes read fCallsAttri write fCallsAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property EntryPointAttri: TSynHighlighterAttributes read fEntryPointAttri write fEntryPointAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property InstructionsAttri: TSynHighlighterAttributes read fInstructionsAttri write fInstructionsAttri;
    property JumpsAttri: TSynHighlighterAttributes read fJumpsAttri write fJumpsAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property KlucSlovaAttri: TSynHighlighterAttributes read fKlucSlovaAttri write fKlucSlovaAttri;
    property LoopsAttri: TSynHighlighterAttributes read fLoopsAttri write fLoopsAttri;
    property ReturnsAttri: TSynHighlighterAttributes read fReturnsAttri write fReturnsAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri write fStringAttri;
    property TestAttri: TSynHighlighterAttributes read fTestAttri write fTestAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

{$IFDEF SYN_COMPILER_3_UP}
resourcestring
{$ELSE}
const
{$ENDIF}
  SYNS_FilterTatraDAS = 'x86 Assembly Files (*.asm)|*.asm';
  SYNS_LangTatraDAS = 'TatraDAS';
  SYNS_AttrCalls = 'Calls';
  SYNS_AttrEntryPoint = 'EntryPoint';
  SYNS_AttrInstructions = 'Instructions';
  SYNS_AttrJumps = 'Jumps';
  SYNS_AttrKlucSlova = 'KlucSlova';
  SYNS_AttrLoops = 'Loops';
  SYNS_AttrReturns = 'Returns';
  SYNS_AttrTest = 'Test';

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable : array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else
      Identifiers[I] := False;
    end;
    case I in ['_', 'A'..'Z', 'a'..'z'] of
      True:
        begin
          if (I > #64) and (I < #91) then
            mHashTable[I] := Ord(I) - 64
          else if (I > #96) then
            mHashTable[I] := Ord(I) - 95;
        end;
    else
      mHashTable[I] := 0;
    end;
  end;
end;

procedure TSynTatraDASSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do
  begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[3] := Func3;
  fIdentFuncTable[6] := Func6;
  fIdentFuncTable[8] := Func8;
  fIdentFuncTable[9] := Func9;
  fIdentFuncTable[11] := Func11;
  fIdentFuncTable[12] := Func12;
  fIdentFuncTable[13] := Func13;
  fIdentFuncTable[15] := Func15;
  fIdentFuncTable[16] := Func16;
  fIdentFuncTable[17] := Func17;
  fIdentFuncTable[18] := Func18;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[21] := Func21;
  fIdentFuncTable[22] := Func22;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[24] := Func24;
  fIdentFuncTable[25] := Func25;
  fIdentFuncTable[26] := Func26;
  fIdentFuncTable[27] := Func27;
  fIdentFuncTable[28] := Func28;
  fIdentFuncTable[29] := Func29;
  fIdentFuncTable[30] := Func30;
  fIdentFuncTable[31] := Func31;
  fIdentFuncTable[32] := Func32;
  fIdentFuncTable[33] := Func33;
  fIdentFuncTable[34] := Func34;
  fIdentFuncTable[35] := Func35;
  fIdentFuncTable[36] := Func36;
  fIdentFuncTable[37] := Func37;
  fIdentFuncTable[38] := Func38;
  fIdentFuncTable[39] := Func39;
  fIdentFuncTable[40] := Func40;
  fIdentFuncTable[41] := Func41;
  fIdentFuncTable[42] := Func42;
  fIdentFuncTable[43] := Func43;
  fIdentFuncTable[44] := Func44;
  fIdentFuncTable[45] := Func45;
  fIdentFuncTable[46] := Func46;
  fIdentFuncTable[47] := Func47;
  fIdentFuncTable[48] := Func48;
  fIdentFuncTable[49] := Func49;
  fIdentFuncTable[50] := Func50;
  fIdentFuncTable[51] := Func51;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[53] := Func53;
  fIdentFuncTable[54] := Func54;
  fIdentFuncTable[55] := Func55;
  fIdentFuncTable[56] := Func56;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[58] := Func58;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[60] := Func60;
  fIdentFuncTable[61] := Func61;
  fIdentFuncTable[62] := Func62;
  fIdentFuncTable[63] := Func63;
  fIdentFuncTable[64] := Func64;
  fIdentFuncTable[65] := Func65;
  fIdentFuncTable[66] := Func66;
  fIdentFuncTable[67] := Func67;
  fIdentFuncTable[68] := Func68;
  fIdentFuncTable[69] := Func69;
  fIdentFuncTable[70] := Func70;
  fIdentFuncTable[71] := Func71;
  fIdentFuncTable[72] := Func72;
  fIdentFuncTable[73] := Func73;
  fIdentFuncTable[74] := Func74;
  fIdentFuncTable[75] := Func75;
  fIdentFuncTable[76] := Func76;
  fIdentFuncTable[77] := Func77;
  fIdentFuncTable[78] := Func78;
  fIdentFuncTable[79] := Func79;
  fIdentFuncTable[80] := Func80;
  fIdentFuncTable[81] := Func81;
  fIdentFuncTable[82] := Func82;
  fIdentFuncTable[83] := Func83;
  fIdentFuncTable[84] := Func84;
  fIdentFuncTable[85] := Func85;
  fIdentFuncTable[86] := Func86;
  fIdentFuncTable[87] := Func87;
  fIdentFuncTable[88] := Func88;
  fIdentFuncTable[89] := Func89;
  fIdentFuncTable[90] := Func90;
  fIdentFuncTable[91] := Func91;
  fIdentFuncTable[92] := Func92;
  fIdentFuncTable[93] := Func93;
  fIdentFuncTable[94] := Func94;
  fIdentFuncTable[96] := Func96;
  fIdentFuncTable[97] := Func97;
  fIdentFuncTable[98] := Func98;
  fIdentFuncTable[99] := Func99;
  fIdentFuncTable[100] := Func100;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[104] := Func104;
  fIdentFuncTable[105] := Func105;
  fIdentFuncTable[106] := Func106;
  fIdentFuncTable[107] := Func107;
  fIdentFuncTable[108] := Func108;
  fIdentFuncTable[109] := Func109;
  fIdentFuncTable[110] := Func110;
  fIdentFuncTable[111] := Func111;
  fIdentFuncTable[112] := Func112;
  fIdentFuncTable[113] := Func113;
  fIdentFuncTable[114] := Func114;
  fIdentFuncTable[116] := Func116;
  fIdentFuncTable[118] := Func118;
  fIdentFuncTable[119] := Func119;
  fIdentFuncTable[120] := Func120;
  fIdentFuncTable[121] := Func121;
  fIdentFuncTable[125] := Func125;
  fIdentFuncTable[127] := Func127;
  fIdentFuncTable[128] := Func128;
  fIdentFuncTable[130] := Func130;
  fIdentFuncTable[131] := Func131;
  fIdentFuncTable[136] := Func136;
end;

function TSynTatraDASSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynTatraDASSyn.KeyComp(const aKey: String): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  Temp := fToIdent;
  if Length(aKey) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if Temp^ <> aKey[i] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end else Result := False;
end;

function TSynTatraDASSyn.Func3: TtkTokenKind;
begin
  if KeyComp('AAA') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func6: TtkTokenKind;
begin
  if KeyComp('DAA') then Result := tkInstructions else
    if KeyComp('AAD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func8: TtkTokenKind;
begin
  if KeyComp('ADC') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func9: TtkTokenKind;
begin
  if KeyComp('ADD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func11: TtkTokenKind;
begin
  if KeyComp('JA') then Result := tkJumps else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func12: TtkTokenKind;
begin
  if KeyComp('JB') then Result := tkJumps else
    if KeyComp('DEC') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func13: TtkTokenKind;
begin
  if KeyComp('JC') then Result := tkJumps else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func15: TtkTokenKind;
begin
  if KeyComp('JE') then Result := tkJumps else
    if KeyComp('AAM') then Result := tkInstructions else
      if KeyComp('FADD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func16: TtkTokenKind;
begin
  if KeyComp('JAE') then Result := tkJumps else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func17: TtkTokenKind;
begin
  if KeyComp('JG') then Result := tkJumps else
    if KeyComp('JBE') then Result := tkJumps else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func18: TtkTokenKind;
begin
  if KeyComp('LEA') then Result := tkInstructions else
    if KeyComp('CLC') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func19: TtkTokenKind;
begin
  if KeyComp('CMC') then Result := tkInstructions else
    if KeyComp('AND') then Result := tkInstructions else
      if KeyComp('CLD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func21: TtkTokenKind;
begin
  if KeyComp('AAS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func22: TtkTokenKind;
begin
  if KeyComp('FLD1') then Result := tkInstructions else
    if KeyComp('FLD') then Result := tkInstructions else
      if KeyComp('BT') then Result := tkInstructions else
        if KeyComp('JL') then Result := tkJumps else
          if KeyComp('JGE') then Result := tkJumps else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func23: TtkTokenKind;
begin
  if KeyComp('IN') then Result := tkInstructions else
    if KeyComp('SBB') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func24: TtkTokenKind;
begin
  if KeyComp('CLI') then Result := tkInstructions else
    if KeyComp('FIADD') then Result := tkInstructions else
      if KeyComp('CDQ') then Result := tkInstructions else
        if KeyComp('FBLD') then Result := tkInstructions else
          if KeyComp('FIADD') then Result := tkInstructions else
            if KeyComp('DAS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func25: TtkTokenKind;
begin
  if KeyComp('JO') then Result := tkJumps else
    if KeyComp('JNA') then Result := tkJumps else
      if KeyComp('BTC') then Result := tkInstructions else
        if KeyComp('UD2') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func26: TtkTokenKind;
begin
  if KeyComp('JNB') then Result := tkJumps else
    if KeyComp('JP') then Result := tkJumps else
      if KeyComp('INC') then Result := tkInstructions else
        if KeyComp('NEG') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func27: TtkTokenKind;
begin
  if KeyComp('JNC') then Result := tkJumps else
    if KeyComp('JLE') then Result := tkJumps else
      if KeyComp('BSF') then Result := tkInstructions else
        if KeyComp('PADDB') then Result := tkInstructions else
          if KeyComp('LAHF') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func28: TtkTokenKind;
begin
  if KeyComp('FABS') then Result := tkInstructions else
    if KeyComp('CBW') then Result := tkInstructions else
      if KeyComp('CALL') then Result := tkCalls else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func29: TtkTokenKind;
begin
  if KeyComp('PADDD') then Result := tkInstructions else
    if KeyComp('PFACC') then Result := tkInstructions else
      if KeyComp('JS') then Result := tkJumps else
        if KeyComp('JNE') then Result := tkJumps else
          if KeyComp('ADDPD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func30: TtkTokenKind;
begin
  if KeyComp('JNAE') then Result := tkJumps else
    if KeyComp('CWD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func31: TtkTokenKind;
begin
  if KeyComp('JNG') then Result := tkJumps else
    if KeyComp('JPE') then Result := tkJumps else
      if KeyComp('JNBE') then Result := tkJumps else
        if KeyComp('Call') then Result := tkKlucSlova else
          if KeyComp('PFADD') then Result := tkInstructions else
            if KeyComp('LAR') then Result := tkInstructions else
              if KeyComp('FADDP') then Result := tkInstructions else
                if KeyComp('FILD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func32: TtkTokenKind;
begin
  if KeyComp('CMP') then Result := tkInstructions else
    if KeyComp('SAL') then Result := tkInstructions else
      if KeyComp('ADDSD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func33: TtkTokenKind;
begin
  if KeyComp('XADD') then Result := tkInstructions else
    if KeyComp('RCL') then Result := tkInstructions else
      if KeyComp('OR') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func34: TtkTokenKind;
begin
  if KeyComp('SAHF') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func35: TtkTokenKind;
begin
  if KeyComp('PAND') then Result := tkInstructions else
    if KeyComp('LDS') then Result := tkInstructions else
      if KeyComp('CWDE') then Result := tkInstructions else
        if KeyComp('PI2FD') then Result := tkInstructions else
          if KeyComp('PF2ID') then Result := tkInstructions else
            if KeyComp('DIV') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func36: TtkTokenKind;
begin
  if KeyComp('JZ') then Result := tkJumps else
    if KeyComp('LES') then Result := tkInstructions else
      if KeyComp('FCHS') then Result := tkInstructions else
        if KeyComp('JNGE') then Result := tkJumps else
          if KeyComp('JNL') then Result := tkJumps else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func37: TtkTokenKind;
begin
  if KeyComp('FCOM') then Result := tkInstructions else
    if KeyComp('LFS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func38: TtkTokenKind;
begin
  if KeyComp('SAR') then Result := tkInstructions else
    if KeyComp('LGS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func39: TtkTokenKind;
begin
  if KeyComp('SHL') then Result := tkInstructions else
    if KeyComp('JNO') then Result := tkJumps else
      if KeyComp('JMP') then Result := tkJumps else
        if KeyComp('FLDL2E') then Result := tkInstructions else
          if KeyComp('BSR') then Result := tkInstructions else
            if KeyComp('RCR') then Result := tkInstructions else
              if KeyComp('ANDPD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func40: TtkTokenKind;
begin
  if KeyComp('BTR') then Result := tkInstructions else
    if KeyComp('JNP') then Result := tkJumps else
      if KeyComp('FFREE') then Result := tkInstructions else
        if KeyComp('HLT') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func41: TtkTokenKind;
begin
  if KeyComp('BTS') then Result := tkInstructions else
    if KeyComp('FLDLG2') then Result := tkInstructions else
      if KeyComp('LOCK') then Result := tkInstructions else
        if KeyComp('FXCH') then Result := tkInstructions else
          if KeyComp('JPO') then Result := tkJumps else
            if KeyComp('JNLE') then Result := tkJumps else
              if KeyComp('FDIV') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func42: TtkTokenKind;
begin
  if KeyComp('INS') then Result := tkInstructions else
    if KeyComp('XCHG') then Result := tkInstructions else
      if KeyComp('PADDQ') then Result := tkInstructions else
        if KeyComp('SCAS') then Result := tkInstructions else
          if KeyComp('SUB') then Result := tkInstructions else
            if KeyComp('rep') then Result := tkInstructions else
              if KeyComp('STC') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func43: TtkTokenKind;
begin
  if KeyComp('INT') then Result := tkInstructions else
    if KeyComp('SHLD') then Result := tkInstructions else
      if KeyComp('RET') then Result := tkReturns else
        if KeyComp('F2XM1') then Result := tkInstructions else
          if KeyComp('LGDT') then Result := tkInstructions else
            if KeyComp('STD') then Result := tkInstructions else
              if KeyComp('JNS') then Result := tkJumps else
                if KeyComp('FCOS') then Result := tkInstructions else
                  if KeyComp('LSL') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func44: TtkTokenKind;
begin
  if KeyComp('FXAM') then Result := tkInstructions else
    if KeyComp('IDIV') then Result := tkInstructions else
      if KeyComp('SCASB') then Result := tkInstructions else
        if KeyComp('ADDPS') then Result := tkInstructions else
          if KeyComp('INSB') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func45: TtkTokenKind;
begin
  if KeyComp('LIDT') then Result := tkInstructions else
    if KeyComp('SHR') then Result := tkInstructions else
      if KeyComp('LFENCE') then Result := tkInstructions else
        if KeyComp('lock') then Result := tkInstructions else
          if KeyComp('FST') then Result := tkInstructions else
            if KeyComp('LEAVE') then Result := tkInstructions else
              if KeyComp('JMPF') then Result := tkJumps else
                if KeyComp('NOP') then Result := tkInstructions else
                  if KeyComp('ROL') then Result := tkInstructions else
                    if KeyComp('SETA') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func46: TtkTokenKind;
begin
  if KeyComp('INSD') then Result := tkInstructions else
    if KeyComp('PADDSB') then Result := tkInstructions else
      if KeyComp('MFENCE') then Result := tkInstructions else
        if KeyComp('MUL') then Result := tkInstructions else
          if KeyComp('SETB') then Result := tkInstructions else
            if KeyComp('SCASD') then Result := tkInstructions else
              if KeyComp('FICOM') then Result := tkInstructions else
                if KeyComp('FSCALE') then Result := tkInstructions else
                  if KeyComp('FCOMI') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func47: TtkTokenKind;
begin
  if KeyComp('ARPL') then Result := tkInstructions else
    if KeyComp('SETC') then Result := tkInstructions else
      if KeyComp('POP') then Result := tkInstructions else
        if KeyComp('FLDPI') then Result := tkInstructions else
          if KeyComp('ADDSS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func48: TtkTokenKind;
begin
  if KeyComp('FLDLN2') then Result := tkInstructions else
    if KeyComp('FLDZ') then Result := tkInstructions else
      if KeyComp('POPA') then Result := tkInstructions else
        if KeyComp('FSIN') then Result := tkInstructions else
          if KeyComp('STI') then Result := tkInstructions else
            if KeyComp('repe') then Result := tkInstructions else
              if KeyComp('PAVGB') then Result := tkInstructions else
                if KeyComp('FLDCW') then Result := tkInstructions else
                  if KeyComp('PADDW') then Result := tkInstructions else
                    if KeyComp('FSUB') then Result := tkInstructions else
                      if KeyComp('LLDT') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func49: TtkTokenKind;
begin
  if KeyComp('NOT') then Result := tkInstructions else
    if KeyComp('PANDN') then Result := tkInstructions else
      if KeyComp('RETF') then Result := tkReturns else
        if KeyComp('SHRD') then Result := tkInstructions else
          if KeyComp('SETE') then Result := tkInstructions else
            if KeyComp('INVD') then Result := tkInstructions else
              if KeyComp('POR') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func50: TtkTokenKind;
begin
  if KeyComp('JNZ') then Result := tkJumps else
    if KeyComp('FIDIV') then Result := tkInstructions else
      if KeyComp('RSM') then Result := tkInstructions else
        if KeyComp('EMMS') then Result := tkInstructions else
          if KeyComp('MOV') then Result := tkInstructions else
            if KeyComp('SETAE') then Result := tkInstructions else
              if KeyComp('LTR') then Result := tkInstructions else
                if KeyComp('LSS') then Result := tkInstructions else
                  if KeyComp('FCLEX') then Result := tkInstructions else
                    if KeyComp('LODS') then Result := tkInstructions else
                      if KeyComp('SGDT') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func51: TtkTokenKind;
begin
  if KeyComp('SETBE') then Result := tkInstructions else
    if KeyComp('ROR') then Result := tkInstructions else
      if KeyComp('CMPS') then Result := tkInstructions else
        if KeyComp('FNOP') then Result := tkInstructions else
          if KeyComp('SETG') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func52: TtkTokenKind;
begin
  if KeyComp('CMPPD') then Result := tkInstructions else
    if KeyComp('IRET') then Result := tkReturns else
      if KeyComp('LODSB') then Result := tkInstructions else
        if KeyComp('FMUL') then Result := tkInstructions else
          if KeyComp('SIDT') then Result := tkInstructions else
            if KeyComp('POPAD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func53: TtkTokenKind;
begin
  if KeyComp('ORPD') then Result := tkInstructions else
    if KeyComp('FSAVE') then Result := tkInstructions else
      if KeyComp('CMPSB') then Result := tkInstructions else
        if KeyComp('WAIT') then Result := tkInstructions else
          if KeyComp('POPF') then Result := tkInstructions else
            if KeyComp('FCOMP') then Result := tkInstructions else
              if KeyComp('JMPN') then Result := tkJumps else
                if KeyComp('CPUID') then Result := tkInstructions else
                  if KeyComp('ANDNPD') then Result := tkInstructions else
                    if KeyComp('used') then Result := tkKlucSlova else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func54: TtkTokenKind;
begin
  if KeyComp('MOVD') then Result := tkInstructions else
    if KeyComp('CMOVA') then Result := tkInstructions else
      if KeyComp('FLDL2T') then Result := tkInstructions else
        if KeyComp('ANDPS') then Result := tkInstructions else
          if KeyComp('LODSD') then Result := tkInstructions else
            if KeyComp('FIST') then Result := tkInstructions else
              if KeyComp('RDPMC') then Result := tkInstructions else
                if KeyComp('CLTS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func55: TtkTokenKind;
begin
  if KeyComp('DIVPD') then Result := tkInstructions else
    if KeyComp('SLDT') then Result := tkInstructions else
      if KeyComp('CMOVB') then Result := tkInstructions else
        if KeyComp('IMUL') then Result := tkInstructions else
          if KeyComp('CMPSD') then Result := tkInstructions else
            if KeyComp('CMPSD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func56: TtkTokenKind;
begin
  if KeyComp('FEMMS') then Result := tkInstructions else
    if KeyComp('SETGE') then Result := tkInstructions else
      if KeyComp('IRETD') then Result := tkReturns else
        if KeyComp('BOUND') then Result := tkInstructions else
          if KeyComp('MINPD') then Result := tkInstructions else
            if KeyComp('SETL') then Result := tkInstructions else
              if KeyComp('Hello') then Result := tkKey else
                if KeyComp('from') then Result := tkKlucSlova else
                  if KeyComp('OUT') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func57: TtkTokenKind;
begin
  if KeyComp('POPFD') then Result := tkInstructions else
    if KeyComp('RETN') then Result := tkReturns else
      if KeyComp('FISUB') then Result := tkInstructions else
        if KeyComp('XLAT') then Result := tkInstructions else
          if KeyComp('STR') then Result := tkInstructions else
            if KeyComp('FPTAN') then Result := tkInstructions else
              if KeyComp('XOR') then Result := tkInstructions else
                if KeyComp('FDIVP') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func58: TtkTokenKind;
begin
  if KeyComp('FUCOM') then Result := tkInstructions else
    if KeyComp('INTO') then Result := tkInstructions else
      if KeyComp('DIVSD') then Result := tkInstructions else
        if KeyComp('PFMIN') then Result := tkInstructions else
          if KeyComp('PSRAD') then Result := tkInstructions else
            if KeyComp('CMOVE') then Result := tkInstructions else
              if KeyComp('MAXPD') then Result := tkInstructions else
                if KeyComp('LOOP') then Result := tkLoops else
                  if KeyComp('FPREM1') then Result := tkInstructions else
                    if KeyComp('FPATAN') then Result := tkInstructions else
                      if KeyComp('FPREM') then Result := tkInstructions else
                        if KeyComp('FINIT') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func59: TtkTokenKind;
begin
  if KeyComp('SETNA') then Result := tkInstructions else
    if KeyComp('FDIVR') then Result := tkInstructions else
      if KeyComp('FWAIT') then Result := tkInstructions else
        if KeyComp('PFRCP') then Result := tkInstructions else
          if KeyComp('CMOVAE') then Result := tkInstructions else
            if KeyComp('MINSD') then Result := tkInstructions else
              if KeyComp('SETO') then Result := tkInstructions else
                if KeyComp('XLATB') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func60: TtkTokenKind;
begin
  if KeyComp('SETP') then Result := tkInstructions else
    if KeyComp('PFMAX') then Result := tkInstructions else
      if KeyComp('CMOVBE') then Result := tkInstructions else
        if KeyComp('PSUBB') then Result := tkInstructions else
          if KeyComp('SETNB') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func61: TtkTokenKind;
begin
  if KeyComp('SETNC') then Result := tkInstructions else
    if KeyComp('SETLE') then Result := tkInstructions else
      if KeyComp('FSTP') then Result := tkInstructions else
        if KeyComp('Loop') then Result := tkKlucSlova else
          if KeyComp('FCMOVB') then Result := tkInstructions else
            if KeyComp('MAXSD') then Result := tkInstructions else
              if KeyComp('BSWAP') then Result := tkInstructions else
                if KeyComp('FIMUL') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func62: TtkTokenKind;
begin
  if KeyComp('FCOMIP') then Result := tkInstructions else
    if KeyComp('ENTER') then Result := tkInstructions else
      if KeyComp('PSUBD') then Result := tkInstructions else
        if KeyComp('FICOMP') then Result := tkInstructions else
          if KeyComp('SUBPD') then Result := tkInstructions else
            if KeyComp('PAUSE') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func63: TtkTokenKind;
begin
  if KeyComp('VERR') then Result := tkInstructions else
    if KeyComp('JCXZ') then Result := tkJumps else
      if KeyComp('repne') then Result := tkInstructions else
        if KeyComp('PSLLD') then Result := tkInstructions else
          if KeyComp('LOOPE') then Result := tkLoops else
            if KeyComp('SETNE') then Result := tkInstructions else
              if KeyComp('FBSTP') then Result := tkInstructions else
                if KeyComp('FLDENV') then Result := tkInstructions else
                  if KeyComp('SETS') then Result := tkInstructions else
                    if KeyComp('Jump') then Result := tkKlucSlova else
                      if KeyComp('COMISD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func64: TtkTokenKind;
begin
  if KeyComp('PFSUB') then Result := tkInstructions else
    if KeyComp('FCMOVE') then Result := tkInstructions else
      if KeyComp('FSUBP') then Result := tkInstructions else
        if KeyComp('TEST') then Result := tkInstructions else
          if KeyComp('SETNAE') then Result := tkInstructions else
            if KeyComp('FNCLEX') then Result := tkInstructions else
              if KeyComp('FNCLEX') then Result := tkInstructions else
                if KeyComp('PUSH') then Result := tkInstructions else
                  if KeyComp('RDTSC') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func65: TtkTokenKind;
begin
  if KeyComp('SETNBE') then Result := tkInstructions else
    if KeyComp('CMOVL') then Result := tkInstructions else
      if KeyComp('PMADDWD') then Result := tkInstructions else
        if KeyComp('SUBSD') then Result := tkInstructions else
          if KeyComp('PSADBW') then Result := tkInstructions else
            if KeyComp('PUSHA') then Result := tkInstructions else
              if KeyComp('SCASW') then Result := tkInstructions else
                if KeyComp('SETNG') then Result := tkInstructions else
                  if KeyComp('INSW') then Result := tkInstructions else
                    if KeyComp('FTST') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func66: TtkTokenKind;
begin
  if KeyComp('FCMOVBE') then Result := tkInstructions else
    if KeyComp('FSUBR') then Result := tkInstructions else
      if KeyComp('PFCMPGE') then Result := tkInstructions else
        if KeyComp('MULPD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func67: TtkTokenKind;
begin
  if KeyComp('PADDSW') then Result := tkInstructions else
    if KeyComp('LMSW') then Result := tkInstructions else
      if KeyComp('FYL2X') then Result := tkInstructions else
        if KeyComp('PADDUSB') then Result := tkInstructions else
          if KeyComp('MOVQ') then Result := tkInstructions else
            if KeyComp('FNSAVE') then Result := tkInstructions else
              if KeyComp('CMPPS') then Result := tkInstructions else
                if KeyComp('FUCOMI') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func68: TtkTokenKind;
begin
  if KeyComp('VERW') then Result := tkInstructions else
    if KeyComp('ORPS') then Result := tkInstructions else
      if KeyComp('FIDIVR') then Result := tkInstructions else
        if KeyComp('PFMUL') then Result := tkInstructions else
          if KeyComp('CMOVO') then Result := tkInstructions else
            if KeyComp('ANDNPS') then Result := tkInstructions else
              if KeyComp('JECXZ') then Result := tkJumps else
                if KeyComp('FMULP') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func69: TtkTokenKind;
begin
  if KeyComp('PAVGW') then Result := tkInstructions else
    if KeyComp('PUSHAD') then Result := tkInstructions else
      if KeyComp('CMOVP') then Result := tkInstructions else
        if KeyComp('PSRLD') then Result := tkInstructions else
          if KeyComp('MOVS') then Result := tkInstructions else
            if KeyComp('FCOMPP') then Result := tkInstructions else
              if KeyComp('repz') then Result := tkInstructions else
                if KeyComp('MULSD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func70: TtkTokenKind;
begin
  if KeyComp('PUSHF') then Result := tkInstructions else
    if KeyComp('DIVPS') then Result := tkInstructions else
      if KeyComp('SETNL') then Result := tkInstructions else
        if KeyComp('SETZ') then Result := tkInstructions else
          if KeyComp('CMOVLE') then Result := tkInstructions else
            if KeyComp('SETNGE') then Result := tkInstructions else
              if KeyComp('FISTP') then Result := tkInstructions else
                if KeyComp('CMPSS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func71: TtkTokenKind;
begin
  if KeyComp('MINPS') then Result := tkInstructions else
    if KeyComp('FSTCW') then Result := tkInstructions else
      if KeyComp('MOVAPD') then Result := tkInstructions else
        if KeyComp('MOVSB') then Result := tkInstructions else
          if KeyComp('POPAW') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func72: TtkTokenKind;
begin
  if KeyComp('MOVDQA') then Result := tkInstructions else
    if KeyComp('FNINIT') then Result := tkInstructions else
      if KeyComp('RCPPS') then Result := tkInstructions else
        if KeyComp('RDMSR') then Result := tkInstructions else
          if KeyComp('CMOVNE') then Result := tkInstructions else
            if KeyComp('PCMPEQB') then Result := tkInstructions else
              if KeyComp('CMOVS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func73: TtkTokenKind;
begin
  if KeyComp('DIVSS') then Result := tkInstructions else
    if KeyComp('PXOR') then Result := tkInstructions else
      if KeyComp('STOS') then Result := tkInstructions else
        if KeyComp('LODSW') then Result := tkInstructions else
          if KeyComp('FDECSTP') then Result := tkInstructions else
            if KeyComp('MAXPS') then Result := tkInstructions else
              if KeyComp('SETNO') then Result := tkInstructions else
                if KeyComp('MOVSD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func74: TtkTokenKind;
begin
  if KeyComp('PCMPEQD') then Result := tkInstructions else
    if KeyComp('CMPSW') then Result := tkInstructions else
      if KeyComp('FUCOMP') then Result := tkInstructions else
        if KeyComp('WBINVD') then Result := tkInstructions else
          if KeyComp('MINSS') then Result := tkInstructions else
            if KeyComp('SMSW') then Result := tkInstructions else
              if KeyComp('SHUFPD') then Result := tkInstructions else
                if KeyComp('PUSHFD') then Result := tkInstructions else
                  if KeyComp('PSHUFD') then Result := tkInstructions else
                    if KeyComp('SETNP') then Result := tkInstructions else
                      if KeyComp('CMPXCHG') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func75: TtkTokenKind;
begin
  if KeyComp('FCMOVNB') then Result := tkInstructions else
    if KeyComp('FISUBR') then Result := tkInstructions else
      if KeyComp('IRETW') then Result := tkReturns else
        if KeyComp('FDIVRP') then Result := tkInstructions else
          if KeyComp('PMINUB') then Result := tkInstructions else
            if KeyComp('STOSB') then Result := tkInstructions else
              if KeyComp('OUTS') then Result := tkInstructions else
                if KeyComp('SETPO') then Result := tkInstructions else
                  if KeyComp('RCPSS') then Result := tkInstructions else
                    if KeyComp('SETNLE') then Result := tkInstructions else
                      if KeyComp('PSUBQ') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func76: TtkTokenKind;
begin
  if KeyComp('PFCMPEQ') then Result := tkInstructions else
    if KeyComp('POPFW') then Result := tkInstructions else
      if KeyComp('PSLLQ') then Result := tkInstructions else
        if KeyComp('CMPXCHG8B') then Result := tkInstructions else
          if KeyComp('MAXSS') then Result := tkInstructions else
            if KeyComp('module') then Result := tkKlucSlova else
              if KeyComp('World') then Result := tkKey else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func77: TtkTokenKind;
begin
  if KeyComp('STOSD') then Result := tkInstructions else
    if KeyComp('SUBPS') then Result := tkInstructions else
      if KeyComp('LOOPNE') then Result := tkLoops else
        if KeyComp('XORPD') then Result := tkInstructions else
          if KeyComp('PMAXUB') then Result := tkInstructions else
            if KeyComp('FXSAVE') then Result := tkInstructions else
              if KeyComp('PSRAW') then Result := tkInstructions else
                if KeyComp('OUTSB') then Result := tkInstructions else
                  if KeyComp('PCMPGTB') then Result := tkInstructions else
                    if KeyComp('SETNS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func78: TtkTokenKind;
begin
  if KeyComp('COMISS') then Result := tkInstructions else
    if KeyComp('FCMOVNE') then Result := tkInstructions else
      if KeyComp('MOVHPD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func79: TtkTokenKind;
begin
  if KeyComp('CMOVNL') then Result := tkInstructions else
    if KeyComp('PSUBSB') then Result := tkInstructions else
      if KeyComp('point') then Result := tkEntryPoint else
        if KeyComp('PCMPGTD') then Result := tkInstructions else
          if KeyComp('OUTSD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func80: TtkTokenKind;
begin
  if KeyComp('FCMOVNBE') then Result := tkInstructions else
    if KeyComp('INVLPG') then Result := tkInstructions else
      if KeyComp('FCMOVU') then Result := tkInstructions else
        if KeyComp('PSLLDQ') then Result := tkInstructions else
          if KeyComp('SUBSS') then Result := tkInstructions else
            if KeyComp('FSQRT') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func81: TtkTokenKind;
begin
  if KeyComp('PFCMPGT') then Result := tkInstructions else
    if KeyComp('PSUBW') then Result := tkInstructions else
      if KeyComp('MULPS') then Result := tkInstructions else
        if KeyComp('CLFLUSH') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func82: TtkTokenKind;
begin
  if KeyComp('PFSUBR') then Result := tkInstructions else
    if KeyComp('PSLLW') then Result := tkInstructions else
      if KeyComp('PSRLQ') then Result := tkInstructions else
        if KeyComp('FSUBRP') then Result := tkInstructions else
          if KeyComp('MOVLPD') then Result := tkInstructions else
            if KeyComp('CMOVNO') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func83: TtkTokenKind;
begin
  if KeyComp('FYL2XP1') then Result := tkInstructions else
    if KeyComp('CMOVNP') then Result := tkInstructions else
      if KeyComp('FUCOMIP') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func84: TtkTokenKind;
begin
  if KeyComp('repnz') then Result := tkInstructions else
    if KeyComp('SETNZ') then Result := tkInstructions else
      if KeyComp('LOOPZ') then Result := tkLoops else
        if KeyComp('CMOVNLE') then Result := tkInstructions else
          if KeyComp('UCOMISD') then Result := tkInstructions else
            if KeyComp('MULSS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func85: TtkTokenKind;
begin
  if KeyComp('FNSTCW') then Result := tkInstructions else
    if KeyComp('FRNDINT') then Result := tkInstructions else
      if KeyComp('FSINCOS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func86: TtkTokenKind;
begin
  if KeyComp('CMOVNS') then Result := tkInstructions else
    if KeyComp('CVTPD2DQ') then Result := tkInstructions else
      if KeyComp('CVTDQ2PD') then Result := tkInstructions else
        if KeyComp('Entry') then Result := tkEntryPoint else
          if KeyComp('MOVAPS') then Result := tkInstructions else
            if KeyComp('PSRLDQ') then Result := tkInstructions else
              if KeyComp('FSTENV') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func87: TtkTokenKind;
begin
  if KeyComp('FSTSW') then Result := tkInstructions else
    if KeyComp('FINCSTP') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func88: TtkTokenKind;
begin
  if KeyComp('MOVQ2DQ') then Result := tkInstructions else
    if KeyComp('PFRCPIT2') then Result := tkInstructions else
      if KeyComp('PFRCPIT1') then Result := tkInstructions else
        if KeyComp('MOVSS') then Result := tkInstructions else
          if KeyComp('MOVDQ2Q') then Result := tkInstructions else
            if KeyComp('PUSHAW') then Result := tkInstructions else
              if KeyComp('PADDUSW') then Result := tkInstructions else
                if KeyComp('PSRLW') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func89: TtkTokenKind;
begin
  if KeyComp('SHUFPS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func90: TtkTokenKind;
begin
  if KeyComp('FUCOMPP') then Result := tkInstructions else
    if KeyComp('CVTPI2PD') then Result := tkInstructions else
      if KeyComp('CVTPD2PI') then Result := tkInstructions else
        if KeyComp('FISTTP') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func91: TtkTokenKind;
begin
  if KeyComp('MOVUPD') then Result := tkInstructions else
    if KeyComp('WRMSR') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func92: TtkTokenKind;
begin
  if KeyComp('MOVSW') then Result := tkInstructions else
    if KeyComp('FXTRACT') then Result := tkInstructions else
      if KeyComp('MOVDQU') then Result := tkInstructions else
        if KeyComp('XORPS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func93: TtkTokenKind;
begin
  if KeyComp('PSHUFW') then Result := tkInstructions else
    if KeyComp('PCMPEQW') then Result := tkInstructions else
      if KeyComp('MOVSX') then Result := tkInstructions else
        if KeyComp('MOVNTI') then Result := tkInstructions else
          if KeyComp('LDMXCSR') then Result := tkInstructions else
            if KeyComp('UNPCKHPD') then Result := tkInstructions else
              if KeyComp('PMULHW') then Result := tkInstructions else
                if KeyComp('PUSHFW') then Result := tkInstructions else
                  if KeyComp('MOVHPS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func94: TtkTokenKind;
begin
  if KeyComp('FCMOVNU') then Result := tkInstructions else
    if KeyComp('PMINWS') then Result := tkInstructions else
      if KeyComp('SQRTPD') then Result := tkInstructions else
        if KeyComp('PACKSSWB') then Result := tkInstructions else
          if KeyComp('Program') then Result := tkEntryPoint else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func96: TtkTokenKind;
begin
  if KeyComp('CVTSI2SD') then Result := tkInstructions else
    if KeyComp('PACKSSDW') then Result := tkInstructions else
      if KeyComp('STOSW') then Result := tkInstructions else
        if KeyComp('FRSTOR') then Result := tkInstructions else
          if KeyComp('PACKUSWB') then Result := tkInstructions else
            if KeyComp('CVTSD2SI') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func97: TtkTokenKind;
begin
  if KeyComp('MOVLPS') then Result := tkInstructions else
    if KeyComp('SQRTSD') then Result := tkInstructions else
      if KeyComp('PMULLW') then Result := tkInstructions else
        if KeyComp('UNPCKLPD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func98: TtkTokenKind;
begin
  if KeyComp('PCMPGTW') then Result := tkInstructions else
    if KeyComp('LOOPNZ') then Result := tkLoops else
      if KeyComp('OUTSW') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func99: TtkTokenKind;
begin
  if KeyComp('UCOMISS') then Result := tkInstructions else
    if KeyComp('PINSRW') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func100: TtkTokenKind;
begin
  if KeyComp('PSUBSW') then Result := tkInstructions else
    if KeyComp('FNSTENV') then Result := tkInstructions else
      if KeyComp('PSUBUSB') then Result := tkInstructions else
        if KeyComp('CVTPS2PD') then Result := tkInstructions else
          if KeyComp('CVTPD2PS') then Result := tkInstructions else
            if KeyComp('MOVZX') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func101: TtkTokenKind;
begin
  if KeyComp('CVTDQ2PS') then Result := tkInstructions else
    if KeyComp('FNSTSW') then Result := tkInstructions else
      if KeyComp('SynEdit') then Result := tkTest else
        if KeyComp('PSHUFHW') then Result := tkInstructions else
          if KeyComp('MOVNTQ') then Result := tkInstructions else
            if KeyComp('CMOVXX') then Result := tkInstructions else
              if KeyComp('PREFETCHT0') then Result := tkInstructions else
                if KeyComp('PREFETCHT2') then Result := tkInstructions else
                  if KeyComp('CVTPS2DQ') then Result := tkInstructions else
                    if KeyComp('PREFETCHT1') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func104: TtkTokenKind;
begin
  if KeyComp('PMULUDQ') then Result := tkInstructions else
    if KeyComp('MOVNTPD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func105: TtkTokenKind;
begin
  if KeyComp('MOVLHPS') then Result := tkInstructions else
    if KeyComp('CVTPS2PI') then Result := tkInstructions else
      if KeyComp('PSHUFLW') then Result := tkInstructions else
        if KeyComp('PFRSQIT1') then Result := tkInstructions else
          if KeyComp('CVTPI2PS') then Result := tkInstructions else
            if KeyComp('MOVNTDQ') then Result := tkInstructions else
              if KeyComp('MOVHLPS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func106: TtkTokenKind;
begin
  if KeyComp('CVTTPD2DQ') then Result := tkInstructions else
    if KeyComp('CVTSD2SS') then Result := tkInstructions else
      if KeyComp('CVTSS2SD') then Result := tkInstructions else
        if KeyComp('MOVUPS') then Result := tkInstructions else
          if KeyComp('PWAXSW') then Result := tkInstructions else
            if KeyComp('PEXTRW') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func107: TtkTokenKind;
begin
  if KeyComp('Imported') then Result := tkKlucSlova else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func108: TtkTokenKind;
begin
  if KeyComp('UNPCKHPS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func109: TtkTokenKind;
begin
  if KeyComp('SQRTPS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func110: TtkTokenKind;
begin
  if KeyComp('CVTTPD2PI') then Result := tkInstructions else
    if KeyComp('function') then Result := tkKlucSlova else
      if KeyComp('PUNPCKHDQ') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func111: TtkTokenKind;
begin
  if KeyComp('CVTSS2SI') then Result := tkInstructions else
    if KeyComp('MASKMOVQ') then Result := tkInstructions else
      if KeyComp('CVTSI2SS') then Result := tkInstructions else
        if KeyComp('PMOVMSKB') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func112: TtkTokenKind;
begin
  if KeyComp('UNPCKPLS') then Result := tkInstructions else
    if KeyComp('SQRTSS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func113: TtkTokenKind;
begin
  if KeyComp('MOVMSKPD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func114: TtkTokenKind;
begin
  if KeyComp('PUNPCKLDQ') then Result := tkInstructions else
    if KeyComp('PFRSQRT') then Result := tkInstructions else
      if KeyComp('PUNPCKHBW') then Result := tkInstructions else
        if KeyComp('Exported') then Result := tkKlucSlova else
          if KeyComp('PMULHUW') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func116: TtkTokenKind;
begin
  if KeyComp('PUNPCKHWD') then Result := tkInstructions else
    if KeyComp('CVTTSD2SI') then Result := tkInstructions else
      if KeyComp('STMXCSR') then Result := tkInstructions else
        if KeyComp('PREFETCHNTA') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func118: TtkTokenKind;
begin
  if KeyComp('PUNPCKLBW') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func119: TtkTokenKind;
begin
  if KeyComp('MOVNTPS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func120: TtkTokenKind;
begin
  if KeyComp('FXRSTOR') then Result := tkInstructions else
    if KeyComp('PUNPCKLWD') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func121: TtkTokenKind;
begin
  if KeyComp('CVTTPS2DQ') then Result := tkInstructions else
    if KeyComp('PSUBUSW') then Result := tkInstructions else
      if KeyComp('SYSEXIT') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func125: TtkTokenKind;
begin
  if KeyComp('CVTTPS2PI') then Result := tkInstructions else
    if KeyComp('SYSENTER') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func127: TtkTokenKind;
begin
  if KeyComp('RSQRTPS') then Result := tkInstructions else
    if KeyComp('PUNPCKHQDQ') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func128: TtkTokenKind;
begin
  if KeyComp('MOVMSKPS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func130: TtkTokenKind;
begin
  if KeyComp('RSQRTSS') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func131: TtkTokenKind;
begin
  if KeyComp('CVTTSS2SI') then Result := tkInstructions else
    if KeyComp('PUNPCKLQDQ') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.Func136: TtkTokenKind;
begin
  if KeyComp('MASKMOVDQU') then Result := tkInstructions else Result := tkIdentifier;
end;

function TSynTatraDASSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynTatraDASSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey <= MaxKey then
    Result := fIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

procedure TSynTatraDASSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0: fProcTable[I] := NullProc;
      #10: fProcTable[I] := LFProc;
      #13: fProcTable[I] := CRProc;
      '{': fProcTable[I] := PascalCommentOpenProc;
      ';': fProcTable[I] := AsmCommentOpenProc;
      '''': fProcTable[I] := StringOpenProc;
      #1..#9,
      #11,
      #12,
      #14..#32 : fProcTable[I] := SpaceProc;
      '0'..'9', 'A'..'Z', 'a'..'z', '_': fProcTable[I] := IdentProc;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

procedure TSynTatraDASSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    inc(Run);
  until not (fLine[Run] in [#1..#32]);
end;

procedure TSynTatraDASSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynTatraDASSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end;

procedure TSynTatraDASSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynTatraDASSyn.PascalCommentOpenProc;
begin
  Inc(Run);
  fRange := rsPascalComment;
  PascalCommentProc;
  fTokenID := tkComment;
end;

procedure TSynTatraDASSyn.PascalCommentProc;
begin
  case fLine[Run] of
     #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    begin
      fTokenID := tkComment;
      repeat
        if (fLine[Run] = '}') then
        begin
          Inc(Run, 1);
          fRange := rsUnKnown;
          Break;
        end;
        if not (fLine[Run] in [#0, #10, #13]) then
          Inc(Run);
      until fLine[Run] in [#0, #10, #13];
    end;
  end;
end;

procedure TSynTatraDASSyn.AsmCommentOpenProc;
begin
  Inc(Run);
  fRange := rsAsmComment;
  AsmCommentProc;
  fTokenID := tkComment;
end;

procedure TSynTatraDASSyn.AsmCommentProc;
begin
  fTokenID := tkComment;
  repeat
    if (fLine[Run] = 'W') and
       (fLine[Run + 1] = 'h') and
       (fLine[Run + 2] = 'a') and
       (fLine[Run + 3] = 't') and
       (fLine[Run + 4] = 'D') and
       (fLine[Run + 5] = 'o') and
       (fLine[Run + 6] = 'I') and
       (fLine[Run + 7] = 'H') and
       (fLine[Run + 8] = 'a') and
       (fLine[Run + 9] = 'v') and
       (fLine[Run + 10] = 'e') and
       (fLine[Run + 11] = 'T') and
       (fLine[Run + 12] = 'o') and
       (fLine[Run + 13] = 'W') and
       (fLine[Run + 14] = 'r') and
       (fLine[Run + 15] = 'i') and
       (fLine[Run + 16] = 't') and
       (fLine[Run + 17] = 'e') and
       (fLine[Run + 18] = 'H') and
       (fLine[Run + 19] = 'e') and
       (fLine[Run + 20] = 'r') and
       (fLine[Run + 21] = 'e') and
       (fLine[Run + 22] = 'T') and
       (fLine[Run + 23] = 'o') and
       (fLine[Run + 24] = 'M') and
       (fLine[Run + 25] = 'a') and
       (fLine[Run + 26] = 'k') and
       (fLine[Run + 27] = 'e') and
       (fLine[Run + 28] = 'I') and
       (fLine[Run + 29] = 't') and
       (fLine[Run + 30] = 'S') and
       (fLine[Run + 31] = 'i') and
       (fLine[Run + 32] = 'n') and
       (fLine[Run + 33] = 'g') and
       (fLine[Run + 34] = 'l') and
       (fLine[Run + 35] = 'e') and
       (fLine[Run + 36] = 'L') and
       (fLine[Run + 37] = 'i') and
       (fLine[Run + 38] = 'n') and
       (fLine[Run + 39] = 'e') and
       (fLine[Run + 40] = 'C') and
       (fLine[Run + 41] = 'o') and
       (fLine[Run + 42] = 'm') and
       (fLine[Run + 43] = 'm') and
       (fLine[Run + 44] = 'e') and
       (fLine[Run + 45] = 'n') and
       (fLine[Run + 46] = 't') then
    begin
      Inc(Run, 47);
      fRange := rsUnKnown;
      Break;
    end;
    if not (fLine[Run] in [#0, #10, #13]) then
      Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynTatraDASSyn.StringOpenProc;
begin
  Inc(Run);
  fRange := rsString;
  StringProc;
  fTokenID := tkString;
end;

procedure TSynTatraDASSyn.StringProc;
begin
  fTokenID := tkString;
  repeat
    if (fLine[Run] = '''') then
    begin
      Inc(Run, 1);
      fRange := rsUnKnown;
      Break;
    end;
    if not (fLine[Run] in [#0, #10, #13]) then
      Inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

constructor TSynTatraDASSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCallsAttri := TSynHighLighterAttributes.Create(SYNS_AttrCalls);
  fCallsAttri.Style := [fsbold];
  fCallsAttri.Foreground := $00CC00;
  AddAttribute(fCallsAttri);

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clNavy;
  AddAttribute(fCommentAttri);

  fEntryPointAttri := TSynHighLighterAttributes.Create(SYNS_AttrEntryPoint);
  fEntryPointAttri.Style := [fsbold];
  fEntryPointAttri.Foreground := clPurple;
  AddAttribute(fEntryPointAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fInstructionsAttri := TSynHighLighterAttributes.Create(SYNS_AttrInstructions);
  fInstructionsAttri.Style := [fsbold];
  fInstructionsAttri.Foreground := clBlack;
  AddAttribute(fInstructionsAttri);

  fJumpsAttri := TSynHighLighterAttributes.Create(SYNS_AttrJumps);
  fJumpsAttri.Style := [fsbold];
  fJumpsAttri.Foreground := $00CC00;
  AddAttribute(fJumpsAttri);

  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);

  fKlucSlovaAttri := TSynHighLighterAttributes.Create(SYNS_AttrKlucSlova);
  fKlucSlovaAttri.Style := [fsbold];
  fKlucSlovaAttri.Foreground := clblue;
  AddAttribute(fKlucSlovaAttri);

  fLoopsAttri := TSynHighLighterAttributes.Create(SYNS_AttrLoops);
  fLoopsAttri.Style := [fsbold];
  fLoopsAttri.Foreground := $00CC00;
  AddAttribute(fLoopsAttri);

  fReturnsAttri := TSynHighLighterAttributes.Create(SYNS_AttrReturns);
  fReturnsAttri.Style := [fsbold];
  fReturnsAttri.Foreground := $FF66FF;
  AddAttribute(fReturnsAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Style := [fsBold];
  fStringAttri.Foreground := clRed;
  AddAttribute(fStringAttri);

  fTestAttri := TSynHighLighterAttributes.Create(SYNS_AttrTest);
  fTestAttri.Style := [fsUnderline, fsItalic];
  fTestAttri.Foreground := clBlue;
  fTestAttri.Background := clSilver;
  AddAttribute(fTestAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterTatraDAS;
  fRange := rsUnknown;
end;

procedure TSynTatraDASSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLineRef := NewValue;
  fLine := PChar(fLineRef);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynTatraDASSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do
    Inc(Run);
end;

procedure TSynTatraDASSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run,2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynTatraDASSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsPascalComment: PascalCommentProc;
  else
    begin
      fRange := rsUnknown;
      fProcTable[fLine[Run]];
    end;
  end;
end;

function TSynTatraDASSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT    : Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER : Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD    : Result := fKeyAttri;
    SYN_ATTR_STRING     : Result := fStringAttri;
    SYN_ATTR_WHITESPACE : Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynTatraDASSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynTatraDASSyn.GetKeyWords: string;
begin
  Result := 
    'AAA,AAD,AAM,AAS,ADC,ADD,ADDPD,ADDPS,ADDSD,ADDSS,AND,ANDNPD,ANDNPS,AND' +
    'PD,ANDPS,ARPL,BOUND,BSF,BSR,BSWAP,BT,BTC,BTR,BTS,CALL,CBW,CDQ,CLC,CLD,' +
    'CLFLUSH,CLI,CLTS,CMC,CMOVA,CMOVAE,CMOVB,CMOVBE,CMOVE,CMOVL,CMOVLE,CMOV' +
    'NE,CMOVNL,CMOVNLE,CMOVNO,CMOVNP,CMOVNS,CMOVO,CMOVP,CMOVS,CMOVXX,CMP,CM' +
    'PPD,CMPPS,CMPS,CMPSB,CMPSD,CMPSS,CMPSW,CMPXCHG,CMPXCHG8B,COMISD,COMISS' +
    ',CPUID,CVTDQ2PD,CVTDQ2PS,CVTPD2DQ,CVTPD2PI,CVTPD2PS,CVTPI2PD,CVTPI2PS,' +
    'CVTPS2DQ,CVTPS2PD,CVTPS2PI,CVTSD2SI,CVTSD2SS,CVTSI2SD,CVTSI2SS,CVTSS2S' +
    'D,CVTSS2SI,CVTTPD2DQ,CVTTPD2PI,CVTTPS2DQ,CVTTPS2PI,CVTTSD2SI,CVTTSS2SI' +
    ',CWD,CWDE,DAA,DAS,DEC,DIV,DIVPD,DIVPS,DIVSD,DIVSS,EMMS,ENTER,Entry,Exp' +
    'orted,F2XM1,FABS,FADD,FADDP,FBLD,FBSTP,FCLEX,FCMOVB,FCMOVBE,FCMOVE,FCM' +
    'OVNB,FCMOVNBE,FCMOVNE,FCMOVNU,FCMOVU,FCOM,FCOMI,FCOMIP,FCOMP,FCOMPP,FC' +
    'OS,FDECSTP,FDIV,FDIVP,FDIVR,FDIVRP,FEMMS,FFREE,FCHS,FIADD,FICOM,FICOMP' +
    ',FIDIV,FIDIVR,FILD,FIMUL,FINCSTP,FINIT,FIST,FISTP,FISTTP,FISUB,FISUBR,' +
    'FLD,FLD1,FLDCW,FLDENV,FLDL2E,FLDL2T,FLDLG2,FLDLN2,FLDPI,FLDZ,FMUL,FMUL' +
    'P,FNCLEX,FNINIT,FNOP,FNSAVE,FNSTCW,FNSTENV,FNSTSW,FPATAN,FPREM,FPREM1,' +
    'FPTAN,FRNDINT,from,FRSTOR,FSAVE,FSCALE,FSIN,FSINCOS,FSQRT,FST,FSTCW,FS' +
    'TENV,FSTP,FSTSW,FSUB,FSUBP,FSUBR,FSUBRP,FTST,FUCOM,FUCOMI,FUCOMIP,FUCO' +
    'MP,FUCOMPP,function,FWAIT,FXAM,FXCH,FXRSTOR,FXSAVE,FXTRACT,FYL2X,FYL2X' +
    'P1,Hello,HLT,IDIV,Imported,IMUL,IN,INC,INS,INSB,INSD,INSW,INT,INTO,INV' +
    'D,INVLPG,IRET,IRETD,IRETW,JA,JAE,JB,JBE,JC,JCXZ,JE,JECXZ,JG,JGE,JL,JLE' +
    ',JMP,JMPF,JMPN,JNA,JNAE,JNB,JNBE,JNC,JNE,JNG,JNGE,JNL,JNLE,JNO,JNP,JNS' +
    ',JNZ,JO,JP,JPE,JPO,JS,Jump,JZ,LAHF,LAR,LDMXCSR,LDS,LEA,LEAVE,LES,LFENC' +
    'E,LFS,LGDT,LGS,LIDT,LLDT,LMSW,LOCK,LODS,LODSB,LODSD,LODSW,LOOP,LOOPE,L' +
    'OOPNE,LOOPNZ,LOOPZ,LSL,LSS,LTR,MASKMOVDQU,MASKMOVQ,MAXPD,MAXPS,MAXSD,M' +
    'AXSS,MFENCE,MINPD,MINPS,MINSD,MINSS,module,MOV,MOVAPD,MOVAPS,MOVD,MOVD' +
    'Q2Q,MOVDQA,MOVDQU,MOVHLPS,MOVHPD,MOVHPS,MOVLHPS,MOVLPD,MOVLPS,MOVMSKPD' +
    ',MOVMSKPS,MOVNTDQ,MOVNTI,MOVNTPD,MOVNTPS,MOVNTQ,MOVQ,MOVQ2DQ,MOVS,MOVS' +
    'B,MOVSD,MOVSS,MOVSW,MOVSX,MOVUPD,MOVUPS,MOVZX,MUL,MULPD,MULPS,MULSD,MU' +
    'LSS,NEG,NOP,NOT,OR,ORPD,ORPS,OUT,OUTS,OUTSB,OUTSD,OUTSW,PACKSSDW,PACKS' +
    'SWB,PACKUSWB,PADDB,PADDD,PADDQ,PADDSB,PADDSW,PADDUSB,PADDUSW,PADDW,PAN' +
    'D,PANDN,PAUSE,PAVGB,PAVGW,PCMPEQB,PCMPEQD,PCMPEQW,PCMPGTB,PCMPGTD,PCMP' +
    'GTW,PEXTRW,PF2ID,PFACC,PFADD,PFCMPEQ,PFCMPGE,PFCMPGT,PFMAX,PFMIN,PFMUL' +
    ',PFRCP,PFRCPIT1,PFRCPIT2,PFRSQIT1,PFRSQRT,PFSUB,PFSUBR,PI2FD,PINSRW,PM' +
    'ADDWD,PMAXUB,PMINUB,PMINWS,PMOVMSKB,PMULHUW,PMULHW,PMULLW,PMULUDQ,poin' +
    't,POP,POPA,POPAD,POPAW,POPF,POPFD,POPFW,POR,PREFETCHNTA,PREFETCHT0,PRE' +
    'FETCHT1,PREFETCHT2,Program,PSADBW,PSHUFD,PSHUFHW,PSHUFLW,PSHUFW,PSLLD,' +
    'PSLLDQ,PSLLQ,PSLLW,PSRAD,PSRAW,PSRLD,PSRLDQ,PSRLQ,PSRLW,PSUBB,PSUBD,PS' +
    'UBQ,PSUBSB,PSUBSW,PSUBUSB,PSUBUSW,PSUBW,PUNPCKHBW,PUNPCKHDQ,PUNPCKHQDQ' +
    ',PUNPCKHWD,PUNPCKLBW,PUNPCKLDQ,PUNPCKLQDQ,PUNPCKLWD,PUSH,PUSHA,PUSHAD,' +
    'PUSHAW,PUSHF,PUSHFD,PUSHFW,PWAXSW,PXOR,RCL,RCPPS,RCPSS,RCR,RDMSR,RDPMC' +
    ',RDTSC,rep,repe,repne,repnz,repz,RET,RETF,RETN,ROL,ROR,RSM,RSQRTPS,RSQ' +
    'RTSS,SAHF,SAL,SAR,SBB,SCAS,SCASB,SCASD,SCASW,SETA,SETAE,SETB,SETBE,SET' +
    'C,SETE,SETG,SETGE,SETL,SETLE,SETNA,SETNAE,SETNB,SETNBE,SETNC,SETNE,SET' +
    'NG,SETNGE,SETNL,SETNLE,SETNO,SETNP,SETNS,SETNZ,SETO,SETP,SETPO,SETS,SE' +
    'TZ,SGDT,SHL,SHLD,SHR,SHRD,SHUFPD,SHUFPS,SIDT,SLDT,SMSW,SQRTPD,SQRTPS,S' +
    'QRTSD,SQRTSS,STC,STD,STI,STMXCSR,STOS,STOSB,STOSD,STOSW,STR,SUB,SUBPD,' +
    'SUBPS,SUBSD,SUBSS,SynEdit,SYSENTER,SYSEXIT,TEST,UCOMISD,UCOMISS,UD2,UN' +
    'PCKHPD,UNPCKHPS,UNPCKLPD,UNPCKPLS,used,VERR,VERW,WAIT,WBINVD,World,WRM' +
    'SR,XADD,XCHG,XLAT,XLATB,XOR,XORPD,XORPS';
end;

function TSynTatraDASSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynTatraDASSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynTatraDASSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkCalls: Result := fCallsAttri;
    tkComment: Result := fCommentAttri;
    tkEntryPoint: Result := fEntryPointAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkInstructions: Result := fInstructionsAttri;
    tkJumps: Result := fJumpsAttri;
    tkKey: Result := fKeyAttri;
    tkKlucSlova: Result := fKlucSlovaAttri;
    tkLoops: Result := fLoopsAttri;
    tkReturns: Result := fReturnsAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkTest: Result := fTestAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynTatraDASSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynTatraDASSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynTatraDASSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

function TSynTatraDASSyn.GetSampleSource: string;
begin
  Result := '{ Sample source for the demo highlighter }'#13#10 +
            #13#10 +
            'This highlighter will recognize the words Hello and'#13#10 +
            'World as keywords. It will also highlight "Strings".'#13#10 +
            #13#10 +
            'And a special keyword type: SynEdit'#13#10 +
            #13#10 +
            '/* This style of comments is also highlighted */';
end;

function TSynTatraDASSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterTatraDAS;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TSynTatraDASSyn.GetLanguageName: string;
begin
  Result := SYNS_LangTatraDAS;
end;

procedure TSynTatraDASSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynTatraDASSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynTatraDASSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynTatraDASSyn);
{$ENDIF}
end.
