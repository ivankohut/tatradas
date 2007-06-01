unit TDAS_SynEditThings;

interface

uses
  SynEditTypes,
  Math,
  Classes;


//*****************************************************
// From SynEditTypes
//*****************************************************


type
  PSynSelectionMode = ^TSynSelectionMode;
  TSynSelectionMode = (smNormal, smLine, smColumn);

  //todo: better field names. CharIndex and LineIndex?
  TBufferCoord = record
    Char: integer;
    Line: integer;
  end;


//*****************************************************
// From SynEditMiscProcs
//*****************************************************


type
  TConvertTabsProcEx = function(const Line: AnsiString; TabWidth: integer;
    var HasTabs: boolean): AnsiString;

function GetBestConvertTabsProcEx(TabWidth: integer): TConvertTabsProcEx;



implementation


// Don't change this function; no stack frame and efficient register use.
function GetHasTabs(pLine: PChar; var CharsBefore: integer): boolean;
begin
  CharsBefore := 0;
  if Assigned(pLine) then begin
    while (pLine^ <> #0) do begin
      if (pLine^ = #9) then break;
      Inc(CharsBefore);
      Inc(pLine);
    end;
    Result := (pLine^ = #9);
  end else
    Result := FALSE;
end;



function ConvertTabs1Ex(const Line: AnsiString; TabWidth: integer;
  var HasTabs: boolean): AnsiString;
var
  pDest: PChar;
  nBeforeTab: integer;
begin
  Result := Line;  // increment reference count only
  if GetHasTabs(pointer(Line), nBeforeTab) then begin
    HasTabs := TRUE;
    pDest := @Result[nBeforeTab + 1]; // this will make a copy of Line
    // We have at least one tab in the string, and the tab width is 1.
    // pDest points to the first tab char. We overwrite all tabs with spaces.
    repeat
      if (pDest^ = #9) then pDest^ := ' ';
      Inc(pDest);
    until (pDest^ = #0);
  end else
    HasTabs := FALSE;
end;



function IsPowerOfTwo(TabWidth: integer): boolean;
var
  nW: integer;
begin
  nW := 2;
  repeat
    if (nW >= TabWidth) then break;
    Inc(nW, nW);
  until (nW >= $10000);  // we don't want 64 kByte spaces...
  Result := (nW = TabWidth);
end;



function ConvertTabs2nEx(const Line: AnsiString; TabWidth: integer;
  var HasTabs: boolean): AnsiString;
var
  i, DestLen, TabCount, TabMask: integer;
  pSrc, pDest: PChar;
begin
  Result := Line;  // increment reference count only
  if GetHasTabs(pointer(Line), DestLen) then begin
    HasTabs := TRUE;
    pSrc := @Line[1 + DestLen];
    // We have at least one tab in the string, and the tab width equals 2^n.
    // pSrc points to the first tab char in Line. We get the number of tabs
    // and the length of the expanded string now.
    TabCount := 0;
    TabMask := (TabWidth - 1) xor $7FFFFFFF;
    repeat
      if (pSrc^ = #9) then begin
        DestLen := (DestLen + TabWidth) and TabMask;
        Inc(TabCount);
      end else
        Inc(DestLen);
      Inc(pSrc);
    until (pSrc^ = #0);
    // Set the length of the expanded string.
    SetLength(Result, DestLen);
    DestLen := 0;
    pSrc := PChar(Line);
    pDest := PChar(Result);
    // We use another TabMask here to get the difference to 2^n.
    TabMask := TabWidth - 1;
    repeat
      if (pSrc^ = #9) then begin
        i := TabWidth - (DestLen and TabMask);
        Inc(DestLen, i);
        //This is used for both drawing and other stuff and is meant to be #9 and not #32
        repeat
          pDest^ := #9;
          Inc(pDest);
          Dec(i);
        until (i = 0);
        Dec(TabCount);
        if (TabCount = 0) then begin
          repeat
            Inc(pSrc);
            pDest^ := pSrc^;
            Inc(pDest);
          until (pSrc^ = #0);
          exit;
        end;
      end else begin
        pDest^ := pSrc^;
        Inc(pDest);
        Inc(DestLen);
      end;
      Inc(pSrc);
    until (pSrc^ = #0);
  end else
    HasTabs := FALSE;
end;



function ConvertTabsEx(const Line: AnsiString; TabWidth: integer;
  var HasTabs: boolean): AnsiString;
var
  i, DestLen, TabCount: integer;
  pSrc, pDest: PChar;
begin
  Result := Line;  // increment reference count only
  if GetHasTabs(pointer(Line), DestLen) then begin
    HasTabs := TRUE;
    pSrc := @Line[1 + DestLen];
    // We have at least one tab in the string, and the tab width is greater
    // than 1. pSrc points to the first tab char in Line. We get the number
    // of tabs and the length of the expanded string now.
    TabCount := 0;
    repeat
      if (pSrc^ = #9) then begin
        DestLen := DestLen + TabWidth - DestLen mod TabWidth;
        Inc(TabCount);
      end else
        Inc(DestLen);
      Inc(pSrc);
    until (pSrc^ = #0);
    // Set the length of the expanded string.
    SetLength(Result, DestLen);
    DestLen := 0;
    pSrc := PChar(Line);
    pDest := PChar(Result);
    repeat
      if (pSrc^ = #9) then begin
        i := TabWidth - (DestLen mod TabWidth);
        Inc(DestLen, i);
        repeat
          pDest^ := #9;
          Inc(pDest);
          Dec(i);
        until (i = 0);
        Dec(TabCount);
        if (TabCount = 0) then begin
          repeat
            Inc(pSrc);
            pDest^ := pSrc^;
            Inc(pDest);
          until (pSrc^ = #0);
          exit;
        end;
      end else begin
        pDest^ := pSrc^;
        Inc(pDest);
        Inc(DestLen);
      end;
      Inc(pSrc);
    until (pSrc^ = #0);
  end else
    HasTabs := FALSE;
end;



function GetBestConvertTabsProcEx(TabWidth: integer): TConvertTabsProcEx;
begin
  if (TabWidth < 2) then Result := TConvertTabsProcEx(@ConvertTabs1Ex)
    else if IsPowerOfTwo(TabWidth) then
      Result := TConvertTabsProcEx(@ConvertTabs2nEx)
    else
      Result := TConvertTabsProcEx(@ConvertTabsEx);
end;


end.