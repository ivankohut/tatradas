unit Exporters;

interface

uses
  SysUtils, Classes, Types,
  procmat,
  ExecFileUnit;

const
  cNasmLineIndent = '  ';

type
  TExporter = class
  protected
    class function ExportLineToNASM(const ALine: string): string;
    class function NasmIsReferenceFromCode(const ALine: string): Boolean;
    class procedure ExportSectionToNASM(Disassembled: TStrings; Bit32: Boolean; AStream: TStream);
  public
    class procedure ExportToFile(AExportOption: TExportOption; AExportCustomDASOptions: TExportCustomDASOptions; AExecFile: TExecutableFile; ADestFileName: string);
  end;

implementation


uses
  StrUtils, Math,
  LoggerUnit, CodeSectionUnit, StringUtilities, SectionUnit;


class function TExporter.NasmIsReferenceFromCode(const ALine: string): Boolean;
begin
  Result := GetLineType(ALine) in [ltJumpRef, ltCallRef, ltLoopRef];
end;



class function TExporter.ExportLineToNASM(const ALine: string): string;
var
  InstructionStr: string;
  TargetAddress: Cardinal;
  InsertIndex: Integer;
  RelativeAddress: Integer;
begin
  // Empty line
  if Length(ALine) = 0 then begin
    Result := '';
    Exit;
  end;

  // Comment line
  if ALine[1] = ';' then begin
    Result := ALine;
    Exit;
  end;

  case ALine[2] of

    // Jump reference, Call reference or Loop reference, Exported function, Imported function or Program entry point
    'u', 'a', 'o', 'x', 'm', 'r': Result := ';' + ALine;

    else begin
      InstructionStr := Copy(ALine, ilInstructionMnemonicIndex, maxInt);

      // Control flow instruction - insert appropriate NASM keyword and change target address to label by adding "_" prefix
      if GetTargetAddress(ALine, TargetAddress) then begin
        InsertIndex := 1;
        while InstructionStr[InsertIndex] <> ' ' do
          Inc(InsertIndex);
        Inc(InsertIndex);

        RelativeAddress := Integer(TargetAddress - (GetLineAddress(ALine) + GetLineBytes(ALine)));

        // same segment, relative JMP, Jxx, CALL
        if (RelativeAddress < -128) or (RelativeAddress > 127) then
          Result := cNasmLineIndent + InsertStr('near _', InstructionStr, InsertIndex)
        // short (-127 .. 128) JMP, Jxx, JxCXZ, LOOP
        else
          // JMP - must use "short" to use short range JMP
          if InstructionStr[2] = 'M' then
            Result := cNasmLineIndent + InsertStr('short _', InstructionStr, InsertIndex)
          // Jxx, JxCXZ, LOOP
          else
            Result := cNasmLineIndent + InsertStr('_', InstructionStr, InsertIndex);
      end

      // Non-Control flow instruction
      else begin
        if InstructionStr <> '' then
          case InstructionStr[1] of
            // byte
            'b': begin
              if InstructionStr[6] = '-' then
                InstructionStr := 'db ' + Copy(InstructionStr, 6, 5)
              else
                InstructionStr := 'db ' + Copy(InstructionStr, 6, 4);
            end;
            // word
            'w': InstructionStr := 'dw ' + Copy(InstructionStr, 6, 7);
            // dword or double
            'd': case InstructionStr[2] of
                  'w': InstructionStr := 'dd ' + Copy(InstructionStr, 7, 11);
                  'o': InstructionStr := 'dq ' + Copy(InstructionStr, 8, 30);
                end;
            // qword
            'q': InstructionStr := 'dq ' + Copy(InstructionStr, 7, 19);
            // single
            's': InstructionStr := 'dd ' + Copy(InstructionStr, 8, 30);
            // extended
            'e': InstructionStr := 'dt ' + Copy(InstructionStr, 10, 30);
            // Pascal string
            'p': InstructionStr := 'db 0x' + IntToHex(GetLineBytes(ALine) - 1, 2) + ',' + Copy(InstructionStr, 9, 255);
            // C string
            'c': InstructionStr := 'db '+ Copy(InstructionStr, 9, 255) + ',0x00';
          end;
        Result := cNasmLineIndent + InstructionStr;
      end;
    end;
  end;
end;



class procedure TExporter.ExportSectionToNASM(Disassembled: TStrings; Bit32: Boolean; AStream: TStream);
var
  LineIndex: integer;
  Line: string;
  IsTargetAddress: boolean;
begin
  case Bit32 of
    true:  WriteLnToStream(AStream, 'BITS 32');
    false: WriteLnToStream(AStream, 'BITS 16');
  end;
  WriteLnToStream(AStream);

  IsTargetAddress := false;
  for LineIndex := 0 to Disassembled.Count - 1 do begin
    ProgressManager.IncPosition;
    if ProgressData.AbortExecution then
      Abort;

    // Labels creation
    Line := Disassembled[LineIndex];
    if IsTargetAddress then begin
      if GetLineType(Line) = ltInstruction then begin
        WriteLnToStream(AStream, '_0x' + LeftStr(Line, 8) + ':');
        IsTargetAddress := False;
      end;
    end
    else
     if NasmIsReferenceFromCode(Line) then
       IsTargetAddress := True;

    WriteLnToStream(AStream, ExportLineToNASM(Line));
  end;
  WriteLnToStream(AStream);
end;



procedure ExportSectionToDAS(Disassembled: TStrings; AStream: TStream);
var
  LineIndex: Integer;
begin
  for LineIndex := 0 to Disassembled.Count - 1 do begin
    WriteLnToStream(AStream, Disassembled[LineIndex]);
    ProgressManager.IncPosition;
    if ProgressData.AbortExecution then
      Abort;
  end;
  WriteLnToStream(AStream);
end;



procedure ExportSectionToCustomDAS(Disassembled: TStrings; AStream: TStream; AExportCustomDASOptions: TExportCustomDASOptions);
type
  TSaveInstruction = (siNone, siAddr, siPar, siDis, siAddr_Par, siPar_Dis, siAddr_Dis, siAll);
var
  si: TSaveInstruction;
  LineIndex: integer;
  Line: string;
begin
  si:= siNone;
  if (soAddress in AExportCustomDASOptions) and (soParsed in AExportCustomDASOptions) and (soDisassembled in AExportCustomDASOptions) then si:=siAll
  else if (soAddress in AExportCustomDASOptions) and (soParsed in AExportCustomDASOptions) then si:=siAddr_Par
  else if (soParsed in AExportCustomDASOptions) and (soDisassembled in AExportCustomDASOptions) then si:=siPar_Dis
  else if (soAddress in AExportCustomDASOptions) and (soDisassembled in AExportCustomDASOptions) then si:=siAddr_Dis
  else if (soAddress in AExportCustomDASOptions) then si:=siAddr
  else if (soParsed in AExportCustomDASOptions) then si:=siPar
  else if (soDisassembled in AExportCustomDASOptions) then si:=siDis;

  for LineIndex := 0 to Disassembled.Count - 1 do begin
    ProgressManager.IncPosition;
    if ProgressData.AbortExecution then
      Abort;

    Line:= Disassembled[LineIndex];
    if Line = '' then begin
      WriteLnToStream(AStream);
      Continue;
    end;
    case Line[2] of
      'u': if not (soJump in AExportCustomDASOptions) then Line:= '';
      'a': if not (soCall in AExportCustomDASOptions) then Line:= '';
      'x': if not (soExport in AExportCustomDASOptions) then Line:= '';
      'm': if not (soImport in AExportCustomDASOptions) then Line:= '';
      'r': if not (soEntryPoint in AExportCustomDASOptions) then Line:= '';
      else begin
        case si of
          siAll: ;
          siAddr:     Line:= LeftStr(Line, ilAddressLength);
          siPar:      Line:= TrimRight(Copy(Line, ilParsedIndex, ilMaxParsedLength));
          siDis:      Line:= Copy(Line, ilInstructionMnemonicIndex, MaxInt);
          siAddr_Par: Line:= TrimRight(LeftStr(Line, ilInstructionMnemonicIndex - 1));
          siPar_Dis:  Line:= Copy(Line, ilParsedIndex, MaxInt);
          siAddr_Dis: Line:= LeftStr(Line, ilAddressLength) + ' ' + Copy(Line, ilInstructionMnemonicIndex, MaxInt);
        end;
      end;
    end;
    WriteLnToStream(AStream, Line);
  end;
  WriteLnToStream(AStream);
end;



class procedure TExporter.ExportToFile(AExportOption: TExportOption; AExportCustomDASOptions: TExportCustomDASOptions; AExecFile: TExecutableFile; ADestFileName: string);
var
  SectionIndex: Integer;
  CodeSection: TCodeSection;
  OutStream: TStream;
begin
  OutStream := TFileStream.Create(ADestFileName, fmCreate);
  try
    // Save disassembled code sections
    WriteLnToStream(OutStream, InjectStr(DASFileFirstLine, [AExecFile.FileName]));
    WriteLnToStream(OutStream);
    for SectionIndex := 0 to AExecFile.Sections.Count - 1 do begin
      if AExecFile.Sections[SectionIndex].typ = stCode then begin
        CodeSection := (AExecFile.Sections[SectionIndex] as TCodeSection);
        Logger.Info('Start: Exporting code section ' + IntToStr(CodeSection.CodeSectionIndex));
        ProgressManager.StartPhase(ProcessText.SavingDAS + IntToStr(CodeSection.CodeSectionIndex), CodeSection.Disassembled.Count);
        WriteLnToStream(OutStream, '; ********************************************');
        WriteLnToStream(OutStream, '; Code Section Number: '  + IntToStr(CodeSection.CodeSectionIndex));
        WriteLnToStream(OutStream, '; ********************************************');
        case AExportOption of
          eoDAS: ExportSectionToDAS(CodeSection.Disassembled, OutStream);
          eoCustomDAS: ExportSectionToCustomDAS(CodeSection.Disassembled, OutStream, AExportCustomDASOptions);
          eoNASM: ExportSectionToNASM(CodeSection.Disassembled, CodeSection.Bit32, OutStream);
        end;
      end;
    end;
  finally
    OutStream.Free;
  end;
end;



end.
