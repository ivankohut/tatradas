unit Exporters;

interface

uses
  procmat, 
  ExecFileUnit;


procedure ExportToFile(AExportOption: TExportOption; AExportCustomDASOptions: TExportCustomDASOptions; AExecFile: TExecutableFile; ADestFileName: string);


implementation


uses
  SysUtils, Classes, Types, StrUtils, Math,
  LoggerUnit, CodeSectionUnit, StringUtilities, SectionUnit;

  


procedure ExportSectionToNASM(Disassembled: TStrings; Bit32: Boolean; CodeArray: TByteDynArray; var DAS: TextFile);
var
  LineIndex: integer;
  Line: string;

  IsTargetAddress: boolean;
  TargetAddress: cardinal;
  InsertIndex: integer;
  buf, adresa: cardinal;
  InstructionStr: string;
  RelativeAddress: integer;

begin
  case Bit32 of
    true:  Writeln(DAS, 'BITS 32');
    false: Writeln(DAS, 'BITS 16');
  end;
  Writeln(DAS);

  IsTargetAddress := false;
  for LineIndex := 0 to Disassembled.Count - 1 do begin
    Inc(ProgressData.Position);
    if ProgressData.ErrorStatus = errUserTerminated then
      raise EUserTerminatedProcess.Create('');

    Line := Disassembled[LineIndex];

    // Empty line
    if Length(Line) = 0 then begin
      WriteLn(DAS);
      Continue;
    end;

    // Comment line
    if Line[1] = ';' then begin
      WriteLn(DAS, Line);
      Continue;
    end;

    case Line[2] of

      // Jump reference, Call reference or Loop reference
      'u', 'a', 'o': begin
        WriteLn(DAS, ';' + Line);
        IsTargetAddress := true;
      end;

      // Exported function, Imported function or Program entry point
      'x', 'm', 'r': WriteLn(DAS, ';' + Line);

      else begin
        // Create new label from address if it is target of s jump, call or loop instruction
        if IsTargetAddress then begin
          WriteLn(DAS, '_0x' + LeftStr(Line, 8) + ':');
          IsTargetAddress := false;
        end;
        InstructionStr := Copy(Line, ilInstructionMnemonicIndex, maxInt);

        // Control flow instruction - insert appropriate NASM keyword and change target address to label by adding "_" prefix
        if GetTargetAddress(Line, TargetAddress) then begin
          InsertIndex := 1;
          while InstructionStr[InsertIndex] <> ' ' do
            Inc(InsertIndex);
          Inc(InsertIndex);

          RelativeAddress := Integer(TargetAddress - (GetLineAddress(Line) + GetLineBytes(Line)));

          // same segment, relative JMP, Jxx, CALL
          if (RelativeAddress < -128) or (RelativeAddress > 127) then
            Writeln(DAS, '  ' + InsertStr('near _', InstructionStr, InsertIndex))
          // short (-127 .. 128) JMP, Jxx, JxCXZ, LOOP
          else
            // JMP - must use "short" to use short range JMP
            if InstructionStr[2] = 'M' then
              Writeln(DAS, '  ' + InsertStr('short _', InstructionStr, InsertIndex))
            // Jxx, JxCXZ, LOOP
            else
              Writeln(DAS, '  ' + InsertStr('_', InstructionStr, InsertIndex))
        end

        // Non-Control flow instruction
        else begin
//          parsed8:=StrToInt('$'+Line[10]+Line[11]);
              if InstructionStr <> '' then
              case InstructionStr[1] of
                'b': InstructionStr:='db '+TrimRight(Copy(InstructionStr, 6, 5));
                'w': InstructionStr:='dw '+Copy(InstructionStr,6,7);
                'd': case InstructionStr[2] of
                      'w':InstructionStr:='dd '+Copy(InstructionStr, 7, 11);
                      'o':InstructionStr:='dq '+Copy(InstructionStr, 8, 30);
                    end;
                'q': begin
                      adresa:= GetLineAddress(Line);
                      buf:= Cardinal(CodeArray[adresa]);
                      Writeln(DAS, '  ' + 'dd 0x'+IntToHex(buf, 8));
                      buf:= Cardinal(CodeArray[adresa]);

                      InstructionStr:= 'dd 0x' + IntToHex(buf, 8);
                    end;
                's': InstructionStr:= 'dd ' + Copy(InstructionStr, 8, 30);
                'e': InstructionStr:= 'dt ' + Copy(InstructionStr, 10, 30);
                'p': begin
                      adresa:= GetLineAddress(Line);
                      InstructionStr:= 'db 0x'+IntToHex(CodeArray[adresa], 2)+','+Copy(InstructionStr, 9, 255);
                    end;
                'c': InstructionStr:= 'db '+ Copy(InstructionStr, 9, 255) + ',0x00';
              end;
              Writeln(DAS, '  ' + InstructionStr);
        end;
      end;
    end;
  end;
  Writeln(DAS);

end;



procedure ExportSectionToDAS(Disassembled: TStrings; var DAS: TextFile);
var
  LineIndex: Integer;
begin
  for LineIndex := 0 to Disassembled.Count - 1 do begin
    WriteLn(DAS, Disassembled[LineIndex]);
    Inc(ProgressData.Position);
    if ProgressData.ErrorStatus = errUserTerminated then
      raise EUserTerminatedProcess.Create('');
   end;
  Writeln(DAS);
end;



procedure ExportSectionToCustomDAS(Disassembled: TStrings; var DAS: TextFile; AExportCustomDASOptions: TExportCustomDASOptions);
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

  for LineIndex:= 0 to Disassembled.Count - 1 do begin
    Inc(ProgressData.Position);
    if ProgressData.ErrorStatus = errUserTerminated then
      raise EUserTerminatedProcess.Create('');

    Line:= Disassembled[LineIndex];
    if Line = '' then begin
      WriteLn(DAS);
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
    WriteLn(DAS, Line);
  end;
  WriteLn(DAS);
end;



procedure ExportToFile(AExportOption: TExportOption; AExportCustomDASOptions: TExportCustomDASOptions; AExecFile: TExecutableFile; ADestFileName: string);
var
  DAS: TextFile;
  SectionIndex: Integer;
  CodeSection: TCodeSection;
begin
  AssignFile(DAS, ADestFileName);
  Rewrite(DAS);

  // Save disassembled code sections
  WriteLn(DAS, InjectStr(DASFileFirstLine, [AExecFile.FileName]));
  Writeln(DAS);
  for SectionIndex := 0 to AExecFile.Sections.Count - 1 do begin
    if AExecFile.Sections[SectionIndex].typ = stCode then begin
      CodeSection := (AExecFile.Sections[SectionIndex] as TCodeSection);
      Logger.Info('Start: Exporting code section ' + IntToStr(CodeSection.CodeSectionIndex));
      ProgressData.Name := ProcessText.SavingDAS + IntToStr(CodeSection.CodeSectionIndex);
      ProgressData.Position := 0;
      ProgressData.Maximum := CodeSection.Disassembled.Count;

      Writeln(DAS, '; ********************************************');
      Writeln(DAS, '; Code Section Number: '  + IntToStr(CodeSection.CodeSectionIndex));
      Writeln(DAS, '; ********************************************');
      case AExportOption of
        eoDAS: ExportSectionToDAS(CodeSection.Disassembled, DAS);
        eoCustomDAS: ExportSectionToCustomDAS(CodeSection.Disassembled, DAS, AExportCustomDASOptions);
        eoNASM: ExportSectionToNASM(CodeSection.Disassembled, CodeSection.Bit32, CodeSection.CodeArray, DAS);
      end;
    end;
  end;

  CloseFile(DAS);
end;



end.
