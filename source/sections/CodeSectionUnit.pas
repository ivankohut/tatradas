{ TODO:
  - dtCUniCodeStr:    v ChangeToStringData
  - osetrit code section nulovej velkosti
  - zjednodusit moynosti ukladanie disassembled
  - najst zmysel pre Statistics (zatial je Statistics zakomentovane)

  dorobit Count do function TCodeSection.GetLineFromData

}

{
   TOTEST
     Save & Load

}

{
  problemy
    constructor ma strasne vela parametrov (a potreboval by dalsie)


}

unit CodeSectionUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  Classes,
  SysUtils,
  Math,
  StrUtils,
  Types,

  procmat,
  StringUtilities,
  DisassemblerUnit,
  ImportSectionUnit,
  ExportSectionUnit,
  DisassemblerTypes,
  SectionUnit;

const
  c_DASHeaderLinesCount = 5;
  c_MaxSpaceCount = 24;

type

   TCodeSection = class(TSection)
     private
       fBit32: boolean;
       fCodeSize: cardinal;                  // Velkost kodu tejto sekcie v bajtoch
       fIsDisassembled: boolean;
       fDisassembled: TTatraDASStringList;

       fHasEntryPoint: boolean;
       fEntryPointAddress: cardinal;

       fFileOffset: cardinal;
       fFileSize: cardinal;
       fMemOffset: cardinal;
       fMemSize: cardinal;

       fCodeSectionIndex: integer;             // Cislo sekcie v ramci kodovych sekcii

       InstructionLineCount, DataLineCount, ReferenceLineCount, BlankLineCount: cardinal;
//       Statistics: TStatistics;
       InstructionsCount: cardinal;
       fLastItem: cardinal;

       procedure SetEntryPointAddress(Address: cardinal);
       function GetMaxAddress: cardinal;

     public
// Polia
       CodeArray: TByteDynArray;        // Pole obsahujuce kod
       CodeStream: TMyMemoryStream;         // Stream obsahujuci kod
       DisassemblerMap: TByteDynArray;

       constructor Create(InputStream: TStream; aBit32: boolean; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aCodeSectionIndex: integer; aName: string; aExecFile: TObject); overload;
       constructor Create(ExecFile: TObject); overload;
       destructor Destroy; override;

       procedure DisassembleAll(Options: TDisassembleOptions);
       procedure DisassemblePart(Options: TDisassembleOptions);

       function IsInSection(MemAddress: cardinal): boolean;
       function GetPosition(MemAddress: cardinal): cardinal; // Get position in Disassembled from Address (memory address)
       function FindAddressableLine(Position: cardinal): cardinal; // forward search, returns $FFFFFFFF if reaches end of Disassembled
       procedure ClearDisassembled;
       function GetLineFromDataEx(PData: Pointer; DataType: cardinal; Signed: boolean; const StartAddress: cardinal; Count: integer): TStrings; // StartAddress is memory address
       procedure ReplaceLines(StartLineIndex, StartAddress, ByteCount: cardinal; NewLines: TStrings);

       function SaveToFile  (DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; override;
       function LoadFromFile(DHF: TStream; var DAS: TextFile): boolean; overload; override;

       property Bit32: boolean read fBit32;
       property CodeSize: cardinal read fCodeSize;                  // Velkost kodu tejto sekcie v bajtoch
       property IsDisassembled: boolean read fIsDisassembled;
       property Disassembled: TTatraDASStringList read fDisassembled;

       property FileOffset: cardinal read fFileOffset;
       property FileSize: cardinal read fFileSize;
       property MemOffset: cardinal read fMemOffset;
       property MemSize: cardinal read fMemSize;
       property MaxAddress: cardinal read GetMaxAddress;
       property LastItem: cardinal read fLastItem;
       property CodeSectionIndex: integer read fCodeSectionIndex;

       property EntryPointAddress: cardinal read fEntryPointAddress write SetEntryPointAddress; // currently relative to code section
       property HasEntryPoint: boolean read fHasEntryPoint;
     end;


function GetLineAddress(line: string): cardinal;
function GetLineBytes(line: string): cardinal;
function GetLineType(line: string): TLineType;
function GetTargetAddress(Line: string; var Address: cardinal): boolean;



implementation

uses
  LoggerUnit,
  DateUtils,
  ExecFileUnit,
  PEFileUnit;


//******************************************************************************
// TCodeSection class
//******************************************************************************


constructor TCodeSection.Create(InputStream: TStream; aBit32: boolean; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aCodeSectionIndex: integer; aName: string; aExecFile: TObject);
var position: cardinal;
    i: integer;
begin
  inherited Create(aName, aExecFile);

  fTyp:=stCode;
  fBit32:=aBit32;

  fFileOffset:= aFileOffset;
  fFileSize:= aFileSize;
  fMemOffset:= aMemOffset;
  fMemSize:= aMemSize;

  fCodeSectionIndex:= aCodeSectionIndex;



  // Set CodeSize
  if aFileSize <> 0 then
    fCodeSize:=aMemSize // consider Min(aMemSize, aFileSize) ?
  else
    fCodeSize:=0;

  // Set CodeArray & CodeStream
  SetLength(CodeArray, CodeSize + CodeArrayReserve);
  CodeStream:= TMyMemoryStream.Create;
  CodeStream.SetMemory(Pointer(CodeArray), CodeSize + CodeArrayReserve);
  position:= InputStream.Position;
  InputStream.Position:=aFileOffset;
  CodeStream.CopyFrom(InputStream, Min(InputStream.Size - InputStream.Position, CodeSize)); //+CodeArrayReserve
  InputStream.Position:=position;
  for i:= CodeSize to CodeSize + CodeArrayReserve - 1 do
    CodeArray[i]:=0;  // vynulovanie pola nad byty precitane zo suboru

  // Set DisassemblerMap
  SetLength(DisassemblerMap, CodeSize + CodeArrayReserve);
  for i:=0 to CodeSize + CodeArrayReserve - 1 do
    DisassemblerMap[i]:= 0;

  fDisassembled:= TTatraDASStringList.Create;
end;



constructor TCodeSection.Create(ExecFile: TObject);
begin
  fExecFile:= ExecFile;
end;



destructor TCodeSection.Destroy;
begin
  Disassembled.Free;
  inherited;
end;



procedure TCodeSection.SetEntryPointAddress(Address: cardinal);
begin
  fEntryPointAddress:= Address;
  fHasEntryPoint:= true;
end;



function TCodeSection.GetMaxAddress: cardinal;
begin
  result:= MemOffset + MemSize - 1;
end;



procedure TCodeSection.ClearDisassembled;
var CodeIndex: cardinal;
begin
  if CodeSize > 0 then begin
    Disassembled.Clear;
    for CodeIndex:= 0 to CodeSize - 1 do
      DisassemblerMap[CodeIndex]:= 0;
  end;
  fIsDisassembled:= false;
end;


// StartAddress is section address (not memory)
procedure TCodeSection.ReplaceLines(StartLineIndex, StartAddress, ByteCount: cardinal; NewLines: TStrings);
var
  AfterAddress: cardinal;
  Address: cardinal;
  LineIndex: cardinal;
  PreCodeIndex, RestCodeIndex: cardinal;
  PreLines: TStrings;
begin
  // Konverzia zaciatku instrukcie, v ktorej zaciname nahradzovanie, na typ "byte"
  PreLines:= TStringList.Create;
  for PreCodeIndex:= GetLineAddress(Disassembled[StartLineIndex]) to StartAddress + MemOffset - 1 do
    DisassemblerMap[PreCodeIndex - MemOffset]:= dfNone;

  PreLines.AddStrings(GetLineFromDataEx(
    Addr(CodeArray[GetLineAddress(Disassembled[StartLineIndex]) - MemOffset]),
    dtByte,
    false,
    GetLineAddress(Disassembled[StartLineIndex]),
    (StartAddress + MemOffset - 1) - GetLineAddress(Disassembled[StartLineIndex])
  ));
  PreLines.AddStrings(NewLines);
//  NewLines.Free;
  NewLines:= PreLines;

  // Jednoduche zistenie kolko riadkov nahradime (bezadresne riadky sa vyhodia)
  LineIndex:= StartLineIndex;
  Address:= StartAddress;
  AfterAddress:= StartAddress + ByteCount;
  while Address < AfterAddress do begin
    if GetLineType(Disassembled[LineIndex]) = ltInstruction then
      Inc(Address, GetLineBytes(Disassembled[LineIndex]));
    Inc(LineIndex);
  end;

  // Konverzia zvysnych bytov na datovy typ "byte"
  for RestCodeIndex:=1 to Address - AfterAddress do
    DisassemblerMap[AfterAddress + RestCodeIndex - 1]:= dfNone;
  NewLines.AddStrings(GetLineFromDataEx(Addr(CodeArray[AfterAddress]), dtByte, false, AfterAddress + MemOffset, Address - AfterAddress));

  // Actual replace
  Disassembled.BeginUpdate;
  Disassembled.DeleteLines(StartLineIndex, LineIndex - StartLineIndex);
  Disassembled.InsertStrings(StartLineIndex, NewLines);
  Disassembled.EndUpdate;
  //sm('Pokracuj');
end;



procedure TCodeSection.DisassemblePart(Options: TDisassembleOptions);
var
  BlockIndex: integer;
  CodeIndex: cardinal;
  ReferenceIndex: integer;
  NewLines: TStrings;
  Disassembler: TDisassembler;
begin
  ProgressData.Name:= ProcessText.Disassembling;
  NewLines:= nil;
  Disassembler:= nil;
  try
    // Disassemble
    Disassembler:= TDisassembler.Create(CodeArray, DisassemblerMap, MemOffset, Options.Bit32);
    Disassembler.CAJ.Add(Options.Address - MemOffset);
    Disassembler.CAJ.Process(Options.Address - MemOffset + Options.Size - 1);

    Disassembler.Disassemble(Options.Recursive);
  //  Statistics:=Disassembler.Statistics;

    // Process all disassembled blocks
    ProgressData.Name:= ProcessText.PreparingOutput;
    ProgressData.Position:= 0;
    ProgressData.Maximum:= Disassembler.BlockCount;

    NewLines:= TStringList.Create;
    for BlockIndex:=0 to Disassembler.BlockCount - 1 do begin
      Inc(ProgressData.Position);
      if ProgressData.ErrorStatus = errUserTerminated then
        raise EUserTerminatedProcess.Create('');

      NewLines.Clear;
      CodeIndex:= Disassembler.Blocks[BlockIndex].Address;
      while CodeIndex < Disassembler.Blocks[BlockIndex].Address + Disassembler.Blocks[BlockIndex].Size do begin
        if (DisassemblerMap[CodeIndex] <> 0) and ((DisassemblerMap[CodeIndex] and dfNewInstr) = 0) then // otestovat ci to funguje tak ako ma
          Continue;
        // References
        for ReferenceIndex:=0 to (Disassembler.Disassembled[CodeIndex].ReferCount - 1) do
          Newlines.Add(Disassembler.Disassembled[CodeIndex].Refer[ReferenceIndex]);

        // Instruction
        DisassemblerMap[CodeIndex]:= (DisassemblerMap[CodeIndex] AND Byte(255 - dfNewInstr));
        DisassemblerMap[CodeIndex]:= (DisassemblerMap[CodeIndex] OR Byte(dfInstruction));
  //      Dec(DisassemblerMap[CodeIndex], dfNewInstr);
  //      Inc(DisassemblerMap[CodeIndex], dfInstruction);

        Newlines.Add(Disassembler.Disassembled[CodeIndex].Disassembled);

        Inc(CodeIndex);
      end;

      ReplaceLines(GetPosition(Disassembler.Blocks[BlockIndex].Address + MemOffset), Disassembler.Blocks[BlockIndex].Address, Disassembler.Blocks[BlockIndex].Size, NewLines);
    end;

    // Compute LastItem
    CodeIndex:= CodeSize - 1;
    while (((DisassemblerMap[CodeIndex] and dfInstruction) = 0) and (DisassemblerMap[CodeIndex] <> 0)) do
      Dec(CodeIndex);
    fLastItem:= CodeIndex;

  finally
    NewLines.Free;
    Disassembler.Free;
  end;
end;



procedure TCodeSection.DisassembleAll(Options: TDisassembleOptions);               // Disasseblovanie, hladanie skokov, spracovanie vystupu...
var
  i,j,k: integer;

  IndexAddressStr: string;
  IndexAddress: cardinal;
  CallInstrAddress: cardinal;
  FunctionFromModul: string; // nazov importovanej funkcie a modulu

  Position: cardinal; // Position of exported function's entry point in Disassembled

  ReferenceIndex: integer; // Index of instruction references
  CodeIndex: cardinal;

  ImportSection: TImportSection;
  ExportSection: TExportSection;
  Disassembler: TDisassembler;

begin
  // Empty section is considered already disassembled
  if CodeSize = 0 then
    Exit;

  Disassembler:= nil;

  try

    ImportSection:= (ExecFile as TExecutableFile).ImportSection;
    ExportSection:= (ExecFile as TExecutableFile).ExportSection;

    ProgressData.Name:= ProcessText.Disassembling + IntToStr(CodeSectionIndex);

    Disassembler:= TDisassembler.Create(CodeArray, DisassemblerMap, MemOffset, Bit32);

    if (ExecFile as TExecutableFile).ExeFormat = ffNE then
      if ImportSection <> nil then begin
        for i:=0 to Integer(ImportSection.ModulCount) - 1 do begin
          for j:=0 to Integer(ImportSection.Moduls[i].FunctionCount) - 1 do begin
            for k:=0 to Length(ImportSection.Moduls[i].Functions[j].Occurs)-1 do begin
              if ImportSection.Moduls[i].Functions[j].Occurs[k].SectionIndex = SectionIndex then begin
                Disassembler.CAJ.Add(ImportSection.Moduls[i].Functions[j].Occurs[k].Address);
              end;
            end;
          end;
        end;
      end;

    // Add exported functions' entry points to Disassembler.CAJ
    if ExportSection <> nil then begin
      Disassembler.CAJ.Capacity:=Disassembler.CAJ.Capacity + ExportSection.functioncount;
      for i:=0 to ExportSection.FunctionCount-1 do
        if SectionIndex = ExportSection.functions[i].section then
          Disassembler.CAJ.Add(ExportSection.functions[i].CodeSectionOffset);
    end;

    // Add program's entry point to Disassembler.CAJ if present
    if fHasEntryPoint then
      Disassembler.CAJ.Add(EntryPointAddress);

    Disassembler.CAJ.Process(CodeSize);


    // Disassemble !!!
    Logger.Info('Start: Disassemble');
    Disassembler.DisassembleAll;
    Logger.Info('Finish: Disassemble');
    //  Statistics:=Disassembler.Statistics;


    // Vyhladanie EntryPoint-u a jeho zapisanie do pola Disassembler.Disassembled
    if fHasEntryPoint then begin
      with Disassembler.Disassembled[EntryPointAddress] do begin
        Inc(ReferCount, 3);
        SetLength(refer, ReferCount);
        refer[Refercount-3]:= '';
        refer[Refercount-2]:= 'Program Entry point';
        refer[Refercount-1]:= '';
      end;
    //    Inc(Statistics.References);
    //    Inc(Statistics.Blanks,2);
    end;

    // Process imported functions (if execfile is PE file)
    if (ImportSection <> nil) then begin
      case (ExecFile as TExecutableFile).ExeFormat of

        ffPE: begin
          for i:=0 to Length(Disassembler.Imported)-1 do begin
            // Priprava parametrov pre GetModulAndFunction
            IndexAddressStr:=Disassembler.Disassembled[Disassembler.Imported[i]].Disassembled;
            IndexAddressStr:=Copy(IndexAddressStr, ilInstructionMnemonicIndex + 3 + 10 + Math.IfThen(Disassembler.Disassembled[Disassembler.Imported[i]].Disassembled[ilInstructionMnemonicIndex] = 'C', 1, 0), 8);
            IndexAddress:=cardinal(StrToIntDef('$'+IndexAddressStr, 0));
            if IndexAddress = 0 then
              Continue;
            CallInstrAddress:= StrToInt('$' + LeftStr(Disassembler.Disassembled[Disassembler.Imported[i]].Disassembled, 8));
            FunctionFromModul:=ImportSection.AddFunctionOccurence(IndexAddress, CallInstrAddress, SectionIndex);
            if FunctionFromModul <> '' then begin
    //            Inc(Statistics.References);
    //            Inc(Statistics.Blanks);
              with Disassembler.Disassembled[Disassembler.Imported[i]] do begin
                Inc(ReferCount,2);
                SetLength(Refer, ReferCount);
                refer[ReferCount-2]:= '';
                refer[ReferCount-1]:= 'Imported function '''+FunctionFromModul + ''' used';
              end;
            end;
          end;
        end;

        ffNE: begin
          for i:=0 to ImportSection.ModulCount - 1 do begin
            for j:=0 to integer(ImportSection.Moduls[i].FunctionCount) - 1 do begin
              for k:=0 to Length(ImportSection.Moduls[i].Functions[j].Occurs) - 1 do begin
                if ImportSection.Moduls[i].Functions[j].Occurs[k].SectionIndex = SectionIndex then begin
                  with Disassembler.Disassembled[ImportSection.Moduls[i].Functions[j].Occurs[k].Address] do begin
                    Inc(ReferCount, 2);
                    SetLength(Refer, ReferCount);
                    refer[ReferCount-2]:= '';
                    if ImportSection.Moduls[i].Functions[j].ByOrdinal then
                      refer[ReferCount-1]:= 'Imported function ''' + IntToHex(ImportSection.Moduls[i].Functions[j].Ordinal, 4) + '''(Ordinal) from ''' + ImportSection.Moduls[i].Name + ''' used'
                    else
                      refer[ReferCount-1]:= 'Imported function ''' + ImportSection.Moduls[i].Functions[j].Name + ''' from ''' + ImportSection.Moduls[i].Name + ''' used';
                  end;
                end;
              end;
            end;
          end;
        end;

      end;
    end;

    // Process exported functions
    if ExportSection <> nil then begin
      for i:=0 to ExportSection.FunctionCount-1 do begin
        if ExportSection.functions[i].section = SectionIndex then begin
    //        Inc(Statistics.References);
    //        Inc(Statistics.Blanks);

          Position:=ExportSection.functions[i].CodeSectionOffset;
          with Disassembler.Disassembled[Position] do begin
            Inc(ReferCount, 2);
            SetLength(refer, ReferCount);
            refer[ReferCount-2]:='';
            if (ExportSection.Functions[i].Name <> '') then
              refer[ReferCount-1]:= 'Exported function ''' + ExportSection.functions[i].name + ''''
            else
              refer[ReferCount-1]:= 'Exported function ' + 'ordinal: ''' + IntToStr(ExportSection.functions[i].ordinal) + '''';
          end;
        end;
      end;
    end;

    // Set Disassembled string list

    //  Inc(Statistics.Blanks);                             // prazdny riadok na zaciatku

    ProgressData.Name:= ProcessText.PreparingOutput + IntToStr(CodeSectionIndex);
    ProgressData.Position:= 0;
    ProgressData.Maximum:= CodeSize - 1;

    fDisassembled.Add('');  // prazdny riadok na zaciatku

    Logger.Info('Start: Preparing output');

    for i:= 0 to CodeSize - 1 do begin
      Inc(ProgressData.Position);
      if ProgressData.ErrorStatus = errUserTerminated then
        raise EUserTerminatedProcess.Create('');

      // Continue if current address does not contain an instruction
      if (DisassemblerMap[i] <> 0) and ((DisassemblerMap[i] and dfNewInstr) = 0) then
        Continue;

      // Update current address' metainfo from NewInstruction to Instruction
      if (DisassemblerMap[i] and dfNewInstr) <> 0 then begin
        Dec(DisassemblerMap[i], dfNewInstr);
        Inc(DisassemblerMap[i], dfInstruction);
      end;

      // Proccess instruction's references
      for ReferenceIndex:=0 to (Disassembler.Disassembled[i].refercount - 1) do
        Disassembled.Add(Disassembler.Disassembled[i].refer[ReferenceIndex]);

      Disassembled.Add(Disassembler.Disassembled[i].Disassembled);
    end;
    Logger.Info('Finish: Preparing output');

    // Compute LastItem
    CodeIndex:= CodeSize - 1;
    while (((DisassemblerMap[CodeIndex] and dfInstruction) = 0) and (DisassemblerMap[CodeIndex] <> 0)) do
      Dec(CodeIndex);
    fLastItem:= CodeIndex;
    fIsDisassembled:= true;

  except
    on E: Exception do begin
      Logger.Fatal('CodeSection.Disassemble: ' + E.Message);
      fIsDisassembled:= false;
      Disassembler.Free;
      raise;
    end;
  end;
  Disassembler.Free;
end;



function TCodeSection.IsInSection(MemAddress: cardinal): boolean;
begin
  result:= (MemAddress >= fMemOffset) and (MemAddress < (fMemOffset + fMemSize));
end;



function TCodeSection.SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean;
type
  TSaveInstruction = (siNone, siAddr, siPar, siDis, siAddr_Par, siPar_Dis, siAddr_Dis, siAll);
var
  si: TSaveInstruction;
  LineIndex: integer;
  Line: string;

  IsTargetAddress: boolean;
  TargetAddress: cardinal;
  InsertIndex: integer;
  buf, adresa: cardinal;
  InstructionStr: string;
  RelativeAddress: integer;
  DisasmCount: cardinal;
begin
  inherited SaveToFile(DHF, DAS, SaveOptions);

  // DHF project file
  if soProject in SaveOptions then begin
    Logger.Info('Start: Saving DHF - code section ' + IntToStr(CodeSectionIndex));
    // Static data
    DHF.Write(fCodeSectionIndex, 4);
    DHF.Write(fBit32, 4);
    DHF.Write(fFileOffset, 4);
    DHF.Write(fFileSize, 4);
    DHF.Write(fMemOffset, 4);
    DHF.Write(fMemSize, 4);
    DHF.Write(fCodeSize, 4);
    DHF.Write(fHasEntryPoint, 1);
    DHF.Write(EntryPointAddress, 4);
    // Dynamic data
    DHF.Write(InstructionLineCount, 4);
    DHF.Write(DataLineCount, 4);
    DHF.Write(ReferenceLineCount, 4);
    DHF.Write(BlankLineCount, 4);
//    DHF.Write(Statistics,sizeof(TStatistics));
    DHF.Write(fLastItem, 4);
    DHF.Write(IsDisassembled, 1);
    DisasmCount:= Disassembled.Count;
    DHF.Write(DisasmCount, 4);
    DHF.Write(InstructionsCount, 4);
    // Arrays
    DHF.Write(CodeArray[0], CodeSize);
    DHF.Write(DisassemblerMap[0], CodeSize);
  end;

  // DAS file

  Logger.Info('Start: Saving DAS - code section ' + IntToStr(CodeSectionIndex));
  ProgressData.Name:= ProcessText.SavingDAS + IntToStr(CodeSectionIndex);
  ProgressData.Position:= 0;
  ProgressData.Maximum:= Disassembled.Count;
  Writeln(DAS, '; ********************************************');
  Writeln(DAS, '; Code Section Number: '  + IntToStr(CodeSectionIndex));
  Writeln(DAS, '; ********************************************');


  // Standard DAS file
  if (soProject in SaveOptions) or (soDisassembly in SaveOptions) then begin
    for LineIndex:= 0 to Disassembled.Count - 1 do begin
      WriteLn(DAS, Disassembled[LineIndex]);
      Inc(ProgressData.Position);
      if ProgressData.ErrorStatus = errUserTerminated then
        raise EUserTerminatedProcess.Create('');
     end;
    Writeln(DAS);
  end


  // NASM compilable DAS file
  else if soNASM in SaveOptions then begin
    case Bit32 of
      true:  Writeln(DAS, 'BITS 32');
      false: Writeln(DAS, 'BITS 16');
    end;
    Writeln(DAS);

    IsTargetAddress:= false;
    for LineIndex:=0 to Disassembled.Count - 1 do begin
      Inc(ProgressData.Position);
      if ProgressData.ErrorStatus = errUserTerminated then
        raise EUserTerminatedProcess.Create('');

      Line:= Disassembled[LineIndex];

      // Empty line
      if Length(Line) = 0 then begin
        WriteLn(DAS);
        Continue;
      end;

      case Line[2] of

        // Jump reference
        'u': begin
               WriteLn(DAS, ';' + Line);
               IsTargetAddress:= true;
             end;

        // Call reference
        'a': begin
               WriteLn(DAS, ';' + Line);
               IsTargetAddress:= true;
             end;

        // Loop reference
        'o': begin
               WriteLn(DAS, ';' + Line);
               IsTargetAddress:= true;
             end;

        // Exported function
        'x': WriteLn(DAS, ';' + Line);

        // Imported function
        'm': WriteLn(DAS, ';' + Line);

        // Program entry point
        'r': WriteLn(DAS, ';' + Line);

      else begin
        // Create new label from address if it is target of s jump, call or loop instruction
        if IsTargetAddress then begin
          WriteLn(DAS, '_0x' + LeftStr(Line, 8) + ':');
          IsTargetAddress:= false;
        end;
        InstructionStr:= Copy(Line, ilInstructionMnemonicIndex, maxInt);

        // control flow instruction
        //   - insert appropriate NASM keyword and change target address to label by adding "_"
        if GetTargetAddress(Line, TargetAddress) then begin
          InsertIndex:= 1;
          while InstructionStr[InsertIndex] <> ' ' do
            Inc(InsertIndex);
          Inc(InsertIndex);

          RelativeAddress:= Integer(TargetAddress - (GetLineAddress(Line) + GetLineBytes(Line)));

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


        else begin
//          parsed8:=StrToInt('$'+Line[10]+Line[11]);
              if InstructionStr<>'' then
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
  end


  // Custom DAS file
  else begin
    si:= siNone;
    if (soAddress in SaveOptions) and (soParsed in SaveOptions) and (soDisassembled in SaveOptions) then si:=siAll
    else if (soAddress in SaveOptions) and (soParsed in SaveOptions) then si:=siAddr_Par
    else if (soParsed in SaveOptions) and (soDisassembled in SaveOptions) then si:=siPar_Dis
    else if (soAddress in SaveOptions) and (soDisassembled in SaveOptions) then si:=siAddr_Dis
    else if (soAddress in SaveOptions) then si:=siAddr
    else if (soParsed in SaveOptions) then si:=siPar
    else if (soDisassembled in SaveOptions) then si:=siDis;

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
        'u': if not (soJump in SaveOptions) then Line:= '';
        'a': if not (soCall in SaveOptions) then Line:= '';
        'x': if not (soExport in SaveOptions) then Line:= '';
        'm': if not (soImport in SaveOptions) then Line:= '';
        'r': if not (soEntryPoint in SaveOptions) then Line:= '';
        else begin
          case si of
            siAll: ;
            siAddr:     Line:= LeftStr(Line, ilMaxAddressLength);
            siPar:      Line:= TrimRight(Copy(Line, ilMaxAddressLength + 2, ilMaxParsedLength));
            siDis:      Line:= Copy(Line, ilInstructionMnemonicIndex, MaxInt);
            siAddr_Par: Line:= TrimRight(LeftStr(Line, ilInstructionMnemonicIndex - 1));
            siPar_Dis:  Line:= Copy(Line, ilMaxAddressLength + 2, MaxInt);
            siAddr_Dis: Line:= LeftStr(Line, ilMaxAddressLength) + ' ' + Copy(Line, ilInstructionMnemonicIndex, MaxInt);
          end;
        end;
      end;
      WriteLn(DAS, Line);
    end;
    WriteLn(DAS);
  end;
  Logger.Info('Finish: Saving DAS - code section ' + IntToStr(CodeSectionIndex));

  result:= true;
end;



function TCodeSection.LoadFromFile(DHF: TStream; var DAS: TextFile): boolean;
var
  Line: string;
  LineIndex: integer;
  DisasmCount: cardinal;
begin
  inherited LoadFromFile(DHF, DAS);

  // DHF project file:

  // Static data
  DHF.Read(fCodeSectionIndex, 4);
  DHF.Read(fBit32, 4);
  DHF.Read(fFileOffset, 4);
  DHF.Read(fFileSize, 4);
  DHF.Read(fMemOffset, 4);
  DHF.Read(fMemSize, 4);
  DHF.Read(fCodeSize, 4);
  DHF.Read(fHasEntryPoint, 1);
  DHF.Read(fEntryPointAddress,4);
  // Dynamic data
  DHF.Read(InstructionLineCount, 4);
  DHF.Read(DataLineCount, 4);
  DHF.Read(ReferenceLineCount, 4);
  DHF.Read(BlankLineCount, 4);
//  DHF.Read(Statistics, SizeOf(TStatistics));
  DHF.Read(fLastItem, 4);
  DHF.Read(fIsDisassembled, 1);
  DHF.Read(DisasmCount, 4);
  DHF.Read(InstructionsCount, 4);
  // Arrays
  SetLength(CodeArray, CodeSize);
  DHF.Read(CodeArray[0], CodeSize);
  SetLength(DisassemblerMap, CodeSize);
  DHF.Read(DisassemblerMap[0], CodeSize);
  CodeStream:= TMyMemoryStream.Create;
  CodeStream.SetMemory(Pointer(CodeArray), CodeSize);

  // DAS file

  ProgressData.Position:= 0;
  ProgressData.Maximum:= DisasmCount;
  ProgressData.Name := ProcessText.LoadingDAS + IntToStr(CodeSectionIndex);
  Readln(DAS);
  Readln(DAS);
  Readln(DAS);

  fDisassembled:= TTatraDASStringList.Create;
  fDisassembled.Capacity:= DisasmCount;
  try
    for LineIndex:= 0 to DisasmCount - 1 do begin
      ProgressData.Position:= LineIndex;
      if ProgressData.ErrorStatus = errUserTerminated then
        raise EUserTerminatedProcess.Create('');
      ReadLn(DAS, line);
      fDisassembled.Add(line);
    end;
  except
    on E: EInOutError do
      if E.ErrorCode <> 100 then
        raise;
    else
      raise;
  end;

  ProgressData.Position:= 0;
  ProgressData.Finished:= true;
  result:=true;
end;



// Get position in Disassembled from Address (memory address)
function TCodeSection.GetPosition(MemAddress: cardinal): cardinal;
var
  Estimate: cardinal;
  DisIndex: cardinal;
begin
  // Range check of Address
  if (MemAddress < MemOffset) or (MemAddress > MaxAddress) then begin
    result:= cardinal(-1);
    Exit;
  end;

  // Process if Address is in last item
  if MemAddress >= MemOffset + LastItem then begin
    DisIndex:= Disassembled.Count - 1;
    while (GetLineAddress(Disassembled[DisIndex]) = $FFFFFFFF) do
      Dec(DisIndex);
    result:= DisIndex;
    Exit;
  end;

  // Estimate position
  Estimate:= Min(Round ( (MemAddress - fMemOffset) / (fMemSize/Disassembled.Count)), Disassembled.Count - 1);

  // Find a addressable (e.g. not comment, reference, blank etc.) line close to estimated position
  DisIndex:= Estimate;

    // backwards search
    while (GetLineAddress(Disassembled[DisIndex]) = $FFFFFFFF) and (DisIndex > 0) do begin
      Dec(DisIndex);
    end;

    // forward search if backward not succesful
    if DisIndex = 0 then
      DisIndex:= Estimate;
      while (GetLineAddress(Disassembled[DisIndex]) = $FFFFFFFF) do begin
        Inc(DisIndex);
      end;

  // Find line with address equal to (or lower than) Address
  if GetLineAddress(Disassembled[DisIndex]) < MemAddress then begin
    result:= DisIndex;
    while true do begin
      if GetLineAddress(Disassembled[DisIndex]) = $FFFFFFFF then begin
        Inc(DisIndex);
        Continue;
      end;
      case CompareValue(GetLineAddress(Disassembled[DisIndex]), MemAddress) of
        LessThanValue: begin
          result:= DisIndex;
          Inc(DisIndex);
        end;

        EqualsValue: begin
          result:= DisIndex;
          Break;
        end;

        GreaterThanValue:
          Break;
      end;
    end;
  end

  else begin
    while (MemAddress < GetLineAddress(Disassembled[DisIndex])) or (GetLineAddress(Disassembled[DisIndex])=$FFFFFFFF) do
      Dec(DisIndex);
    result:= DisIndex;
  end;
end;



function TCodeSection.FindAddressableLine(Position: cardinal): cardinal; // forward search, returns $FFFFFFFF if reaches end of Disassembled
begin
  result:= Position;
  while result < cardinal(Disassembled.Count) do begin
    if GetLineType(Disassembled[result]) = ltInstruction then
      Exit
    else
      Inc(result);
  end;
  result:= $FFFFFFFF;
end;


// StartAddress is memory address
function TCodeSection.GetLineFromDataEx(PData: Pointer; DataType: cardinal; Signed: boolean; const StartAddress: cardinal; Count: integer): TStrings;
var
  Address: cardinal;
  ItemSize: integer;
  Line: string;
  LineIndex: integer;
  DataPtr, BytePtr: Pointer;
  i: integer;
begin
  ItemSize:= DataTypeSizes[DataType];
  Address:= StartAddress;
  DataPtr:= PData;
  result:= TStringList.Create;
  result.Capacity:= Count;

  for LineIndex:= 0 to Count - 1 do begin
//    Address:=  StartAddress + cardinal(LineIndex * ItemSize);
    Line:= IntToHex(Address, 8) + ' ';

    // Parsed
//    DataPtr:= Pointer(cardinal(PData) + cardinal(LineIndex*ItemSize));
    BytePtr:= DataPtr;
    for i:= 1 to ItemSize do begin
      Line:= Line + IntToHex(Byte(BytePtr^), 2);
      Inc(cardinal(BytePtr));
    end;
    Line:= StringRightPad(Line, ilInstructionMnemonicIndex - 1);

    // Data
    if Signed then begin
      case DataType of
        dtByte:
          if Sign(shortint(DataPtr^)) = -1 then
            Line:= Line + 'byte ' + '-0x' + IntToHex(Abs(shortint(DataPtr^)), 2)
          else
            Line:= Line + 'byte ' +  '0x' + IntToHex(byte(DataPtr^), 2);

        dtWord:
          if Sign(smallInt(DataPtr^)) = -1 then
            Line:= Line + 'word ' + '-0x' + IntToHex(Abs(smallInt(DataPtr^)), 4)
          else
            Line:= Line + 'word ' +  '0x' + IntToHex(word(DataPtr^), 4);

        dtDword:
          if Sign(integer(DataPtr^)) = -1 then
            Line:= Line + 'dword ' + '-0x' + IntToHex(Abs(integer(DataPtr^)), 8)
          else
            Line:= Line + 'dword ' +  '0x' + IntToHex(cardinal(DataPtr^), 8);

        dtQword:
          if Sign(Int64(DataPtr^)) = -1 then
            Line:= Line + 'qword ' + '-0x' + IntToHex(Abs(Int64(DataPtr^)), 16)
          else
            Line:= Line + 'qword ' +  '0x' + IntToHex(Int64(DataPtr^), 16);
      end;
    end

    else
      try
        case DataType of
          dtByte: Line:= Line + 'byte 0x' + IntToHex(byte(DataPtr^), 2);
          dtWord: Line:= Line + 'word 0x' + IntToHex(word(DataPtr^), 4);
          dtDword: Line:= Line + 'dword 0x' + IntToHex(cardinal(DataPtr^), 8);
          dtQword: Line:= Line + 'qword 0x' + IntToHex(int64(DataPtr^), 16);
          dtSingle: Line:= Line + 'single ' + FirstCommaToPoint(FloatToStrF(single(DataPtr^), ffGeneral, 7, 4));
          dtDouble: Line:= Line + 'double ' + FirstCommaToPoint(FloatToStrF(double(DataPtr^), ffGeneral, 15, 4));
          dtDoubleEx: Line:= Line + 'extended ' + FirstCommaToPoint(FloatToStrF(extended(DataPtr^), ffGeneral, 18, 4));
        end;
      except
        case DataType of
          dtSingle: Line:= Line + 'single NAN';
          dtDouble: Line:= Line + 'double NAN';
          dtDoubleEx: Line:= Line + 'unsupported extended real number';
        end;
      end;

    if DataType = dtByte then
      if not ( byte(DataPtr^) in [$00, $0A, $0D] ) then
        Line:= Line + ' ''' + Char(byte(DataPtr^)) + '''';

    Result.Add(Line);

    Inc(Address, ItemSize);
    Inc(cardinal(DataPtr), ItemSize);
  end;
end;



//******************************************************************************
// Utilities
//******************************************************************************


function GetLineType(line: string): TLineType;
begin
  if length(line) = 0 then begin
    result:= ltEmpty;
    Exit;
  end;
  case line[1] of
    ';': result:=ltComment;
    'C': if line[3]='l' then
           result:=ltCallRef
         else
           result:=ltInstruction;

    'J': result:=ltJumpRef;
    'L': result:=ltLoopRef;
    'I': result:=ltImportRef;
    'E': if line[2]='x' then
           result:=ltExportRef
         else
           result:=ltInstruction;

    'P': result:=ltEntryPointRef;

    else
      result:=ltInstruction;
  end;
end;



function GetLineAddress(line: string): cardinal;
begin
  result:= cardinal(StrToIntDef('$' + LeftStr(line, 8), -1));
end;



function GetLineBytes(line: string): cardinal;
var i:cardinal;
begin
  if line[10] = 'b' then
    result:= StrToInt('$' + Copy(line, 17, 8))
  else begin
    i:=11;
    while line[i] <> ' ' do inc(i, 2);
    result:= (i-11) div 2;
  end;
end;



// type of Line is ltInstruction
function GetTargetAddress(Line: string; var Address: cardinal): boolean;

  function HexToAddress(Index: integer): boolean;
  begin
    // Exit if target address does not begin with '0x' or if it is far pointer (0x????:?...)
    if (Line[Index] <> '0') or (Line[Index + 1] <> 'x') or (Line[Index+6] = ':') then begin
      result:= false;
      Exit;
    end;
    Inc(Index, 2);
    try
      Address:= cardinal(StrToInt('$' + Trim(Copy(Line, Index, 8))));
      result:= true;
    except
      result:= false;
    end;
  end;

var
  Index: integer;
begin
  result:= false;

  // Este to nie je spravne osetrene!!!
  if Length(Line) < ilInstructionMnemonicIndex + 10 then
    Exit;

  Index:= ilInstructionMnemonicIndex;

  // JMP, Jxx
  if Line[Index] = 'J' then begin
    while Line[Index] <> ' ' do
      Inc(Index);
    Inc(Index);
    result:= HexToAddress(Index);
  end

  // CALL
  else if Copy(Line, Index ,4) = 'CALL' then begin
    Inc(Index, 5);
    result:= HexToAddress(Index);
  end

  // LOOPxx
  else if Copy(Line, Index, 4) = 'LOOP' then begin
    while Line[Index] <> ' ' do Inc(Index);
    Inc(Index);
    result:= HexToAddress(Index);
  end
end;



end.
