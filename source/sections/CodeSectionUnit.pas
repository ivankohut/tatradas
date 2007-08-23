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
  mozne chyby:
    citanie IsDissambled zo suboru, predtym SizeOf(TSectionStatus), teraz SizeOf(boolean)

  problemy
    constructor ma strasne vela parametrov (a potreboval by dalsie)


}

{$DEFINE TATRADAS_FAST}

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
  disassembler,
  ImportSectionUnit,
  ExportSectionUnit,
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

       Decoder: TDisassembler;

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
// Dynamicke data
        //       PocetInstrukcii:cardinal;            // Pocet instrukcii v DisBloku
// Polia
       CodeArray: TByteDynamicArray;        // Pole obsahujuce kod
       CodeStream: TMyMemoryStream;         // Stream obsahujuci kod
       DisassemblerMap: TByteDynamicArray;

       Exportt: TExportSection;
       Import: TImportSection;

       constructor Create(InputStream: TStream; bb:boolean; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aCodeSectionIndex: integer; aName: string; aExecFile: TObject); overload;
       constructor Create(ExecFile: TObject); overload;
       destructor Destroy; override;

       function DisassembleAll(Options: TDisassembleOptions): boolean;
       function DisassemblePart(Options: TDisassembleOptions): boolean;

       function IsInSection(MemAddress: cardinal): boolean;
       function GetPosition(Address: cardinal): cardinal; // Get position in Disassembled from Address (memory address)
       function FindAddressableLine(Position: cardinal): cardinal; // forward search, returns $FFFFFFFF if reaches end of Disassembled
       procedure ClearDisassembled;
       function GetLineFromDataEx(PData: Pointer; DataType: cardinal; Signed: boolean; StartAddress: cardinal; Count: integer): TStrings;
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



implementation

uses
  ExecFileUnit,
  PEFileUnit;


//******************************************************************************
// TCodeSection class
//******************************************************************************


constructor TCodeSection.Create(InputStream: TStream; bb:boolean; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aCodeSectionIndex: integer; aName: string; aExecFile: TObject);
var position: cardinal;
    i: integer;
begin
  inherited Create(aName, aExecFile);

  fTyp:=stCode;
  fBit32:=bb;

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
  Disassembled.Clear;
  if CodeSize > 0 then
    for CodeIndex:= 0 to CodeSize - 1 do
      DisassemblerMap[CodeIndex]:= 0;
  fIsDisassembled:= false;
end;


// StartAddress is section address (not memory)
procedure TCodeSection.ReplaceLines(StartLineIndex, StartAddress, ByteCount: cardinal; NewLines: TStrings);
var
  AfterAddress: cardinal;
  Address: cardinal;
  LineIndex: cardinal;
  RestCodeIndex: cardinal;
begin
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
  NewLines.AddStrings(GetLineFromDataEx(Addr(CodeArray[AfterAddress]), dtByte, false, AfterAddress, Address - AfterAddress));

  // Actual replace
  Disassembled.DeleteLines(StartLineIndex, LineIndex - StartLineIndex);
  Disassembled.InsertStrings(StartLineIndex, NewLines);
end;



function TCodeSection.DisassemblePart(Options: TDisassembleOptions): boolean;
var
  BlockIndex: integer;
  CodeIndex: cardinal;
  ReferenceIndex: integer;
  Line: string;
  NewLines: TStrings;
begin
  // Disassemble
  Decoder:=TDisassembler.Create(CodeArray, DisassemblerMap, MemOffset, Options.Bit32);
//  Decoder.IsDisassembled:=True;
  Decoder.CAJ.Add(Options.Address - MemOffset);
  Decoder.CAJ.Process(Options.Address - MemOffset + Options.Size - 1);
  if not Decoder.Disassemble(Options.Recursive) then begin
    Decoder.Free;
    Decoder:=nil;
    result:=false;
    Exit;
  end;
//  Statistics:=Decoder.Statistics;

  // Process all disassembled blocks
  NewLines:=TStringList.Create;
  for BlockIndex:=0 to Decoder.BlockCount - 1 do begin
    NewLines.Clear;
    CodeIndex:=Decoder.Blocks[BlockIndex].Address;
    while CodeIndex < Decoder.Blocks[BlockIndex].Address + Decoder.Blocks[BlockIndex].Size do begin
      // References
      for ReferenceIndex:=0 to (Decoder.Disassembled[CodeIndex].ReferCount - 1) do
        Newlines.Add(Decoder.Disassembled[CodeIndex].Refer[ReferenceIndex]);

      // Instruction
      Dec(DisassemblerMap[CodeIndex], dfNewInstr);
      Inc(DisassemblerMap[CodeIndex], dfInstruction);

      Line:= IntToHex(Decoder.Disassembled[CodeIndex].Address + fMemOffset, 8) + ' ' + StringRightPad(Decoder.Disassembled[CodeIndex].parsed, MaxParsedLength) + ' ';

      if Decoder.Disassembled[CodeIndex].prefix <> '' then
        Line:=Line + Decoder.Disassembled[CodeIndex].prefix + ' ';
      Line:=Line + Decoder.Disassembled[CodeIndex].name + ' ' + Decoder.Disassembled[CodeIndex].operandy;
      Newlines.Add(Line);

      Inc(CodeIndex, Length(Decoder.Disassembled[CodeIndex].parsed) div 2);
    end;

    ReplaceLines(GetPosition(Decoder.Blocks[BlockIndex].Address + MemOffset), Decoder.Blocks[BlockIndex].Address, Decoder.Blocks[BlockIndex].Size, NewLines);
  end;
  NewLines.Free;

  // Compute LastItem
  CodeIndex:= CodeSize - 1;
  while (((DisassemblerMap[CodeIndex] and dfInstruction) = 0) and (DisassemblerMap[CodeIndex] <> 0)) do
    Dec(CodeIndex);
  fLastItem:= CodeIndex;

  Decoder.Free;
  Decoder:= nil;
  Result:=True;
end;



function TCodeSection.DisassembleAll(Options: TDisassembleOptions):boolean;               // Disasseblovanie, hladanie skokov, spracovanie vystupu...
var
  i,j,k: integer;

  IndexAddressStr: string;
  IndexAddress: cardinal;
  CallInstrAddress: cardinal;
  FunctionFromModul:string; // nazov importovanej funkcie a modulu

  Position: cardinal; // Position of exported function's entry point in Disassembled

  ReferenceIndex: integer; // Index of instruction references
  Line: string;         // retazec, do ktoreho sa pridavaju jednotlive casti intstruckie, nakoniec je zapisany do pola Disassembled (priprava vystupu)
  CodeIndex: cardinal;

  procedure CancelDisassemblying;
  begin
    Decoder.Free;
    Decoder:=nil;
    fDisassembled.Free;
    fDisassembled:=nil;
    Result:=false;
  end;

begin
  // Empty section is considered already disassembled
  if CodeSize = 0 then begin
    result:=true;        // tu mozno treba este nieco pridat
    Exit;
  end;

  ProgressData.Name:= ProcessText.Disassemblying + IntToStr(CodeSectionIndex);

  Decoder:= TDisassembler.Create(CodeArray, DisassemblerMap, MemOffset, Bit32);

  if (ExecFile as TExecutableFile).ExeFormat = ffNE then
    if Import <> nil then begin
      for i:=0 to Integer(Import.ModulCount) - 1 do begin
        for j:=0 to Integer(Import.Moduls[i].FunctionCount) - 1 do begin
          for k:=0 to Length(Import.Moduls[i].Functions[j].Occurs)-1 do begin
            if Import.Moduls[i].Functions[j].Occurs[k].SectionIndex = SectionIndex then begin
              Decoder.CAJ.Add(Import.Moduls[i].Functions[j].Occurs[k].Address);
            end;
          end;
        end;
      end;
    end;

  // Add exported functions' entry points to Decoder.CAJ
  if Exportt <> nil then begin
    Decoder.CAJ.Capacity:=Decoder.CAJ.Capacity + Exportt.functioncount;
    for i:=0 to Exportt.functioncount-1 do
      if SectionIndex = Exportt.functions[i].section then
        Decoder.CAJ.Add(Exportt.functions[i].CodeSectionOffset);
  end;

  // Add program's entry point to Decoder.CAJ if present
  if fHasEntryPoint then
    Decoder.CAJ.Add(EntryPointAddress);

  Decoder.CAJ.Process(CodeSize);


  // Disassemble !!!
  if not Decoder.DisassembleAll then begin
    CancelDisassemblying;
    Exit;
  end;
//  Statistics:=Decoder.Statistics;


  // Vyhladanie EntryPoint-u a jeho zapisanie do pola Decoder.Disassembled
  if fHasEntryPoint then begin
    with Decoder.Disassembled[EntryPointAddress] do begin
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
  if (Import <> nil) then begin
    case (ExecFile as TExecutableFile).ExeFormat of

      ffPE: begin
        for i:=0 to Length(Decoder.Imported)-1 do begin
          // Priprava parametrov pre GetModulAndFunction
          IndexAddressStr:=Decoder.Disassembled[Decoder.Imported[i]].operandy;
          IndexAddressStr:=Copy(IndexAddressStr, 10, 8);
          IndexAddress:=cardinal(StrToIntDef('$'+IndexAddressStr, 0));
          if IndexAddress = 0 then
            Continue;
          CallInstrAddress:=Decoder.Disassembled[Decoder.Imported[i]].Address + MemOffset;
          FunctionFromModul:=Import.AddFunctionOccurence(IndexAddress, CallInstrAddress, SectionIndex);
          if FunctionFromModul <> '' then begin
//            Inc(Statistics.References);
//            Inc(Statistics.Blanks);
            with Decoder.Disassembled[Decoder.Imported[i]] do begin
              Inc(ReferCount,2);
              SetLength(Refer, ReferCount);
              refer[ReferCount-2]:= '';
              refer[ReferCount-1]:= 'Imported function '''+FunctionFromModul + ''' used';
            end;
          end;
        end;
      end;

      ffNE: begin
        for i:=0 to Import.ModulCount - 1 do begin
          for j:=0 to integer(Import.Moduls[i].FunctionCount) - 1 do begin
            for k:=0 to Length(Import.Moduls[i].Functions[j].Occurs) - 1 do begin
              if Import.Moduls[i].Functions[j].Occurs[k].SectionIndex = SectionIndex then begin
                with Decoder.Disassembled[Import.Moduls[i].Functions[j].Occurs[k].Address] do begin
                  Inc(ReferCount, 2);
                  SetLength(Refer, ReferCount);
                  refer[ReferCount-2]:= '';
                  if Import.Moduls[i].Functions[j].ByOrdinal then
                    refer[ReferCount-1]:= 'Imported function '''+ IntToHex(Import.Moduls[i].Functions[j].Ordinal, 4) + '''(Ordinal) from ''' + Import.Moduls[i].Name + ''' used'
                  else
                    refer[ReferCount-1]:= 'Imported function '''+ Import.Moduls[i].Functions[j].Name + ''' from ''' + Import.Moduls[i].Name + ''' used';
                end;
              end;
            end;
          end;
        end;
      end;

    end;
  end;

  // Process exported functions
  if Exportt <> nil then begin
    for i:=0 to Exportt.FunctionCount-1 do begin
      if Exportt.functions[i].section = SectionIndex then begin
//        Inc(Statistics.References);
//        Inc(Statistics.Blanks);

        Position:=Exportt.functions[i].CodeSectionOffset;
        with Decoder.Disassembled[Position] do begin
          Inc(ReferCount, 2);
          SetLength(refer, ReferCount);
          refer[ReferCount-2]:='';
          if (Exportt.Functions[i].Name <> '') then
            refer[ReferCount-1]:='Exported function ''' + Exportt.functions[i].name + ''''
          else
            refer[ReferCount-1]:='Exported function ' + 'ordinal: ''' + IntToStr(Exportt.functions[i].ordinal) + '''';
        end;
      end;
    end;
  end;

  // Set Disassembled string list

//  Inc(Statistics.Blanks);                             // prazdny riadok na zaciatku

  ProgressData.Name:= ProcessText.PreparingOutput + IntToStr(CodeSectionIndex);
  ProgressData.Position:= 0;
  ProgressData.Maximum:= CodeSize - 1;

  fDisassembled:=TTatraDASStringList.Create;

  fDisassembled.Add('');  // prazdny riadok na zaciatku

  for i:= 0 to CodeSize - 1 do begin
    Inc(ProgressData.Position);

    // Continue if current address does not contain an instruction
    if (DisassemblerMap[i] <> 0) and ((DisassemblerMap[i] and dfNewInstr) = 0) then
      Continue;

    // Update current address' metainfo from NewInstruction to Instruction
    if (DisassemblerMap[i] and dfNewInstr) <> 0 then begin
      Dec(DisassemblerMap[i], dfNewInstr);
      Inc(DisassemblerMap[i], dfInstruction);
    end;

    // Proccess instruction's references
    for ReferenceIndex:=0 to (Decoder.Disassembled[i].refercount - 1) do
      Disassembled.Add(Decoder.Disassembled[i].refer[ReferenceIndex]);

    // Create intruction line of Disassembled from particles
    Line:= IntToHex(Decoder.Disassembled[i].Address + fMemOffset, 8) + ' ' + StringRightPad(Decoder.Disassembled[i].parsed, MaxParsedLength) + ' ';

    if Decoder.Disassembled[i].prefix <> '' then
      Line:=Line + Decoder.Disassembled[i].prefix + ' ';
    Line:=Line + Decoder.Disassembled[i].name + ' ' + Decoder.Disassembled[i].operandy;

    Disassembled.Add(Line);
  end;

  // Compute LastItem
  CodeIndex:= CodeSize - 1;
  while (((DisassemblerMap[CodeIndex] and dfInstruction) = 0) and (DisassemblerMap[CodeIndex] <> 0)) do
    Dec(CodeIndex);
  fLastItem:= CodeIndex;

  fIsDisassembled:= true;
  Decoder.Free;
  Decoder:=nil;
  Result:=True;
end;



function TCodeSection.IsInSection(MemAddress: cardinal): boolean;
begin
  result:= (MemAddress >= fMemOffset) and (MemAddress < (fMemOffset + fMemSize));
end;



function TCodeSection.SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean;
type TSaveInstruction = (siNone, siAddr, siPar, siDis, siAddr_Par, siPar_Dis, siAddr_Dis, siAll);
var si: TSaveInstruction;
    i,j: cardinal;
    TargetAddress: boolean;
    temp,buf,adresa: cardinal;
    temps: string;
    reladresa: integer;
    DisasmCount: cardinal;
begin
  inherited SaveToFile(DHF, DAS, SaveOptions);

  if (soProject in SaveOptions) or (soDisassembly in SaveOptions) then begin
  // Zapis do suboru "*.das"
    Writeln(DAS,'------------------------------');
    Writeln(DAS,'Code Section Number: ' + IntToStr(SectionIndex));
    Writeln(DAS,'Number of lines: ' + IntToStr(Disassembled.Count));

    ProgressData.Maximum := Disassembled.Count;
    ProgressData.Position := 0;
    ProgressData.Name := ProcessText.SavingDAS;
    for i := 0 to Disassembled.Count - 1 do begin
      WriteLn(DAS, Disassembled[i]);
      Inc(ProgressData.Position);
     end;
    Writeln(DAS);
  end;

  // Zapis do suboru "*.dhf"
  if soProject in SaveOptions then begin

// Staticke data
    DHF.Write(fCodeSectionIndex, 4);
    DHF.Write(fBit32, 4);
    DHF.Write(fFileOffset, 4);
    DHF.Write(fFileSize, 4);
    DHF.Write(fMemOffset, 4);
    DHF.Write(fMemSize, 4);
    DHF.Write(fCodeSize, 4);

    DHF.Write(fHasEntryPoint, 1);
    DHF.Write(EntryPointAddress, 4);
// Dynamicke data
    DHF.Write(InstructionLineCount, 4);
    DHF.Write(DataLineCount, 4);
    DHF.Write(ReferenceLineCount, 4);
    DHF.Write(BlankLineCount, 4);
//    DHF.Write(Statistics,sizeof(TStatistics));
    DHF.Write(IsDisassembled, 1);
    DisasmCount:= Disassembled.Count;
    DHF.Write(DisasmCount, 4);
    DHF.Write(InstructionsCount, 4);
// Polia
    DHF.Write(CodeArray[0], CodeSize);
    DHF.Write(DisassemblerMap[0], CodeSize);
  end;

  if not (soProject in SaveOptions) and not (soDisassembly in SaveOptions) and not (soNASM in SaveOptions) then begin
    ProgressData.Name:= ProcessText.SavingDAS + IntToStr(CodeSectionIndex);
    ProgressData.Position:= 0;
    ProgressData.Maximum:= Disassembled.Count;

    si:=siNone;
    if (soAddress in SaveOptions) and (soParsed in SaveOptions) and (soDisassembled in SaveOptions) then si:=siAll
    else if (soAddress in SaveOptions) and (soParsed in SaveOptions) then si:=siAddr_Par
    else if (soParsed in SaveOptions) and (soDisassembled in SaveOptions) then si:=siPar_Dis
    else if (soAddress in SaveOptions) and (soDisassembled in SaveOptions) then si:=siAddr_Dis
    else if (soAddress in SaveOptions) then si:=siAddr
    else if (soParsed in SaveOptions) then si:=siPar
    else if (soDisassembled in SaveOptions) then si:=siDis;

    Append(DAS);
    Writeln(DAS,'------------------------------');
    Writeln(DAS,'Code Section Number: '+IntToStr(SectionIndex));
//    Writeln(DAS,'Number of lines: '+IntToStr(PocetDisassembled));       nema zmysel (pocet naozaj zapisanych riadkov je stale iny

    for i:=0 to Disassembled.Count - 1 do begin
      Inc(ProgressData.Position);

      if Disassembled[i]='' then begin WriteLn; Continue; end;
      case Disassembled[i][2] of
        'u': if soJump in SaveOptions then writeln(DAS,Disassembled[i]);
        'a': if soCall in SaveOptions then writeln(DAS,Disassembled[i]);
        'x': if soExport in SaveOptions then writeln(DAS,Disassembled[i]);
        'm': if soImport in SaveOptions then writeln(DAS,Disassembled[i]);
        'r': if soEntryPoint in SaveOptions then writeln(DAS,Disassembled[i]);
      else begin
        case si of
          siAll: writeln(DAS,Disassembled[i]);
          siAddr: writeln(DAS,LeftStr(Disassembled[i],8));
          siPar: writeln(DAS,TrimRight(Copy(Disassembled[i],10,22)));
          siDis: writeln(DAS,Copy(Disassembled[i],34,20));
          siAddr_Par: writeln(DAS,TrimRight(LeftStr(Disassembled[i],33)));
          siPar_Dis: writeln(DAS,Copy(Disassembled[i],10,55));
          siAddr_Dis: writeln(DAS,LeftStr(Disassembled[i],8) + ' ' + Copy(Disassembled[i],34,20));
        end;
      end;
      end;
    end;
    Writeln(DAS);
  end;

  if soNASM in SaveOptions then begin
    ProgressData.Name:= ProcessText.SavingDAS + IntToStr(CodeSectionIndex);
    ProgressData.Position:= 0;
    ProgressData.Maximum:= Disassembled.Count;

    Append(DAS);
    Writeln(DAS,'; ------------------------------');
    Writeln(DAS,'; Code Section Number: '+IntToStr(SectionIndex));
    Writeln(DAS);
    case Bit32 of
      true:  Writeln(DAS,'BITS 32');
      false: Writeln(DAS,'BITS 16');
    end;
    Writeln(DAS);
//    Writeln(DAS,'Number of lines: '+IntToStr(PocetDisassembled));       nema zmysel (pocet naozaj zapisanych riadkov je stale iny

    TargetAddress:= false;
    for i:=0 to Disassembled.Count - 1 do begin
      Inc(ProgressData.Position);

      // Empty line
      if Disassembled[i]='' then begin
        WriteLn(DAS);
        Continue;
      end;

      case Disassembled[i][2] of

        // Jump reference
        'u': begin
               WriteLn(DAS, ';' + Disassembled[i]);
               TargetAddress:=true;
             end;

        // Call reference
        'a': begin
               WriteLn(DAS, ';' + Disassembled[i]);
               TargetAddress:=true;
             end;

        // Loop reference
        'o': begin
               WriteLn(DAS, ';' + Disassembled[i]);
               TargetAddress:=true;
             end;

        // Exported function
        'x': WriteLn(DAS, ';' + Disassembled[i]);

        // Imported function
        'm': WriteLn(DAS, ';' + Disassembled[i]);

        // Program entry point
        'r': WriteLn(DAS, ';' + Disassembled[i]);

      else begin
       // Create new label from address if it is target of any instruction
        if TargetAddress then begin
          WriteLn(DAS, '_0x' + LeftStr(Disassembled[i], 8) + ':');
          TargetAddress:= false;
        end;
        temps:=Copy(Disassembled[i], 34, 100);

        if GetTargetAddress(Disassembled[i],temp) then begin
          j:=1;
          while temps[j]<>' ' do inc(j);
          inc(j);
          reladresa:= temp - (GetLineAddress(Disassembled[i])+GetLineBytes(Disassembled[i]));
          if (reladresa < -128) or (reladresa > 127) then
            Writeln(DAS,'  '+InsertStr('near _',temps,j))
          else
            if temps[2]='M' then Writeln(DAS,'  '+InsertStr('short _',temps,j))
            else Writeln(DAS,'  '+InsertStr('_',temps,j))
        end
        else begin
//          parsed8:=StrToInt('$'+disassembled[i][10]+disassembled[i][11]);
              if temps<>'' then
              case temps[1] of
                'b': temps:='db '+TrimRight(Copy(temps,6,5));
                'w': temps:='dw '+Copy(temps,6,7);
                'd': case temps[2] of
                       'w':temps:='dd '+Copy(temps,7,11);
                       'o':temps:='dq '+Copy(temps,8,30);
                     end;
                'q': begin
                       adresa:=GetLineAddress(Disassembled[i]);
{
                       CodeStream.Position:=adresa;
                       CodeStream.Read(buf,4);
                       Writeln(DAS,'  '+'dd 0x'+IntToHex(buf,8));
                       CodeStream.Read(buf,4);
}
                       buf:=Cardinal(CodeArray[adresa]);
                       Writeln(DAS,'  '+'dd 0x'+IntToHex(buf,8));
                       buf:=Cardinal(CodeArray[adresa]);

                       temps:='dd 0x'+IntToHex(buf,8);
                     end;
                's': temps:='dd '+Copy(temps,8,30);
                'e': temps:='dt '+Copy(temps,10,30);
                'p': begin
                       adresa:=GetLineAddress(Disassembled[i]);
                       temps:='db 0x'+IntToHex(CodeArray[adresa],2)+','+Copy(temps,9,255);
                     end;
                'c': temps:='db '+ Copy(temps,9,255)+',0x00';
              end;
              Writeln(DAS,'  '+temps);
        end;
      end;
      end;
    end;
    Writeln(DAS);
  end;
  result:=true;
end;



function TCodeSection.LoadFromFile(DHF: TStream; var DAS: TextFile): boolean;
var
  line: string;
  i: integer;
  DisasmCount: cardinal;
begin
  inherited LoadFromFile(DHF, DAS);
  // DHF subor:
  // Staticke data
  DHF.Read(fCodeSectionIndex, 4);
  DHF.Read(fBit32, 4);
  DHF.Read(fFileOffset, 4);
  DHF.Read(fFileSize, 4);
  DHF.Read(fMemOffset, 4);
  DHF.Read(fMemSize, 4);
  DHF.Read(fCodeSize, 4);

  DHF.Read(fHasEntryPoint, 1);
  DHF.Read(fEntryPointAddress,4);
  // Dynamicke data
  DHF.Read(InstructionLineCount, 4);
  DHF.Read(DataLineCount, 4);
  DHF.Read(ReferenceLineCount, 4);
  DHF.Read(BlankLineCount, 4);
//  DHF.Read(Statistics, SizeOf(TStatistics));
  DHF.Read(fIsDisassembled, 1);
  DHF.Read(DisasmCount, 4);

  DHF.Read(InstructionsCount, 4);

  // Polia
  SetLength(CodeArray, CodeSize);
  DHF.Read(CodeArray[0], CodeSize);

  SetLength(DisassemblerMap, CodeSize);
  DHF.Read(DisassemblerMap[0], CodeSize);

  CodeStream:=TMyMemoryStream.Create;
  CodeStream.SetMemory(Pointer(CodeArray), CodeSize);

  // Nacitanie DAS suboru

  Readln(DAS, line);
  Readln(DAS, line);
  Readln(DAS, line);

  fDisassembled:= TTatraDASStringList.Create;

{
  fDisassembled.Add('');
  ProgressPosition:=0;
  while not EOF(DAS) do begin
    Inc(ProgressPosition);
    ReadLn(DAS, line);
    Disassembled.Add(line);
  end;
  ProgressPosition:=0;
}

  fDisassembled.Capacity:= DisasmCount;
  ProgressData.Position:= 0;
  ProgressData.Maximum:= DisasmCount;
  for i:=0 to DisasmCount - 1 do begin
    ProgressData.Position:= i;
    ReadLn(DAS, line);
    fDisassembled.Add(line);
  end;
  ProgressData.Position:= 0;
  ProgressData.Finished:= true;

  result:=true;
end;



// Get position in Disassembled from Address (memory address)
function TCodeSection.GetPosition(Address: cardinal): cardinal;
var
  Estimate: cardinal;
  DisIndex: cardinal;
begin
  // Range check of Address
  if (Address < MemOffset) or (Address > MaxAddress) then begin
    result:= cardinal(-1);
    Exit;
  end;

  // Process if Address is in last item
  if Address >= MemOffset + LastItem then begin
    DisIndex:= Disassembled.Count - 1;
    while (GetLineAddress(Disassembled[DisIndex]) = $FFFFFFFF) do
      Dec(DisIndex);
    result:= DisIndex;
    Exit;
  end;

  // Estimate position
  Estimate:= Min(Round ( (Address - fMemOffset) / (fMemSize/Disassembled.Count)), Disassembled.Count - 1);

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
  if GetLineAddress(Disassembled[DisIndex]) < Address then begin
    result:= DisIndex;
    while true do begin
      if GetLineAddress(Disassembled[DisIndex]) = $FFFFFFFF then begin
        Inc(DisIndex);
        Continue;
      end;
      case CompareValue(GetLineAddress(Disassembled[DisIndex]), Address) of
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
    while (Address < GetLineAddress(Disassembled[DisIndex])) or (GetLineAddress(Disassembled[DisIndex])=$FFFFFFFF) do
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


function TCodeSection.GetLineFromDataEx(PData: Pointer; DataType: cardinal; Signed: boolean; StartAddress: cardinal; Count: integer): TStrings;
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
    Line:= StringRightPad(Line, MaxAddressLength + 1 + MaxParsedLength + 1);

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
  result:=cardinal(StrToIntDef('$' + LeftStr(line, 8), -1));
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


end.
