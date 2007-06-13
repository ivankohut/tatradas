{ TODO:
  - dtCUniCodeStr:    v ChangeToStringData
  - osetrit code section nulovej velkosti
  - zjednodusit moynosti ukladanie disassembled
}

{
  mozne chyby:
    citanie IsDissambled zo suboru, predtym SizeOf(TSectionStatus), teraz SizeOf(boolean)

  problemy
    constructor ma strasne vela parametrov (a potreboval by dalsie)

    zamysliet sa nad LinesCount

}

{$DEFINE TATRADAS_FAST}

unit CodeSectionUnit;

{$INCLUDE 'delver.inc'}

interface

uses
  Classes,
  SysUtils,
  Math,

  {$IFDEF DELPHI}
//    StrUtils,//UProgressThread,ProgressFormUnit,
  {$ENDIF}
  StrUtils,

  //TatraDAS_SynEditStringList,
  //SynEditTextBuffer,

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
//       fDisassembled: TSynEditStringList;
       fDisassembled: TTatraDASStringList;

       EntryPointPresent: boolean;
       fEntryPointAddress: cardinal;

       Decoder: TDisassembler;

       fFileOffset: cardinal;
       fFileSize: cardinal;
       fMemOffset: cardinal;
       fMemSize: cardinal;

       procedure SetEntryPointAddress(Address: cardinal);

     public
// Staticke data
       fCodeSectionIndex: integer;             // Cislo sekcie v ramci kodovych sekcii

// Dynamicke data
       InstructionLineCount, DataLineCount, ReferenceLineCount, BlankLineCount: cardinal;
        //       PocetInstrukcii:cardinal;            // Pocet instrukcii v DisBloku
       Statistics: TStatistics;
       MaxAddress: cardinal;                 // Maximalna adresa DisBloku
       InstructionsCount: cardinal;
       LinesCount: cardinal; // na co to sluzi ?
// Polia
       CodeArray: TByteDynamicArray;        // Pole obsahujuce kod
       CodeStream: TMyMemoryStream;         // Stream obsahujuci kod
       DisassemblerMap: TByteDynamicArray;

//       EntryPointPosition: cardinal; // program Entry point index in Disassembled // vlastne sa to vobec nepouziva

       Exportt: TExportSection;
       Import: TImportSection;

       constructor Create(InputStream: TStream; bb:boolean; aName: string; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aSectionIndex: integer; aExecFile: TObject); overload;
       constructor Create(efile: TObject); overload;
       destructor Destroy; override;
       procedure ClearDisassembled;
       procedure ReplaceLines(StartLineIndex, StartAddress, ByteCount: cardinal; NewLines: TStrings);

       function DisassembleAll(Options: TDisassembleOptions):boolean;
       function DisassemblePart(Options: TDisassembleOptions):boolean;


       function SaveToFile  (DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; override;
       function LoadFromFile(DHF: TStream; var DAS: TextFile):boolean; overload; override;


       function InSection(MemAddress: cardinal): boolean;

       // nepaci sa mi to v tejto triede
       function GetPosition(address:cardinal):cardinal;      // Ziskanie pozicie v disasm z adresy

       property Bit32: boolean read fBit32;
       property CodeSize: cardinal read fCodeSize;                  // Velkost kodu tejto sekcie v bajtoch
       property IsDisassembled: boolean read fIsDisassembled;
//       property Disassembled: TSynEditStringList read fDisassembled;
       property Disassembled: TTatraDASStringList read fDisassembled;

       property MemOffset: cardinal read fMemOffset;
       property MemSize: cardinal read fMemSize;
       property CodeSectionIndex: integer read fCodeSectionIndex;

       property EntryPointAddress: cardinal read fEntryPointAddress write SetEntryPointAddress;
       property HasEntryPoint: boolean read EntryPointPresent;
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


constructor TCodeSection.Create(InputStream: TStream; bb:boolean; aName: string; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aSectionIndex: integer; aExecFile: TObject);
var position: cardinal;
    i: integer;
begin
  inherited Create(aName, aSectionIndex, aExecFile);

  fTyp:=stCode;
  fBit32:=bb;

  fFileOffset:= aFileOffset;
  fFileSize:= aFileSize;
  fMemOffset:= aMemOffset;
  fMemSize:= aMemSize;



  // Set CodeSize
  if aFileSize <> 0 then
    fCodeSize:=aMemSize
  else
    fCodeSize:=0;

  // Set CodeArray & CodeStream
  SetLength(CodeArray, CodeSize+CodeArrayReserve);
  CodeStream:=TMyMemoryStream.Create;
  CodeStream.SetMemory(Pointer(CodeArray), CodeSize+CodeArrayReserve);
  position:= InputStream.Position;
  InputStream.Position:=aFileOffset;
  CodeStream.CopyFrom(InputStream, Min(InputStream.Size - InputStream.Position, CodeSize)); //+CodeArrayReserve
//  CodeStream.CopyFrom(InputStream, FileSize); //+CodeArrayReserve
  InputStream.Position:=position;
  for i:=aFileSize to CodeSize+CodeArrayReserve-1 do
    CodeArray[i]:=0;  // vynulovanie pola nad byty precitane zo suboru

  // Set DisassemblerMap
  SetLength(DisassemblerMap, CodeSize + CodeArrayReserve);
  for i:=0 to CodeSize + CodeArrayReserve - 1 do
    DisassemblerMap[i]:= 0;
end;



constructor TCodeSection.Create(efile: TObject);
begin
  fExecFile:=efile;
end;



destructor TCodeSection.Destroy;
begin
//  Disassembled.Free;          padne
  inherited;
end;



procedure TCodeSection.SetEntryPointAddress(Address: cardinal);
begin
  fEntryPointAddress:= Address;
  EntryPointPresent:= true;
end;



procedure TCodeSection.ClearDisassembled;
var CodeIndex: integer;
begin
  Disassembled.Clear;
  for CodeIndex:=0 to integer(CodeSize)-1 do
    DisassemblerMap[CodeIndex]:= 0;
  fIsDisassembled:= false;
end;



procedure TCodeSection.ReplaceLines(StartLineIndex, StartAddress, ByteCount: cardinal; NewLines: TStrings);
var
  AfterAddress: cardinal;
  Address: cardinal;
  LineIndex: cardinal;
  RestCodeIndex: cardinal;
  ByteDataChar: string;
  ByteData: Byte;
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
  for RestCodeIndex:=1 to Address - AfterAddress do begin
    ByteData:= Codearray[AfterAddress + RestCodeIndex - 1];
    if ByteData in [$00, $0A, $0D] then
      ByteDataChar:=''
    else
      ByteDataChar:=Chr(ByteData);

    NewLines.Add(
      IntToHex(AfterAddress + RestCodeIndex - 1, 8) +
      ' '+IntToHex(ByteData, 2) +
      '                      ' +
      'byte 0x'+IntToHex(ByteData, 2)+
      ' '''+ ByteDataChar+''''
    );
    DisassemblerMap[AfterAddress + RestCodeIndex - 1]:= dfNone;
  end;

  // Actual replace
  Disassembled.DeleteLines(StartLineIndex, LineIndex - StartLineIndex);
  Disassembled.InsertStrings(StartLineIndex, NewLines);
end;



function TCodeSection.DisassemblePart(Options: TDisassembleOptions): boolean;
var
  BlockIndex: integer;
  CodeIndex: cardinal;
  ReferenceIndex: integer;
  SpaceIndex: integer;
  Line: string;
  NewLines: TStrings;
begin
  // Disassemble
  Decoder:=TDisassembler.Create(CodeArray, DisassemblerMap);
//  Decoder.IsDisassembled:=True;
  Decoder.CAJ.Add(Options.Address);
  Decoder.CAJ.Process(Options.Address + Options.Size);
  if not Decoder.Disassemble then begin
    Decoder.Free;
    Decoder:=nil;
    result:=false;
    Exit;
  end;
  Statistics:=Decoder.Statistics;

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

      Line:=IntToHex(Decoder.Disassembled[CodeIndex].Address, 8)+' '+Decoder.Disassembled[CodeIndex].parsed;
      for SpaceIndex:= Length(Decoder.Disassembled[CodeIndex].parsed) to 23 do
        Line:=Line + ' ';
      if Decoder.Disassembled[CodeIndex].prefix <> '' then
        Line:=Line + Decoder.Disassembled[CodeIndex].prefix + ' ';
      Line:=Line + Decoder.Disassembled[CodeIndex].name + ' ' + Decoder.Disassembled[CodeIndex].operandy;
      Newlines.Add(Line);

      Inc(CodeIndex, Length(Decoder.Disassembled[CodeIndex].parsed) div 2);
    end;

    ReplaceLines(GetPosition(Decoder.Blocks[BlockIndex].Address), Decoder.Blocks[BlockIndex].Address, Decoder.Blocks[BlockIndex].Size, NewLines);
  end;
  NewLines.Free;

  Decoder.Free;
  Decoder:=nil;
  Result:=True;
end;



function TCodeSection.DisassembleAll(Options: TDisassembleOptions):boolean;               // Disasseblovanie, hladanie skokov, spracovanie vystupu...
var
  i,j: integer;

  IndexAddressStr: string;
  IndexAddress: cardinal;
  CallInstrAddress: cardinal;
  FunctionFromModul:string; // nazov importovanej funkcie a modulu

  Position: cardinal; // Position of exported function's entry point in Disassembled

  ReferenceIndex: integer; // Index of instruction references
  SpaceCount:integer; // Number of spaces between instruction's bytecode and asm code
  Line: string;         // retazec, do ktoreho sa pridavaju jednotlive casti intstruckie, nakoniec je zapisany do pola Disassembled (priprava vystupu)


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

  Decoder:=TDisassembler.Create(CodeArray, DisassemblerMap);
//  Decoder.IsDisassembled:= false;

  // Add exported functions' entry points to Decoder.CAJ
  if Exportt <> nil then begin
    Decoder.CAJ.Capacity:=Decoder.CAJ.Capacity + Exportt.functioncount;
    for i:=0 to Exportt.functioncount-1 do
      if SectionIndex = Exportt.functions[i].section then
        Decoder.CAJ.Add(Exportt.functions[i].CodeSectionOffset);
  end;
  // Add program's entry point to Decoder.CAJ if present
  if EntryPointPresent then
    Decoder.CAJ.Add(EntryPointAddress);

  Decoder.CAJ.Process(CodeSize);


  // Disassemble !!!
  if not Decoder.DisassembleAll(bit32) then begin
    CancelDisassemblying;
    Exit;
  end;
  Statistics:=Decoder.Statistics;


  // Vyhladanie EntryPoint-u a jeho zapisanie do pola Decoder.Disassembled
  if EntryPointPresent then begin
    with Decoder.Disassembled[EntryPointAddress] do begin
      Inc(ReferCount, 3);
      SetLength(refer, ReferCount);
      refer[Refercount-3]:= '';
      refer[Refercount-2]:= 'Program Entry point';
      refer[Refercount-1]:= '';
    end;
    Inc(Statistics.References);
    Inc(Statistics.Blanks,2);
  end;

  // Process imported functions (if execfile is PE file)
  if (Import <> nil) and ((ExecFile as TExecutableFile).ExeFormat = ffPE) then begin
    for i:=0 to Length(Decoder.Imported)-1 do begin
      // Priprava parametrov pre GetModulAndFunction
      IndexAddressStr:=Decoder.Disassembled[Decoder.Imported[i]].operandy;
      IndexAddressStr:=Copy(IndexAddressStr, 10, 8);
      IndexAddress:=cardinal(StrToIntDef('$'+IndexAddressStr, 0));
      if IndexAddress = 0 then
        Continue;
      CallInstrAddress:=Decoder.Disassembled[Decoder.Imported[i]].Address + MemOffset;
      FunctionFromModul:=Import.AddFunctionOccurence(IndexAddress, CallInstrAddress);
      if FunctionFromModul <> '' then begin
        Inc(Statistics.References);
        Inc(Statistics.Blanks);
        with Decoder.Disassembled[Decoder.Imported[i]] do begin
          Inc(ReferCount,2);
          SetLength(Refer, ReferCount);
          refer[ReferCount-2]:= '';
          refer[ReferCount-1]:= 'Imported function '''+FunctionFromModul + ''' used';
        end;
      end;
    end;
  end;

  // Process exported functions
  if Exportt <> nil then begin
    for i:=0 to Exportt.FunctionCount-1 do begin
      if Exportt.functions[i].section = SectionIndex then begin
        Inc(Statistics.References);
        Inc(Statistics.Blanks);

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


  // nad tymto sa treba este zamysliet
  Inc(Statistics.Blanks);                             // prazdny riadok na zaciatku
  LinesCount:= Statistics.Instructions                // normalne instrukcie
                    + Statistics.Data                 // data (byte 0x??)
                    + Statistics.References           // referencie na JUMP, CALL, importovane a exportovane funkcie
                    + Statistics.Blanks;              // prazdne riadky

//  fDisassembled:=TSynEditStringList.Create;
  fDisassembled:=TTatraDASStringList.Create;

  fDisassembled.Add('');  // prazdny riadok na zaciatku

  for i:=0 to CodeSize-1 do begin

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

    // Set program's entry point position in Disassembled
    { EntryPointPosition not used anywhere
    if i = EntryPointAddress then
      if EntryPointPresent then
        EntryPointPosition:= Disassembled.Count - 2;
    }

    // Create intruction line of Disassembled from particles
    Line:= (IntToHex(Decoder.Disassembled[i].Address, 8) + ' ' + Decoder.Disassembled[i].parsed);
    SpaceCount:= Nezaporne(c_MaxSpaceCount - Length(Decoder.Disassembled[i].parsed));
    for j:=1 to SpaceCount do
      Line:=Line + ' ';
    if Decoder.Disassembled[i].prefix <> '' then
      Line:=Line + Decoder.Disassembled[i].prefix + ' ';
    Line:=Line + Decoder.Disassembled[i].name + ' ' + Decoder.Disassembled[i].operandy;

    Disassembled.Add(Line);
  end;

  fIsDisassembled:= true;
  MaxAddress:= Statistics.LastItem;
  Decoder.Free;
  Decoder:=nil;
  Result:=True;
end;



function TCodeSection.InSection(MemAddress: cardinal): boolean;
begin
  result:= (MemAddress >= fMemOffset) and (MemAddress < (fMemOffset + fMemSize));
end;



function TCodeSection.SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean;
type TSaveInstruction = (siNone, siAddr, siPar, siDis, siAddr_Par, siPar_Dis, siAddr_Dis, siAll);
var si: TSaveInstruction;
    i,j,counter:cardinal;
    TargetAddress: boolean;
    temp,buf,adresa: cardinal;
    temps: string;
    reladresa: integer;

begin
  inherited SaveToFile(DHF, DAS, SaveOptions);

  if (soProject in SaveOptions) or (soDisassembly in SaveOptions) then begin
  // Zapis do suboru "*.das"
    Writeln(DAS,'------------------------------');
    Writeln(DAS,'Code Section Number: '+IntToStr(SectionIndex));
    Writeln(DAS,'Number of lines: '+IntToStr(LinesCount));

    ProgressMaximum := LinesCount;
    ProgressPosition := 0;
    ProgressName := ProcessText.SavingDAS;
    for i := 1 to LinesCount - 1 do begin
      WriteLn(DAS,Disassembled[i]);
      Inc(ProgressPosition);
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

    DHF.Write(EntryPointPresent, 1);
//    DHF.Write(EntryPointPosition,4);
    DHF.Write(EntryPointAddress, 4);
// Dynamicke data
    DHF.Write(InstructionLineCount, 4);
    DHF.Write(DataLineCount, 4);
    DHF.Write(ReferenceLineCount, 4);
    DHF.Write(BlankLineCount, 4);
    DHF.Write(Statistics,sizeof(TStatistics));
    DHF.Write(MaxAddress, 4);
    DHF.Write(IsDisassembled, 1);
    DHF.Write(LinesCount, 4);
    DHF.Write(InstructionsCount, 4);
// Polia
    DHF.Write(CodeArray[0], CodeSize);
    DHF.Write(DisassemblerMap[0], CodeSize);
  end;

  if not (soProject in SaveOptions) and not (soDisassembly in SaveOptions) and not (soNASM in SaveOptions) then begin
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

///    Ctrls.ProgressFunction(0,LinesCount,ProcessText.SavingDAS + IntToStr(SectionNumber));
    counter:=1000;
    for i:=1 to LinesCount-1 do begin
      if i >= counter then begin
{///
        if not Ctrls.ProgressFunction(i,LinesCount,'') then begin
          CloseFile(DAS);
          Ctrls.ProgressFunction(0,0,'');
          Exit;
        end;
}
        inc(counter,1000);
      end;

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
    Append(DAS);
    Writeln(DAS,'; ------------------------------');
    Writeln(DAS,'; Code Section Number: '+IntToStr(SectionIndex));
    Writeln(DAS);
    case bit32 of
      true: Writeln(DAS,'BITS 32');
      false: Writeln(DAS,'BITS 16');
    end;
    Writeln(DAS);
//    Writeln(DAS,'Number of lines: '+IntToStr(PocetDisassembled));       nema zmysel (pocet naozaj zapisanych riadkov je stale iny

///    Ctrls.ProgressFunction(0,LinesCount,ProcessText.SavingDAS + IntToStr(SectionNumber));
    counter:=1000;
    for i:=1 to LinesCount-1 do begin
      if i >= counter then begin
{//
        if not Ctrls.ProgressFunction(i,LinesCount,'') then begin
          CloseFile(DAS);
          Ctrls.ProgressFunction(0,0,'');
          Exit;
        end;
}
        inc(counter,1000);
      end;
      if Disassembled[i]='' then begin WriteLn; Continue; end;
      case Disassembled[i][2] of
        'u': begin
               writeln(DAS,';'+Disassembled[i]);
               TargetAddress:=true;
             end;
        'a': begin
               writeln(DAS,';'+Disassembled[i]);
               TargetAddress:=true;
             end;
        'o': begin
               writeln(DAS,';'+Disassembled[i]);
               TargetAddress:=true;
             end;
        'x': writeln(DAS,';'+Disassembled[i]);
        'm': writeln(DAS,';'+Disassembled[i]);
        'r': writeln(DAS,';'+Disassembled[i]);
      else begin
// Referencia JUMP, CALL
        if TargetAddress then begin
          Writeln(DAS,'_0x'+LeftStr(Disassembled[i],8)+':');
          TargetAddress:=false;
        end;
        temps:=Copy(Disassembled[i],34,100);

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

  DHF.Read(EntryPointPresent, 1);
//  DHF.Read(EntryPointPosition,4);
  DHF.Read(fEntryPointAddress,4);
  // Dynamicke data
  DHF.Read(InstructionLineCount, 4);
  DHF.Read(DataLineCount, 4);
  DHF.Read(ReferenceLineCount, 4);
  DHF.Read(BlankLineCount, 4);
  DHF.Read(Statistics, SizeOf(TStatistics));
  DHF.Read(MaxAddress, 4);
  DHF.Read(fIsDisassembled, 1);
  DHF.Read(LinesCount, 4);
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

//  fDisassembled:=TSynEditStringList.Create;
  fDisassembled:= TTatraDASStringList.Create;

  fDisassembled.Add('');
  ProgressPosition:=0;
  while not EOF(DAS) do begin
    Inc(ProgressPosition);
    ReadLn(DAS, line);
    Disassembled.Add(line);
  end;
  ProgressPosition:=0;

  result:=true;
end;



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



function GetLineAddress(line: string):cardinal;
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



function TCodeSection.GetPosition(address:cardinal):cardinal;      // Ziskanie pozicie v disasm z adresy
var tip,dk:cardinal;
    riadky:TStrings;
begin
  riadky:=Disassembled;
  tip:=Min(Round(Address/(fMemSize/LinesCount)), Disassembled.Count);
  result:=tip;
  while (GetLineAddress(riadky[result])=$FFFFFFFF) and (result>0) do begin
    dec(result);
  end;
  if result = 0 then
    while (GetLineAddress(riadky[result])=$FFFFFFFF) do begin
      inc(result);
    end;


  if address > GetLineAddress(riadky[result]) then begin
    dk:=result;
    result:=tip;
    while GetLineAddress(riadky[result])=$FFFFFFFF do begin
      inc(result);
    end;
    if address < GetLineAddress(riadky[result]) then begin
      result:=dk;
      exit;
    end;
  end;

  if (address < GetLineAddress(riadky[result])) then
    while (Address<GetLineAddress(riadky[result])) or (GetLineAddress(riadky[result])=$FFFFFFFF) do dec(result)
  else
    while (Address>GetLineAddress(riadky[result])) or (GetLineAddress(riadky[result])=$FFFFFFFF) do inc(result);
end;

end.
