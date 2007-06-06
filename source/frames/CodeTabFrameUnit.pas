unit CodeTabFrameUnit;

interface

uses
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Menus,
  ComCtrls,
  Contnrs,
  Classes,
  SysUtils,
  Math,
  INIFiles,
  strutils,

  SynEdit,
  SynEditTypes,
  SynEditTextBuffer,
  TatraDASHighlighter,

  TabFrameTemplateUnit,
  GotoAddressFormUnit,
  AdvancedChangingToDataFormUnit,
  AdvancedDisassembleFormUnit,
  InsertCommentFormUnit,
  StringRes,
  procmat,
  disassembler,
  CodeSectionUnit,
  SectionUnit;

//type
//  TSeekOrigin = (soFromBeginning, so


type
  TCodeTabFrame = class(TTabFrameTemplate)
    GotoEntryPointButton: TButton;
    GotoAddressButton: TButton;
    Bevel1: TBevel;
    FollowButton: TButton;
    ReturnButton: TButton;
    Bevel2: TBevel;
    LineLabel: TLabel;
    LineDataLabel: TLabel;
    CodePopupMenu: TPopupMenu;
    oggleBookmarks1: TMenuItem;
    Bookmark11: TMenuItem;
    Bookmark21: TMenuItem;
    Bookmark31: TMenuItem;
    Bookmark41: TMenuItem;
    Bookmark51: TMenuItem;
    Bookmark01: TMenuItem;
    Bookmark61: TMenuItem;
    Bookmark71: TMenuItem;
    Bookmark81: TMenuItem;
    Bookmark91: TMenuItem;
    GotoBookmarks2: TMenuItem;
    N5: TMenuItem;
    Changetounsigneddata3: TMenuItem;
    Changetosigneddata2: TMenuItem;
    Changetounsigneddata4: TMenuItem;
    Changetostringdata2: TMenuItem;
    Advancedchangetodata2: TMenuItem;
    N6: TMenuItem;
    Dis2: TMenuItem;
    Advanceddisassemble2: TMenuItem;
    N7: TMenuItem;
    Insert2: TMenuItem;
    Comment2: TMenuItem;
    Emptyline2: TMenuItem;
    RemoveLineMenuItem: TMenuItem;
    Bookmark02: TMenuItem;
    Bookmark12: TMenuItem;
    Bookmark22: TMenuItem;
    Bookmark32: TMenuItem;
    Bookmark42: TMenuItem;
    Bookmark52: TMenuItem;
    Bookmark62: TMenuItem;
    Bookmark72: TMenuItem;
    Bookmark82: TMenuItem;
    Bookmark92: TMenuItem;
    Pascal1: TMenuItem;
    C1: TMenuItem;
    PascalUnicode1: TMenuItem;
    CUnicode1: TMenuItem;
    SINGLE32bits1: TMenuItem;
    DOUBLE64bits1: TMenuItem;
    EXTENDED80bits1: TMenuItem;
    BYTE8bits1: TMenuItem;
    WORD16bits1: TMenuItem;
    DWORD32bits1: TMenuItem;
    QWORD64bits1: TMenuItem;
    BYTE8bits2: TMenuItem;
    WORD16bits2: TMenuItem;
    DWORD32bits2: TMenuItem;
    QWORD64bits2: TMenuItem;
  private
    fSection: TCodeSection;
    fOnClick: TNotifyEvent;
    fOnKeyDown: TKeyEvent;
  public
    plocha: TSynEdit;

    function NewLinesCount(Options:TDataChangeOptions; Address, Index: cardinal):integer;

  public
    CursorLine: cardinal;

    constructor Create(AOwner: TComponent; ASection: TSection); overload; override;
    destructor Destroy; override;
// Udalosti:
  published
    procedure PlochaStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure GotoEntryPointButtonClick(Sender: TObject);
    procedure GotoAddressButtonClick(Sender: TObject);
    procedure FollowButtonClick(Sender: TObject);
    procedure ReturnButtonClick(Sender: TObject);
    procedure ToggleBookmarkClick(Sender: TObject);
    procedure GotoBookmarkClick(Sender: TObject);
    procedure PlochaMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DisassembleClick(Sender: TObject);

    procedure CodePopupMenuPopup(Sender: TObject);

    procedure InsertCommentClick(Sender: TObject);
    procedure InsertEmptyLineClick(Sender: TObject);
    procedure RemoveLineClick(Sender: TObject);

    procedure Advancedchanging1Click(Sender: TObject);
    procedure AdvancedDisassemble1Click(Sender: TObject);
    procedure ChangeStringClick(Sender: TObject);
    procedure NormalDisassembleClick(Sender: TObject);
    procedure ChangeUDataClick(Sender: TObject);
    procedure ChangeSDataClick(Sender: TObject);
    procedure ChangeFDataClick(Sender: TObject);

  public

    JumpPosition: cardinal; // Pozicia mozneho skoku
    Zasobnik: TStack; // zosobnik vykonanych skokov

    procedure ChangeToData(Options: TDataChangeOptions);
    procedure ChangeToStringData(Options: TDataChangeOptions);

    procedure Disassemble(Options: TDisassembleFormOptions);
    procedure ReplaceLines(NewLines: TStrings; index: Cardinal; bytes: cardinal; address:cardinal);



// Ostatne:
    procedure Init;

    function GetPosition(Address:cardinal):cardinal;
    procedure GotoPosition(Offset: LongInt; Origin: TSeekOrigin); // Offset zacina od 0

    procedure FindString(SearchText:string; options: TFindOptions);
    procedure Translate(ini:TMemINIFile; error:string);

  protected
    function GetSection: TSection; override;

  end;

var
  CodeTabFrame: TCodeTabFrame;

implementation

uses ExecFileUnit,ExportSectionUnit,PEFileUnit;

{
constructor TCodeTabFrame.Create(MainFile: TFileEx; Number: integer);
var I:cardinal;
    MI: TMenuItem;
begin
  inherited;
  fSection:=MainFile.ExecFile.Sections[Number] as TCodeSection;

// Vytvorenie Textovej plochy SynEdit
  plocha:=TSynEdit.Create(Panel);
  plocha.Parent:=Panel;
  plocha.SetSubComponent(true);
  plocha.TabOrder:=0;
  plocha.Width:=Panel.Width - 124;
  plocha.Height:=Panel.Height - 16;
  plocha.Left:=8;
  plocha.Top:=8;
  plocha.Anchors:=[akLeft,akRight,akTop,akBottom];
  plocha.ReadOnly:=true;
  plocha.Font.Name:='Courier New';
  plocha.Font.Size:=8;
  plocha.HideSelection:=true;
  plocha.ScrollBars:=ssVertical;
//  Nova verzia syeditu neberie
//  plocha.MaxLeftChar:=80;

//  plocha.Options:=plocha.Options + [eoScrollPastEol];

  plocha.Highlighter:=TSynTatraDASSyn.Create(plocha);
  plocha.OnStatusChange:=PlochaStatusChange;
  plocha.OnMouseDown:=PlochaMouseDown;


  GotoEntrypointButton.Enabled:=fSection.EntryPointPresent;
  Plocha.PopupMenu:=CodePopupMenu;
end;
}

constructor TCodeTabFrame.Create(AOwner: TComponent; ASection: TSection);

  function CountCodeSize(): cardinal;
  var i: integer;
  begin
    result:=0;
    for i:=0 to fSection.CodeSize - 1 do
      if (fSection.DisassemblerMap[i] and dfPart) <> 0 then
        Inc(result);
  end;

  function CountFunctions: integer;
  var i: integer;
  begin
    result:=0;
    i:=0;
    while i < fSection.Disassembled.Count do begin
      if LeftStr(fSection.Disassembled[i],9) = 'Call from' then begin
        Inc(result);
        while i < fSection.Disassembled.Count do begin
          if Copy(fSection.Disassembled[i],6,4) <> 'from' then
            break
          else
            Inc(i);
        end;
      end;
      Inc(i);
    end;
  end;    

//var MI: TMenuItem;
begin
  inherited Create(AOwner);
  fSection:=aSection as TCodeSection;
  Zasobnik:=TStack.Create;
  Caption:='Code section #' + IntToStr(fSection.CodeSectionIndex);

// Vytvorenie Textovej plochy SynEdit
  plocha:=TSynEdit.Create(Panel);
  plocha.Parent:=Panel;
  plocha.SetSubComponent(true);
  plocha.TabOrder:=0;
  plocha.Width:=Panel.Width - 124;
  plocha.Height:=Panel.Height - 16;
  plocha.Left:=8;
  plocha.Top:=8;
  plocha.Anchors:=[akLeft,akRight,akTop,akBottom];
  plocha.ReadOnly:=true;
  plocha.Font.Name:='Courier New';
  plocha.Font.Size:=8;
  plocha.HideSelection:=true;
  plocha.ScrollBars:=ssVertical;
//  Nova verzia syeditu neberie
//  plocha.MaxLeftChar:=80;

//  plocha.Options:=plocha.Options + [eoScrollPastEol];

  plocha.Highlighter:=TSynTatraDASSyn.Create(plocha);
  plocha.OnStatusChange:=PlochaStatusChange;
  plocha.OnMouseDown:=PlochaMouseDown;
  plocha.HookTextBuffer(fSection.Disassembled,plocha.UndoList,plocha.RedoList);

  GotoEntrypointButton.Enabled:=fSection.HasEntryPoint;
  Plocha.PopupMenu:=CodePopupMenu;

    // temporary
    // count disassembled code size
//    showmessage('Disassembled code size: ' + IntToStr(CountCodeSize) + '         Functions count: ' + IntToStr(CountFunctions));
//    showmessage('Bytes per function: ' + IntToStr(CountCodeSize div CountFunctions));
    // end temporary

end;



procedure ChangeToUnSignedDataClick(Sender: TObject);
begin
    ;;
end;

function TCodeTabFrame.NewLinesCount(Options:TDataChangeOptions; Address, Index: cardinal):integer;
var DataTypeSize: byte;
    i:cardinal;
begin
  DataTypeSize:=DataTypeSizes[Options.datatype];
  case Options.option of
    dcItems: result:=Min(Options.value,(fSection.MemSize - Address) div DataTypeSize);
    dcBytes: result:=Min(Options.value+Address,fSection.MemSize) div DataTypeSize; //dcBytes: result:= Options.value shr Options.datatype
    dcMaxAddress: result:=(Options.value - Address) div DataTypeSize;
    dcEndSection: result:=(fSection.MemSize - Address) div  DataTypeSize;
    dcCode: begin
      i:=Index;
      for i:=Index to plocha.Lines.Count do begin
        if (GetLineType(plocha.lines[i]) <> ltInstruction) then continue;
        if plocha.lines[i][34] <> UpCase(plocha.lines[i][34]) then continue;
        result:=(GetLineAddress(plocha.lines[i]) - Address) div DataTypeSize;
        break;
      end;
    end;
  end;
end;

procedure TCodeTabFrame.ChangeToData(Options: TDataChangeOptions);
var i,j,itemsize,count,address,Index:cardinal;
    data: Int64;
    TryStr,line,byteChar:string;
    NewLines: TStrings;
    temp8:byte;
    data8: shortint;
    data16: smallint;
    data32: integer;
    data64: Int64;
    floatdata32: single;
    floatdata64: double;
    floatdata80: extended;
    znamienko: string;
begin
  itemsize:=DataTypeSizes[Options.datatype];
  Index:=plocha.CaretY-1;
  while GetLineType(plocha.lines[Index]) <> ltInstruction do inc(Index);
  address:=GetLineAddress(plocha.Lines.Strings[Index]);
  count:=NewLinesCount(Options,address,Index);
  if count = 0 then exit;

  for i:=address to address+count*itemsize-1 do
    fSection.DisassemblerMap[i]:=byte(fSection.DisassemblerMap[i] and (maxByte-dfInstruction-dfPart));

  fSection.CodeStream.Position:=address;
  NewLines:=TStringList.Create;
  NewLines.Capacity:=count;
{$R-}
  case Options.signed of
    false:
      case Options.datatype of
        dtByte,dtWord,dtDword,dtQword: begin
          for i:=0 to count-1 do begin
            line:='';
            data:=0;
            fSection.CodeStream.Read(data,itemSize);
            for j:=0 to itemsize-1 do line:=line + IntToHex(fSection.CodeArray[Address+i*itemsize+j],2);
            line:=IntToHex(address+i*itemsize,8)+' '+ line;
            for j:=length(line)+1 to 33 do line:=line+' ';
            case Options.datatype of
              dtByte: begin
                        if (byte(data)=$00) or (byte(data)=$0A) or (byte(data)=$0D) then byteChar:=''
                        else byteChar:=Chr(byte(data));
                        line:=line + 'byte 0x' + IntToHex(data,2)+' '''+byteChar+'''';
                      end;
              dtWord: line:=line + 'word 0x' + IntToHex(data,4);
              dtDword: line:=line + 'dword 0x' + IntToHex(data,8);
              dtQword: line:=line + 'qword 0x' + IntToHex(data,16);
            end;
            NewLines.Add(line);
          end;
        end;
        dtSingle: begin
          for i:=0 to count-1 do begin
            fSection.CodeStream.Read(floatdata32,4);
            line:='';
            for j:=0 to 3 do line:=line + IntToHex(fSection.CodeArray[Address+i*4+j],2);
            line:=IntToHex(address+i*4,8)+' '+ line;
            for j:=length(line)+1 to 33 do line:=line+' ';
            if MyIsNan(floatdata32) then TryStr:='NAN' else TryStr:=FirstCommaToPoint(FloatToStrF(floatdata32,ffGeneral,7,4));
            NewLines.Add(line + 'single ' +TryStr);
          end;
        end;
        dtDouble: begin
          for i:=0 to count-1 do begin
            fSection.CodeStream.Read(floatdata64,8);
            line:='';
            for j:=0 to 7 do line:=line + IntToHex(fSection.CodeArray[Address+i*8+j],2);
            line:=IntToHex(address+i*8,8)+' '+ line;
            for j:=length(line)+1 to 33 do line:=line+' ';
            if MyIsNan(floatdata64) then TryStr:='NAN' else TryStr:=FirstCommaToPoint(FloatToStrF(floatdata64,ffGeneral,15,4));
            NewLines.Add(line + 'double ' + TryStr);
          end;
        end;
        dtDoubleEx: begin
          for i:=0 to count-1 do begin
            fSection.CodeStream.Read(floatdata80,10);
            line:='';
            for j:=0 to 9 do line:=line + IntToHex(fSection.CodeArray[Address+i*10+j],2);
            line:=IntToHex(address+i*10,8)+' '+ line;
            for j:=length(line)+1 to 33 do line:=line+' ';
            TryStr:='extended ';
            try
              TryStr:=TryStr + FirstCommaToPoint(FloatToStr(floatdata80));
            except
              TryStr:='unsupported extended real number';
            end;
            NewLines.Add(line + TryStr);
          end;
        end;
      end;

    true: begin
      case Options.datatype of
        // BYTE
        dtByte:
          for i:=0 to count-1 do begin
            line:='';
            fSection.CodeStream.Read(data8,1);
            temp8:=byte(data8);
            if Abs(data8) <> data8 then begin
              znamienko:='-';
              data8:=Abs(Data8);
            end
            else znamienko:='';
            for j:=0 to 0 do line:=line + IntToHex(fSection.CodeArray[Address+i+j],2);
            line:=IntToHex(address+i,8)+' '+ line;
            for j:=length(line)+1 to 33 do line:=line+' ';
            if (temp8=$00) or (temp8=$0A) or (temp8=$0D) then byteChar:=''
            else byteChar:=Chr(temp8);
            line:=line + 'byte ' + znamienko + '0x'+IntToHex(data8,2)+' '''+byteChar+'''';
            NewLines.Add(line);
          end;
         // WORD
        dtWord:
          for i:=0 to count-1 do begin
            fSection.CodeStream.Read(data16,2);
            if Abs(data16) <> data16 then begin
              znamienko:='-';
              data16:=Abs(data16);
            end
            else znamienko:='';
            line:='';
            for j:=0 to 1 do line:=line + IntToHex(fSection.CodeArray[Address+i*2+j],2);
            line:=IntToHex(address+i*2,8)+' '+ line;
            for j:=length(line)+1 to 33 do line:=line+' ';
            NewLines.Add(line + 'word ' + znamienko +'0x'+ IntToHex(data16,4));
          end;
          // DWORD
        dtDword:
          for i:=0 to count-1 do begin
            fSection.CodeStream.Read(data32,4);
            if Abs(data32) <> data32 then begin
              znamienko:='-';
              data32:=Abs(data32);
            end
            else znamienko:='';
            line:='';
            for j:=0 to 3 do line:=line + IntToHex(fSection.CodeArray[Address+i*4+j],2);
            line:=IntToHex(address+i*4,8)+' '+ line;
            for j:=length(line)+1 to 33 do line:=line+' ';
            NewLines.Add(line + 'dword ' + znamienko +'0x'+ IntToHex(data32,8));
          end;
          // QWORD
        dtQword:
          for i:=0 to count-1 do begin
            fSection.CodeStream.Read(data64,8);
            if Abs(data64) <> data64 then begin
              znamienko:='-';
              data64:=Abs(data64);
            end
            else znamienko:='';
            line:='';
            for j:=0 to 7 do line:=line + IntToHex(fSection.CodeArray[Address+i*8+j],2);
            line:=IntToHex(address+i*8,8)+' '+ line;
            for j:=length(line)+1 to 33 do line:=line+' ';
            NewLines.Add(line + 'qword ' + znamienko +'0x'+ IntToHex(data64,16));
          end;
      end;
  end;end;
{$IFDEF RangeChecking}
  {$R+}
{$ENDIF}
  ReplaceLines(NewLines,Index,itemsize*count,address);
end;


procedure TCodeTabFrame.ChangeToStringData(Options: TDataChangeOptions);
var i,index,address:cardinal;
    line:string;
    WLine: WideString;
    StrLength8: byte;
    StrLength16: word;
    NewLines: TStrings;
    Bytes: cardinal;
    sign: string;
    Char16: WideChar;
begin
// zistime index riadku s adresou
  index:=plocha.CaretY-1;
  while GetLineType(plocha.lines[index]) <> ltInstruction do inc(index);
// adresa tohto riadku
  address:=GetLineAddress(plocha.Lines.Strings[index]);

  fSection.CodeStream.Position:=address;
  NewLines:=TStringList.Create;
  Bytes:=0; StrLength8:=0; StrLength16:=0;
    case Options.datatype of
      dtPascalStr: begin
        fSection.CodeStream.Read(StrLength8,1);
        SetLength(line,StrLength8);
        fSection.CodeStream.Read(line[1],StrLength8);
        wLine:=line;
        sign:='p';
        Bytes:=Strlength8+1;
      end;
      dtCStr: begin
        ReadStringFromStream(fSection.CodeStream,fSection.CodeStream.Position,line);
        StrLength8:=Length(line);
        wLine:=line;
        sign:='c';
        Bytes:=Strlength8+1;
      end;
      dtPascalUniCodeStr: begin
        fSection.CodeStream.Read(StrLength16,2);
        SetLength(WLine,StrLength16);
        fSection.CodeStream.Read(Wline[1],StrLength16*2);
        sign:='pu';
        Bytes:=Strlength16+2;
      end;
      dtCUniCodeStr: begin //nedokoncene
        StrLength8:=0;
        WLine:='';
        fSection.CodeStream.Read(Char16,2);
        while Char16 = #0 do begin
          inc(StrLength8);
          WLine:=WLine+Char16;
          fSection.CodeStream.Read(Char16,2);
        end;
        sign:='cu';
        Bytes:=Strlength8*2+2;
      end;
    end;
//  inc(Bytes,Strlength+1);
  NewLines.Add(IntToHex(address,8) + ' bytes: '+IntToHex(Bytes,8)+'(hex)'+'    '+sign+'string '''+ wLine + '''');
  for i:=address to address+Bytes-1 do fSection.DisassemblerMap[i]:=byte(fSection.DisassemblerMap[i] and (maxByte-dfInstruction-dfPart));
  ReplaceLines(NewLines,index,bytes,address);
end;

procedure TCodeTabFrame.Disassemble(Options: TDisassembleFormOptions);
var index: cardinal;
    Opt: TDisassembleOptions;
begin
// zistime index riadku s adresou
  index:=plocha.CaretY-1;
  while GetLineType(plocha.lines[index]) <> ltInstruction do inc(index);
// adresa tohto riadku
  Opt.address:=GetLineAddress(plocha.Lines.Strings[index]);
  Opt.typ:=doSize;
  case Options.option of
    dcItems:  Opt.size:=Options.value;
    dcBytes: begin Opt.size:=Options.value; Opt.typ:=doCount; end;
    dcMaxAddress: Opt.size:= Max(Opt.address - Options.value,0);
    dcCode: Opt.size:= fSection.CodeSize;
    dcNormal: Opt.size:=fSection.CodeSize-Opt.address;
  end;
  Opt.bit32:= Options.bit32;
  fSection.DisassemblePart(Opt);
  GotoPosition(GetPosition(Opt.address), soBeginning);
///  fSection.Ctrls.ProgressFunction(0,0,'');
end;

procedure TCodeTabFrame.ReplaceLines(NewLines: TStrings; index: Cardinal; bytes: cardinal; address:cardinal);
var
  NewStrings: TStrings;
  i,j: cardinal;
  nlIndex: cardinal; // NewLines index
  lastaddress,
  lastbytes: cardinal;
  nonInstruction: boolean;
  startIndex: cardinal;
  curaddress: cardinal;
  byteChar: string;
begin
// Jednoduche zistenie kolko riadkov nahradime, bezadresne riadky sa vyhodia
  startIndex:=index;
  curaddress:=address;
  while address+bytes > curaddress do begin
    if GetLineType(plocha.Lines[index]) = ltInstruction then begin
      inc(curaddress,GetLineBytes(plocha.Lines[index]));
    end;
    inc(index);
  end;



{
  NewStrings:=TStringList.Create;
  repeat
    inc(j); inc(i);
    NewStrings.Add(NewLines[j]);
  until GetLineType(NewLines[j]) = ltInstruction;
  na:=GetLineBytes(NewLines[j]);

  while

  nestaddress

}
// Stale je to chybne, asi by to chcelo cele prepisat
{
  startIndex:=index;
  lastaddress:=address;
  nlIndex:=0;
// Pridame pomocny riadok
  NewLines.Add(IntToHex(GetLineAddress(NewLines[NewLines.Count-1])+GetLineBytes(NewLines[NewLines.Count-1]),8));
  repeat
    case GetLineType(plocha.Lines[index]) of
      ltInstruction: begin
        lastaddress:=GetLineAddress(plocha.Lines[index]);
        lastbytes:= GetLineBytes(plocha.Lines[index]);
        if lastaddress < address + bytes then begin
          inc(index);
          continue;
        end
         else break;
      end;
      ltJumpRef: begin
        nonInstruction:=true;
      end;
      ltCallRef: begin
        nonInstruction:=true;
      end;
      ltImportRef: begin
        nonInstruction:=true;
      end;
      ltExportRef: begin
        nonInstruction:=true;
      end;
      ltEntryPointRef: begin
        nonInstruction:=true;
      end;
      ltComment: nonInstruction:=true;
      ltEmpty: begin
        nonInstruction:=true;
      end;
    end;
    if nonInstruction then begin
//      inc(NonInstrCount);
      repeat
        if GetLineType(NewLines[nlIndex])=ltInstruction then
          if lastAddress + lastBytes <= GetLineAddress(NewLines[nlIndex]) then begin
            NewLines.Insert(nlIndex,plocha.Lines[index]);
            inc(nlIndex);
            break;
          end;
        inc(nlIndex);
      until false;

    end;
    inc(index);
    nonInstruction:=false;
  until false;

// Odstranime pomocny riadok
  NewLines.Delete(NewLines.Count-1);
}

// Konverzia zvysnych bytov na datovy typ "byte"
  for i:=1 to curaddress - (address + bytes) do begin
    if (fSection.codearray[address+bytes+i-1]=$00) or (fSection.codearray[address+bytes+i-1]=$0A) or (fSection.codearray[address+bytes+i-1]=$0D)
      then  byteChar:=''
    else byteChar:=Chr(fSection.codearray[address+bytes+i-1]);
    NewLines.Add(IntToHex(address + bytes + i - 1,8)+
      ' '+IntToHex(fSection.codearray[address+bytes+i-1],2)+
      '                      '+
      'byte 0x'+IntToHex(fSection.codearray[address+bytes+i-1],2)+
      ' '''+ byteChar+'''');
    fSection.DisassemblerMap[curaddress-i]:=0;
  end;

// novy sposob - testovaci:
  (plocha.Lines as TSynEditStringList).DeleteLines(startindex,Index-startindex);
  (plocha.Lines as TSynEditStringList).InsertStrings(startIndex,NewLines);
  PlochaStatusChange(plocha,[scModified]);
//  (fSection.ExecFile as TExecutableFile).Modified:=true;
  exit;



// Nahradenie a pridanie (alebo ubratie) riadkov v "plocha.lines" z "NewLines"
  j:=0;
  // Novych riadkov je menej ako nahradzovanych
  if NewLines.Count < Index - startIndex then begin
    plocha.Lines.BeginUpdate;
    for i:=startIndex to startIndex + NewLines.Count - 1 do begin
      plocha.Lines[i]:= NewLines[j];
      inc(j);
    end;
    // Pomoze to ? (bude to tak rychlejsie ?)
    for i:=startIndex + NewLines.Count to index-1 do begin
      plocha.Lines[i]:= '';
    end;
    //
    plocha.Lines.EndUpdate;
    for i:=startIndex + NewLines.Count to index-1 do begin
      plocha.Lines.Delete(startIndex+NewLines.Count);
    end;
  end
  // Novych riadkov je rovno-viac ako nahradzovanych
  else begin
    plocha.Lines.BeginUpdate;
    for i:=startIndex to index-1 do begin
      plocha.Lines[i]:= NewLines[j];
      inc(j);
    end;
    plocha.Lines.EndUpdate;
    for i:=j to NewLines.Count-1 do begin
      plocha.Lines.Insert(index,NewLines[i]);
      inc(index);
    end;
  end;
end;

procedure TCodeTabFrame.InsertCommentClick(Sender: TObject);
begin
  if InsertCommentForm.ShowModal = mrOK then begin
    plocha.Lines.Insert(plocha.CaretY - 1, '; ' + InsertCommentForm.InsertCommentEdit.Text);
    PlochaStatusChange(self,[scCaretY]);
  end;
end;

procedure TCodeTabFrame.InsertEmptyLineClick(Sender: TObject);
begin
  plocha.Lines.Insert(plocha.CaretY - 1, '');
  PlochaStatusChange(self, [scCaretY, scModified]);
end;

procedure TCodeTabFrame.RemoveLineClick(Sender: TObject);
begin
  plocha.Lines.Delete(plocha.CaretY - 1);
  PlochaStatusChange(self,[scCaretY,scModified]);
end;

procedure TCodeTabFrame.DisassembleClick(Sender: TObject);
begin
  ;;;
end;

procedure TCodeTabFrame.PlochaMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  plocha.CaretXY:=TBufferCoord(plocha.PixelsToRowColumn(X,Y));
end;

procedure TCodeTabFrame.GotoBookmarkClick(Sender: TObject);
begin
  Plocha.GotoBookMark((Sender as TMenuItem).MenuIndex);
end;

procedure TCodeTabFrame.ToggleBookMarkClick(Sender: TObject);
var X,Y,BookNum: integer;
begin
  BookNum:=(Sender as TMenuItem).MenuIndex;
  plocha.GetBookMark(BookNum,X,Y);
  if plocha.CaretY = Y then begin
    plocha.ClearBookMark(BookNum);
//    MainForm.ToggleBookmarks.Items[BookNum].Checked:=true;
//    MainForm.GotoBookmarks.Items[BookNum].Checked:=true;
    (Sender as TMenuItem).Checked:=false;
    CodePopupMenu.Items[1].Items[BookNum].Checked:=false;
  end
  else begin
    Plocha.SetBookMark(BookNum,plocha.CaretX,plocha.CaretY);
//    MainForm.ToggleBookmarks.Items[BookNum].Checked:=true;
//    MainForm.GotoBookmarks.Items[BookNum].Checked:=true;
    (Sender as TMenuItem).Checked:=true;
    CodePopupMenu.Items[1].Items[BookNum].Checked:=true;
  end;
end;

procedure TCodeTabFrame.CodePopupMenuPopup(Sender: TObject);
begin
  if GetLineType(plocha.Lines.Strings[plocha.CaretY]) = ltComment then begin
    CodePopupMenu.Items[4].Enabled:=true;
//    MainForm.RemoveComment.Enabled:=true;
  end
  else begin
    CodePopupMenu.Items[4].Enabled:=false;
//    MainForm.RemoveComment.Enabled:=false;
  end;
end;

destructor TCodeTabFrame.Destroy;
begin
  plocha.Free;
  GotoEntrypointButton.Free;
  GotoAddressButton.Free;
  FollowButton.Free;
  ReturnButton.Free;
{
  BytesPerInstructionLabel.Free;
  BytesPerInstructionDataLabel.Free;
  InstructionCountLabel.Free;
  InstructionCountDataLabel.Free;
}
  LineLabel.Free;
  LineDataLabel.Free;
  Bevel1.Free;
  Bevel2.Free;

  inherited;
end;

procedure TCodeTabFrame.GotoEntrypointButtonClick(Sender: TObject);     // Premiestnenie na Entrypoint
begin
  GotoPosition(GetPosition(fSection.EntryPointAddress)-2, soBeginning);
  plocha.SetFocus;
end;

procedure TCodeTabFrame.GotoAddressButtonClick(Sender: TObject);    // Premiestnenie na zadanu adresu
begin
  GotoAddressForm.GotoAddressEdit.Text:='';
  GotoAddressForm.MaxAddress:=fSection.MaxAddress;
  if GotoAddressForm.ShowModal = mrOK then GotoPosition(Getposition(GotoAddressForm.address), soBeginning);
  plocha.SetFocus;
end;



function TCodeTabFrame.GetPosition(Address: cardinal): cardinal;      // Ziskanie pozicie v disasm z adresy
var
//  AddressEstimate: cardinal; // position estimate
  tip: cardinal;
  dk: cardinal;
  riadky: TStrings;
begin
  if (Address < Section.MemOffset) or (Address > Section.MemOffset + Section.MemSize - 1) then begin
    result:=cardinal(-1);
    Exit;
  end;

  // Docasne, nutne prerobit vo final
  Address:= Address - Section.MemOffset;
  //*********

  riadky:=plocha.Lines;
  result:=Min(Round(Address/(fSection.MemSize/fSection.LinesCount)), Plocha.Lines.Count);
  result:=tip;
  while (GetLineAddress(riadky[result])=$FFFFFFFF) and (result>0) do begin
    Dec(result);
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



procedure TCodeTabFrame.PlochaStatusChange(Sender: TObject; Changes: TSynStatusChanges);
var adresa:cardinal;
begin
  if scCaretY in Changes then begin
    FollowButton.Enabled:=false;
//    MainForm.RemoveComment.Enabled:=false;
    RemoveLineMenuItem.Enabled:=false;
    case GetLineType(Plocha.Lines.Strings[plocha.CaretY - 1]) of
      ltInstruction: begin
        if GetTargetAddress(Plocha.Lines.Strings[plocha.CaretY - 1], adresa) then begin
          if adresa < fSection.CodeSize then begin // osetrenie jumpov za koniec sekcie
            FollowButton.Enabled:=true;
            JumpPosition:=GetPosition(adresa);
          end;
        end;
      end;
      ltComment: begin
//        MainForm.RemoveComment.Enabled:=true;
        RemoveLineMenuItem.Enabled:=true;
      end;
      ltEmpty: begin
//        MainForm.RemoveComment.Enabled:=true;
        RemoveLineMenuItem.Enabled:=true;
      end;
      ltJumpRef, ltCallRef, ltLoopRef: begin
        FollowButton.Enabled:=true;
        JumpPosition:=GetPosition(StrToInt64('$'+Copy(Plocha.Lines.Strings[plocha.CaretY - 1],13,8)));
      end;
    end;
    LineDataLabel.Caption:=IntToStr(plocha.CaretY ); 
  end;
  if scModified in Changes then begin
    fSection.LinesCount:=Plocha.Lines.Count;
  end;
end;

//=============================================================================
//=============================================================================


procedure TCodeTabFrame.Init;
begin
  plocha.CaretY:= 1;
  //pozn. nasledujuca operacia pouziva takmer vsetok cas po disassemblovani, ked je progressbar na 100%
//  Plocha.Lines:=fSection.Disassembled;           // -> skopirovanie obsahu,
//  fSection.Disassembled.Free;                    //
//  fSection.Disassembled:=Plocha.Lines;

//  if fSection.Statistics.Instructions > 0 then BytesPerInstructionDataLabel.Caption:=FloatToStrf(fSection.Statistics.InstructionBytes/fSection.Statistics.Instructions,ffGeneral,5,7)
//  else BytesPerInstructionDataLabel.Caption:='0';
//  InstructionCountDataLabel.Caption:=IntToStr(fSection.Statistics.Instructions);
  LineDataLabel.Caption:='1';
  FollowButton.Enabled:=false;
  ReturnButton.Enabled:=false;
  if fSection.FileSize = 0 then GotoAddressButton.Enabled:=false;
end;



procedure TCodeTabFrame.GotoPosition(Offset: LongInt; Origin: TSeekOrigin); // Offset zacina od 0
var Position: integer;
begin
  case Origin of
    soBeginning: Position:= Nezaporne(Offset);
    soCurrent: Position:= Nezaporne(Plocha.CaretY - 1 + Offset);
  end;
  Plocha.CaretY:= Min(Position, fSection.LinesCount-1) + 1;
  Plocha.TopLine:= Plocha.CaretY;
end;



procedure TCodeTabFrame.FollowButtonClick(Sender: TObject);         // Nasledovanie skoku
var Ukaz:^cardinal;
begin
  new(ukaz);
  ukaz^:= Plocha.CaretY - 1;
  Zasobnik.Push(ukaz);
  ReturnButton.Enabled:=true;
  GotoPosition(JumpPosition, soBeginning);
  plocha.SetFocus;
end;



procedure TCodeTabFrame.ReturnButtonClick(Sender: TObject);         // Navrat skoku
var ukaz:^cardinal;
begin
  ukaz:=Zasobnik.Pop;
  if Zasobnik.Count=0 then ReturnButton.Enabled:=false;
  GotoPosition(ukaz^, soBeginning);
  plocha.SetFocus;
end;



// treba prerobit na pouzitie bez fSection.Disassembled ale s Plocha.Lines
procedure TCodeTabFrame.FindString(SearchText:string; options: TFindOptions);
var i:cardinal;
    nasiel:boolean;
    UpSearchText:string;
//    sso:TSynSearchOptions;
begin
{
  if frMatchCase in Options then sso:=[ssoMatchCase];
  if plocha.SearchReplace(SearchText,'',[ssoMatchCase])=0 then begin
    MessageDlg('"'+SearchText+'" '+NotFound,mtWarning,[mbOK],0);
//    plocha.SelEnd:=0;
  end;
  exit;
}
  i:=Plocha.CaretY - 1;
  nasiel:=false;
  UpSearchText:=UpperCase(SearchText);
  if frdown in Options then begin
    if frMatchCase in Options then
      repeat
        inc(i);
        if pos(searchtext,fSection.Disassembled[i])<>0 then nasiel:=true
      until (nasiel or (i>(fSection.LinesCount-2)))
    else
      repeat
        inc(i);
        if pos(upsearchtext,UpperCase(fSection.Disassembled[i]))<>0 then nasiel:=true
      until (nasiel or (i>(fSection.LinesCount-2)));
  end
  else begin
    if frMatchCase in Options then
      repeat
        dec(i);
        if pos(searchtext,fSection.Disassembled[i])<>0 then nasiel:=true;
      until (nasiel or (i=0))
    else
      repeat
        dec(i);
        if pos(Upsearchtext,UpperCase(fSection.Disassembled[i]))<>0 then nasiel:=true;
      until (nasiel or (i=0));
  end;
  if nasiel then GotoPosition(i, soBeginning) else MessageDlg('"'+searchtext+'" '+NotFoundStr, mtWarning,[mbOK],0);

end;


procedure TCodeTabFrame.Translate(ini:TMemINIFile; error:string);
var i:integer;
begin
  (Parent as TTabSheet).Caption:=ini.ReadString('Code','Caption',error) + IntToStr(fSection.CodeSectionIndex);
// Popisky tlacidiel
  GotoEntrypointButton.Caption:=ini.ReadString('Code','GotoEntrypointButton',error);
  GotoAddressButton.Caption:=ini.ReadString('Code','GotoAddressButton',error);
  FollowButton.Caption:=ini.ReadString('Code','FollowButton',error);
  ReturnButton.Caption:=ini.ReadString('Code','ReturnButton',error);

// Popisky Label
{
  BytesperInstructionLabel.Caption:=ini.ReadString('Code','BytesperInstructionLabel',error);
  InstructionCountLabel.Caption:=ini.ReadString('Code','InstructionCountLabel',error);
}
  LineLabel.Caption:=ini.ReadString('Code','LineLabel',error);

// Hinty
  GotoEntrypointButton.Hint:=ini.ReadString('Code','GotoEntrypointButtonHint',error);
  GotoAddressButton.Hint:=ini.ReadString('Code','GotoAddressButtonHint',error);
  FollowButton.Hint:=ini.ReadString('Code','FollowButtonHint',error);
  ReturnButton.Hint:=ini.ReadString('Code','ReturnButtonHint',error);

// Popup menu
  CodePopupMenu.Items[0].Caption:=ini.ReadString('Code','ToggleBookmark',error);
  CodePopupMenu.Items[0].Hint:=ini.ReadString('Code','MainToggleBookmarkHint',error);
  for i:=0 to 9 do begin
    CodePopupMenu.Items[0].Items[i].Caption:=ini.ReadString('Code','Bookmark',error)+' '+IntToStr(i);
    CodePopupMenu.Items[0].Items[i].Hint:=ini.ReadString('Code','ToggleBookmarkHint',error)+' '+IntToStr(i);
  end;
  CodePopupMenu.Items[1].Caption:=ini.ReadString('Code','GotoBookmark',error);
  CodePopupMenu.Items[1].Hint:=ini.ReadString('Code','MainGotoBookmarkHint',error);
  for i:=0 to 9 do begin
    CodePopupMenu.Items[1].Items[i].Caption:=ini.ReadString('Code','Bookmark',error)+' '+IntToStr(i);
    CodePopupMenu.Items[1].Items[i].Hint:=ini.ReadString('Code','GotoBookmarkHint',error)+' '+IntToStr(i);
  end;
  CodePopupMenu.Items[3].Caption:=ini.ReadString('Code','ChangeToUnsigned',error);
  CodePopupMenu.Items[3].Items[0].Caption:=ini.ReadString('Code','ChangeByte',error);
  CodePopupMenu.Items[3].Items[1].Caption:=ini.ReadString('Code','ChangeWord',error);
  CodePopupMenu.Items[3].Items[2].Caption:=ini.ReadString('Code','ChangeDword',error);
  CodePopupMenu.Items[3].Items[3].Caption:=ini.ReadString('Code','ChangeQword',error);
  CodePopupMenu.Items[4].Caption:=ini.ReadString('Code','ChangeToSigned',error);
  CodePopupMenu.Items[4].Items[0].Caption:=ini.ReadString('Code','ChangeByte',error);
  CodePopupMenu.Items[4].Items[1].Caption:=ini.ReadString('Code','ChangeWord',error);
  CodePopupMenu.Items[4].Items[2].Caption:=ini.ReadString('Code','ChangeDword',error);
  CodePopupMenu.Items[4].Items[3].Caption:=ini.ReadString('Code','ChangeQword',error);
  CodePopupMenu.Items[5].Caption:=ini.ReadString('Code','ChangeToFloat',error);
  CodePopupMenu.Items[5].Items[0].Caption:=ini.ReadString('Code','ChangeSingle',error);
  CodePopupMenu.Items[5].Items[1].Caption:=ini.ReadString('Code','ChangeDouble',error);
  CodePopupMenu.Items[5].Items[2].Caption:=ini.ReadString('Code','ChangeExtended',error);
  CodePopupMenu.Items[6].Caption:=ini.ReadString('Code','ChangeToString',error);
  CodePopupMenu.Items[6].Items[0].Caption:=ini.ReadString('Code','ChangePascal',error);
  CodePopupMenu.Items[6].Items[1].Caption:=ini.ReadString('Code','ChangeC',error);
  CodePopupMenu.Items[7].Caption:=ini.ReadString('Code','AdvancedDataChange',error);
  CodePopupMenu.Items[9].Caption:=ini.ReadString('Code','Disassemble',error);
  CodePopupMenu.Items[10].Caption:=ini.ReadString('Code','AdvancedDisassemble',error);
  CodePopupMenu.Items[12].Caption:=ini.ReadString('Code','Insert',error);
  CodePopupMenu.Items[12].Items[0].Caption:=ini.ReadString('Code','InsertComment',error);
  CodePopupMenu.Items[12].Items[1].Caption:=ini.ReadString('Code','InsertEmpty',error);
  CodePopupMenu.Items[13].Caption:=ini.ReadString('Code','Remove',error);

end;

procedure TCodeTabFrame.ChangeUDataClick(Sender: TObject);
var Options: TDataChangeOptions;
begin
  Options.signed:=false;
  Options.option:=dcItems;
  Options.value:=1;
  case (Sender as TMenuItem).MenuIndex of
    0: Options.datatype:=dtByte;
    1: Options.datatype:=dtWord;
    2: Options.datatype:=dtDword;
    3: Options.datatype:=dtQword;
  end;
  ChangeToData(Options);
end;

procedure TCodeTabFrame.ChangeSDataClick(Sender: TObject);
var Options: TDataChangeOptions;
begin
  Options.signed:=true;
  Options.option:=dcItems;
  Options.value:=1;
  case (Sender as TMenuItem).MenuIndex of
    0: Options.datatype:=dtByte;
    1: Options.datatype:=dtWord;
    2: Options.datatype:=dtDword;
    3: Options.datatype:=dtQword;
  end;
  ChangeToData(Options);
end;

procedure TCodeTabFrame.ChangeFDataClick(Sender: TObject);
var Options: TDataChangeOptions;
begin
  Options.signed:=false;
  Options.option:=dcItems;
  Options.value:=1;
  case (Sender as TMenuItem).MenuIndex of
    0: Options.datatype:=dtSingle;
    1: Options.datatype:=dtDouble;
    2: Options.datatype:=dtDoubleEx;
  end;
  ChangeToData(Options);
end;

procedure TCodeTabFrame.ChangeStringClick(Sender: TObject);
var Options: TDataChangeOptions;
begin
  Options.signed:=false;
  Options.option:=dcItems;
  Options.value:=1;
  case (Sender as TMenuItem).MenuIndex of
    0: Options.datatype:=dtPascalStr;
    1: Options.datatype:=dtCStr;
    2: Options.datatype:=dtPascalUniCodeStr;
    3: Options.datatype:=dtCUniCodeStr;
  end;
  ChangeToStringData(Options);
end;

procedure TCodeTabFrame.NormalDisassembleClick(Sender: TObject);
var Options: TDisassembleFormOptions;
begin
  Options.option:=dcCode;
  Options.value:=0;
  Options.bit32:=fSection.Bit32;
  Disassemble(Options);
end;

procedure TCodeTabFrame.AdvancedChanging1Click(Sender: TObject);
begin
  AdvancedChangingToDataForm.MaxAddressHexEdit.MaxValue:=fSection.MaxAddress;
  if AdvancedChangingToDataForm.ShowModal=mrOK then ChangeToData(AdvancedChangingToDataForm.Options);
end;

procedure TCodeTabFrame.AdvancedDisassemble1Click(Sender: TObject);
begin
  AdvancedDisassembleForm.MaxAddressBinHexEdit.MaxValue:=fSection.MaxAddress; // podobne by sa zislo nastavit MaxValue aj pre ostatne edity
  if AdvancedDisassembleForm.ShowModal=mrOK then Disassemble(AdvancedDisassembleForm.Options);
end;



{$R *.dfm}

function TCodeTabFrame.GetSection: TSection;
begin
  result:=fSection;
end;

end.
