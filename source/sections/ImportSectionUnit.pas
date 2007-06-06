unit ImportSectionUnit;

{$INCLUDE 'DELVER.INC'}

interface

uses
  Classes,
  INIFiles,
  SysUtils,

  procmat,
  SectionUnit;

type

  TImportFunction = record
    AddressRVA:cardinal;
    RVAtoname:cardinal;
    Hint:word;
    ByOrdinal:boolean;
    Ordinal:cardinal;
    name: string;
    vyskyty: array of cardinal; // Memory addresses

    MemAddress: cardinal;
  end;

  TImportModul = record
    FunctionCount: cardinal;
    Functions:array of TImportFunction;
    Name:string;
  end;

  TImportDirectoryEntry = record
    lookuptableRVA:cardinal;
    res1,res2:cardinal;
    namerva:cardinal;
    addresstableRVA:cardinal;
  end;

  TImportSection = class(TSection)
    ModulCount: integer;
    TotalFunctionCount: integer;
    Moduls: array of TImportModul;
    constructor Create(efile:TObject); overload;
//    constructor Create(a:TStream; RVA,Offset,size,ImTableRVA: cardinal; efile:TObject); overload;
//    constructor CreateFromPEFile(a:TStream; RVA,Offset,size,ImTableRVA: cardinal; efile:TObject); overload;
    constructor CreateFromPEFile(a:TStream; ImTableRVA: cardinal; ImageBase: cardinal; aName: string; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aExecFile: TObject); virtual;
    constructor CreateFromNEFile(a:TStream; ModuleTableOffset, ImportTableOffset, ModuleCount, ImportTableSize: cardinal; efile:TObject);
//    constructor CreateFromELFFile(a:TStream; Offset,Size,StrTabOffset,StrTabSize: cardinal; efile:TObject);
    destructor Destroy; override;
    function SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions): boolean; override;

    function LoadFromFile(var f: TextFile; a: TStream):boolean; overload; override;
    function LoadFromFile(DHF: TFileStream; DAS: TTextFileStream):boolean; overload; override;
    
    function AddFunctionOccurence(IndexAddress: cardinal; CallInstrAddress: cardinal): string;
//    procedure AddFunctionOccurence(ModulIndex, FunctionIndex: integer; Address: cardinal);
  end;

  TSymbolTableEntry = record
    st_name: cardinal;
    st_value: cardinal;
    st_size: cardinal;
    st_info: byte;
    st_other: byte;
    st_shndx: word;
  end;

  TSymbol = record
    name: string;
  end;

  TELFImportSection = class(TImportSection)
    Symbols: array of TSymbol;
    SymbolCount: integer;
    SymbolTable: array of TSymbolTableEntry;
    constructor Create(a:TStream; Offset,Size,StrTabOffset,StrTabSize: cardinal; efile:TObject); overload;
  end;

implementation

uses
  ExecFileUnit,
  CodeSectionUnit;


constructor TImportSection.Create(efile:TObject);
begin
  fTyp:=stImport;
  execfile:=efile;
end;



destructor TImportSection.Destroy;
begin
  inherited;
end;



function TImportSection.AddFunctionOccurence(IndexAddress: cardinal; CallInstrAddress: cardinal): string;
var
  ModIndex, FunIndex: integer;
  ModulIndex, FunctionIndex: integer;
  FunctionFound: boolean;
  label FOUND;
begin
  FunctionFound:=false;

  // Find function and its modul 
  for ModIndex:=0 to ModulCount - 1 do
    for FunIndex:=0 to Moduls[ModIndex].FunctionCount - 1 do
      if IndexAddress = moduls[ModIndex].functions[FunIndex].MemAddress then begin
        ModulIndex:= ModIndex;
        FunctionIndex:= FunIndex;
        FunctionFound:= True;
        goto FOUND;
      end;

FOUND:

  if FunctionFound then begin
    // Add function's occurence into its 'vyskyty' array 
    with moduls[ModulIndex].functions[FunctionIndex] do begin
      SetLength(vyskyty, Length(vyskyty) + 1);
      vyskyty[high(vyskyty)]:=CallInstrAddress;

      if ByOrdinal then
        result:=IntToHex(Ordinal, 8) + '''(Ordinal)' + ' from ''' + moduls[ModulIndex].name
      else
        result:= name +''' from ''' + moduls[ModulIndex].name;
    end;  

  end;
end;



constructor TImportSection.CreateFromPEFile(a:TStream; ImTableRVA, ImageBase: cardinal; aName: string; aFileOffset, aFileSize, aMemOffset, aMemSize: cardinal; aExecFile: TObject);
//constructor TImportSection.CreateFromPEFile(a:TStream; RVA,Offset,size,ImTableRVA: cardinal; efile:TObject);
var b:TMemoryStream;
    c,e:cardinal;              // Import object RVA, ordinal/function name RVA, loop variable
    d:TImportDirectoryEntry;
    f:byte;               // Ordinal/Function name bit
    i,j:integer;           // loop variable
    rva,functiontableSRVA:cardinal;

begin
  inherited Create(aName, aFileOffset, aFileSize, aMemOffset, aMemSize, MaxInt, aExecFile);
  fTyp:=stImport;

  rva:=MemOffset - ImageBase;
  c:=rva;
  a.Seek(FileOffset,0);
  b:=TMemoryStream.Create;
  b.CopyFrom(a,FileSize);
  b.Seek(ImTableRVA-c,0);
  b.Read(d,20);
  TotalFunctionCount:=0;
  i:=-1;
  while d.namerva<>0 do begin                                   // Zaciatok nacitavania modulov
    inc(i);
    setlength(moduls,i+1);                               // Zvacsanie pola modulov o 1
    ReadStringFromStream(b,d.namerva-c,moduls[i].name); // Zistenie nazvu modulu
    if d.lookuptablerva<>0 then functiontableSRVA:=d.lookuptableRVA-c // Zistenie SRVA tab. funkcii mudulu }
    else functiontableSRVA:=d.addresstableRVA-c;

    b.Seek(functiontableSRVA,0);
    b.Read(e,4);
    j:=-1;
    while e<>0 do begin
      inc(j);
      setlength(moduls[i].functions,j+1);                          // Zvacsenie pola funkcii o 1
      f:=e shr 31;
      e:=e shl 1;
      e:=e shr 1;
      moduls[i].functions[j].RVAtoname:=e;
      moduls[i].functions[j].AddressRVA:=d.addresstableRVA+4*(j);

      // nasl. riadok dodany v 2.9.8 alpha
      moduls[i].functions[j].MemAddress:= moduls[i].functions[j].AddressRVA + ImageBase;

      if f=0 then begin                                                   // Import by name
        b.seek(e-c,0);
        b.Read(moduls[i].functions[j].hint,2);
        ReadStringFromStream(b,e-c+2,moduls[i].functions[j].name);
      end
      else begin                                                          // Import by ordinal
        moduls[i].functions[j].ordinal:=e;
        moduls[i].functions[j].byordinal:=true;
      end;
      b.Seek(functiontableSRVA+4*(j+1),0);
      b.Read(e,4);
    end;                                                                  // Koniec nacitavania funkcie
    moduls[i].functioncount:=j+1;
    inc(TotalFunctionCount,j+1);
    b.Seek(ImTableRVA-c+(i+1)*20,0);
    b.Read(d,20);
  end;                                                                    // Koniec nacitavania modulov
  Modulcount:=i+1;
  b.Free;
end;



constructor TELFImportSection.Create(a:TStream; Offset,Size,StrTabOffset,StrTabSize: cardinal; efile:TObject);
var
    i:integer;           // loop variable
begin
  fTyp:=stImport;
  execfile:=efile;

  SymbolCount:=Size div SizeOf(TSymbolTableEntry);
  SetLength(SymbolTable,SymbolCount);
  a.Position:=Offset;
  a.Read(SymbolTable[0],SymbolCount * SizeOf(TSymbolTableEntry));
  SetLength(Symbols,SymbolCount);
  SetLength(moduls,1);
  modulcount:=1;
  moduls[0].name:='modul1';
  SetLength(moduls[0].functions,SymbolCount);
  moduls[0].functioncount:=SymbolCount;
  for i:=0 to SymbolCount-1 do begin
    ReadStringFromStream(a,SymbolTable[i].st_name + StrTabOffset,Symbols[i].name);
    moduls[0].functions[i].name:=Symbols[i].name;
    moduls[0].functions[i].addressRVA:=SymbolTable[i].st_value;
  end;
end;



constructor TImportSection.CreateFromNEFile(a:TStream; ModuleTableOffset, ImportTableOffset, ModuleCount, ImportTableSize: cardinal; efile:TObject);
var
    i,j:integer;           // loop variable
    adresa: word;
    dlzka: byte;
    indexy: array of word;
begin
  fTyp:=stImport;
  execfile:=efile;

  TotalFunctionCount:=0;
  modulcount:= ModuleCount;
  SetLength(moduls,modulcount);
  SetLength(indexy,modulcount+1);
  a.Position:= ModuleTableOffset;
  a.Read(indexy[0],modulcount*2);
  indexy[modulcount]:=ImportTableSize+1;

  for i:=0 to modulcount-1 do begin
    a.Position:=ImportTableOffset + indexy[i];
    a.Read(dlzka,1);
    SetLength(moduls[i].name,dlzka);
    a.Read(moduls[i].name[1],dlzka);
    j:=0;
    a.Read(dlzka,1);
    while (dlzka > 0) and (a.Position < ImportTableOffset + indexy[i+1]) do begin
      SetLength(moduls[i].functions,j+1);
      SetLength(moduls[i].functions[j].name,dlzka);
      a.Read(moduls[i].functions[j].name[1],dlzka);
      a.Read(dlzka,1);
      inc(j);
    end;
    moduls[i].functioncount:=j;
    inc(TotalFunctionCount,j);
  end;
  indexy:=nil;
end;



function TImportSection.SaveToFile(DHF: TStream; var DAS: TextFile; SaveOptions: TSaveOptions):boolean;
var i,j,k,pocetvyskytov: integer;
begin
  DHF.Write(TotalFunctionCount,4);
  DHF.Write(modulcount,4);
  for i:=0 to modulcount-1 do begin
    DHF.Write(pchar(moduls[i].name)^,length(moduls[i].name)+1);
    DHF.Write(moduls[i].functioncount,4);
    for j:=0 to moduls[i].functioncount-1 do begin
      DHF.write(moduls[i].functions[j],sizeof(TImportFunction)-8);
      DHF.write(pchar(moduls[i].functions[j].name)^,length(moduls[i].functions[j].name)+1);
      pocetvyskytov:=length(moduls[i].functions[j].vyskyty);
      DHF.Write(pocetvyskytov,4);
      for k:=0 to pocetvyskytov-1 do DHF.Write(moduls[i].functions[j].vyskyty[k],4);
    end;
  end;
  result:=true;
end;



function TImportSection.LoadFromFile(var f: TextFile; a: TStream):boolean;
var i,j,k,pocetvyskytov: integer;
begin
  a.Read(TotalFunctionCount,4);
  a.Read(ModulCount,4);
  setlength(moduls,modulcount);
  for i:=0 to modulcount-1 do begin
    ReadStringFromStream(a,a.Position,moduls[i].name);
    a.Read(moduls[i].functioncount,4);
    SetLength(moduls[i].functions, moduls[i].functioncount);
    for j:=0 to moduls[i].functioncount-1 do begin
      a.Read(moduls[i].functions[j],sizeof(TImportFunction)-8);
      ReadStringFromStream(a,a.Position,moduls[i].functions[j].name);
      a.Read(pocetvyskytov,4);
      SetLength(moduls[i].functions[j].vyskyty,pocetvyskytov);
      for k:=0 to pocetvyskytov-1 do a.Read(moduls[i].functions[j].vyskyty[k],4);
    end;
  end;
  result:=true;
end;



function TImportSection.LoadFromFile(DHF: TFileStream; DAS: TTextFileStream): boolean;
var i,j,k,pocetvyskytov: integer;
begin
  DHF.Read(TotalFunctionCount,4);
  DHF.Read(ModulCount,4);
  setlength(moduls,modulcount);
  for i:=0 to modulcount-1 do begin
    ReadStringFromStream(DHF,DHF.Position,moduls[i].name);
    DHF.Read(moduls[i].functioncount,4);
    SetLength(moduls[i].functions, moduls[i].functioncount);
    for j:=0 to moduls[i].functioncount-1 do begin
      DHF.Read(moduls[i].functions[j],sizeof(TImportFunction)-8);
      ReadStringFromStream(DHF,DHF.Position,moduls[i].functions[j].name);
      DHF.Read(pocetvyskytov,4);
      SetLength(moduls[i].functions[j].vyskyty,pocetvyskytov);
      for k:=0 to pocetvyskytov-1 do DHF.Read(moduls[i].functions[j].vyskyty[k],4);
    end;
  end;
  result:=true;
end;



end.
