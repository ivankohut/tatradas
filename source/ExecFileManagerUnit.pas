unit ExecFileManagerUnit;

{$INCLUDE 'delver.inc'}

{ TODO:
  spravne osetrit citanie a zapisovanie suborou (vynimky atd)
}

interface

uses
  SysUtils,
  Classes,

  procmat,
  ExecFileUnit,
  PEFileUnit,
  MZFileUnit,
  COMFileUnit,
  NEFileUnit,
  LEFileUnit,
  LXFileUnit,
  ELFFileUnit,
  CustomFileUnit,
  StringRes;

const

  c_MZLowID = $5A4D;
  c_ELFLowID = $457F;
  c_ELFHighID = $464C;

  c_PESign =  $4550;
  c_NESign =  $454E;
  c_LESign =  $454C;
  c_LXSign =  $4558;

type

//  TFileError = (errNone, errOpen, errUnknownFormat, errBadFormat, errDASNotFound, errBadProjectVersion, errSave, errCanceled);

  TExecFileManager = class
   private
//    fError: TFileError;
    function GetExecFileFormat(AFileStream: TFileStream): TExecFileFormat;

   public
    function LoadExecFileFromFile(AFileName: TFileName) : TExecutableFile;    // uses TextFile
    function SaveExecFileToFile(ExecFile: TExecutableFile; AFileName: TFileName; SaveOptions: TSaveOptions): boolean;
    function CreateNewExecFile(AFileName: TFileName): TExecutableFile;
    function CreateNewCustomExecFile(AFileName: TFileName; AParameters: TCustomFileParameters): TExecutableFile;

//    property Error: TFileError read fError;
  end;


implementation

{ TExecFileManager }

function TExecFileManager.GetExecFileFormat(AFileStream: TFileStream): TExecFileFormat;
var
  LowID, HighID: word;
  Header: TMZHeader;
begin
  if AFileStream.Size > SizeOf(Header) then begin
    AFileStream.Seek(0, 0);
    AFileStream.Read(LowID, 2);
    AFileStream.Read(HighID, 2);
    case LowID of

      // MZ class of executable file formats
      c_MZLowID: begin
        AFileStream.Position := 0;
        AFileStream.Read(Header, SizeOf(Header));
        // Format newer than MZ
        if Header.RelocTableOffset = $40 then begin
          AFileStream.Seek(Header.Reserved[9], 0);
          AFileStream.Read(Header.Sign, 2);
          case Header.Sign of
            c_PESign: result := ffPE;      // PE - Portable Executable 32-bit
            c_NESign: result := ffNE;      // NE - New Executable 16-bit
            c_LESign: result := ffUnknown; // LE - Linear Executable - not supported yet
            c_LXSign: result := ffUnknown; // LX - Linear Executable 32-bit - not supported yet
            else
              result := ffUnknown;
          end;
        end
        // MZ file format
        else
          result := ffMZ;
      end;

      // ELF class of executable file formats
      c_ELFLowID:
        if HighID = c_ELFHighID then
          result := ffELF
        else
          result := ffUnknown;

      // other executable file formats
      else
        // If the first instruction is JUMP and the size of file is < 64K then it is probably 16 bit COM file
        if (((LowID mod $100) = $E9) or ((LowID mod $100) = $EB)) and (AFileStream.Size < High(Word) + 1) then
          result := ffCOM
        else
          result := ffUnknown;
    end;
  end
  else
    result := ffUnknown;
end;



function TExecFileManager.CreateNewExecFile(aFileName: TFileName): TExecutableFile;
var
  FileFormat: TExecFileFormat;
  InputFile: TFileStream;
begin
  ProgressData.ErrorStatus:= errNone;
  result := nil;

  try
    InputFile := TFileStream.Create(aFileName, fmShareDenyNone);
  except
    ProgressData.ErrorStatus := errOpen;
    Exit;
  end;

  try
    // Create appropriate ExecFile object
    FileFormat := GetExecFileFormat(InputFile);
    case FileFormat of
      ffPE:  result := TPEFile.Create(InputFile, aFileName);
      ffCOM: result := TCOMFile.Create(InputFile, aFileName);
      ffMZ:  result := TMZFile.Create(InputFile, aFileName);
      ffNE:  result := TNEFile.Create(InputFile, aFileName);
      ffELF: result := TELFFile.Create(InputFile, aFileName);
      // LE: result:=TLEFile.Create(InputFile, aFileName)
      // LX: result:=TLXFile.Create(InputFile, aFileName)
      ffUnknown: begin
        result := nil;
        Exit;
      end;
    end;

    if result.ExeFormat = ffError then begin
      ProgressData.ErrorStatus := errBadFormat;
      result.Free;
    end;

  finally
    InputFile.Free;
  end;
end;



function TExecFileManager.CreateNewCustomExecFile(AFileName: TFileName; AParameters: TCustomFileParameters): TExecutableFile;
var
  InputFile: TFileStream;
begin
  result := nil;
  try
    InputFile := TFileStream.Create(aFileName, fmShareDenyNone);
  except
    ProgressData.ErrorStatus := errOpen;
    Exit;
  end;

  try
    result := TCustomFile.Create(InputFile, AFileName, AParameters);
  finally
    InputFile.Free;
  end;
end;



function TExecFileManager.SaveExecFileToFile(ExecFile: TExecutableFile; AFileName: TFileName; SaveOptions: TSaveOptions): boolean;
var
  DAS: TextFile;
  DHF: TFileStream;
  DHF_FileName: string;
  DAS_FileName: string;
  Version: cardinal;
begin
  DHF := nil;
  // Save as TatraDAS Project
  if soProject in SaveOptions then begin

    // Initialize DHF project file
    DHF_FileName:=ChangeFileExt(aFileName,'.dhf');;
    try
      DHF:= TFileStream.Create(DHF_FileName, fmCreate);
    except
      ProgressData.ErrorStatus:= errSave;
      DHF.Free;
      result:= false;
      Exit;
    end;
    Version:= TatraDASProjectVersion;
    DHF.Write(Version, 4);
    DHF.Write(ExecFile.ExeFormat, SizeOf(TExecFileFormat));

    DAS_FileName:=ChangeFileExt(aFileName, '.das');
  end;

  // Save disassembled code sections
  if soDisassembly in SaveOptions then
    DAS_FileName:=ChangeFileExt(aFileName, '.das');

  // Save disassembled code sections as NASM compilable code
  if (soNASM in SaveOptions) then
    DAS_FileName:= aFileName;

  // Save disassembled code sections using user defined constraints
  if (SaveOptions * [soProject, soDisassembly, soNASM]) = [] then
    DAS_FileName:= aFileName;

  // Initialize DAS file
  AssignFile(DAS, DAS_FileName);
  Rewrite(DAS);

  // Save :)
  result:= ExecFile.SaveToFile(DHF, DAS, SaveOptions);

  CloseFile(DAS);
  DHF.Free;
end;



function TExecFileManager.LoadExecFileFromFile(aFileName: TFileName): TExecutableFile;
var
    DHF_FileName, DAS_FileName: string;
    DHF: TMemoryStream;
    DAS: TextFile;
    ProjectVersion: cardinal;
    ProjectExecFileFormat: TExecFileFormat;
begin
  result:= nil;
  DHF_FileName:= aFileName;
  DAS_FileName:= ChangeFileExt(DHF_FileName, '.das');

  if not FileExists(DAS_FileName) then begin
    ProgressData.ErrorStatus:= errDASNotFound;
    result:= nil;
    Exit;
  end;

  DHF:= TMemoryStream.Create;
  DHF.LoadFromFile(DHF_FileName);
  DHF.Read(ProjectVersion, 4);
  case ProjectVersion of
    TatraDASProjectVersion: begin

      DHF.Read(ProjectExecFileFormat, SizeOf(TExecFileFormat));
      case ProjectExecFileFormat of
        ffCOM: result:=TCOMFile.Create;
        ffMZ:  result:=TMZFile.Create;
        ffNE:  result:=TNEFile.Create;
        ffPE:  result:=TPEFile.Create;
        ffELF: result:=TELFFile.Create;
        ffCustom: result:=TCustomFile.Create;
        else begin
          DHF.Free;
          ProgressData.ErrorStatus:= errBadFormat;
          Exit;
        end;
      end;

      AssignFile(DAS, DAS_FileName);
      Reset(DAS);
      Readln(DAS);
      Readln(DAS);

      if not result.LoadFromFile(DHF, DAS) then begin
        result.Free;
        result:= nil;
        DHF.Free;
        CloseFile(DAS);
        Exit;
      end;

      CloseFile(DAS);
    end

    else begin
      ProgressData.ErrorStatus:= errBadProjectVersion;
      result:= nil;
      DHF.Free;
      Exit;
    end;
  end;
  DHF.Free;
end;


end.
