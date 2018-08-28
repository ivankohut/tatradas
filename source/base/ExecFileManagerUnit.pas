unit ExecFileManagerUnit;

{$INCLUDE 'delver.inc'}

{ TODO:
  spravne osetrit citanie a zapisovanie suborou (vynimky atd)
}

interface

uses
  SysUtils,
  Classes,
  // project units
  ExceptionsUnit,
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

  c_PESign = $4550;
  c_NESign = $454E;
  c_LESign = $454C;
  c_LXSign = $4558;

type

//  TFileError = (errNone, errOpen, errUnknownFormat, errBadFormat, errDASNotFound, errBadProjectVersion, errSave, errCanceled);

  TExecFileManager = class
  private
//    fError: TFileError;
    function GetExecFileFormat(AFileStream: TFileStream): TExecFileFormat;

  public
    function LoadExecFileFromFile(AProjectFileName: TFileName): TExecutableFile;    // uses TextFile
    procedure SaveExecFileToFile(ExecFile: TExecutableFile; AFileName: TFileName);
    function CreateNewExecFile(AFileName: TFileName): TExecutableFile;
    function CreateNewCustomExecFile(AFileName: TFileName; AParameters: TCustomFileParameters): TExecutableFile;
//    property Error: TFileError read fError;
  end;

var
  ExecFileManager: TExecFileManager;

implementation


{ TExecFileManager }


function TExecFileManager.GetExecFileFormat(AFileStream: TFileStream): TExecFileFormat;
var
  LowID, HighID: Word;
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
            c_PESign: Result := ffPE;      // PE - Portable Executable 32-bit
            c_NESign: Result := ffNE;      // NE - New Executable 16-bit
            c_LESign: Result := ffUnknown; // LE - Linear Executable - not supported yet
            c_LXSign: Result := ffUnknown; // LX - Linear Executable 32-bit - not supported yet
            else
              Result := ffUnknown;
          end;
        end
        // MZ file format
        else
          Result := ffMZ;
      end;

      // ELF class of executable file formats
      c_ELFLowID:
        if HighID = c_ELFHighID then
          Result := ffELF
        else
          Result := ffUnknown;

      // other executable file formats
      else
        // If the first instruction is JUMP and the size of file is < 64K then it is probably 16 bit COM file
        if (((LowID mod $100) = $E9) or ((LowID mod $100) = $EB)) and (AFileStream.Size < High(Word) + 1) then
          Result := ffCOM
        else
          Result := ffUnknown;
    end;
  end
  else
    Result := ffUnknown;
end;


{
returns:
  - TExecutableFile object if the format of input file is recognized, supported and correct 
  - nil if the format of input file is not recognized or is not supported
  - ETatraDASException
  - EFileCorrupted
}
function TExecFileManager.CreateNewExecFile(aFileName: TFileName): TExecutableFile;
var
  FileFormat: TExecFileFormat;
  InputFile: TFileStream;
begin
  try
    InputFile := TFileStream.Create(aFileName, fmShareDenyNone);
  except
    raise ETatraDASException.Create(CouldNotOpenFileStr);
  end;

  try
    // Create appropriate ExecFile object
    Result := nil;
    FileFormat := GetExecFileFormat(InputFile);
    case FileFormat of
      ffPE: Result := TPEFile.Create(InputFile, aFileName);
      ffCOM: Result := TCOMFile.Create(InputFile, aFileName);
      ffMZ: Result := TMZFile.Create(InputFile, aFileName);
      ffNE: Result := TNEFile.Create(InputFile, aFileName);
      ffELF: Result := TELFFile.Create(InputFile, aFileName);
      // LE: result := TLEFile.Create(InputFile, aFileName)
      // LX: result := TLXFile.Create(InputFile, aFileName)
      ffUnknown: Result := nil;
      else
        raise EIllegalState.Create('Bad file format');
    end;
  finally
    InputFile.Free;
  end;
end;



function TExecFileManager.CreateNewCustomExecFile(AFileName: TFileName; AParameters: TCustomFileParameters): TExecutableFile;
var
  InputFile: TFileStream;
begin
  try
    InputFile := TFileStream.Create(aFileName, fmShareDenyNone);
  except
    raise ETatraDASException.Create(CouldNotOpenFileStr);
  end;

  try
    Result := TCustomFile.Create(InputFile, AFileName, AParameters);
  finally
    InputFile.Free;
  end;
end;


// Save as TatraDAS Project
procedure TExecFileManager.SaveExecFileToFile(ExecFile: TExecutableFile; AFileName: TFileName);
var
  DAS: TextFile;
  DHF: TFileStream;
  DHF_FileName: string;
  DAS_FileName: string;
  Version: Cardinal;
begin
  // Initialize DHF project file
  DHF_FileName := ChangeFileExt(aFileName, '.dhf');
  DHF := TFileStream.Create(DHF_FileName, fmCreate);
  try
    // Save project file version
    Version := TatraDASProjectVersion;
    DHF.Write(Version, 4);
    DHF.Write(ExecFile.ExeFormat, SizeOf(TExecFileFormat));

    // Save disassembled code sections
    DAS_FileName := ChangeFileExt(aFileName, DASFileExtension);
    AssignFile(DAS, DAS_FileName);
    Rewrite(DAS);
    try
      ExecFile.SaveToFile(DHF, DAS);
    finally
      CloseFile(DAS);
    end;
  finally
    DHF.Free;
  end;
end;


{
  returns:
  - TExecutableFile object
  - exception in case of any problem
}
function TExecFileManager.LoadExecFileFromFile(AProjectFileName: TFileName): TExecutableFile;
var
  DHF_FileName, DAS_FileName: string;
  DHF: TMemoryStream;
  DAS: TextFile;
  ProjectVersion: Cardinal;
  ProjectExecFileFormat: TExecFileFormat;
begin
  Result := nil;
  DHF_FileName := AProjectFileName;
  DAS_FileName := ChangeFileExt(DHF_FileName, DASFileExtension);

  if not FileExists(DAS_FileName) then
    raise ETatraDASException.Create(CouldNotFindDASFileStr + ' - ' + DAS_FileName);

  DHF := TMemoryStream.Create;
  try
    DHF.LoadFromFile(DHF_FileName);
    DHF.Read(ProjectVersion, 4);
    case ProjectVersion of
      TatraDASProjectVersion: begin

        DHF.Read(ProjectExecFileFormat, SizeOf(TExecFileFormat));
        case ProjectExecFileFormat of
          ffCOM: Result := TCOMFile.Create;
          ffMZ: Result := TMZFile.Create;
          ffNE: Result := TNEFile.Create;
          ffPE: Result := TPEFile.Create;
          ffELF: Result := TELFFile.Create;
          ffCustom: Result := TCustomFile.Create;
          else
            raise ETatraDASException.Create(FileCorruptedStr + ' - ' + AProjectFileName);
        end;

        AssignFile(DAS, DAS_FileName);
        Reset(DAS);
        try
          try
            Readln(DAS);
            Readln(DAS);
            Result.LoadFromFile(DHF, DAS);
          except
            FreeAndNil(Result);
            raise;
          end;
        finally
          CloseFile(DAS);
        end;
      end

      // Incompatible project file version
      else
        raise ETatraDASException.Create(InCompatibleProjectVersion + '.' + #13 + CurrentVersion + ' ' + IntToHex(TatraDASProjectVersion, 8) + '.');
    end;
  finally
    DHF.Free;
  end;
end;


initialization
  ExecFileManager := TExecFileManager.Create;

finalization
  ExecFileManager.Free;

end.
