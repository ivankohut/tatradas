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
  {$IFDEF GUI_B}
  Controls,
  UnknownFileFormUnit,
  {$ENDIF}
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

  TFileError = (errNone, errOpen, errUnknownFormat, errBadFormat, errDASNotFound, errBadProjectVersion, errSave, errCanceled);

  TExecFileManager = class
   private
    fError: TFileError;
    function GetExecFileFormat(FileStream: TFileStream): TExecFileFormat;

   public
    function LoadExecFileFromFile(AFileName: TFileName) : TExecutableFile;    // uses TextFile
    function SaveExecFileToFile(ExecFile: TExecutableFile; AFileName: TFileName; SaveOptions: TSaveOptions): boolean;
    function CreateNewExecFile(AFileName: TFileName): TExecutableFile;

    property Error: TFileError read fError;
  end;


implementation

{ TExecFileManager }

function TExecFileManager.CreateNewExecFile(aFileName: TFileName): TExecutableFile;
var
  FileFormat: TExecFileFormat;
  InputFile: TFileStream;
begin
  fError:= errNone;
  result:=nil;

  try
    InputFile:=TFileStream.Create(aFileName, fmShareDenyNone);
  except
    fError:=errOpen;
    Exit;
  end;

  // Create appropriate ExecFile object
  FileFormat:= GetExecFileFormat(InputFile);
  case FileFormat of
    ffPE:  result:=TPEFile.Create(InputFile, aFileName);
    ffCOM: result:=TCOMFile.Create(InputFile, aFileName);
    ffMZ:  result:=TMZFile.Create(InputFile, aFileName);
    NE:  result:=TNEFile.Create(InputFile, aFileName);
    // ELF: result:=TELFFile.Create(InputFile, aFileName);
    // LE: result:=TLEFile.Create(InputFile, aFileName)
    // LX: result:=TLXFile.Create(InputFile, aFileName)

    {$IFDEF GUI_B}
    ffUnknown: begin
      UnknownFileFormatForm.FileName:= ExtractFileName(aFileName);
      UnknownFileFormatForm.FileSize:= InputFile.Size;
      if UnknownFileFormatForm.ShowModal = mrOK then
        result:=TCustomFile.Create(InputFile, aFileName, UnknownFileFormatForm.Parameters)
      else begin
        fError:= errCanceled;
        Exit;
      end;
    end;
   {$ELSE}
    ffUnknown: begin
      fError:=errUnknownFormat;
      Exit;
    end;
   {$ENDIF}
  end;

  if result.ExeFormat = ffError then begin
    fError:= errBadFormat;
    result.Free;
  end;

  InputFile.Free;
end;



function TExecFileManager.GetExecFileFormat(FileStream: TFileStream): TExecFileFormat;
var LowID, HighID: word;
    Header: TMZHeader;
begin
  if FileStream.Size > SizeOf(Header) then begin
    FileStream.Seek(0,0);
    FileStream.Read(LowID,2); FileStream.Read(HighID,2);
    case LowID of

      // MZ class of executable file formats
      c_MZLowID: begin
        FileStream.Position:=0;
        FileStream.Read(header,SizeOf(Header));
        if header.reloctableoffset = $40 then begin // newer format than MZ
          FileStream.Seek(header.reserved[9],0);
          FileStream.Read(header.Sign,2);
          case header.sign of
            c_PESign: result:=ffPE;                     // Portable Executable 32-bit
            c_NESign: result:=NE;                     // New Executable 16-bit
            c_LESign: result:=ffUnknown; //LE;                     // Linear Executable
            c_LXSign: result:=ffUnknown; //LX;                     // Linear Executable 32 - bit
            else
              result:=ffUnknown;
          end;
        end
        else begin                                  // MZ file format
          result:= ffMZ;
        end;
      end;

      // ELF class of executable file formats
      c_ELFLowID:
        if HighID = c_ELFHighID then
          result:=ELF
        else
          result:=ffUnknown;

      // other executable file formats
      else
        if (((LowID mod $100) = $E9) or ((LowID mod $100) = $EB)) and (FileStream.Size < High(Word)+1) then
          result:= ffCOM
        else
          result:=ffUnknown;
    end;
  end
  else
    result:=ffUnknown;

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
      fError:=errSave;
      DHF.Free;
      result:= false;
      Exit;
    end;
    Version:=TatraDASProjectVersion;
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
  WriteLn(DAS, 'DisASsembled file, Original file: ' + ExecFile.FileName + '  ' + TatraDASFullNameVersion + ', Ivan Kohut (c) 2007');
  Writeln(DAS);

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
    UselessLine: string;
begin
  result:= nil;
  DHF_FileName:= aFileName;
  DAS_FileName:= ChangeFileExt(DHF_FileName, '.das');

  if not FileExists(DAS_FileName) then begin
    fError:= errDASNotFound;
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
        ffPE:  result:=TPEFile.Create;
        ffMZ:  result:=TMZFile.Create;
//        NE: result:=TNEFile.Create(Ctrls);
        ffCOM: result:=TCOMFile.Create;
//        ELF: result:=TELFFile.Create(Ctrls);
//        Unknown: result:=TUnknownFile.Create(ctrls);
        else begin
          DHF.Free;
          fError:= errBadFormat;
          Exit;
        end;
      end;

      AssignFile(DAS, DAS_FileName);
      Reset(DAS);
      Readln(DAS, UselessLine);
      Readln(DAS, UselessLine);

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
      fError:= errBadProjectVersion;
      result:= nil;
      DHF.Free;
      Exit;
    end;
  end;
  DHF.Free;
end;


end.
