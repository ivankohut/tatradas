unit ExecFileManagerUnit;

{$INCLUDE 'delver.inc'}

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

  TFileError = (errNone, errOpen, errUnknownFormat, errBadFormat, errDASNotFound, errSave, errCanceled);

  TExecFileManager = class
   private
    fError: TFileError;
    fThread_LoadExecFileFromFile_Param1: TFileName;
    fThread_LoadExecFileFromFile_Result: TExecFileFormat;

   public
    function GetExecFileFormat(FileStream: TFileStream): TExecFileFormat;
   public
    function NewLoadExecFileFromFile(AFileName: TFileName) : TExecutableFile; // uses TTextFileStream
    function LoadExecFileFromFile(AFileName: TFileName) : TExecutableFile;    // uses TextFile
    function SaveExecFileToFile(ExecFile: TExecutableFile; AFileName: TFileName; SaveOptions: TSaveOptions): boolean;
    function CreateNewExecFile(AFileName: TFileName): TExecutableFile;

//    procedure InitLoadExecFileFromFile(AFileName: TFileName);
    function ThreadLoadExecFileFromFile(AFileName: TFileName) : TExecutableFile;
//    function FinishLoadExecFileFromFile: TExecutableFile;
    
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
    InputFile:=TFileStream.Create(aFileName,fmShareDenyNone);
  except
    fError:=errOpen;
    Exit;
  end;

  // Create appropriate ExecFile object
  FileFormat:= GetExecFileFormat(InputFile);
  case FileFormat of
    PE:  result:=TPEFile.Create(InputFile, aFileName);
    COM: result:=TCOMFile.Create(InputFile, aFileName);
    MZ:  result:=TMZFile.Create(InputFile, aFileName);
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
            c_PESign: result:=PE;                     // Portable Executable 32-bit
            c_NESign: result:=NE;                     // New Executable 16-bit
            c_LESign: result:=ffUnknown; //LE;                     // Linear Executable
            c_LXSign: result:=ffUnknown; //LX;                     // Linear Executable 32 - bit
            else
              result:=ffUnknown;
          end;
        end
        else begin                                  // MZ file format
          result:=MZ;
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
          result:=COM
        else
          result:=ffUnknown;
    end;
  end
  else
    result:=ffUnknown;


end;



function TExecFileManager.LoadExecFileFromFile(aFileName: TFileName): TExecutableFile;
var
    DHF_FileName, DAS_FileName: string;
    DHF: TMemoryStream;
    DAS: TextFile;
    Version: cardinal;
    ProjectFileName: string;
    ProjectFileSize: cardinal;
    ProjectExecFileFormat: TExecFileFormat;
    temps: string;
begin
  DHF_FileName:=aFileName;
  DAS_FileName:=ChangeFileExt(DHF_FileName,'.das');
// toto osetrenie presunut inam:
  if not FileExists(DAS_FileName) then begin
    fError:=errDASNotFound;
//    MessageDlg(FileNotFoundStr + '"' + DAS_FileName+'" !',mtError,[mbOK],0);
    result:=nil;
    Exit;
  end;

  DHF:=TMemoryStream.Create;
  DHF.LoadFromFile(DHF_FileName);
  DHF.Read(Version,4);
  case Version of
    TatraDASProjectVersion: begin
      ReadStringFromStream(DHF, 4, ProjectFileName);                         // Nazov disassemblovaneho suboru
      DHF.Position:=68;
      DHF.Read(ProjectFileSize,4);                                              // Velkost disassemblovaneho suboru
      DHF.Read(ProjectExecFileFormat,sizeof(TExecFileFormat));                          // Format disassemblovaneho suboru
      case ProjectExecFileFormat of
        PE: result:=TPEFile.Create;
        MZ: result:=TMZFile.Create;
        COM: result:=TCOMFile.Create;
//        NE: result:=TNEFile.Create(Ctrls);
//        ELF: result:=TELFFile.Create(Ctrls);
//        Unknown: result:=TUnknownFile.Create(ctrls);
      end;
    // upravit  
    //  OnExecFileCreateSection:=MainForm.CreateSection;
      AssignFile(DAS,DAS_FileName);
      Reset(DAS);
      Readln(DAS,temps);
      Readln(DAS,temps);
      if not result.LoadFromFile(DAS,DHF) then begin
        DHF.Free;
        CloseFile(DAS);
        Exit;
      end;
      CloseFile(DAS);
    end;

    NewTatraDASProjectVersion: begin
      DHF.Read(ProjectExecFileFormat,SizeOf(TExecFileFormat));                          // Format disassemblovaneho suboru
      case ProjectExecFileFormat of
        PE: result:=TPEFile.Create;
//        MZ: ExecFile:=TMZFile.Create(ctrls);
//        NE: ExecFile:=TNEFile.Create(Ctrls);
        COM: result:=TCOMFile.Create;
//        ELF: ExecFile:=TELFFile.Create(Ctrls);
//        Unknown: ExecFile:=TUnknownFile.Create(ctrls);
      end;

      AssignFile(DAS,DAS_FileName);
      Reset(DAS);
      Readln(DAS,temps);
      Readln(DAS,temps);
      if not result.LoadFromFileEx(DAS,DHF) then begin
        DHF.Free;
        CloseFile(DAS);
        Exit;
      end;
      CloseFile(DAS);
    end
    else begin
// presunut inam    
//      Showmessage(InCompatibleProjectVersion + ' ('+IntToHex(Version,8)+') !'+#13+CurrentVersion + ' ' + IntToHex(TatraDASProjectVersion,8) + '.');
      DHF.Free;
      Exit;
    end;
  end;
  DHF.Free;
end;



function TExecFileManager.NewLoadExecFileFromFile(aFileName: TFileName): TExecutableFile;
var
    DHF_FileName: string;
    DAS_FileName: string;
    DHF: TFileStream;
    DAS: TTextFileStream;
    Version: cardinal;
    ProjectFileName: string;
    ProjectFileSize: cardinal;
    ProjectExecFileFormat: TExecFileFormat;
begin
  DHF_FileName:=aFileName;
  DAS_FileName:=ChangeFileExt(DHF_FileName,'.das');
// toto osetrenie presunut inam:
  if not FileExists(DAS_FileName) then begin
// presunut inam    
//    MessageDlg(FileNotFoundStr + '"' + DAS_FileName+'" !',mtError,[mbOK],0);
    result:=nil;
    Exit;
  end;

  DHF:=TFileStream.Create(DHF_FileName,fmShareDenyNone);
  DHF.Read(Version,4);
  case Version of
    TatraDASProjectVersion: begin
      ReadStringFromStream(DHF, 4, ProjectFileName);                         // Nazov disassemblovaneho suboru
      DHF.Position:=68;
      DHF.Read(ProjectFileSize,4);                                              // Velkost disassemblovaneho suboru
      DHF.Read(ProjectExecFileFormat,sizeof(TExecFileFormat));                          // Format disassemblovaneho suboru
      case ProjectExecFileFormat of
        PE: result:=TPEFile.Create;
        MZ: result:=TMZFile.Create;
//        NE: ExecFile:=TNEFile.Create(Ctrls);
        COM: result:=TCOMFile.Create;
//        ELF: ExecFile:=TELFFile.Create(Ctrls);
//        Unknown: ExecFile:=TUnknownFile.Create(ctrls);
        else begin
          DHF.Free;
          DAS.Free;
          fError:=errBadFormat;
          Result:=nil;
          Exit;
        end;
      end;
// upravit
//      OnExecFileCreateSection:=MainForm.CreateSection;
      DAS:=TTextFileStream.Create(DAS_FileName,fmOpenRead);
      DAS.ReadLine;
      DAS.ReadLine;
      if not result.LoadFromFile(DHF,DAS) then begin
        DHF.Free;
        DAS.Free;
        Exit;
      end;
      DAS.Free;
    end;

    {
    NewTatraDASProjectVersion: begin
      DHF.Read(ProjectExecFileFormat,SizeOf(TExecFileFormat));                          // Format disassemblovaneho suboru
      case ProjectExecFileFormat of
        PE: result:=TPEFile.Create;
//        MZ: ExecFile:=TMZFile.Create(ctrls);
//        NE: ExecFile:=TNEFile.Create(Ctrls);
//        COM: result:=TCOMFile.Create;
//        ELF: ExecFile:=TELFFile.Create(Ctrls);
//        Unknown: ExecFile:=TUnknownFile.Create(ctrls);
      end;

      AssignFile(DAS,DAS_FileName);
      Reset(DAS);
      Readln(DAS,temps);
      Readln(DAS,temps);
      if not result.LoadFromFileEx(DAS,DHF) then begin
        DHF.Free;
        CloseFile(DAS);
        Exit;
      end;
      CloseFile(DAS);
    end
    }
    else begin
// presunut inam    
//      Showmessage(InCompatibleProjectVersion + ' ('+IntToHex(Version,8)+') !'+#13+CurrentVersion + ' ' + IntToHex(TatraDASProjectVersion,8) + '.');
      DHF.Free;
      Exit;
    end;
  end;
  DHF.Free;
end;



function TExecFileManager.SaveExecFileToFile(ExecFile: TExecutableFile; AFileName: TFileName; SaveOptions: TSaveOptions): boolean;
var
  DAS: TextFile;
  DHF: TFileStream;
  DHF_FileName: string;
  DAS_FileName: string;
  Version: cardinal;
begin

  // Save as TatraDAS Project
  if soProject in SaveOptions then begin

    // Initialize DHF project file
    DHF_FileName:=ChangeFileExt(aFileName,'.dhf');;
    try
      DHF:= TFileStream.Create(DHF_FileName, fmCreate);
    except
      fError:=errSave;
      DHF.Free;
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

  if result then begin
    DeleteFile(DHF_FileName);
    DeleteFile(DAS_FileName);
  end;
end;



function TExecFileManager.ThreadLoadExecFileFromFile(AFileName: TFileName): TExecutableFile;
begin

end;

end.
