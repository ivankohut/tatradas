{ DONE:
}

unit StringRes;

interface

uses
  IniFiles,

  procmat;

procedure TranslateStrings(ini: TMemINIFile);

var
// Files
  FileNotFoundStr: string = 'Could not find file ';
  CouldNotOpenFileStr: string = 'Could not open file ';
  CouldNotOpenProjectStr: string ='Could not open project !';
  CouldNotOpenReadWriteFileStr: string = 'Could not open file for write access';
  CouldNotFindDASFileStr: string = 'Could not find project DAS file';
  ProjectFilterStr: string = 'Disassembled file ';
  SaveDisassemblyFilterStr: string = 'Disassembled file ';
  FileModifiedStr: string = 'File has been modified. Do you want to save changes ?';

// Address
  InvalidAddressStr: string = 'Invalid address';
  InvalidStartOffsetStr: string = 'Invalid start offset';
  InvalidSizeStr: string = 'Invalid size';
  InvalidEntrypointStr: string = 'Invalid entrypoint';
  FileOffsetStr: string = 'File offset';
  SectionOffsetStr: string = 'Section offset';

// Section
  CodeSectionStr: string = 'Code Section #';
  SectionStr: string = 'Section';

// TatraDAS general
  InCompatibleProjectVersion: string = 'Incompatible Project version';
  CurrentVersion: string = 'Current version is';
  NoLanguageFilesStr: string = 'Unable to find any language file. Exiting.';

// Common
  DivisionByZeroStr: string = 'Division by zero!';
  NotFoundStr: string = 'was not found!';

implementation

procedure TranslateStrings(ini: TMemINIFile);
begin

  FileNotFoundStr:=ini.ReadString('Texty','FileNotFound',TranslateErrorStr);
  CouldNotOpenFileStr:=ini.ReadString('Texty','CouldNotOpenFile',TranslateErrorStr);
  CouldNotOpenProjectStr:=ini.ReadString('Texty','CouldNotOpenProject',TranslateErrorStr);
  CouldNotOpenReadWriteFileStr:=ini.ReadString('Texty','CouldNotOpenReadWrite',TranslateErrorStr);
  ProjectFilterStr:=ini.ReadString('Texty','ProjectFilter',TranslateErrorStr);
  SaveDisassemblyFilterStr:=ini.ReadString('Texty','SaveDisassemblyFilter',TranslateErrorStr);
  FileModifiedStr:=ini.ReadString('Texty','FileModified',TranslateErrorStr);

  InvalidAddressStr:=ini.ReadString('Texty','InvalidAddress',TranslateErrorStr);
  InvalidStartOffsetStr:=ini.ReadString('Texty','InvalidStartOffset',TranslateErrorStr);
  InvalidSizeStr:=ini.ReadString('Texty','InvalidSize',TranslateErrorStr);
  InvalidEntrypointStr:=ini.ReadString('Texty','InvalidEntryPoint',TranslateErrorStr);
  FileOffsetStr:=ini.ReadString('Texty','FileOffset',TranslateErrorStr);
  SectionOffsetStr:=ini.ReadString('Texty','SectionOffset',TranslateErrorStr);

  CodeSectionStr:=ini.ReadString('Texty','CodeSection',TranslateErrorStr);
  SectionStr:=ini.ReadString('Texty','Section',TranslateErrorStr);

  InCompatibleProjectVersion:=ini.ReadString('Texty','InCompatibleProjectVersion',TranslateErrorStr);
  CurrentVersion:=ini.ReadString('Texty','CurrentVersion',TranslateErrorStr);

  DivisionByZeroStr:=ini.ReadString('Texty','DivisionByZero',TranslateErrorStr);
  NotFoundStr:=ini.ReadString('Texty','NotFound',TranslateErrorStr);
end;

end.
