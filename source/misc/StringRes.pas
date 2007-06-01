{ DONE:
}

unit StringRes;

interface

uses INIFiles;

procedure TranslateStrings(ini: TMemINIFile; error: string);

var
// Files
  FileNotFoundStr: string = 'Could not find file ';
  CouldNotOpenFileStr: string = 'Could not open file ';
  CouldNotOpenProjectStr: string ='Could not open project !';
  CouldNotOpenReadWriteFileStr: string = 'Could not open file for write access';
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

procedure TranslateStrings(ini: TMemINIFile; error: string);
begin

  FileNotFoundStr:=ini.ReadString('Texty','FileNotFound',error);
  CouldNotOpenFileStr:=ini.ReadString('Texty','CouldNotOpenFile',error);
  CouldNotOpenProjectStr:=ini.ReadString('Texty','CouldNotOpenProject',error);
  CouldNotOpenReadWriteFileStr:=ini.ReadString('Texty','CouldNotOpenReadWrite',error);
  ProjectFilterStr:=ini.ReadString('Texty','ProjectFilter',error);
  SaveDisassemblyFilterStr:=ini.ReadString('Texty','SaveDisassemblyFilter',error);
  FileModifiedStr:=ini.ReadString('Texty','FileModified',error);

  InvalidAddressStr:=ini.ReadString('Texty','InvalidAddress',error);
  InvalidStartOffsetStr:=ini.ReadString('Texty','InvalidStartOffset',error);
  InvalidSizeStr:=ini.ReadString('Texty','InvalidSize',error);
  InvalidEntrypointStr:=ini.ReadString('Texty','InvalidEntryPoint',error);
  FileOffsetStr:=ini.ReadString('Texty','FileOffset',error);
  SectionOffsetStr:=ini.ReadString('Texty','SectionOffset',error);

  CodeSectionStr:=ini.ReadString('Texty','CodeSection',error);
  SectionStr:=ini.ReadString('Texty','Section',error);

  InCompatibleProjectVersion:=ini.ReadString('Texty','InCompatibleProjectVersion',error);
  CurrentVersion:=ini.ReadString('Texty','CurrentVersion',error);

  DivisionByZeroStr:=ini.ReadString('Texty','DivisionByZero',error);
  NotFoundStr:=ini.ReadString('Texty','NotFound',error);
end;

end.
