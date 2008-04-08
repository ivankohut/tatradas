unit StringRes;

interface

var
// Files
  FileNotFoundStr: string = 'Could not find file ';
  CouldNotOpenFileStr: string = 'Could not open file ';
  CouldNotOpenProjectStr: string ='Could not open project !';
  CouldNotOpenReadWriteFileStr: string = 'Could not open file for write access';
  CouldNotFindDASFileStr: string = 'Could not find project DAS file';
  ProjectFilterStr: string = 'Disassembled file ';
  SaveDisassemblyFilterStr: string = 'Disassembled file ';
  ProjectModifiedStr: string = 'Project <project> has been modified. Do you want to save changes ?';
  FileModifiedStr: string = 'File <filename> has been modified. Do you want to save changes ?';

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


end.
