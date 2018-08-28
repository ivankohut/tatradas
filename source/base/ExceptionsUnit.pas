unit ExceptionsUnit;

interface

uses
  SysUtils;

type
  // Exceptions - general exceptions which can be used everywhere in TatraDAS

  ETatraDASException = class(Exception);

  // Raise in case of unexpected state = indicates bug
  // Messages in English only
  EIllegalState = class(ETatraDASException);

  // User exceptions,
  // Should be translated
  // User terminated a process
  //  EUserTerminatedProcess = class (ETatraDASException);
  EFileCorrupted = class(ETatraDASException); // FileCorruptedStr // TODO

implementation

end.
