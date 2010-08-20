unit GlobalsUnit;

interface

uses
  AbstractProgressManager;
  
type  
  TProgressData = record
    AbortExecution: Boolean;
    Result: Pointer;
  end;

var
  ProgressData: TProgressData;
  ProgressManager: TAbstractProgressManager;

implementation

end.