unit AbstractProgressManager;

interface

uses
  Classes, SyncObjs;

type
  TShowProgressProc = procedure (APhase: string; AProgress: Double);

  TAbstractProgressManager = class
  private
    fPosition: Cardinal;
  public
    // Methods supposed to called from main thread
    procedure StartProgress(AThread: TThread); virtual; abstract;

    // Methods supposed to be called from slave thread
    procedure StartPhase(AName: String; AMaximum: Cardinal); virtual; abstract;
    procedure Finish(ASuccessful: Boolean); virtual; abstract;
    procedure IncPosition; 
    property Position: Cardinal read fPosition write fPosition;
  end;                  

implementation


procedure TAbstractProgressManager.IncPosition;
begin
  Inc(fPosition);
end;



end.