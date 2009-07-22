unit ProgressManagerUnit;

interface

uses
  Classes, SyncObjs;

const
  cRefreshInterval = 50; // milliseconds

type
  TShowProgressProc = procedure (APhase: string; AProgress: Double);

  TProgressManager = class
  private
    fFinished: Boolean;
    fPosition: Cardinal;
    fMaximum: Cardinal;
    fPhaseName: String;
    fShowProgressProc: TShowProgressProc;

    fLock: TCriticalSection;
    fFinishedPhases: TStrings;
    procedure FinishPhase;
    procedure ShowFinishedPhases; // Ensures all phases are shown (even the shortest, at least 100% progress is shown)
  public
    // Methods supposed to called from main thread
    constructor Create(AShowProgressProc: TShowProgressProc);
    destructor Destroy; override;
    procedure StartProgress(AThread: TThread);

    // Methods supposed to be called from slave thread
    procedure StartPhase(AName: String; AMaximum: Cardinal);
    procedure Finish(ASuccessful: Boolean);
    procedure IncPosition; 
    property Position: Cardinal read fPosition write fPosition;
  end;


implementation


uses
  LoggerUnit, SysUtils, ProgressThreads;


constructor TProgressManager.Create(AShowProgressProc: TShowProgressProc);
begin
  fShowProgressProc := AShowProgressProc;
  fFinished := True;
  fFinishedPhases := TStringList.Create;
  fLock := TCriticalSection.Create;
end;



destructor TProgressManager.Destroy;
begin
  fLock.Acquire;
  try
    fFinishedPhases.Free;
  finally
    fLock.Release;
    fLock.Free;
  end;
end;



procedure TProgressManager.ShowFinishedPhases;
var
  FinishedPhaseIndex: Integer;
begin
  fLock.Acquire;
  try
    for FinishedPhaseIndex := 0 to fFinishedPhases.Count - 1 do
      fShowProgressProc(fFinishedPhases[FinishedPhaseIndex], 1);
    fFinishedPhases.Clear;
  finally
    fLock.Release;
  end;
end;



procedure TProgressManager.StartProgress(AThread: TThread);
var
  ProgressThread: TProgressThread;
begin
  fFinished := False;
  AThread.Resume;
  while not fFinished do begin
    ShowFinishedPhases;
    if (fPhaseName <> '') and (fMaximum > 0) then
      fShowProgressProc(fPhaseName, fPosition / fMaximum);
    Sleep(cRefreshInterval);
  end;

  ShowFinishedPhases;
  AThread.WaitFor;

  // Reraise thread's exception in main thread
  if (AThread is TProgressThread) then begin
    ProgressThread := (AThread as TProgressThread);
    if ProgressThread.WasException then
      raise ProgressThread.ExceptionClass.Create(ProgressThread.ExceptionMessage);
  end;
end;



procedure TProgressManager.StartPhase(AName: String; AMaximum: Cardinal);
begin
  FinishPhase;
  fPhaseName := AName;
  fPosition := 0;
  fMaximum := AMaximum;
end;



procedure TProgressManager.Finish(ASuccessful: Boolean);
begin
  if not fFinished then begin
    if ASuccessful then
      FinishPhase;
    fFinished := True;
  end;
end;



procedure TProgressManager.FinishPhase;
begin
  // Only if have a phase to finish
  if fPhaseName <> '' then begin
    fLock.Acquire;
    try
      fFinishedPhases.Add(fPhaseName);
    finally
      fLock.Release;
    end;
  end;
end;



procedure TProgressManager.IncPosition;
begin
  Inc(fPosition);
end;



end.