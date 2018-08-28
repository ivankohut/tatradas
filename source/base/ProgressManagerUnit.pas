unit ProgressManagerUnit;

interface

uses
  Classes, SyncObjs, AbstractProgressManager;

const
  cRefreshInterval = 50; // milliseconds

type
  TShowProgressProc = procedure(APhase: string; AProgress: Double);

  TProgressManager = class(TAbstractProgressManager)
  private
    fFinished: Boolean;
    fMaximum: Cardinal;
    fPhaseName: string;
    fShowProgressProc: TShowProgressProc;

    fLock: TCriticalSection;
    fFinishedPhases: TStrings;
    procedure FinishPhase;
    procedure ShowFinishedPhases; // Ensures all phases are shown (even the shortest, at least 100% progress is shown)
  public
    // Methods supposed to called from main thread
    constructor Create(AShowProgressProc: TShowProgressProc);
    destructor Destroy; override;
    procedure StartProgress(AThread: TThread); override;

    // Methods supposed to be called from slave thread
    procedure StartPhase(AName: string; AMaximum: Cardinal); override;
    procedure Finish(ASuccessful: Boolean); override;
  end;


implementation


uses
  LoggerUnit,
  ProgressThreads,
  SysUtils;



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
      fShowProgressProc(fPhaseName, Position / fMaximum);
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



procedure TProgressManager.StartPhase(AName: string; AMaximum: Cardinal);
begin
  FinishPhase;
  fPhaseName := AName;
  Position := 0;
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



end.
