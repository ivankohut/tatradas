{
  Trieda TWorker umoznuje vykonat skupinu nezavislych uloh pomocou viacerych
  vlakien spustenych sucasne. Kazde vlakno dostane jednu ulohu a ked ju dokonci,
  tak dostane dalsiu. Takto to funguje az kym sa ulohy neminu.
  Vsetky ulohy
  Jedna uloha znamena vykonanie procedury typu TWorkerJob s parametrom typu
  TObject, ktory obsahuje potrebne parametre (jednotlive ulohy sa lisia len
  tymto objektom, procedura je stale ta ista). Obhjekty pre ulohy a proceduru
  poskytuje uzivatel ako parametre do metody Work.
  Ulohy musia byt nezavisle. Ak pracuju s rovnakymi datami, tak uzivatel musi
  zabezpecit synchronizaciu.
  Metoda Work caka, kym vsetky vlakna dokoncia svoju poslednu ulohu a potom
  skonci, teda ak je nejaka posledna uloha vacsia ako ostatne, tak sa moze stat,
  ze vsetky vlakna uz budu hotove a bude sa cakat na posledne, kym dokonci
  svoju velku ulohu.

  Ivan Kohut 2008
}

unit Workers;

interface

uses
  Classes,
  Syncobjs,
  Contnrs;

type

  TWorkerJob = procedure (ATask: TObject);

  TWorker = class
  private
    fTasks: TObjectQueue;
    fCriticalSection: TCriticalSection;
    fJob: TWorkerJob;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Work(ATasks: TObjectQueue; AJob: TWorkerJob; const AThreadCount: integer);
  end;


implementation


type

  { TWorkerThread }

  TWorkerThread = class(TThread)
  private
    fWorker: TWorker;
  public
    constructor Create(CreateSuspended: Boolean; AWorker: TWorker);
    procedure Execute; override;
  end;


{ TWorker }


constructor TWorker.Create;
begin
  fCriticalSection := TCriticalSection.Create;
end;



destructor TWorker.Destroy;
begin
  fCriticalSection.Free;
end;



procedure TWorker.Work(ATasks: TObjectQueue; AJob: TWorkerJob; const AThreadCount: integer);
var
  ThreadPool: array of TThread;
  i: integer;
begin
  fTasks := ATasks;
  fJob := AJob;
  SetLength(ThreadPool, AThreadCount);
  for i := 0 to AThreadCount - 1 do begin
    ThreadPool[i] := TWorkerThread.Create(false, self);
  end;

  for i := 0 to AThreadCount - 1 do begin
    ThreadPool[i].WaitFor;
    ThreadPool[i].Free;
  end;
end;


{ TWorkerThread }


constructor TWorkerThread.Create(CreateSuspended: Boolean; AWorker: TWorker);
begin
  inherited Create(CreateSuspended);
  fWorker := AWorker;
end;



procedure TWorkerThread.Execute;
var
  TheTask: TObject;
  Finished: boolean;
begin
  Finished := false;
  while True do begin

    fWorker.fCriticalSection.Acquire;
    if fWorker.fTasks.Count > 0 then
      TheTask := fWorker.fTasks.Pop
    else
      Finished := true;
    fWorker.fCriticalSection.Release;

    if Finished then
      Break;

    fWorker.fJob(TheTask);
  end;
end;


end.
