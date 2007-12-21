unit LoggerUnit;

interface

uses
  Classes, Contnrs,
  SysUtils;


const
  LoggerTimeStampFormat = 'yyyy-mm-dd hh:nn:ss:zzz';  

type

  TLoggerListener = class
    procedure WriteMessage(msg: string); virtual; abstract;
  end;


  TLogger = class
  private
    fListeners: TObjectList;

    procedure WriteMessage(msg: string);

  public
    constructor Create;
    destructor Destroy; override;
    procedure Info(Msg: string);
    procedure Warning(Msg: string);
    procedure Fatal(Msg: string);

    procedure AddListener(Listener: TLoggerListener);
    procedure RemoveListener(Listener: TLoggerListener);
  end;


  { TTextFileLoggerListener }

  TTextFileLoggerListener = class(TLoggerListener)
  private
    fLogFile: TextFile;
  public
    constructor Create(AFilename: string);
    destructor Destroy; override;
    procedure WriteMessage(msg: string); override;
  end;

var
  Logger: TLogger;


implementation


constructor TLogger.Create;
begin
  fListeners:= TObjectList.Create;
end;



destructor TLogger.Destroy;
begin
  fListeners.OwnsObjects:= true;
  fListeners.Free;
end;



procedure TLogger.WriteMessage(msg: string);
var
  ListenerIndex: integer;
  TimeStampStr: string;
begin
  DateTimeToString(TimeStampStr, LoggerTimeStampFormat, Now);
  msg:= '[' + TimeStampStr + ']   ' + msg;
  for ListenerIndex:= 0 to fListeners.Count - 1 do
    (fListeners[ListenerIndex] as TLoggerListener).WriteMessage(msg);
end;



procedure TLogger.Info(Msg: string);
begin
  WriteMessage('INFO - ' + msg);
end;



procedure TLogger.Warning(Msg: string);
begin
  WriteMessage('WARNING - ' + msg);
end;



procedure TLogger.Fatal(Msg: string);
begin
  WriteMessage('FATAL - ' + msg);
end;



procedure TLogger.AddListener(Listener: TLoggerListener);
begin
  fListeners.Add(Listener);
end;



procedure TLogger.RemoveListener(Listener: TLoggerListener);
begin
  fListeners.Remove(Listener);
end;


{ TTextFileLoggerListener }


constructor TTextFileLoggerListener.Create(AFilename: string);
begin
  try
    AssignFile(fLogFile, AFilename);
    if FileExists(AFilename) then
      System.Append(fLogFile)
    else
      System.Rewrite(fLogFile);

  except
    CloseFile(fLogFile);
    raise Exception.Create('Logger listener file opening failed.');
  end;
end;



destructor TTextFileLoggerListener.Destroy;
begin
  CloseFile(fLogFile);
  inherited Destroy;
end;



procedure TTextFileLoggerListener.WriteMessage(msg: string);
begin
  WriteLn(fLogFile, msg);
end;


  
initialization
  Logger:= TLogger.Create;

finalization
  Logger.Free;

end.
