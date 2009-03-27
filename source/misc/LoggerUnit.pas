unit LoggerUnit;

interface

uses
  Classes, SysUtils, Contnrs;


const
  LoggerTimeStampFormat = 'yyyy-mm-dd hh:nn:ss:zzz';

type

  TLoggerListener = class
    procedure WriteMessage(Msg: string); virtual; abstract;
  end;

  TLogLevel = (llTrace, llDebug, llInfo, llWarning, llError, llFatal);

  TLogger = class
  private
    fLogLevel: TLogLevel;
    fListeners: TObjectList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Trace(Msg: string);
    procedure Debug(Msg: string);
    procedure Info(Msg: string);
    procedure Warning(Msg: string);
    procedure Error(Msg: string);
    procedure Fatal(Msg: string);
    procedure Log(Level: TLogLevel; Msg: string);

    procedure AddListener(Listener: TLoggerListener);
    procedure RemoveListener(Listener: TLoggerListener);
    property LogLevel: TLogLevel read fLogLevel write fLogLevel;
  end;


  { TTextFileLoggerListener }

  TTextFileLoggerListener = class(TLoggerListener)
  private
    fLogFileName: string;
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
  fListeners := TObjectList.Create(True);
end;



destructor TLogger.Destroy;
begin
  fListeners.Free;
end;



procedure TLogger.Trace(Msg: string);
begin
  Log(llTrace, Msg);
end;



procedure TLogger.Debug(Msg: string);
begin
  Log(llDebug, Msg);
end;



procedure TLogger.Info(Msg: string);
begin
  Log(llInfo, Msg);
end;



procedure TLogger.Warning(Msg: string);
begin
  Log(llWarning, Msg);
end;



procedure TLogger.Error(Msg: string);
begin
  Log(llError, Msg);
end;



procedure TLogger.Fatal(Msg: string);
begin
  Log(llFatal, Msg);
end;



procedure TLogger.Log(Level: TLogLevel; Msg: string);
var
  ListenerIndex: integer;
  LogMessage, TimeStampStr: string;
begin
  if Level < fLogLevel then
    Exit;

  case Level of
    llTrace:   LogMessage := 'TRACE - ' + Msg;
    llDebug:   LogMessage := 'DEBUG - ' + Msg;
    llInfo:    LogMessage := 'INFO  - ' + Msg;
    llWarning: LogMessage := 'WARN  - ' + Msg;
    llError:   LogMessage := 'ERROR - ' + Msg;
    llFatal:   LogMessage := 'FATAL - ' + Msg;
  end;
  DateTimeToString(TimeStampStr, LoggerTimeStampFormat, Now);
  LogMessage := '[' + TimeStampStr + ']   ' + LogMessage;

  for ListenerIndex:= 0 to fListeners.Count - 1 do
    (fListeners[ListenerIndex] as TLoggerListener).WriteMessage(LogMessage);
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
  fLogFileName := AFileName;
  {
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
  }
end;



destructor TTextFileLoggerListener.Destroy;
begin
//  CloseFile(fLogFile);
  inherited Destroy;
end;



procedure TTextFileLoggerListener.WriteMessage(msg: string);
var
  fLogFile: TextFile;
begin
  try
    AssignFile(fLogFile, fLogFileName);
    if FileExists(fLogFileName) then
      System.Append(fLogFile)
    else
      System.Rewrite(fLogFile);

    WriteLn(fLogFile, msg);
  finally
    CloseFile(fLogFile);
  end;
end;



initialization
  Logger:= TLogger.Create;

finalization
  Logger.Free;

end.
