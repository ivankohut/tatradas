unit ULoadFromFileThread;

interface

uses Classes;

type

  TProgressThread = class(TThread)
    constructor Create(ProgressText: string; ProgressMax: cardinal);
   private
    fProgressText: string;
    fProgressMax: cardinal;
   end;


  TLoadFromFileThread = class(TProgressThread)
   protected
    procedure Execute; override;
   public
    DataFile: TextFile;
//    constructor Create(ProgressText: string; ProgressMax: cardinal);
  end;

implementation


constructor TProgressThread.Create(ProgressText: string; ProgressMax: cardinal);
begin
  inherited Create(true);
  fProgressText:=ProgressText;
  fProgressMax:=ProgressMax;
end;
{
constructor TLoadFromFileThread.Create(ProgressText: string; ProgressMax: cardinal);
begin
  inherited Create(true);
end;
}
procedure TLoadFromFileThread.Execute;
var line: string;
begin
  while (not EOF(f)) and (not (ProgressPosition = ProgressMax)) do begin
    Inc(ProgressPosition);
    ReadLn(f,line);
    Disassembled.Add(line);
    if i >= counter then begin
{///
      if not Ctrls.ProgressFunction(i,LinesCount,'') then begin
        Ctrls.ProgressFunction(0,0,'');
        Exit;
      end;
}
      inc(counter,1000);
    end;
  end;
end;

end.
