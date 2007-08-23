unit TatraDASFormUnit;

interface

uses
  Forms,
  IniFiles;

type

  TTatraDASForm = class(TForm)
    procedure Translate(INI: TMemIniFile); virtual; abstract;
  end;

implementation

end.
 