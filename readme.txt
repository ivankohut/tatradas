You can compile TatraDAS as Windows GUI application using Delphi
or as console application using Delphi(Windows) or Free Pascal(Windows, Linux) or Kylix(Linux).

Windows GUI application:
  Requirements:
    - Delphi 6
    - SynEdit(http://synedit.sf.net) & MPHexEditor(http://www.mirkes.de) components
  How to compile:
    - unpack the source archive
    - copy Synedit sources to "tatradas_source_dir\third\synedit"
    - copy MPHexEditor sources to "tatradas_source_dir\third\mphexeditor"
      - launch Delphi
      - open project file "tatradas_source_dir\source\2.9.7\TatraDAS.dpr"
      - press F9 to compile and run TatraDAS (you can find "TatraDAS.exe" file in "tatradas_source_dir\build\" directory)
    OR
      - execute "tatradas_source_dir\build.bat" batch file

Console application:

 Delphi:
  Requirements:
    - Delphi
  How to compile:
    - unpack the source archive
    - launch "cmd.exe"(Windows 2000/XP) or "command.com"(Windows 9x) and browse to "tatradas_source_dir\source\2.9.7\" directory
    - execute this command "dcc32 -CC TatraDAS.dpr" (you can find "TatraDAS.exe" file in "tatradas_source_dir\build\" directory)

 Free Pascal under Windows:
  Requirements:
    - Free Pascal 1.9.5 (TatraDAS compiled using Free Pascal < 1.9.5 crashes)
  How to compile:
    - unpack the source archive
    - launch "cmd.exe"(Windows 2000/XP) or "command.com"(Windows 9x) and browse to "tatradas_source_dir\source\2.9.7\" directory
    - execute this command "fpc TatraDAS.dpr"
    - you can find "TatraDAS.exe" file in "tatradas_source_dir\source\2.9.7\" directory

 Free Pascal under Linux:
  Requirements:
    - Free Pascal 1.9.5 (TatraDAS compiled using Free Pascal < 1.9.5 crashes)
  How to compile:
    - unpack the source archive
    - browse to "tatradas_source_dir/source/2.9.7/" directory
    - execute this command "fpc TatraDAS.dpr" 
    - you can find "TatraDAS" file in "tatradas_source_dir/source/2.9.7/" directory
    
 Kylix:
  Requirements:
    - Kylix (I do not know which version, I use Kylix 3)
  How to compile:
    - unpack the source archive
    - browse to "tatradas_source_dir/source/2.9.7/" directory
    - execute this command "dcc -CC TatraDAS.dpr" 
    - you can find "TatraDAS" file in "tatradas_source_dir/source/2.9.7/" directory
    
Ivan Kohut 2004
ivankohut@host.sk 
