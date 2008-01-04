Compilation instructions for TatraDAS disassembler 2.9.8
--------------------------------------------------------

You can compile TatraDAS as Windows GUI application using Delphi
or as console application using Delphi (Windows) or Free Pascal (Windows, Linux) or Kylix (Linux).

Windows GUI application:
  Requirements:
    - Delphi 6, 7, 2005, 2006
    - SynEdit(http://synedit.sf.net) & MPHexEditor(http://www.mirkes.de) components
  How to compile:
    - unpack the source archive
    - copy Synedit sources to "tatradas_source_dir\third\synedit"
    - copy MPHexEditor sources to "tatradas_source_dir\third\mphexeditor"
      - launch Delphi
      - open project file "tatradas_source_dir\source\TatraDAS.dpr"
      - press F9 to compile and run TatraDAS
    OR
      - execute "tatradas_source_dir\build.bat" batch file
    - the EXE file named "TatraDAS.exe" will be placed in "tatradas_source_dir\" directory

Console application:

 Delphi (Windows):
  Requirements:
    - Delphi
  How to compile:
    - unpack the source archive
    - launch "cmd.exe" (Windows 2000/XP) or "command.com" (Windows 9x) and browse "tatradas_source_dir\source\" directory
    - execute this command: "dcc32 -CC -B TatraDAS.dpr"
    - the EXE file named "TatraDAS.exe" will be placed in "tatradas_source_dir\" directory)

 Free Pascal (Windows):
  Requirements:
    - Free Pascal 2.2.0
  How to compile:
    - unpack the source archive
    - launch "cmd.exe" (Windows 2000/XP) or "command.com" (Windows 9x) and browse "tatradas_source_dir\source\" directory
    - execute this command "fpc -Sd -B -O3 TatraDAS.dpr"
    - the EXE file named "TatraDAS.exe" will be placed in "tatradas_source_dir\source\" directory

 Free Pascal (Linux):
  Requirements:
    - Free Pascal 2.2.0
  How to compile:
    - unpack the source archive
    - browse "tatradas_source_dir/source/" directory
    - execute this command "fpc -Sd -B -O3 TatraDAS.dpr"
    - the EXE file named "TatraDAS" will be placed in "tatradas_source_dir/source/" directory

 Kylix (Linux):
  Requirements:
    - Kylix 3
  How to compile:
    - unpack the source archive
    - browse "tatradas_source_dir/source/" directory
    - execute this command "dcc -CC -B TatraDAS.dpr"
    - the EXE file named "TatraDAS" will be placed in "tatradas_source_dir/source/" directory

Ivan Kohut 2007
ivankohut@host.sk
