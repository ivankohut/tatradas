set TATRADAS_VERSION=299
set 

rd /S /Q release
mkdir release

REM Compile TatraDAS - console version
cd source
dcc32.exe -B -CC TatraDAS.dpr
cd ..

REM Create console version ZIP package
zip -9 release\tdascon%TATRADAS_VERSION%.zip TatraDAS.exe

REM Compile TatraDAS - GUI version 
cd source
dcc32.exe -B TatraDAS.dpr
cd ..

REM Compile and run unit tests
cd test
dcc32.exe -B -CC TestTatraDAS.dpr
TestTatraDAS.exe
cd ..

REM Create installer package
..\tools\inno_setup\iscc.exe TatraDAS.iss

REM Create sources ZIP package
rd /S /Q build
cd source
cmd /c clean.bat
cd ..
cd test
cmd /c clean.bat
cd ..

mkdir build\tatradas
copy TatraDAS.iss build\tatradas
copy readme.txt build\tatradas
copy copying build\tatradas
copy build.bat build\tatradas
copy changelog.txt build\tatradas
echo .svn > excludes.txt
mkdir build\tatradas\images
mkdir build\tatradas\languages
mkdir build\tatradas\source
mkdir build\tatradas\test
xcopy images build\tatradas\images /E /EXCLUDE:excludes.txt
xcopy languages build\tatradas\languages /E /EXCLUDE:excludes.txt
xcopy source build\tatradas\source /E /EXCLUDE:excludes.txt
xcopy test build\tatradas\test /E /EXCLUDE:excludes.txt
del excludes.txt
mkdir build\tatradas\third

cd build
zip -9 -r ..\release\tdas%TATRADAS_VERSION%src.zip tatradas
cd ..
rd /S /Q build


REM clean
cd source
cmd /c clean.bat
cd ..

cd test
cmd /c clean.bat
cd ..

REM Create components sources ZIP package
cd third
cmd /c ..\source\clean.bat
zip -9 -r ..\release\components%TATRADAS_VERSION%.zip fastmm4 mphexeditor synedit
cd ..

