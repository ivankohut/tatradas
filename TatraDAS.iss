[Setup]
AppName=TatraDAS
AppVerName=TatraDAS 2.9.9 devel
AppPublisher=Ivan Kohút
AppPublisherURL=http://tatradas.sourceforge.net
AppSupportURL=http://tatradas.sourceforge.net
AppUpdatesURL=http://tatradas.sourceforge.net
DefaultDirName={pf}\TatraDAS
DefaultGroupName=TatraDAS
AllowNoIcons=yes
SolidCompression=yes
FlatComponentsList=no
OutputBaseFilename=tdas299
OutputDir=release
PrivilegesRequired=none

[Languages]
Name: "en"; MessagesFile: "compiler:Default.isl"
Name: "sk"; MessagesFile: "compiler:Languages\Slovak.isl"

[Tasks]
Name: "desktopicon"; Description: "Create a &desktop icon"; GroupDescription: "Additional icons:"
Name: "quicklaunchicon"; Description: "Create a &Quick Launch icon"; GroupDescription: "Additional icons:"; Flags: unchecked

[Files]
Source: "TatraDAS.exe"; DestDir: "{app}"; Flags: ignoreversion;
;Source: "doc\TatraDAS.chm"; DestDir: "{app}"; Flags: ignoreversion;
Source: "languages\LangENG.ini"; DestDir: "{app}\languages"; Flags: ignoreversion;
Source: "languages\LangSVK.ini"; DestDir: "{app}\languages"; Flags: ignoreversion;
Source: "languages\en.ico"; DestDir: "{app}\languages"; Flags: ignoreversion;
Source: "languages\sk.ico"; DestDir: "{app}\languages"; Flags: ignoreversion;

[INI]
Filename: "{app}\TatraDAS.url"; Section: "InternetShortcut"; Key: "URL"; String: "http://tatradas.sourceforge.net"

[Icons]
Name: "{group}\TatraDAS"; Filename: "{app}\TatraDAS.exe"
Name: "{group}\TatraDAS on the Web"; Filename: "{app}\TatraDAS.url"
Name: "{group}\Uninstall TatraDAS"; Filename: "{uninstallexe}"
Name: "{userdesktop}\TatraDAS"; Filename: "{app}\TatraDAS.exe"; Tasks: desktopicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\TatraDAS"; Filename: "{app}\TatraDAS.exe"; Tasks: quicklaunchicon

[Run]
Filename: "{app}\TatraDAS.exe"; Description: "Launch TatraDAS"; Flags: nowait postinstall skipifsilent

[UninstallDelete]
Type: files; Name: "{app}\TatraDAS.url"

