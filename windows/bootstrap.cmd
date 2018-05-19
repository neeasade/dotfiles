powershell 'Set-ExecutionPolicy RemoteSigned -scope CurrentUser'
powershell 'iex (new-object net.webclient).downloadstring('https://raw.githubusercontent.com/lukesampson/scoop/master/bin/install.ps1')'
scoop install git-with-openssh
scoop install sudo
scoop install ln

rem fuck
cd %USERPROFILE%\scoop\apps\git-with-openssh\current\usr\bin
copy bash.exe sh.exe
