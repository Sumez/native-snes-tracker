dotnet publish --configuration Release -r win-x64 -p:PublishSingleFile=true -p:IncludeNativeLibrariesForSelfExtract=true -p:ReadyToRun=true -p:PublishTrimmed=true -p:CopyOutputSymbolsToPublishDirectory=false --self-contained true
cd bin\Release\net6.0\win-x64\publish
7z a -tzip ..\..\..\..\bsdj-patcher.zip Patcher.exe