msbuild ./sigdump/sigdump.fsproj /t:Build /p:Configuration="Release"
nuget pack sigdump.nuspec