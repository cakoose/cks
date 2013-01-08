REM Windows Build Script

SET csc=csc /nologo

if exist Output (
	rmdir /q /s Output
)
mkdir Output

%csc% /langversion:ISO-2 /target:library /reference:Mono.Security.dll /out:Output\Cks.Runtime.dll /recurse:Source\*.cs
