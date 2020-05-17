set DotExe="D:\Programs\Graphviz2.38\bin\dot.exe"
pushd output
for /D %%D in (*.*) do (
  pushd %%D
  for %%F in (*.dot) do (
    %DotExe% -T pdf -o %%~nF.pdf %%F
  )
popd
)
popd