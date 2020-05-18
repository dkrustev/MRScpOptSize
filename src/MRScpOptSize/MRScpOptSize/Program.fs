// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    //let (paa, eaa) = Examples.appAppExample ()
    (*
    let (paa, eaa) = Examples.boolEqSymExample ()
    printfn "%A" paa
    printfn "%A" eaa
    *)
    //let gs = MRScp.mrScp (ExpParser.str2defs "f(x,y)=Pair(x,y);") (ExpParser.str2exp "f(A,B)")
    //Examples.mrScp Examples.appDef Examples.appAppExample
    //Examples.mrScp Examples.kmpDef Examples.kmpExample
    (*
    Examples.mrScpDump true @"\Dev\FSharp\MRScpOptSize\output" "trivialTest" (ExpParser.str2defs "f(x,y)=Pair(x,y);") 
        (ExpParser.str2exp "f(A,B)")
    *)
    Examples.mrScpDump true @"\Dev\FSharp\MRScpOptSize\output" "trivialTest" (ExpParser.str2defs "f(x)=Pair(x,x);") 
        (ExpParser.str2exp "f(f(A))")
    Examples.mrScpDump true @"\Dev\FSharp\MRScpOptSize\output" "appApp" Examples.appDef Examples.appAppExample
    Examples.mrScpDump true @"\Dev\FSharp\MRScpOptSize\output" "boolEqSym" Examples.kmpDef Examples.boolEqSymExample
    Examples.mrScpDump true @"\Dev\FSharp\MRScpOptSize\output" "evenOrOdd" Examples.evenOrOddDef Examples.evenOrOddExample
    Examples.mrScpDump true @"\Dev\FSharp\MRScpOptSize\output" "takeLength" Examples.takeLengthDef Examples.takeLengthExample
    Examples.mrScpDump true @"\Dev\FSharp\MRScpOptSize\output" "idNatIdemp" Examples.idNatIdempDef Examples.idNatIdempExample
    Examples.mrScpDump false @"\Dev\FSharp\MRScpOptSize\output" "lengthIntersperse" Examples.lengthIntersperseDef Examples.lengthIntersperseExample
    Examples.mrScpDump true @"\Dev\FSharp\MRScpOptSize\output" "expGrowthSmall" Examples.expGrowthDef Examples.expGrowthSmallExample
    //Examples.mrScpDump true @"\Dev\FSharp\MRScpOptSize\output" "expGrowth" Examples.expGrowthDef Examples.expGrowthExample
    //Examples.mrScpDump false @"\Dev\FSharp\MRScpOptSize\output" "KMP" Examples.kmpDef Examples.kmpExample
    0 // return an integer exit code
