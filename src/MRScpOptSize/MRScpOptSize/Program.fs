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
    let gs = MRScp.mrScp (ExpParser.str2defs "f(x,y)=Pair(x,y);") (ExpParser.str2exp "f(A,B)")
    printfn "%A" gs
    let cgs = MRScp.gset2graphs gs
    for (c, g) in cgs do
        printfn "%A" c
        printfn "%A" g
    0 // return an integer exit code
