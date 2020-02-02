// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv = 
    //let (paa, eaa) = Examples.appAppExample ()
    let (paa, eaa) = Examples.kmpExample ()
    printfn "%A" paa
    printfn "%A" eaa
    0 // return an integer exit code
