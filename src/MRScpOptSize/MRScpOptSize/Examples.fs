module Examples

open ExpParser
open ConfGraphs
open VisualizeGraphs

let stdScp defs e =
    let g = StdScp.buildGraph defs e
    let p = Residualize.graph2prog g
    let p' = ExtExp.extProg2Prog p
    let p'' = Exp.CSE.purgeRepeatedFuns p'
    p''

let mrScp defs e =
    let gs = MRScp.mrScp defs e
    printfn "%A" gs
    let cgs = MRScp.gset2graphs gs
    for (c, g) in cgs do
        printfn "%A" c
        printfn "%A" g
        printfn "%A" (Exp.CSE.purgeRepeatedFuns (ExtExp.extProg2Prog (Residualize.graph2prog g)))

let mrScpDump dumpAllGraphs path name defs e =
    let path = System.IO.Path.Combine(path, name)
    if not (System.IO.Directory.Exists(path)) then
        System.IO.Directory.CreateDirectory(path) |> ignore

    let dumpSelected name1 s gs =
        let gs' = MRScp.gset2graphs gs
        assert (Seq.length gs' = 1)
        let (_, g) = Seq.head gs'
        let p = (Exp.CSE.purgeRepeatedFuns (ExtExp.extProg2Prog (Residualize.graph2prog g)))
        System.IO.File.WriteAllText(System.IO.Path.Combine(path, sprintf "%s_Graph%s.txt" name name1), 
            sprintf "graph size: %i\n%s%A\n%A" (graphSize g) s g p)

    let gs = MRScp.mrScp defs e
    System.IO.File.WriteAllText(System.IO.Path.Combine(path, sprintf "%s_GraphSet.txt" name), sprintf "%A" gs)
    System.IO.File.WriteAllLines(System.IO.Path.Combine(path, sprintf "%s_GraphSet.dot" name), gset2dot gs)

    let (minSize, gsMin) = MRScp.GraphSetOps.minMaxSizeGraph (<=) gs
    dumpSelected "Min" (sprintf "minSize: %i\n" minSize) gsMin
    let (maxSize, gsMax) = MRScp.GraphSetOps.minMaxSizeGraph (>=) gs
    dumpSelected "Max" (sprintf "maxSize: %i\n" maxSize) gsMax
    let gsFirst = MRScp.GraphSetOps.firstGraph gs
    dumpSelected "First" "" gsFirst
    let gsLast = MRScp.GraphSetOps.lastGraph gs
    dumpSelected "Last" "" gsLast
    if dumpAllGraphs then
        let cgs = MRScp.gset2graphs gs
        let mutable count = 0
        let mutable minSize = System.Int32.MaxValue
        let mutable maxSize = 0
        let mutable totalSize = 0
        for (i, (c, g)) in cgs |> Seq.mapi (fun i x -> (i, x)) do
            use tw = System.IO.File.CreateText(System.IO.Path.Combine(path, sprintf "%s_graph_%06i.txt" name i))
            fprintfn tw "%A" c
            fprintfn tw "graph size: %i" (ConfGraphs.graphSize g)
            fprintfn tw "%A" g
            fprintfn tw "%A" (Exp.CSE.purgeRepeatedFuns (ExtExp.extProg2Prog (Residualize.graph2prog g)))
            count <- count + 1
            let gSize = ConfGraphs.graphSize g
            minSize <- min gSize minSize
            maxSize <- max gSize maxSize
            totalSize <- gSize + totalSize
        System.IO.File.WriteAllText(System.IO.Path.Combine(path, sprintf "%s_Stats.txt" name), 
            sprintf "count: %i\nminSize: %i\nmaxSize: %i\naverageSize: %f" count minSize maxSize ((double totalSize) / (double count)))

let appDefStr = """
append(Nil, ys) = ys;
append(Cons(x, xs), ys) = Cons(x, append(xs, ys));
"""
let appDef = str2defs appDefStr
let appAppExample = ExpParser.str2exp "append(append(xs, ys), zs)"

let kmpDefStr = """
not(True)  = False;
not(False) = True;

eqBool(True,  b) = b;
eqBool(False, b) = not(b);

match(Nil,         ss, op, os) = True;
match(Cons(p, pp), ss, op, os) = matchCons(ss, p, pp, op, os);

matchCons(Nil,         p, pp, op, os) = False;
matchCons(Cons(s, ss), p, pp, op, os) = matchHdEq(eqBool(p, s), pp, ss, op, os);

matchHdEq(True,  pp, ss, op, os) = match(pp, ss, op, os);
matchHdEq(False, pp, ss, op, os) = next(os, op);

next(Nil,         op) = False;
next(Cons(s, ss), op) = match(op, ss, op, ss);

isSublist(p, s) = match(p, s, p, s);
"""
let kmpDef = str2defs kmpDefStr
let kmpExample = ExpParser.str2exp "isSublist(Cons(True, Cons(True, Cons(False, Nil))), s)"

let boolEqSymExample = ExpParser.str2exp "eqBool(eqBool(x, y), eqBool(y, x))"

// example 11.4.1 of Soerensen's thesis
let expGrowthDefStr = """
g(Nil,         y) = y;
g(Cons(x, xs), y) = f(g(xs, y));
f(w) = B(w, w);
"""
let expGrowthDef = ExpParser.str2defs expGrowthDefStr
let expGrowthExample = ExpParser.str2exp "g(Cons(A, Cons(A, Cons(A, Nil))), z)"
let expGrowthSmallExample = ExpParser.str2exp "g(Cons(A, Nil), z)"


