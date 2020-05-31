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
            sprintf "graph size: %i\ngraph size without unfold nodes: %i\n%s%A\n%A" (graphSize true g) (graphSize false g) s g p)

    let gs = MRScp.mrScp defs e
    System.IO.File.WriteAllText(System.IO.Path.Combine(path, sprintf "%s_GraphSet.txt" name), sprintf "%A" gs)
    System.IO.File.WriteAllLines(System.IO.Path.Combine(path, sprintf "%s_GraphSet.dot" name), gset2dot false gs)
    System.IO.File.WriteAllLines(System.IO.Path.Combine(path, sprintf "%s_GraphSetPartial.dot" name), gset2dot true gs)

    let confWeightNoUnfold = function (MultiDrive.MDSRUnfold _, _) -> 0 | _ -> 1
    let (minSize, gsMin) = MRScp.GraphSetOps.minMaxSizeGraph (<=) (fun _ -> 1) gs
    dumpSelected "Min" (sprintf "minSize: %i\n" minSize) gsMin
    let (minSizeNoUnfold, gsMinNoUnfold) = MRScp.GraphSetOps.minMaxSizeGraph (<=) confWeightNoUnfold gs
    dumpSelected "MinNoUnfold" (sprintf "minSize (without unfold nodes): %i\n" minSizeNoUnfold) gsMinNoUnfold
    let (maxSize, gsMax) = MRScp.GraphSetOps.minMaxSizeGraph (>=) (fun _ -> 1) gs
    dumpSelected "Max" (sprintf "maxSize: %i\n" maxSize) gsMax
    let (maxSizeNoUnfold, gsMaxNoUnfold) = MRScp.GraphSetOps.minMaxSizeGraph (>=) confWeightNoUnfold gs
    dumpSelected "MaxNoUnfold" (sprintf "maxSize (without unfold nodes): %i\n" maxSizeNoUnfold) gsMaxNoUnfold
    let gsFirst = MRScp.GraphSetOps.firstGraph gs
    dumpSelected "First" "" gsFirst
    let gsLast = MRScp.GraphSetOps.lastGraph gs
    dumpSelected "Last" "" gsLast
    if dumpAllGraphs then
        let cgs = MRScp.gset2graphs gs
        let mutable count = 0
        let mutable minSize = System.Int32.MaxValue
        let mutable maxSize = 0
        let mutable minSizeNoUnfold = System.Int32.MaxValue
        let mutable maxSizeNoUnfold = 0
        let mutable totalSize = 0
        for (i, (c, g)) in cgs |> Seq.mapi (fun i x -> (i, x)) do
            use tw = System.IO.File.CreateText(System.IO.Path.Combine(path, sprintf "%s_graph_%06i.txt" name i))
            fprintfn tw "%A" c
            fprintfn tw "graph size: %i" (ConfGraphs.graphSize true g)
            fprintfn tw "graph size without unfold nodes: %i" (ConfGraphs.graphSize false g)
            fprintfn tw "%A" g
            fprintfn tw "%A" (Exp.CSE.purgeRepeatedFuns (ExtExp.extProg2Prog (Residualize.graph2prog g)))
            count <- count + 1
            let gSize = ConfGraphs.graphSize true g
            minSize <- min gSize minSize
            maxSize <- max gSize maxSize
            let gSizeNoUnfold = ConfGraphs.graphSize false g
            minSizeNoUnfold <- min gSizeNoUnfold minSizeNoUnfold
            maxSizeNoUnfold <- max gSizeNoUnfold maxSizeNoUnfold
            totalSize <- gSize + totalSize
        System.IO.File.WriteAllText(System.IO.Path.Combine(path, sprintf "%s_Stats.txt" name), 
            sprintf "count: %i\nminSize: %i\nmaxSize: %i\nminSize without unfold nodes: %i\nmaxSize without unfold nodes: %i\naverageSize: %f" 
                count minSize maxSize minSizeNoUnfold maxSizeNoUnfold ((double totalSize) / (double count)))

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

let evenOrOddDefStr = """
or(True,  y) = True;
or(False, y) = y;

even(Z)    = True;
even(S(n)) = odd(n);
odd(Z)    = False;
odd(S(n)) = even(n);
"""
let evenOrOddDef = ExpParser.str2defs evenOrOddDefStr
let evenOrOddExample = ExpParser.str2exp "or(even(n), odd(n))"

let idNatIdempDefStr = """
idNat(Z)    = Z;
idNat(S(n)) = S(idNat(n));
"""
let idNatIdempDef = ExpParser.str2defs idNatIdempDefStr
let idNatIdempExample = ExpParser.str2exp "idNat(idNat(n))"

let takeLengthDefStr = """
length(Nil)         = Z;
length(Cons(x, xs)) = S(length(xs));

take(Z,    xs) = Nil;
take(S(n), xs) = takeS(xs, n);
takeS(Nil,         n) = Nil;
takeS(Cons(x, xs), n) = Cons(x, take(n, xs));
"""
let takeLengthDef = ExpParser.str2defs takeLengthDefStr
let takeLengthExample = ExpParser.str2exp "take(length(xs), xs)"

let lengthIntersperseDefStr = """
eqNat(Z,    n) = eqNatZ(n);
eqNat(S(m), n) = eqNatS(n, m);
eqNatZ(Z)    = True;
eqNatZ(S(n)) = False;
eqNatS(Z,    m) = False;
eqNatS(S(n), m) = eqNat(m, n);

length(Nil)         = Z;
length(Cons(x, xs)) = S(length(xs));

intersperse(Nil,         sep) = Nil;
intersperse(Cons(x, xs), sep) = Cons(x, prependToAll(xs, sep));
prependToAll(Nil,         sep) = Nil;
prependToAll(Cons(x, xs), sep) = Cons(sep, Cons(x, prependToAll(xs, sep)));
"""
let lengthIntersperseDef = ExpParser.str2defs lengthIntersperseDefStr
let lengthIntersperseExample = ExpParser.str2exp "eqNat(length(intersperse(xs, s1)), length(intersperse(xs, s2)))"


