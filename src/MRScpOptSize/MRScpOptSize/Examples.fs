module Examples

open ExpParser

let stdScp defs e =
    let g = StdScp.buildGraph defs e
    let p = Residualize.graph2prog g
    let p' = ExtExp.extProg2Prog p
    let p'' = Exp.CSE.purgeRepeatedFuns p'
    p''

let appDefStr = """
append(Nil, ys) = ys;
append(Cons(x, xs), ys) = Cons(x, append(xs, ys));
"""
let appDef = str2defs appDefStr
let appAppExample () = stdScp appDef (ExpParser.str2exp "append(append(xs, ys), zs)")

