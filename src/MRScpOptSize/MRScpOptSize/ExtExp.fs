module ExtExp

open Utils
open Exp

[<StructuredFormatDisplay("{PrettyPrint}")>]
type ExtExp =
    | EEVar of VarName
    | EEApp of AppKind * list<ExtExp>
    | EELet of list<VarName * ExtExp> * ExtExp
    | EECase of ExtExp * list<Pattern * ExtExp>
    member this.PrettyPrint : string =
        let appName ak = 
            match ak with
            | AKCon c -> c
            | AKFun f -> f
        let rec ppEExp offset e =
            match e with
            | EEVar x -> x
            | EEApp(ak, es) -> sprintf "%s(%s)" (appName ak) (es |> Seq.map (ppEExp offset) |> String.concat ",")
            | EELet(binds, e1) -> 
                let offs = "  " + offset
                let ppBind (x, e: ExtExp) = sprintf ("\n%slet %s = %s in ") offs x (ppEExp offs e)
                sprintf "%s\n%s%s" (binds |> Seq.map ppBind |> String.concat "") offs (ppEExp offs e1)
            | EECase(e1, alts) ->
                let offs = "  " + offset
                let ppAlt ((c, xs), e: ExtExp) = sprintf "\n%s%s(%s) -> %s;" offs c (xs |> String.concat ",") (ppEExp offs e)
                sprintf "\n%scase %s of {%s\n%s}" offset (ppEExp offs e1) (alts |> Seq.map ppAlt |> String.concat "") offset
        ppEExp "" this

let rec exp2extExp e =
    match e with
    | EVar x -> EEVar x
    | EApp(ak, es) -> EEApp(ak, List.map exp2extExp es)

(* We need the vars in the order they appear: *)
let rec extExpVars e =
    match e with
    | EEVar x -> [x]
    | EEApp(ak, es) -> List.concat (Seq.map extExpVars es) |> List.removeDuplicates
    | EELet(binds, e) -> 
        let (xs, es) = List.unzip binds
        let vars1 = List.concat (Seq.map extExpVars es) |> List.removeDuplicates
        let vars2 = extExpVars e |> List.removeDuplicates
        vars1 @ (List.difference vars2 xs) |> List.removeDuplicates
    | EECase(e, alts) -> 
        let xss = alts |> Seq.map (fun ((c, xs), e) -> List.difference (extExpVars e) xs)
        (extExpVars e @ List.concat xss) |> List.removeDuplicates

let removeTrivialLets e =
    let rec varCount x e =
        match e with
        | EEVar y -> if x = y then 1 else 0
        | EEApp(_, es) -> es |> Seq.sumBy (varCount x)
        | EELet(binds, e) ->
            let (xs, es) = List.unzip binds
            let cnt1 = es |> Seq.sumBy (varCount x)
            if List.contains x xs then cnt1 else cnt1 + varCount x e
        | EECase(e, alts) ->
            let cntAlts = alts |> Seq.sumBy (fun ((c, xs), e) -> if List.contains x xs then 0 else varCount x e)
            varCount x e + cntAlts
    let isTriv ebody (x, e) =
        match e with
        | EEVar _ -> true
        | _ -> varCount x ebody <= 1
    let hide xs s = xs |> List.fold (fun s x -> Map.add x (EEVar x) s) s
    let rec loop s e =
        match e with
        | EEVar x -> 
            match Map.tryFind x s with
            | None -> e
            | Some e -> e
        | EEApp(ak, es) -> EEApp(ak, List.map (loop s) es)
        | EELet(binds, e) ->
            let (triv, nontriv) = binds |> List.partition (isTriv e)
            let s1 = triv |> List.fold (fun s1 (y, e) -> Map.add y (loop (hide [y] s) e) s1) s
            let nontriv1 = nontriv |> List.map (fun (x, e) -> (x, loop (hide [x] s) e))
            let newBody = loop s1 e
            if List.isEmpty nontriv1 then newBody else EELet(nontriv1, newBody)
        | EECase(e, alts) ->
            let alts1 = alts |> List.map (fun ((c, xs) as p, e) -> (p, loop (hide xs s) e))
            EECase(loop s e, alts1)
    loop Map.empty e

type ExtDef = FunName * list<VarName> * ExtExp

(* *** *)

let extExp2Exp (fprefix: FunName) (funNames: Set<FunName>) (e: ExtExp) : Set<FunName> * list<Def> * Exp =
    let rec loop fnames e =
        match e with
        | EEVar x -> (fnames, [], EVar x)
        | EEApp(ak, es) ->
            let (fnames1, defs, es') =
                List.foldBack (fun e (fnames, defs, es) ->
                    let (fnames1, defs1, e) = loop fnames e
                    (fnames1, defs1 @ defs, e::es))
                    es
                    (fnames, [], [])
            (fnames1, defs, EApp(ak, es'))
        | EECase(e1, alts) ->
            let xss = alts |> Seq.map (fun ((c, xs), e) -> List.difference (extExpVars e) xs)
            let vars = List.concat xss |> List.removeDuplicates
            let f = freshIn (sprintf "%s_case" fprefix) fnames
            let (fnames1, defs, cases) =
                List.foldBack (fun (p, e) (fnames, defs, cases) ->
                    let (fnames1, defs1, e1) = loop fnames e
                    (fnames1, defs1 @ defs, (p, vars, e1)::cases))
                    alts
                    (Set.add f fnames, [], [])
            let (fnames2, defs1, e1') = loop fnames1 e1
            (fnames2, GDef(f, cases) :: defs1 @ defs, EApp(AKFun f, e1' :: List.map EVar vars))
        | EELet(binds, e1) ->
            let (xs, es) = List.unzip binds
            let (fnames1, defs, es') =
                List.foldBack (fun e (fnames, defs, es) ->
                    let (fnames1, defs1, e1) = loop fnames e
                    (fnames1, defs1 @ defs, e1::es))
                    es
                    (fnames, [], [])
            let (fnames2, defs1, e1') = loop fnames1 e1
            let vars = Set.difference (expVars e1') (Set.ofList xs) |> Set.toList
            let f = freshIn (sprintf "%s_let" fprefix) fnames2
            let def = FDef(f, vars @ xs, e1')
            let ecall = EApp(AKFun f, List.map EVar vars @ es')
            (Set.add f fnames2, def :: defs1 @ defs, ecall)
    loop funNames e

let extDefs2Defs (edefs: list<ExtDef>) : list<Def> =
    let funNames = Seq.map (fun (f, _, _) -> f) edefs |> Set.ofSeq
    let (funNames1, defs) =
        List.fold (fun (fnames, defs) (f, xs, e) ->
            let (fnames1, defs1, e1) = extExp2Exp f fnames e
            let def = FDef(f, xs, e1)
            (fnames1, def :: defs1 @ defs))
            (funNames, [])
            edefs
    defs

let extProg2Prog (eprg: list<ExtDef> * ExtExp) : list<Def> * Exp =
    let (edefs, e) = eprg
    let defs = extDefs2Defs edefs
    let funNames = List.map defName defs |> Set.ofList
    let (_, defs1, e1) = extExp2Exp "main" funNames e
    (defs1 @ defs, e1)
