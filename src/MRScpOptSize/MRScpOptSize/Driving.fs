module Driving

open Exp

type Contr = VarName * Pattern

type Conf = option<Contr> * Exp

type DriveStepResult =
    | DSRNone
    | DSRCon of ConName * list<Exp>
    | DSRUnfold of Exp
    | DSRCases of VarName * list<Pattern * Exp>

let driveStepResultMap (freeVars: Set<VarName>) (f: Exp -> Exp) (dsr: DriveStepResult) : DriveStepResult =
    match dsr with
    | DSRNone -> DSRNone
    | DSRCon(c, es) -> DSRCon(c, List.map f es)
    | DSRUnfold e -> DSRUnfold(f e)
    | DSRCases(x, alts) ->
        let g ((c, xs), e) = 
            let xs1 = manyFreshIn xs freeVars
            let e1 = subst (List.zip xs (List.map EVar xs1)) e
            let pat = (c, xs1)
            (pat, f e1 |> subst [(x, pat2exp pat)])
        DSRCases(x, List.map g alts)

let rec drivingStep (defs: Defs) (e: Exp) : DriveStepResult =
    let e' = simplify defs Set.empty e
    if e' <> e then DSRUnfold e' else
    match e with
    | EVar _ -> DSRNone
    | EApp(AKCon c, es) -> DSRCon(c, es)
    | EApp(AKFun fname, es) ->
        match lookupDef fname defs with
        | None -> DSRNone
        | Some(FDef(_, args, body)) -> DSRUnfold (subst (List.zip args es) body)
        | Some(GDef(_, cases)) ->
            match es with
            | [] -> DSRNone
            | EVar x::es1 -> 
                let f ((c, xs), args, body) = 
                    let xs1 = manyFreshIn xs (Set.unionMany (Seq.map expVars es1))
                    let body1 = subst (List.zip xs (List.map EVar xs1)) body
                    let pat = (c, xs1)
                    let es1' = List.map (subst [(x, pat2exp pat)]) es1
                    (pat, subst (List.zip args es1') body1)
                DSRCases(x, List.map f cases)
            | EApp(AKCon c, es1)::es2 ->
                match lookupCase c cases with
                | None -> DSRNone
                | Some ((_, xs), ys, body) -> DSRUnfold(subst (List.zip xs es1 @ List.zip ys es2) body)
            | EApp(AKFun _, _) as e1::es1 -> 
                let vars = Set.unionMany (Seq.map expVars es)
                let dsr = drivingStep defs e1
                driveStepResultMap vars (fun e -> EApp(AKFun fname, e::es1)) dsr
