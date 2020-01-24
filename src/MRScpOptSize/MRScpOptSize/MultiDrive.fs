module MultiDrive

open Exp

type MultiDriveStepResult =
    | MDSRLeaf of Exp
    | MDSRCon of ConName * list<Exp>
    | MDSRUnfold of Exp
    | MDSRCases of VarName * list<Pattern * Exp>
    | MDSRLet of list<VarName * Exp> * Exp

let mdsrSubExps (mdsr: MultiDriveStepResult) : list<Exp> =
    match mdsr with
    | MDSRLeaf _ -> []
    | MDSRCon(_, es) -> es
    | MDSRUnfold e -> [e]
    | MDSRCases(_, cases) -> List.map snd cases
    | MDSRLet(binds, e) -> e :: List.map snd binds

let mdsrMap (freeVars: Set<VarName>) (f: Exp -> Exp) (mdsr: MultiDriveStepResult) : MultiDriveStepResult =
    match mdsr with
    | MDSRLeaf e -> MDSRLeaf (f e) // ??? MDSRUnfold
    | MDSRCon(c, es) -> MDSRUnfold (f (EApp(AKCon c, es)))
    | MDSRUnfold e -> MDSRUnfold(f e)
    | MDSRCases(x, alts) ->
        let g ((c, xs), e) = 
            let freeVars' = Set.union freeVars (expVars e)
            let xs1 = manyFreshIn xs freeVars'
            let e1 = subst (List.zip xs (List.map EVar xs1)) e
            let pat = (c, xs1)
            (pat, f e1 |> subst [(x, pat2exp pat)])
        MDSRCases(x, List.map g alts)
    | MDSRLet(binds, e) ->
        let (xs, es) = List.unzip binds
        let freeVars1 = es |> List.map expVars |> Set.unionMany
        let freeVars2 = Set.difference (expVars e) (Set.ofList xs)
        let freeVars3 = Set.unionMany [freeVars; freeVars1; freeVars2]
        let xs1 = manyFreshIn xs freeVars3
        let e1 = subst (List.zip xs (List.map EVar xs1)) e
        MDSRLet(List.zip xs1 es, f e1)

let rec multiDriveSteps (defs: Defs) (e: Exp) : list<MultiDriveStepResult> =
    match e with
    | EVar _ -> [MDSRLeaf e]
    | EApp(AKCon c, es) -> [MDSRCon(c, es)]
    | EApp(AKFun fname, es) ->
        match lookupDef fname defs with
        | None -> [MDSRLeaf e]  // keep faulty expressions as leafs for simplicity
        | Some(FDef(_, args, body)) -> 
            let vars = expVars e
            let fresh = manyFreshIn args vars
            (* if we do not unfold the body:
            let r1 = MDSRLet(List.zip fresh es, EApp(AKFun fname, List.map EVar fresh))
            ... then this rule will be applicable immediately again and we'll have to whistle *)
            let f es = subst (List.zip args es) body
            let r1 = MDSRLet(List.zip fresh es, f (List.map EVar fresh))
            let r2 = MDSRUnfold (f es)
            [r1; r2]
        | Some(GDef(_, cases)) ->
            match es with
            | [] -> [MDSRLeaf e]  // keep faulty expressions as leafs for simplicity
            | EVar x::es1 -> 
                let f ((c, xs), args, body) = 
                    let xs1 = manyFreshIn xs (Set.unionMany [expVars e; Set.ofList args; expVars body])
                    let body1 = subst (List.zip xs (List.map EVar xs1)) body
                    let pat = (c, xs1)
                    let es1' = List.map (subst [(x, pat2exp pat)]) es1
                    (pat, subst (List.zip args es1') body1)
                [MDSRCases(x, List.map f cases)]
            | EApp(AKCon c, es1)::es2 ->
                match lookupCase c cases with
                | None -> [MDSRLeaf e]  // keep faulty expressions as leafs for simplicity
                | Some ((_, xs), ys, body) -> 
                    let vars = Set.union (expVars e) (expVars body)
                    let xs1 = manyFreshIn xs vars
                    let ys1 = manyFreshIn ys vars
                    let es1' = List.map EVar xs1
                    let es2' = List.map EVar ys1
                    (* we have to unfold in all cases - see comment above *)
                    let f es1 es2 = subst (List.zip xs es1 @ List.zip ys es2) body
                    [MDSRLet(List.zip xs1 es1 @ List.zip ys1 es2, f es1' es2'); MDSRUnfold(f es1 es2)]
            | EApp(AKFun _, _) as e1::es1 -> 
                let vars = expVars e
                let rs = multiDriveSteps defs e1
                let rs1 = rs |> List.map (mdsrMap vars (fun e -> EApp(AKFun fname, e::es1)))
                let xs = manyFreshIn (List.replicate (List.length es) "x") vars
                MDSRLet(List.zip xs es, EApp(AKFun fname, List.map EVar xs)) :: rs1

