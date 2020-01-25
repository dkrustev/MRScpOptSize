module MRScp

open Utils
open Exp
open ConfGraphs
open MultiDrive

type MConf = MultiDriveStepResult * Exp

type GraphSet =
    | GSNone
    | GSFold of MConf * int * list<VarName * VarName>
    | GSBuild of MConf * list<list<GraphSet>>

let private buildGraph (e: Exp) (cgs: list<MConf * ConfGraph>) : ConfGraph =
    let (confs, gs) = List.unzip cgs
    let mdsrs = List.map fst confs
    match mdsrs with
    | [] -> CGLeaf e
    | mdsr::mdsrs ->
        assert (mdsrs |> List.forall (fun r -> r = mdsr))
        match mdsr with
        | MDSRLeaf e -> CGLeaf e
        | MDSRCon(c, es) -> 
            assert (List.length es = List.length gs)
            CGCon(c, gs)
        | MDSRUnfold e -> 
            assert (List.length gs = 1)
            CGUnfold(List.head gs)
        | MDSRCases(x, alts) ->
            assert (List.length alts = List.length gs)
            CGCases(x, List.zip (List.map fst alts) gs)
        | MDSRLet(binds, _e) ->
            assert (List.length binds + 1 = List.length gs)
            let (g::gs) = gs
            CGLet(List.zip (List.map fst binds) gs, g)

let rec gset2graphs (gs: GraphSet) : seq<MConf * ConfGraph> =
    match gs with
    | GSNone -> Seq.empty
    | GSFold(conf, n, ren) -> seq{ yield (conf, CGFold(n, ren)) }
    | GSBuild(conf, xss) ->
        xss
        |> Seq.collect (fun xs -> xs |> Seq.map gset2graphs |> Seq.cartesian)
        |> Seq.map (fun cgs -> (conf, buildGraph (snd conf) cgs))

type private HistEntryKind = HEGlobal | HELocal

let rec private mrScpRec (defs: Defs) (nestLvl: int) (hist: list<HistEntryKind * int * Exp>) (conf: MConf) : GraphSet =
    let (_, e) = conf
    let relevantHist hek hist =
        match hek with
        | HELocal -> hist |> List.takeWhile (fun (hek, _, _) -> hek = HELocal)
        | HEGlobal -> hist |> List.filter (fun (hek, _, _) -> hek = HEGlobal)
    match List.tryFindSome (fun (_, _, e1) -> renaming e1 e) hist with
    | Some ((_, lvl, _), ren) -> GSFold(conf, nestLvl - lvl, ren)
    | None ->
        let rs = multiDriveSteps defs e
        let hek = if List.tryFind (fun mdsr -> match mdsr with MDSRCases _ -> true | _ -> false) rs = None
                  then HELocal else HEGlobal
        let relHist = relevantHist hek hist
        match List.tryFind (fun (_, _, e1) -> homeomorphicEmbedding e1 e) relHist with
        | Some _ -> GSNone
        | None ->
            let confss = rs |> List.map (fun mdsr -> mdsrSubExps mdsr |> List.map (fun e -> (mdsr, e)))
            let newHist = (hek, nestLvl, e)::hist
            GSBuild(conf, List.map (List.map (mrScpRec defs (nestLvl + 1) newHist)) confss)

let mrScp (defs: Defs) (e: Exp) : GraphSet =
    mrScpRec defs 0 [] (MDSRUnfold e, e)

module GraphSetOps =

    let rec minMaxSizeGraph (cmp: int -> int -> bool) (g: GraphSet) : int * GraphSet =
        let selectMinMax (kx: int * 'A) (ky: int * 'A) : int * 'A =
            match kx, ky with
            | (-1, _), _ -> ky
            | _, (-1, _) -> kx
            | (k1, x), (k2, y) -> if cmp k1 k2 then kx else ky
        let minMaxSizeGraphs (gs: list<GraphSet>) : int * list<GraphSet> =
            (0, []) |> List.foldBack (fun g kgs -> 
                match minMaxSizeGraph cmp g, kgs with
                | (-1, g), (_, gs) -> (-1, g::gs)
                | (_, g), (-1, gs) -> (-1, g::gs)
                | (i, g), (j, gs) -> (i + j, g::gs)
                ) gs
        let minMaxSizeGraphss (gss: list<list<GraphSet>>) : int * list<GraphSet> =
            gss |> List.fold (fun kgs gs -> selectMinMax kgs (minMaxSizeGraphs gs)) (-1, [])
        match g with
        | GSNone -> (-1, GSNone)
        | GSFold _ -> (1, g)
        | GSBuild(c, gss) -> 
            match minMaxSizeGraphss gss with
            | -1, _ -> (-1, GSNone)
            | k, gs -> (1 + k, GSBuild(c, [gs]))

    let rec firstGraph (g: GraphSet) : GraphSet =
        let firstGraphs (gs: list<GraphSet>) : option<list<GraphSet>> =
            Some [] |> List.foldBack (fun g ogs -> 
                match firstGraph g, ogs with
                | GSNone, _ -> None
                | _, None -> None
                | g, Some gs -> Some (g::gs)
                ) gs
        let rec firstGraphss (gss: list<list<GraphSet>>) : option<list<GraphSet>> =
            match gss with
            | [] -> None
            | gs::gss -> 
                match firstGraphs gs with
                | Some gs -> Some gs
                | None -> firstGraphss gss
        match g with
        | GSNone -> GSNone
        | GSFold _ -> g
        | GSBuild(c, gss) -> 
            match firstGraphss gss with
            | None -> GSNone
            | Some gs -> GSBuild(c, [gs])

    let rec lastGraph (g: GraphSet) : GraphSet =
        let lastGraphs (gs: list<GraphSet>) : option<list<GraphSet>> =
            Some [] |> List.foldBack (fun g ogs -> 
                match lastGraph g, ogs with
                | GSNone, _ -> None
                | _, None -> None
                | g, Some gs -> Some (g::gs)
                ) gs
        let rec lastGraphss (gss: list<list<GraphSet>>) : option<list<GraphSet>> =
            match gss with
            | [] -> None
            | gs::gss -> 
                match lastGraphs gs with
                | Some gs -> Some gs
                | None -> lastGraphss gss
        match g with
        | GSNone -> GSNone
        | GSFold _ -> g
        | GSBuild(c, gss) -> 
            match lastGraphss (List.rev gss) with
            | None -> GSNone
            | Some gs -> GSBuild(c, [gs])
