module MRScp

open Utils
open Exp
open ConfGraphs
open MultiDrive

type GraphSet =
    | GSNone
    | GSFold of int * list<VarName * VarName>
    | GSBuild of MultiDriveStepResult * list<list<GraphSet>>

let private buildGraph (mdsr: MultiDriveStepResult) (gs: list<ConfGraph>) : ConfGraph =
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

let rec gset2graphs (gs: GraphSet) : seq<ConfGraph> =
    match gs with
    | GSNone -> Seq.empty
    | GSFold(n, ren) -> seq{ yield CGFold(n, ren) }
    | GSBuild(mdsr, xss) ->
        xss
        |> Seq.collect (fun xs -> xs |> Seq.map gset2graphs |> Seq.cartesian)
        |> Seq.map (buildGraph mdsr)

type private HistEntryKind = HEGlobal | HELocal

let rec private mrScpRec (defs: Defs) (nestLvl: int) (hist: list<HistEntryKind * int * Exp>) (e: Exp) : GraphSet =
    let relevantHist hek hist =
        match hek with
        | HELocal -> hist |> List.takeWhile (fun (hek, _, _) -> hek = HELocal)
        | HEGlobal -> hist |> List.filter (fun (hek, _, _) -> hek = HEGlobal)
    match List.tryFindSome (fun (_, _, e1) -> renaming e1 e) hist with
    | Some ((_, lvl, _), ren) -> GSFold(nestLvl - lvl, ren)
    | None ->
        let rs = multiDriveSteps defs e
        let hek = if List.tryFind (fun mdsr -> match mdsr with MDSRCases _ -> true | _ -> false) rs = None
                  then HELocal else HEGlobal
        let relHist = relevantHist hek hist
        match List.tryFind (fun (_, _, e1) -> homeomorphicEmbedding e1 e) relHist with
        | Some _ -> GSNone
        | None ->
            GSBuild()

    