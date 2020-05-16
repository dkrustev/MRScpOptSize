module VisualizeGraphs

open Utils
open MultiDrive
open MRScp

let gset2dot (onlyFirstAlt: bool) (gs: GraphSet) : list<string> =
    let mutable nodeCount = 0
    let newNodeName prefix =
        nodeCount <- nodeCount + 1
        sprintf "%s%d" prefix nodeCount
    let showConf index (mdsr, e) =
        let prefix = 
            match mdsr with
            | MDSRLet(binds, _) when index = 0 -> 
                let s = binds |> List.map (fun (x, _) -> sprintf "%s = _" x) |> String.concat "; "
                sprintf "let %s in " s
            | MDSRCases(x, alts) ->
                match List.tryItem index alts with
                | Some ((con, xs), _) -> sprintf "case %s = %s(%s) : " x con (String.concat ", " xs)
                | None -> "" // TODO: Warn?
                //sprintf "%A" mdsr
            | _ -> ""
        sprintf "%s%A" prefix e
    let rec loop nodeNames altNumber index gs = 
        match gs with
        | GSNone -> [sprintf "%s [label = \"GSNone\"]" (List.head nodeNames)]
        | GSFold (conf, n, ren) -> 
            let conf' = showConf index conf |> String.escape
            let nodeName = List.head nodeNames
            let ren' = ren |> List.map (fun (x, y) -> sprintf "%s := %s" y x) |> String.concat "; "
            let node = sprintf "%s [label = \"GSFold (%s, [%s])\"]" nodeName conf' ren'
            let edge = sprintf "%s -> %s" nodeName (List.item n nodeNames)
            [node; edge]
        | GSBuild (conf, gsss) ->
            let nodeName = List.head nodeNames
            let gsss' = gsss |> List.map List.indexed |> List.indexed
            let (dot, subfields) = 
                ([], []) 
                |> List.foldBack (fun (i, gss) (dot, subfields) -> 
                    let (dot', subsubfields) =
                        (dot, [])
                        |> List.foldBack (fun (j, gs) (dot, subsubfields) -> 
                            let field = sprintf "f_%d_%d" i j
                            let field' = sprintf "<%s>" field
                            let subnodeName = newNodeName "node"
                            let edge = sprintf "%s:%s -> %s" nodeName field subnodeName
                            let subDot = 
                                if not onlyFirstAlt || altNumber = 0
                                then loop (subnodeName::nodeNames) i j gs
                                else [sprintf "%s [label = \"...\"]" subnodeName]
                            (edge :: subDot @ dot, field'::subsubfields)
                            )
                            gss
                    let subfield = sprintf "{ alt. %d | {%s}}" (i+1) (String.concat " |" subsubfields)
                    (dot', subfield :: subfields))
                    gsss'
            let conf' = showConf index conf |> String.escape
            let node = sprintf "%s [label = \"{GSBuild (%s) | {%s}}\"]" nodeName conf' (String.concat " | " subfields)
            node :: dot
    let dot = loop [newNodeName "node"] 0 0 gs
    "digraph MRSCGraphSet {" :: "node [shape=record];" :: dot @ ["}"]

