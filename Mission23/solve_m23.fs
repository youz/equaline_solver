open System

type Cell =
    | Num of int
    | OpAdd
    | OpSub
    | OpMul
type Board = Cell array
type Route = int List
type Kifu =
    { boards : Board List;   // 盤面のスナップショットのリスト (逆順)
      routes : Route List; } // 指し手のリスト (逆順)

let updateboard (r: Route) (b: Board) =
    Array.mapi (fun i e ->
        if List.contains i r then
            match e with
            | Num n -> Num (n + 1)
            | OpAdd -> OpSub
            | OpSub -> OpMul
            | OpMul -> OpAdd
        else
            e
    ) b

let updatekifu (r: Route) (k: Kifu) =
    match k with
    | { boards = b :: bt; routes = rt} -> {
        boards = (updateboard r b) :: b :: bt;
        routes = r :: rt; }
    | _ -> failwith "Bug: kifu.boards is empty"

let cell2str (c: Cell) =
    match c with
    | Num n -> string n
    | OpAdd -> "+"
    | OpSub -> "-"
    | OpMul -> "*"

let board2str (b: Board) =
    Array.map (fun c -> sprintf "%2s " (cell2str c)) b
    |> Array.chunkBySize 3
    |> Array.map (fun r -> "|" + (String.concat "|" r) + "|")
    |> String.concat "\n"
let route2str (r: Route) = List.map string r |> String.concat ""

let calc (b: Board) (r: Route) =
    let cs = List.map (fun i -> b[i]) r
    let rec f v l =
        match l with
        | OpAdd :: Num n :: t -> f (v + n) t
        | OpSub :: Num n :: t -> f (v - n) t
        | OpMul :: Num n :: t -> f (v * n) t
        | [] -> v
        | _  -> failwith ("Bug: wrong board or route [" + (route2str r) + "] on\n" + (board2str b))
    match cs with
    | Num n :: t -> f n t
    | _ -> failwith ("Bug: wrong board or route [" + (route2str r) + "] on\n" + (board2str b))

let search_route (target: int) (board: Board) =
    let rec search pos v prevc route =
        let c = board[pos]
        let nv =
            match c with
            | Num n ->
                    match prevc with
                    | OpAdd -> v + n
                    | OpSub -> v - n
                    | OpMul -> v * n
                    | _ -> failwith ("Bug: wrong board or route [" + (route2str route) + "] on\n" + (board2str board))
            | op -> v
        let a =
            match c with
            | Num _ when nv = target -> [List.rev route]
            | _ -> []
        a @ List.collect (fun (dx, dy) ->
            let nx = (pos % 3) + dx
            let ny = (pos / 3) + dy
            let npos = ny * 3 + nx
            if nx < 0 || nx >= 3 || ny < 0 || ny >= 3 || List.contains npos route then
                []
            else
                search npos nv c (npos :: route)
        ) [(1, 0); (-1, 0); (0, 1); (0, -1)]
    List.collect (fun start -> search start 0 OpAdd [start]) [0; 2; 4; 6; 8]


let solve_dfs (ks: Kifu List) (stgmax: int) (verbose: bool) =
    let rec f kifu stgno =
        let target = stgno * (stgno + 1) / 2
        search_route target kifu.boards.Head
        |> List.collect (fun r ->
            let nk = updatekifu r kifu
            if stgno < stgmax then
                f nk (stgno + 1)
            else
                [nk]
        )
    if verbose then printfn "solving stage 1 to %d ... " stgmax
    let result = List.collect (fun k -> f k 3) ks
    if verbose then printfn "found %d solutions" result.Length
    result

let solve_bfs (ks: Kifu List) (stgmax: int) (verbose: bool) =
    let rec f ks stgno =
        let target = stgno * (stgno + 1) / 2
        let nexts =
            List.collect (fun k ->
                search_route target k.boards.Head
                |> List.map (fun r -> updatekifu r k)
            ) ks
        if verbose then
            printfn "stage %d (target=%d): found %d solutions" stgno target nexts.Length
        if stgno < stgmax then
            f nexts (stgno + 1)
        else
            nexts
    f ks 3

let count_moves (r: Route List) =
    let mhd (p1,  p2) =
        let dx = abs (p1 % 3 - p2 % 3)
        let dy = abs (p1 / 3 - p2 / 3)
        dx + dy
    4 :: List.concat r |> List.pairwise |> List.sumBy mhd

let printkifu (k: Kifu) =
    for (b, r) in List.zip (List.rev k.boards) ((List.rev k.routes) @ [[]]) do
        printfn "-------------\n%s" (board2str b)
        if r.Length > 0 then
            let expr = List.map (fun pos -> cell2str b[pos]) r
                                    |> String.concat ""
            printfn "-------------\n%s (%s)" expr (route2str r)

let printmoves (k: Kifu) =
    List.rev k.routes
    |> List.map route2str
    |> String.concat "\t"
    |> stdout.WriteLine


let initial_states =
    let k0 = {
        boards = [[| Num 1; OpAdd; Num 1; OpAdd; Num 1; OpAdd; Num 1; OpAdd; Num 1 |]];
        routes = [] }
    let k1 = updatekifu [4] k0 |> updatekifu [4; 1; 0]
    let k2 = updatekifu [0] k0 |> updatekifu [0; 1; 4]
    let k3 = updatekifu [0] k0 |> updatekifu [0; 1; 2]
    [k1; k2; k3]

let showhelp_and_exit () =
    printfn "usage: solve_m23.exe [-sNN] [-nNN] [-m] [-c] [-v] [-h]"
    printfn "  [options]"
    printfn "     -sNN : NN番目のステージまで解く (未指定なら10)"
    printfn "     -nNN : 移動回数の少ない順にNN件の解答を出力する (未指定なら1)"
    printfn "     -m   : 盤面は表示せずカーソル移動手順のみ出力する"
    printfn "     -c   : 解の数のみを出力する"
    printfn "     -v   : 各ステージの解の数を表示"
    printfn "     -h   : このヘルプを表示"
    System.Environment.Exit 0

[<EntryPoint>]
let main args =
    let mutable stgmax = 10
    let mutable count_only = false
    let mutable print_topn = 1
    let mutable print_movesonly = false
    let mutable verbose = false
    for i in 0 .. args.Length - 1 do
        match args[i][0..1] with
        | "-c" ->
            count_only <- true
            verbose <- true
        | "-s" -> (
            let (ok, parsed) = Int32.TryParse(args[i][2..])
            if ok && parsed > 0 then
                stgmax <- parsed
            )
        | "-n" -> (
            let (ok, parsed) = Int32.TryParse(args[i][2..])
            if ok && parsed > 0 then
                print_topn <- parsed
            )
        | "-m" -> print_movesonly <- true
        | "-v" -> verbose <- true
        | "-h" -> showhelp_and_exit ()
        | _ -> printfn "(warn) unknown option: %s" args[i]

    let solutions =
        solve_bfs initial_states stgmax verbose
        |> List.sortBy (fun k -> count_moves (List.rev k.routes))
        |> List.truncate print_topn

    if not count_only then
        List.iteri (fun i k ->
            let moves = count_moves (List.rev k.routes)
            if print_movesonly then
                printf "#%d\t%d moves\t" (i + 1) moves
                printmoves k
            else
                printfn "#%d : %d moves\t" (i + 1) moves
                printkifu k
                stdout.WriteLine ""
        ) solutions
    0
