module ImmutP116Exer
 
let clip ceiling (seq: seq<'T>) =
    seq
    |> Seq.map (fun n ->
        match n > ceiling with
        | true -> ceiling
        | false -> n)

let clipExample = 
    [| 1.9; 23.2; 22.; 4.; 19.; |]
    |> clip 20.
    |> Seq.iter (fun f -> printf "%f " f)

let extremes s =
    (Seq.max s, Seq.min s)

let extremesTest = 
    [| 1.9; 23.2; 22.; 33.2; 4.; 19.; |] 
    |> extremes
