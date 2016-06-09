open FSCL.Compiler
open FSCL.Language
open FSCL
open FSCL.Runtime
open System.Diagnostics;

[<EntryPoint>]
let main argv = 
    let rec generateInput (digits : int list) : int list list  =
        if List.isEmpty digits then [[]] else List.collect (combine digits) digits
    and combine (digits : int list) (x : int) : int list list = 
        digits 
        |> List.filter (fun y -> y <> x) 
        |> generateInput 
        |> List.map (fun y -> x::y)

    let input = generateInput [1; 2; 3; 4; 5; 6; 7; 8; 9] |> List.map (fun x -> List.toArray x) |> List.toArray
    let sw = Stopwatch.StartNew()
    let hundreds = <@ Array.filter(fun (a: int[]) -> a.[0] + a.[1] + a.[2] + a.[3] + a.[4] + a.[5] + a.[6] + a.[7] * a.[8] = 100) input @>.Run()
    sw.Stop()
    printfn "%d" sw.ElapsedMilliseconds;
    0 // return an integer exit code
