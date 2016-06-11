open FSCL.Compiler
open FSCL.Language
open FSCL
open FSCL.Runtime
open System.Diagnostics;
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.ExprShape
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

[<EntryPoint>]
let main argv = 
    let sw = Stopwatch.StartNew()
    //generating all unique combinations of digits.
    let rec generateInput (digits : int list) : int list list  =
        match digits with
        | [] -> [[]]
        | _ -> List.collect (combine digits) digits
    and combine (digits : int list) (x : int) : int list list = 
        digits 
        |> List.filter (fun y -> y <> x) 
        |> generateInput 
        |> List.map (fun y -> x::y)
    let input = generateInput [1; 2; 3; 4; 5; 6; 7; 8; 9] |> List.map (fun x -> List.toArray x) |> List.toArray
    printfn "Generated inputs in: %d ms." sw.ElapsedMilliseconds

    //generating all possible combinations of 0-3.
    let rec generateExpressions indexes totalAmount = function
    | n when n = totalAmount -> List.map (fun x -> x) indexes |> List.chunkBySize 1
    | n -> generateExpressions indexes totalAmount (n + 1)
           |> List.collect (fun os -> List.map (fun o -> o::os) indexes)
    let operations = generateExpressions [0..3] 8 1 |> List.map (fun o -> List.toArray o) |> List.toArray

    //function to convert combination of 0-3 to string format for final expression.
    let operationString = [|"+"; "-"; "*"; ""|]
    let operationsCombinationToFormat (operationsCombination: int[]) =
        operationsCombination
        |> Array.mapi (fun index operation -> (index, operation))
        |> Array.fold (fun current (index, operation) -> sprintf "%s%s{%d}" current operationString.[operation] (index + 1)) "{0}"

    //function to convert combination of 0-3 to expression tree, which computes expression and compares it with 100.
    let avaliableOperations = [|
        fun (x : Expr<int>) (y : Expr<int>) -> <@ %x + %y @>; 
        fun (x : Expr<int>) (y : Expr<int>) -> <@ %x - %y @>; 
        fun (x : Expr<int>) (y : Expr<int>) -> <@ %x * %y @>; 
        fun (x : Expr<int>) (y : Expr<int>) -> <@ (%x * 10 + %y) @>; 
        |]
    let arrayType = typeof<int[]>
    let param = new Quotations.Var("a", arrayType)
    let paramValue = Expr.Cast<int[]>(Quotations.Expr.Var(param))
    let basicExpressions = 
        [|0; 1; 2; 3; 4; 5; 6; 7; 8|] |> Array.map (fun x -> 
                let i0 = Expr.Cast<int>(Quotations.Expr.Value(x))
                <@ Array.get %paramValue %i0 @>)
    let foldPriority (expressions : Expr<int>[]) (operationsCombination : int[]) priority = 
        let mutable index = 0
        let mutable expressions1 = expressions
        let mutable operationsCombination1 = operationsCombination
        while index < operationsCombination1.Length do
            if operationsCombination1.[index] = priority then
                let lOperand = expressions1.[index]
                let rOperand = expressions1.[index + 1]
                let expr = avaliableOperations.[priority] lOperand rOperand
                expressions1 <- Array.concat [Array.take index expressions1; [|expr|]; Array.skip (index + 2) expressions1]
                operationsCombination1 <- Array.concat [Array.take index operationsCombination1; Array.skip (index + 1) operationsCombination1]
            else
                index <- index + 1
        (expressions1, operationsCombination1)

    let operationsCombinationToExpression (operationsCombination: int[]) : Expr<int[] -> bool> =
        let tree = List.fold (fun (expr, comb) priority -> foldPriority expr comb priority) (basicExpressions, operationsCombination) [3;2;1;0]
        let (expr, _) = tree
        let sum = expr.[0]
        let equality = <@ %sum = 100 @>
        Quotations.Expr.Cast<int[] -> bool>(Quotations.Expr.Lambda(param, equality))

    let expressionInfos = Array.map (fun operationCombination -> (operationsCombinationToExpression operationCombination, (fun () -> operationsCombinationToFormat operationCombination))) operations
    printfn "Generated expressions: %d ms." sw.ElapsedMilliseconds

    let totalProgress = expressionInfos.Length;
    let mutable currentProgress = 0
    use streamWriter = new System.IO.StreamWriter("out", false)
    let testExpression (expr : Expr<int[] -> bool>, format : unit -> string) : unit =
        printf "\rProgress: %d/%d          " currentProgress totalProgress
        currentProgress <- currentProgress + 1

        let hundreds = <@ Array.filter(%expr) input @>.Run()
        if hundreds.Length > 0 then
            let sFormat = format()
            hundreds
            |> Array.iter (fun xs ->
                let formatParams = Array.map box xs
                let result = System.String.Format(sFormat, formatParams)
                streamWriter.WriteLine(result)) 
        else ()

    Array.iter testExpression expressionInfos
    sw.Stop()
    printfn ""
    printfn "Total time: %d ms." sw.ElapsedMilliseconds;
    let key = System.Console.ReadKey()
    0 // return an integer exit code
