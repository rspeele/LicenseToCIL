module LicenseToCIL.Examples.Switches
open System
open System.Diagnostics
open LicenseToCIL
open LicenseToCIL.Ops
open Microsoft.VisualStudio.TestTools.UnitTesting
    
let integerSwitch() =
    cil {
        yield ldarg 0
        yield Switch.cases
            [
                1, ldstr "one"
                2, ldstr "two"
                3, ldstr "three"
                4, ldstr "four"
                6, ldstr "six"
                
                50, ldstr "fifty"
                51, ldstr "fifty one"

                -100, ldstr "negative one hundred"
                -101, ldstr "negative one hundred and one"
                -102, ldstr "negative one hundred and two"

                19, ldstr "nineteen"
            ] (ldstr "default")
        yield ret
    } |> toDelegate<Func<int, string>> "cilIntegerSwitch"

let digits() =
    [
        "zero", 0
        "one", 1
        "two", 2
        "three", 3
        "four", 4
        "five", 5
        "six", 6
        "seven", 7
        "eight", 8
        "nine", 9
    ]

let fsStringSwitch str =
    match str with
    | "zero" -> 0
    | "one" -> 1
    | "two" -> 2
    | "three" -> 3
    | "four" -> 4
    | "five" -> 5
    | "six" -> 6
    | "seven" -> 7
    | "eight" -> 8
    | "nine" -> 9
    | _ -> -1

let fsStringSwitchCI str =
    if String.Equals(str, "zero", StringComparison.OrdinalIgnoreCase) then 0
    elif String.Equals(str, "one", StringComparison.OrdinalIgnoreCase) then 1
    elif String.Equals(str, "two", StringComparison.OrdinalIgnoreCase) then 2
    elif String.Equals(str, "three", StringComparison.OrdinalIgnoreCase) then 3
    elif String.Equals(str, "four", StringComparison.OrdinalIgnoreCase) then 4
    elif String.Equals(str, "five", StringComparison.OrdinalIgnoreCase) then 5
    elif String.Equals(str, "six", StringComparison.OrdinalIgnoreCase) then 6
    elif String.Equals(str, "seven", StringComparison.OrdinalIgnoreCase) then 7
    elif String.Equals(str, "eight", StringComparison.OrdinalIgnoreCase) then 8
    elif String.Equals(str, "nine", StringComparison.OrdinalIgnoreCase) then 9
    else -1

// replicates IL from F# switch
let stringSwitchIfElse() =
    let equals = typeof<string>.GetMethod("Equals", [|typeof<string>; typeof<string>|])
    cil {
        for str, i in digits() do
            let! next = deflabel
            yield ldarg 0
            yield ldstr str
            yield call2 equals
            yield brfalse's next
            yield ldc'i4 i
            yield ret
            yield mark next
        yield ldc'i4 -1
        yield ret
    } |> toDelegate<Func<string, int>> "cilStringIfElse"

let stringSwitchBy meth options name =
    cil {
        yield ldarg 0
        yield meth
            [ for name, i in options ->
                name, cil { yield ldc'i4 i; yield ret }
            ] zero
        yield ldc'i4 -1
        yield ret
    } |> toDelegate<Func<string, int>> name

let stringSwitch() = stringSwitchBy StringSwitch.sensitive (digits()) "cilStringSwitch"

let stringSwitchCI() = stringSwitchBy StringSwitch.insensitive (digits()) "cilStringSwitchCI"

let stringSwitchHash() = stringSwitchBy StringSwitch.sensitiveByHash (digits()) "cilStringSwitchHash"

let stringSwitchHashCI() = stringSwitchBy StringSwitch.insensitiveByHash (digits()) "cilStringSwitchHashCI"

let stringSwitchBinary() = stringSwitchBy StringSwitch.sensitiveBinary (digits()) "cilStringSwitchBinary"

let stringSwitchBinaryCI() = stringSwitchBy StringSwitch.insensitiveBinary (digits()) "cilStringSwitchBinaryCI"

let bench options name (f : Func<string, int>) =
    let sw = new Stopwatch()
    let arr = [| for str, d in options -> String.Copy(str), d |]
    sw.Start()
    for i = 0 to 20 * 1000 * 1000 do
        let str, d = arr.[i % arr.Length]
        if d <> f.Invoke(str) then failwithf "%d <> %d" d (f.Invoke(str))
    sw.Stop()
    printfn "%s took %dms" name sw.ElapsedMilliseconds
    sw.ElapsedMilliseconds

let benchCI options name (f : Func<string, int>) =
    let sw = new Stopwatch()
    let arr =
        [|
            for str, d in options -> String.Copy(str), d
            for str, d in options -> str.ToUpperInvariant(), d
        |]
    sw.Start()
    for i = 0 to 10 * 1000 * 1000 do
        let str, d = arr.[i % arr.Length]
        if d <> f.Invoke(str) then failwithf "%d <> %d" d (f.Invoke(str))
    sw.Stop()
    printfn "%s took %dms" name sw.ElapsedMilliseconds
    sw.ElapsedMilliseconds

[<TestClass>]
type TestSwitches() =
    [<TestMethod>]
    member __.TestIntegerSwitch() =
        for input, expected in
            [
                0, "default"
                1, "one"
                2, "two"
                3, "three"
                4, "four"
                5, "default"
                6, "six"
                7, "default"
                10, "default"
                19, "nineteen"
                49, "default"
                50, "fifty"
                51, "fifty one"
                52, "default"
                1000, "default"
                -1, "default"
                -99, "default"
                -100, "negative one hundred"
                -101, "negative one hundred and one"
                -102, "negative one hundred and two"
                -103, "default"
                -120, "default"
            ] do Assert.AreEqual(expected, integerSwitch().Invoke(input))

    [<TestMethod>]
    member __.TestStringSwitch() =
        for input, expected in (digits()) do
            Assert.AreEqual(expected, stringSwitch().Invoke(input))

    [<TestMethod>]
    member __.TestStringSwitchPerformance() =
        let fs = bench (digits()) "F#" (Func<string,int>(fsStringSwitch))
        let ifElse = bench (digits()) "If/Else" (stringSwitchIfElse())
        let gen = bench (digits()) "Switch" (stringSwitch())
        let genH = bench (digits()) "Switch Hash" (stringSwitchHash())
        let benB = bench (digits()) "Switch Binary" (stringSwitchBinary())
        if gen > int64 (double fs * 1.1) then failwith "Generated switch much slower than if/else"

    [<TestMethod>]
    member __.TestStringSwitchPerformanceCI() =
        let ciDict =
            let dict = new System.Collections.Generic.Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)
            for str, d in digits() do
                dict.Add(str, d)
            dict

        let fsStringSwitchDictCI str =
            let mutable i = -1
            if ciDict.TryGetValue(str, &i) then i else -1
        let fs = benchCI (digits()) "F#" (Func<string,int>(fsStringSwitchCI))
        let fsDict = benchCI (digits()) "F# dict" (Func<string,int>(fsStringSwitchDictCI))
        let gen = benchCI (digits()) "Switch" (stringSwitchCI())
        let genH = benchCI (digits()) "Switch Hash" (stringSwitchHashCI())
        let genB = benchCI (digits()) "Switch Binary" (stringSwitchBinaryCI())
        if gen > int64 (double fs * 1.1) then failwith "Generated switch much slower than chain of insensitive compares"

    [<TestMethod>]
    member __.TestSwitchLongNames() =
        let approvals =
            [
                "Draft", 0
                "InReview", 1
                "Reviewed", 2
                "ReadyForApproval", 3
                "InApprovalProcess", 4
                "ApprovalEdits", 5
                "ApprovalComplete", 6
                "ApprovalRevoked", 7
                "Show", 8
            ]
        let stringSwitch = stringSwitchBy StringSwitch.sensitive approvals "cilStringSwitch"

        let stringSwitchCI = stringSwitchBy StringSwitch.insensitive approvals "cilStringSwitchCI"

        let stringSwitchHash = stringSwitchBy StringSwitch.sensitiveByHash approvals "cilStringSwitchHash"

        let stringSwitchHashCI = stringSwitchBy StringSwitch.insensitiveByHash approvals "cilStringSwitchHashCI"

        let stringSwitchBinary = stringSwitchBy StringSwitch.sensitiveBinary approvals "cilStringSwitchBinary"

        let stringSwitchBinaryCI = stringSwitchBy StringSwitch.insensitiveBinary approvals "cilStringSwitchBinaryCI"

        [
            bench approvals "Switch" stringSwitch
            bench approvals "Switch Hash" stringSwitchHash
            bench approvals "Switch Binary" stringSwitchBinary

            benchCI approvals "Switch CI" stringSwitchCI
            benchCI approvals "Switch Hash CI" stringSwitchHashCI
            benchCI approvals "Switch Binary CI" stringSwitchBinaryCI
        ] |> ignore

    [<TestMethod>]
    [<Ignore>] // this test is fun, but takes too long to run
    member __.TestSwitchManyNames() =
        let names =
            [ for i = 0 to 1000 do
                yield Guid.NewGuid().ToString("n").Substring(0, 8), i
            ]
        let stringSwitch = stringSwitchBy StringSwitch.sensitive names "cilStringSwitch"

        let stringSwitchCI = stringSwitchBy StringSwitch.insensitive names "cilStringSwitchCI"

        let stringSwitchHash = stringSwitchBy StringSwitch.sensitiveByHash names "cilStringSwitchHash"

        let stringSwitchHashCI = stringSwitchBy StringSwitch.insensitiveByHash names "cilStringSwitchHashCI"

        let stringSwitchBinary = stringSwitchBy StringSwitch.sensitiveBinary names "cilStringSwitchBinary"

        let stringSwitchBinaryCI = stringSwitchBy StringSwitch.insensitiveBinary names "cilStringSwitchBinaryCI"

        [
            bench names "Switch" stringSwitch
            bench names "Switch Hash" stringSwitchHash
            bench names "Switch Binary" stringSwitchBinary

            benchCI names "Switch CI" stringSwitchCI
            benchCI names "Switch Hash CI" stringSwitchHashCI
            benchCI names "Switch Binary CI" stringSwitchBinaryCI
        ] |> ignore

    [<TestMethod>]
    [<Ignore>] // this test is fun, but takes too long to run
    member __.TestSwitchManySimilarNames() =
        let longPrefix = "GiantLongEnumName"
        let names =
            [ for i = 0 to 1000 do
                yield longPrefix + string i, i
            ]
        let stringSwitch = stringSwitchBy StringSwitch.sensitive names "cilStringSwitch"

        let stringSwitchCI = stringSwitchBy StringSwitch.insensitive names "cilStringSwitchCI"

        let stringSwitchHash = stringSwitchBy StringSwitch.sensitiveByHash names "cilStringSwitchHash"

        let stringSwitchHashCI = stringSwitchBy StringSwitch.insensitiveByHash names "cilStringSwitchHashCI"

        let stringSwitchBinary = stringSwitchBy StringSwitch.sensitiveBinary names "cilStringSwitchBinary"

        let stringSwitchBinaryCI = stringSwitchBy StringSwitch.insensitiveBinary names "cilStringSwitchBinaryCI"

        [
            bench names "Switch" stringSwitch
            bench names "Switch Hash" stringSwitchHash
            bench names "Switch Binary" stringSwitchBinary

            benchCI names "Switch CI" stringSwitchCI
            benchCI names "Switch Hash CI" stringSwitchHashCI
            benchCI names "Switch Binary CI" stringSwitchBinaryCI
        ] |> ignore

module Main =
    [<EntryPoint>]
    let main argv =
        printfn "don't run me this way ya goober"
        0