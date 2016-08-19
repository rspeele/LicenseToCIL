module LicenseToCIL.Examples.Switches
open System
open System.Diagnostics
open LicenseToCIL
open LicenseToCIL.Ops
open Microsoft.VisualStudio.TestTools.UnitTesting
    
let integerSwitch =
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

let digits =
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

// replicates IL from F# switch
let stringSwitchIfElse =
    let equals = typeof<string>.GetMethod("Equals", [|typeof<string>; typeof<string>|])
    cil {
        for str, i in digits do
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

let private ciDict =
    let dict = new System.Collections.Generic.Dictionary<string, int>(StringComparer.OrdinalIgnoreCase)
    for str, d in digits do
        dict.Add(str, d)
    dict

let fsStringSwitchDictCI str =
    let mutable i = -1
    if ciDict.TryGetValue(str, &i) then i else -1

type DigitEnum =
    | zero = 0
    | one = 1
    | two = 2
    | three = 3
    | four = 4
    | five = 5
    | six = 6
    | seven = 7
    | eight = 8
    | nine = 9
    | invalid = -1

let stringSwitchBy meth name =
    cil {
        yield ldarg 0
        yield meth
            [ for name, i in digits ->
                name, cil { yield ldc'i4 i; yield ret }
            ] zero
        yield ldc'i4 -1
        yield ret
    } |> toDelegate<Func<string, int>> name

let stringSwitch = stringSwitchBy StringSwitch.sensitive "cilStringSwitch"

let stringSwitchCI = stringSwitchBy StringSwitch.insensitive "cilStringSwitchCI"

let stringSwitchHash = stringSwitchBy StringSwitch.sensitiveByHash "cilStringSwitchHash"

let stringSwitchHashCI = stringSwitchBy StringSwitch.insensitiveByHash "cilStringSwitchHashCI"

let bench name f =
    let sw = new Stopwatch()
    let arr = [| for str, d in digits -> String.Copy(str), d |]
    sw.Start()
    for i = 0 to 10 * 1000 * 1000 do
        let str, d = arr.[i % arr.Length]
        if f str <> d then failwith "broken"
    sw.Stop()
    printfn "%s took %dms" name sw.ElapsedMilliseconds
    sw.ElapsedMilliseconds

let benchCI name f =
    let sw = new Stopwatch()
    let arr =
        [|
            for str, d in digits -> String.Copy(str), d
            for str, d in digits -> str.ToUpperInvariant(), d
        |]
    sw.Start()
    for i = 0 to 5 * 1000 * 1000 do
        let str, d = arr.[i % arr.Length]
        if f str <> d then failwith "broken"
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
            ] do Assert.AreEqual(expected, integerSwitch.Invoke(input))

    [<TestMethod>]
    member __.TestStringSwitch() =
        for input, expected in digits do
            Assert.AreEqual(expected, stringSwitch.Invoke(input))

    [<TestMethod>]
    member __.TestStringSwitchPerformance() =
        let fs = bench "F#" fsStringSwitch
        let ifElse = bench "If/Else" stringSwitchIfElse.Invoke
        let gen = bench "Switch" stringSwitch.Invoke
        let genH = bench "Switch Hash" stringSwitchHash.Invoke
        if gen > ifElse then failwith "Generated switch slower than if/else"
        if gen > fs then failwith "Generated switch slower than a match statement"

    [<TestMethod>]
    member __.TestStringSwitchPerformanceCI() =
        let fs = benchCI "F#" fsStringSwitchCI
        let fsDict = benchCI "F# dict" fsStringSwitchDictCI
        let gen = benchCI "Switch" stringSwitchCI.Invoke
        let genH = benchCI "Switch Hash" stringSwitchHashCI.Invoke
        if gen > fs then failwith "Generated switch slower than a match statement"