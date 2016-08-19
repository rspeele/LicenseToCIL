module LicenseToCIL.StringSwitch
open System
open System.Reflection
open System.Reflection.Emit
open LicenseToCIL
open LicenseToCIL.Stack
open LicenseToCIL.Ops

module private CharMethods =
    let toUpperInvariant =
        typeof<char>.GetMethod("ToUpperInvariant", BindingFlags.Public ||| BindingFlags.Static)

module private StringMethods =
    let length =
        typeof<string>.GetProperty("Length", BindingFlags.Public ||| BindingFlags.Instance).GetGetMethod()
    let charAtIndex =
        typeof<string>.GetProperty("Chars", BindingFlags.Public ||| BindingFlags.Instance).GetGetMethod()
    let compare3 =
        typeof<string>.GetMethod
            ( "Compare"
            , [| typeof<string>;  typeof<string>; typeof<StringComparison> |]
            )

module private StringComparerMethods =
    let ordinalIgnoreCase = typeof<StringComparer>.GetProperty("OrdinalIgnoreCase").GetGetMethod()
    let ordinal = typeof<StringComparer>.GetProperty("Ordinal").GetGetMethod()
    let getHashCode = typeof<StringComparer>.GetMethod("GetHashCode", [| typeof<string> |])

type private SwitchCulture =
    | IgnoreCase
    | CaseSensitive

let private stringComparison culture =
    match culture with
    | IgnoreCase -> StringComparison.OrdinalIgnoreCase
    | CaseSensitive -> StringComparison.Ordinal

let private stringComparer culture =
    match culture with
    | IgnoreCase -> call0 StringComparerMethods.ordinalIgnoreCase
    | CaseSensitive -> call0 StringComparerMethods.ordinal

let private normalizeString culture (str : string) =
    match culture with
    | IgnoreCase -> str.ToUpperInvariant()
    | CaseSensitive -> str

let private normalizeCharCode culture =
    match culture with
    | IgnoreCase -> call1 CharMethods.toUpperInvariant
    | CaseSensitive -> zero

type private StringCase<'stackin, 'stackout> = string * Op<'stackin, 'stackout>

let rec private stringsIfElse
    culture
    (input : Local)
    (cases: StringCase<_, _> array)
    (defaultCase : Op<_, _>) =
    cil {
        let! def = deflabel
        let! exit = deflabel
        for str, code in cases do
            let! next = deflabel
            yield ldstr str
            yield ldloc input
            yield ldc'i4 (int (stringComparison culture))
            yield call3 StringMethods.compare3
            yield brtrue next
            yield code
            yield br exit
            yield mark next
        yield mark def
        yield defaultCase
        yield mark exit
    }

let rec private stringsBinarySearch
    culture
    (input : Local)
    (cases: StringCase<_, _> array)
    (defaultCase : Op<_, _>) =
    cil {
        let! def = deflabel
        let! exit = deflabel
        for str, code in cases do
            let! next = deflabel
            yield ldstr str
            yield ldloc input
            yield ldc'i4 (int (stringComparison culture))
            yield call3 StringMethods.compare3
            yield brtrue next
            yield code
            yield br exit
            yield mark next
        yield mark def
        yield defaultCase
        yield mark exit
    }

let private scoreIndex (strings : string seq) i =
    let groups =
        strings
        |> Seq.groupBy (fun str -> str.[i])
        |> Seq.toArray
    if groups.Length <= 1 then Int32.MinValue else
    let largestGroupSize =
        groups
        |> Seq.map (snd >> Seq.length)
        |> Seq.max
    let branchingFactor =
        Switch.branchingFactor
            [ for (c, _) in groups -> int c, zero ]
            zero
    - branchingFactor
    - largestGroupSize

let private bestIndex length (strings : string seq) =
    seq { 0 .. length - 1}
    |> Seq.maxBy (scoreIndex strings)

let rec private stringsOfLength
    culture
    (input : Local)
    (length : int)
    (cases: StringCase<_, _> array)
    (defaultCase : Op<_, _>) =
    match cases with
    | [||] -> failwith "Logic error: empty arrays are not valid for this function"
    | [| str, code |] ->
        cil {
            yield ldstr str
            yield ldloc input
            yield ldc'i4 (int (stringComparison culture))
            yield call3 StringMethods.compare3
            let! eq = deflabel
            let! exit = deflabel
            yield brfalse eq
            yield defaultCase
            yield br exit
            yield mark eq
            yield code
            yield mark exit
        }
    | cases ->
        let bestIndex = cases |> Seq.map fst |> bestIndex length
        let groups =
            cases
            |> Array.groupBy (fun (s, _) -> s.[bestIndex])
        let subCases =
            seq {
                for chr, cases in groups ->
                    int chr, stringsOfLength culture input length cases defaultCase
            }
        cil {
            yield ldloc input
            yield ldc'i4 bestIndex
            yield call2 StringMethods.charAtIndex
            yield normalizeCharCode culture
            yield Switch.cases subCases defaultCase
        }

let private strings culture (cases : StringCase<_, _> seq) (defaultCase : Op<_, _>) =
    let cases =
        [| for str, code in cases -> normalizeString culture str, code |]
    cil {
        let! str = tmplocal typeof<string>
        let byLen =
            cases
            |> Array.groupBy (fun (s, _) -> s.Length)
            |> Array.map (fun (len, strs) -> len, stringsOfLength culture str len strs defaultCase)
        yield dup
        yield stloc str
        yield call1 StringMethods.length
        yield Switch.cases byLen defaultCase
    }

/// Case-insensitively switch on strings.
let insensitive cases defaultCase = strings IgnoreCase cases defaultCase

/// Case-sensitively switch on strings.
let sensitive cases defaultCase = strings CaseSensitive cases defaultCase

let private stringsByHash culture (cases : StringCase<_, _> seq) (defaultCase : Op<_, _>) =
    cil {
        let! str = tmplocal typeof<string>
        let comparer =
            match culture with
            | IgnoreCase -> StringComparer.OrdinalIgnoreCase
            | CaseSensitive -> StringComparer.Ordinal
        let byHash =
            cases
            |> Seq.groupBy (fun (s, _) -> comparer.GetHashCode(s))
            |> Seq.map (fun (hash, cases) -> hash, stringsIfElse culture str (cases |> Seq.toArray) defaultCase)
        yield stloc str
        yield stringComparer culture
        yield ldloc str
        yield callvirt2 StringComparerMethods.getHashCode
        yield Switch.cases byHash defaultCase
    }

/// Case-insensitively switch on strings by their hashcodes.
let insensitiveByHash cases defaultCase = stringsByHash IgnoreCase cases defaultCase

/// Case-sensitively switch on strings.
let sensitiveByHash cases defaultCase = stringsByHash CaseSensitive cases defaultCase

