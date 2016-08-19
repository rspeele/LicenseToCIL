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
    let compare6 =
        typeof<string>.GetMethod
            ( "Compare"
            , [| typeof<string>; typeof<int>; typeof<string>; typeof<int>; typeof<int>; typeof<StringComparison> |]
            )

type private SwitchCulture =
    | IgnoreCase
    | CaseSensitive

let private stringComparison culture =
    match culture with
    | IgnoreCase -> StringComparison.OrdinalIgnoreCase
    | CaseSensitive -> StringComparison.Ordinal

let private normalizeString culture (str : string) =
    match culture with
    | IgnoreCase -> str.ToUpperInvariant()
    | CaseSensitive -> str

let private normalizeCharCode culture =
    match culture with
    | IgnoreCase -> call1 CharMethods.toUpperInvariant
    | CaseSensitive -> zero

type private StringCase<'stackin, 'stackout> = string * Op<'stackin, 'stackout>

let private largestGroupSize (strings : string seq) i =
    strings
    |> Seq.groupBy (fun str -> str.[i])
    |> Seq.map (snd >> Seq.length)
    |> Seq.max

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
            yield ldc'i4 0
            yield ldloc input
            yield ldc'i4 0
            yield ldc'i4 length
            yield ldc'i4 (int (stringComparison culture))
            yield call6 StringMethods.compare6
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
        let strings = cases |> Seq.map fst
        let bestIndex =
            seq { 0 .. length - 1}
            |> Seq.minBy (largestGroupSize strings)
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
            |> Seq.map (fun (len, strs) -> len, stringsOfLength culture str len strs defaultCase)
        yield dup
        yield stloc str
        yield call1 StringMethods.length
        yield Switch.cases byLen defaultCase
    }

/// Case-insensitively switch on strings.
let insensitive cases defaultCase = strings IgnoreCase cases defaultCase

/// Case-sensitively switch on strings.
let sensitive cases defaultCase = strings CaseSensitive cases defaultCase

