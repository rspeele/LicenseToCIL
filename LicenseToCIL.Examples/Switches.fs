module LicenseToCIL.Examples.Switches
open System
open LicenseToCIL
open LicenseToCIL.Ops
open Microsoft.VisualStudio.TestTools.UnitTesting
    
let fancySwitch =
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
    } |> toDelegate<Func<int, string>> "cilSwitch"

[<TestClass>]
type TestSwitches() =
    [<TestMethod>]
    member __.TestAll() =
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
            ] do Assert.AreEqual(expected, fancySwitch.Invoke(input))
