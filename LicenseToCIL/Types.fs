namespace LicenseToCIL
open LicenseToCIL.Stack

type Op<'stackin, 'stackout> = S<'stackin> -> S<'stackout> -> IL -> unit
