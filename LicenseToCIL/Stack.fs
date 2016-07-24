﻿module LicenseToCIL.Stack

[<AllowNullLiteral>]
type E = class end

/// Represents a stack state by nesting.
/// E S is the empty stack.
/// E S S is a stack with one element.
/// E S S S is a stack with two elements.
[<AllowNullLiteral>]
type S<'a> = class end
/// Represents an empty stack.
type S0 = E S
/// Represents a stack with *at least* one element.
type S1<'x> = 'x S S
/// Represents a stack with *at least* two elements.
type S2<'x> = 'x S S S
/// Represents a stack with *at least* three elements.
type S3<'x> = 'x S S S S

let inline pushed (_ : 'x S) = null : 'x S S
let inline popped (_ : 'x S S) = null : 'x S

let empty = null : S0