module LicenseToCIL.Stack

[<AllowNullLiteral>]
type E = class end

/// Represents a stack state by nesting.
/// E S is the empty stack.
/// E S S is a stack with one element.
/// E S S S is a stack with two elements.
[<AllowNullLiteral>]
type S<'a> = class end

let inline pushed (_ : 'x S) = null : 'x S S
let inline popped (_ : 'x S S) = null : 'x S

let empty = null : E S