// Applicative parser based on this blog post:
//  http://bugsquash.blogspot.com/2011/01/parsing-with-applicative-functors-in-f.html

open FParsec;
open System

// Given %20, feed 2 0 as arguments, this will return a space character
let readHex a b = 
    char <| Byte.Parse(sprintf "%c%c" a b, System.Globalization.NumberStyles.HexNumber)


// Parse.Return puts whatever value we pass into the monad.
// At least, that is the standard defintion of return w.r.t. a monad.
let puree = parse.Return 

// This is a WTF
let (<*>) f a = 
    parse { 
        let! f' = f 
        let! a' = a 
        return f' a'
    } 

// Mimic the modadic parser, but an applicative style
// The puree returns the result in the parse monad (lifts it into the monad). But
// it is applied to the result of applying hex, hex, and skipchair.
let a_hex : Parser<char, unit> = 
    puree (fun skipCharResult hex1result hex2result -> readHex hex1result hex2result)
        <*> skipChar '%'
        <*> hex
        <*> hex

// Make a_hex simpler by using the map function to hide the call to puree
let (<!>) f a = puree f <*> a
// Now, we can use the <!> instead of the puree. We are defining a new operator here, I guess.
let a_hex2 : Parser<char, unit> = 
    (fun skipCharResult hex1result hex2result -> readHex hex1result hex2result)
        <!> skipChar '%'
        <*> hex
        <*> hex

// Next, the skipChair call is unit (in the monadic parser we used do! to ignore it).
// Using *> what we can do is ignore it. I find this below pretty hard to read...  but
// it takes the result of a and b, ignores a and returns only b.
let ( *>) a b = puree (fun resultA resultB -> resultB) <*> a <*> b

// Now, we can do the same thing with the parser, but don't have to do the ignore we did above
// Though, this does not make the code any cleaner.
let a_hex3 : Parser<char, unit> = 
    (fun hex1result hex2result -> readHex hex1result hex2result)
        <!> skipChar '%'
        *> hex
        <*> hex

// Since readHex takes two arguments above, we don't need to put that hex1result function in there
// it is already doing that!
let a_hex4 : Parser<char, unit> = 
    readHex
    <!> skipChar '%'
     *> hex
    <*> hex

// Run the tests. Replace a_hex with whatever is needed.
[<EntryPoint>]
let main argv = 
    match run a_hex4 "%20" with
    | Success(result,_,_) ->
        if result = ' '
            then printfn "Correctly parsed a space"
            else failwithf "Incorrectly parsed %c" result
    | Failure(msg,_,_) -> failwithf "Parsing failed: \n%s" msg
    0 // return an integer exit code
