open System
open FParsec

// Monadic parser based on this blog post:
//  http://bugsquash.blogspot.com/2011/01/parsing-with-applicative-functors-in-f.html

// Given %20, feed 2 0 as arguments, this will return a space character
let readHex a b = 
    char <| Byte.Parse(sprintf "%c%c" a b, System.Globalization.NumberStyles.HexNumber)

// Monadic parser to look for "%" and then two hex digits
let p_hex : Parser<char, unit> = 
    parse { 
        do! skipChar '%' 
        let! a = hex 
        let! b = hex 
        return readHex a b 
    }

[<EntryPoint>]
let main argv = 
    match run p_hex "%20" with
    | Success(result,_,_) ->
        if result = ' '
            then printfn "Correctly parsed a space"
            else failwithf "Incorrectly parsed %c" result
    | Failure(msg,_,_) -> failwithf "Parsing failed: \n%s" msg
    0 // return an integer exit code
