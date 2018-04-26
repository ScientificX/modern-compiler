# Chapter 2

Run the Alex command to generate `Lexer.hs`:

    alex Lexer.x

Usage of `Tokenizer.hs` from `ghci`:

    $ ghci
    GHCi, version 8.2.2: http://www.haskell.org/ghc/  :? for help
    Prelude> :l Tokenizer.hs
    [1 of 2] Compiling Lexer            ( Lexer.hs, interpreted )
    [2 of 2] Compiling Tokenizer        ( Tokenizer.hs, interpreted )
    Ok, two modules loaded.
    *Tokenizer> tokenizeFile "queens.tig"
