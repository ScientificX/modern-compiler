# Chapter 3

Run the Alex command to generate `Lexer.hs`:

    alex Lexer.x

Run the Happy commany to generate `Parser.hs`:

    happy Parser.y

Usage from `ghci`:

    Prelude> :l Main.hs
    [1 of 8] Compiling Abs              ( Abs.hs, interpreted )
    [2 of 8] Compiling Lexer            ( Lexer.hs, interpreted )
    [3 of 8] Compiling Parser           ( Parser.hs, interpreted )
    [4 of 8] Compiling Symbol           ( Symbol.hs, interpreted )
    [5 of 8] Compiling Types            ( Types.hs, interpreted )
    [6 of 8] Compiling Env              ( Env.hs, interpreted )
    [7 of 8] Compiling Semant           ( Semant.hs, interpreted )
    [8 of 8] Compiling Main             ( Main.hs, interpreted )
    Ok, 8 modules loaded.
    *Main>
