# Chapter 3

Run the Alex command to generate `Lexer.hs`:

    alex Lexer.x

Run the Happy commany to generate `Parser.hs`:

    happy Parser.y

Usage of `FileRunner.hs` from `ghci`:

    Prelude> :l FileRunner.hs
    [1 of 3] Compiling Lexer            ( Lexer.hs, interpreted )
    [2 of 3] Compiling Parser           ( Parser.hs, interpreted )
    [3 of 3] Compiling FileRunner       ( FileRunner.hs, interpreted )
    Ok, three modules loaded.
    *FileRunner> parseFile "../tiger-examples/queens.tig"
