# Chapter 4

Run the Alex command to generate `Lexer.hs`:

    alex Lexer.x

Run the Happy commany to generate `Parser.hs`:

    happy Parser.y

Usage of `FileRunner.hs` from `ghci`:

    Prelude> :l FileRunner.hs
    [1 of 4] Compiling Abs              ( Abs.hs, interpreted )
    [2 of 4] Compiling Lexer            ( Lexer.hs, interpreted )
    [3 of 4] Compiling Parser           ( Parser.hs, interpreted )
    [4 of 4] Compiling FileRunner       ( FileRunner.hs, interpreted )
    Ok, four modules loaded.
    *FileRunner> parseFile "../tiger-examples/string.tig"

    LetExp [VarDec "string" True Nothing (StringExp "Test string" (Pos 2 17)) (Pos 2 7)] (SeqExp [CallExp "print" [VarExp (SimpleVar "string" (Pos 4 9))] (Pos 4 3)]) (Pos 1 1)

Usage of `PrettyPrint.hs` from `ghci`:

    Prelude> :l PrettyPrint.hs
    [1 of 2] Compiling Abs              ( Abs.hs, interpreted )
    [2 of 2] Compiling PrettyPrint      ( PrettyPrint.hs, interpreted )
    Ok, two modules loaded.
    *PrettyPrint> putStrLn $ prettyExp (LetExp [VarDec "string" True Nothing (StringExp "Test string" (Pos 2 17)) (Pos 2 7)] (SeqExp [CallExp "print" [VarExp (SimpleVar "string" (Pos 4 9))] (Pos 4 3)]) (Pos 1 1))

    let var string := "Test string" in (print(string)) end
