{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z_]

tokens :-

    $white                                      ;
    \\                                          { \s -> Slash }


    \|\|                                        { \s ->  Or  }
    \^                                          { \s ->  Xor }
    \&\&                                        { \s ->  And }
    \!                                          { \s ->  Not }
    \=\=                                        { \s ->  Eq  }
    \!\=                                        { \s ->  Ne  }
    \>\=                                        { \s ->  Ge  }
    \<\=                                        { \s ->  Le  }
    \>                                          { \s ->  Gt  }
    \<                                          { \s ->  Lt  }
    \+                                          { \s ->  Add }
    \-                                          { \s ->  Sub }
    \*                                          { \s ->  Mul }
    \/                                          { \s ->  Div }

    \.                                          { \s -> Dot                  }
    \,                                          { \s -> Comma                }
    \:                                          { \s -> Colon                }
    \;                                          { \s -> Semicolon            }
    \=                                          { \s -> Assignment           }
    \?                                          { \s -> Question             }
    \(                                          { \s -> OpeningBracket       }
    \)                                          { \s -> ClosingBracket       }
    \[                                          { \s -> OpeningSquareBracket }
    \]                                          { \s -> ClosingSquareBracket }
    \{                                          { \s -> OpeningBrace         }
    \}                                          { \s -> ClosingBrace         }

    import                                      { \s -> Import      }
    public                                      { \s -> Public      }
    static                                      { \s -> Static      }
    class                                       { \s -> Class       }
    void                                        { \s -> Void        }
    main                                        { \s -> Main        }
    java                                        { \s -> Java        }
    util                                        { \s -> Util        }
    Scanner                                     { \s -> Scanner     }
    System                                      { \s -> System      }
    in                                          { \s -> In          }
    out                                         { \s -> Out         }
    nextInt                                     { \s -> NextInt     }
    nextDouble                                  { \s -> NextDouble  }
    nextBoolean                                 { \s -> NextBoolean }
    nextLine                                    { \s -> NextLine    }
    print                                       { \s -> Print       }
    println                                     { \s -> PrintLn     }
    new                                         { \s -> New         }
    if                                          { \s -> If          }
    else                                        { \s -> Else        }
    while                                       { \s -> While       }
    return                                      { \s -> Return      }
    equals                                      { \s -> Equals      }

    boolean                                     { \s -> TBoolean }
    int                                         { \s -> TInt     }
    double                                      { \s -> TDouble  }
    String                                      { \s -> TString  }

    true                                        { \s -> BooleanValue True           }
    false                                       { \s -> BooleanValue False          }
    [1-9][$digit]*|0                            { \s -> IntValue (read s)           }
    [1-9][$digit]*\.[$digit]+|0\.[$digit]+      { \s -> DoubleValue (read s)        }
    \"[^\"]*\"                                  { \s -> StringValue (tail $ init s) }

    $alpha [$alpha $digit]*                     { Name }

{

data Token
    = Slash
    | Or
    | Xor
    | And
    | Not
    | Eq
    | Ne
    | Ge
    | Le
    | Gt
    | Lt
    | Add
    | Sub
    | Mul
    | Div
    | Dot
    | Comma
    | Colon
    | Semicolon
    | Assignment
    | Question
    | OpeningBracket
    | ClosingBracket
    | OpeningSquareBracket
    | ClosingSquareBracket
    | OpeningBrace
    | ClosingBrace
    | Import
    | Public
    | Static
    | Class
    | Void
    | Main
    | Java
    | Util
    | Scanner
    | System
    | In
    | Out
    | NextInt
    | NextDouble
    | NextBoolean
    | NextLine
    | Print
    | PrintLn
    | New
    | If
    | Else
    | While
    | Return
    | Equals
    | TBoolean
    | TInt
    | TDouble
    | TString
    | BooleanValue Bool
    | IntValue Int
    | DoubleValue Double
    | StringValue String
    | Name String
    deriving (Eq, Show)
}