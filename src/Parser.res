let code = `
if (WorldGen.destroyObject)
        {
            return;
        }
        let y: int = j - (world.getTile({ x: i, y: j }).frameY / 18) as int;
        let fX: int = world.getTile({ x: i, y: j }).frameX as int;
        let fX2: int = 0;
        while (fX >= 5000)
        {
            fX -= 5000;
            fX2++;
        }
        if (fX2 != 0)
        {
            fX = (fX2 - 1) * 18;
        }
         for (int f2 = 0; f2 < h; f2++)
            {
                if (world.getTile({ x: x, y: y + f2 }).type == (ushort)type)
                {
                    WorldGen.KillTile(x, y + f2, false, false, false);
                }
            }
`

type value = Value(string)
type variable = VariableName(string)
type variableWithType = Variable(variable, string)

type condition =
    | SimpleExpression(variable)
    | EqualityExpression(variable, variable)

type rec token =
    | If(condition, codeBlock)
    | While(condition, codeBlock)
    | Let(variable, variableWithType)
    | LetValue(variableWithType, value)
    | Return
and codeBlock = CodeBlock(string)

let isNumeric = character => {
    switch character {
        | "0"
        | "1"
        | "2"
        | "3"
        | "4"
        | "5"
        | "6"
        | "7"
        | "8"
        | "9" => true
        | _ => false
    }
}
let isAlphabetical = character => {
    switch character {
        | "a" | "A"
        | "b" | "B"
        | "c" | "C"
        | "d" | "D"
        | "e" | "E"
        | "f" | "F"
        | "g" | "G"
        | "h" | "H"
        | "i" | "I"
        | "j" | "J"
        | "k" | "K"
        | "l" | "L"
        | "m" | "M"
        | "n" | "N"
        | "o" | "O"
        | "p" | "P"
        | "q" | "Q"
        | "r" | "R"
        | "s" | "S"
        | "t" | "T"
        | "u" | "U"
        | "v" | "V"
        | "w" | "W"
        | "x" | "X"
        | "y" | "Y"
        | "z" | "Z" => true
        | _ => false
    }
}

let isAlphanumeric = character => isNumeric(character) || isAlphabetical(character)

let rec parseLetType = (buffer: string, code: list<string>) => {
    switch code {
        | list{head, ...tail} => switch (buffer, head) {
            | (word, character) when isAlphanumeric(character) => parseLetType(word++character, tail)
            | ("", " ") => parseLetType("", tail)
            | (word, " ") => Some((word, tail))
            | (_word, character) => {
                Js.log3("Unexpected", character, "when parsing let type")
                None
            }
        }
        | _ => None
    }
}

let rec parseLetEquals = (buffer: string, code: list<string>) => {
    switch code {
        | list{head, ...tail} => switch (buffer, head) {
            // | ("", "=") => parseLetEqualsTo("", tail)
            | ("", " ") => parseLetEquals("", tail)
            | (_word, character) => {
                Js.log3("Unexpected", character, "when parsing let equals")
                None
            }
        }
        | _ => None
    }
}

let rec parseBlock = (buffer: string, code: list<string>) => {
    switch code {
        | list{head, ...tail} => switch (buffer, head) {
            | (word, character) when isAlphanumeric(character) => parseBlock(word ++ head, tail)
            | ("", "\n") | ("", " ") => parseBlock("", tail)
            | ("", "{") => {
                Js.log("OPEN BLOCK");
                parseBlock("", tail)
            }
            | ("", "}") => {
                Js.log("CLOSE BLOCK");
                parseBlock("", tail)
            }
            | ("if" as word, "(")
            | (("if" | "return") as word, "\n")
            | (("let"| "if" | "return") as word, " ")
            | ("return" as word, ";") => {
                switch word {
                    | "if" => {
                        Js.log("IF")
                        parseIf("", code)
                    }
                    | "return" => {
                        Js.log("RETURN")
                        parseBlock("", tail)
                    }
                    | "let" => {
                        Js.log("LET")
                        parseLet("", tail)
                    }
                    | _ => {
                        Js.log(`Word(${word})`)
                        parseBlock("", tail)
                    }
                }
            }
            | (word, character) => Js.log4("Unexpected", character, "when parsing word", word)
        }
        | list{} => ()
    }
} and parseIf = (buffer: string, code: list<string>) => {
    switch code {
        | list{head, ...tail} => switch (buffer, head) {
            | ("", " ")
            | ("", "(") => parseIf("", tail)
            | (word, character) when isAlphanumeric(character) => {
                parseIf(word++character, tail)
            }
            | (word, " ") => {
                Js.log(`Word(${word})`)
            }
            | (word, ".") => {
                Js.log(`NamespaceOrObject(${word})\nDOT`)
                parseIf("", tail)
            }
            | (word, ")") => {
                Js.log(`Word(${word})\nENDIF`)
                parseBlock("", tail)
            }
            | (_word, character) => Js.log2("Non alphanumeric in IF.", character)
        }
        | list{} => ()
    }
} and parseLet = (buffer: string, code: list<string>) => {
    switch code {
        | list{head, ...tail} => switch (buffer, head) {
            | (word, character) when isAlphanumeric(character) => {
                parseLet(word++character, tail)
            }
            | (word, " ") => {
                Js.log(`Word(${word})`)
            }
            | (word, ".") => {
                Js.log(`NamespaceOrObject(${word})\nDOT`)
                parseLet("", tail)
            }
            | (word, ")") => {
                Js.log(`Word(${word})\nENDIF`)
                parseBlock("", tail)
            }
            | (word, ":") => {
                Js.log(`Word(${word})`)
                switch parseLetType("", tail) {
                    | Some(letType, _tail) => {
                        Js.log(`TYPE(${letType})`)
                    }
                    | None => Js.log("Could not parse let type")
                }
            }
            | (word, character) => Js.log4("Unexpected", character, "when parsing let word", word)
        }
        | list{} => ()
    }
}

let parseCode = codeStr => {
    let code = Js.Array2.fromMap(Js.String2.castToArrayLike(codeStr), x => x)->Belt.List.fromArray
    parseBlock("", code)
}

// parseCode(code)

let strToTokenList = (codeStr) => {
    Js.Array2.fromMap(Js.String2.castToArrayLike(codeStr), x => x)->Belt.List.fromArray
}

type expressionState =
    | ExpressionStart(option<string>)
    | ExpectingWord
    | ProcessingWordOrExpression
    | WaitingTerminateWord
    | WaitingTerminateString
    | WaitingTerminateArgument
    | WaitingTerminateNumberArgument
    | WaitingTerminateNumber
    | ExpectingCloseBracketOrExpression
    | ExpectingCloseBracketOrFunctionArguments
    | WaitingOperation
    | ExpectingExpressionEndOrOperator(option<string>)

let expressionStateToString = state => switch state {
    | ExpressionStart(_)
    | ExpectingWord => "ExpectingWord"
    | ProcessingWordOrExpression => "ProcessingWordOrExpression"
    | WaitingTerminateWord => "WaitingTerminateWord"
    | WaitingTerminateString =>"WaitingTerminateString"
    | WaitingTerminateArgument=> "WaitingTerminateArgument"
    | WaitingTerminateNumberArgument=> "WaitingTerminateNumberArgument"
    | WaitingTerminateNumber=> "WaitingTerminateNumber"
    | ExpectingCloseBracketOrExpression=> "ExpectingCloseBracketOrExpression"
    | ExpectingCloseBracketOrFunctionArguments=> "ExpectingCloseBracketOrFunctionArguments"
    | WaitingOperation => "WaitingOperation"
    | ExpectingExpressionEndOrOperator(_) => "ExpectingExpressionEndOrOperator"
}

let isOperator = character => switch character {
    | "+"
    | "-"
    | "/"
    | "*" => true
    | _ => false
}

type rec parsedToken =
    | OpenBracket
    | ClosedBracket
    | Dot
    | Operator(string)
    | Word(string)
    | String(string)
    | Number(int)
    | Function(string)
    | Argument(string)
    | LastArgument(string)
    | NumberArgument(int)
    | LastNumberArgument(int)

type parsedExpressionResult = {
    tokens: list<parsedToken>,
    remainingCode: list<string>,
}

let parseExpression = (buffer: string, code: list<string>, terminatingToken: option<string>) => {
    let rec innerParseExpression = (
        ~buffer: string,
        ~code: list<string>,
        ~state: expressionState,
        ~stack: list<expressionState>, 
        ~parsedTokens: list<parsedToken>,
    ): option<parsedExpressionResult> => {
        switch code {
            | list{head, ...tail} => switch (state, buffer, head) {
                | (ExpressionStart(terminatingToken), "", "(") when terminatingToken !== Some("(") => {
                    Js.log("OPENBRACKET")
                    innerParseExpression(~buffer="", ~code=tail, ~state=ExpectingCloseBracketOrExpression, ~stack=list{ExpectingExpressionEndOrOperator(terminatingToken), ...stack}, ~parsedTokens=list{OpenBracket, ...parsedTokens})
                }
                | (ExpressionStart(terminatingToken), "", character) when terminatingToken !== Some(character) && isAlphabetical(character) => {
                    innerParseExpression(~buffer=character, ~code=tail, ~state=WaitingTerminateWord, ~stack=list{ExpectingExpressionEndOrOperator(terminatingToken), ...stack}, ~parsedTokens)
                }
                | (ExpressionStart(terminatingToken), "", "\"") when terminatingToken !== Some("\"") => {
                    innerParseExpression(~buffer="", ~code=tail, ~state=WaitingTerminateString, ~stack=list{ExpectingExpressionEndOrOperator(terminatingToken), ...stack}, ~parsedTokens)
                }
                | (ProcessingWordOrExpression, "", "(") => {
                    Js.log("OPENBRACKET")
                    innerParseExpression(~buffer="", ~code=tail, ~state=ExpectingCloseBracketOrExpression, ~stack, ~parsedTokens=list{OpenBracket, ...parsedTokens})
                }
                | (ExpectingCloseBracketOrExpression | ProcessingWordOrExpression, "", " ") => innerParseExpression(~buffer="", ~code=tail, ~state, ~stack, ~parsedTokens)
                | (ProcessingWordOrExpression, "", character) when isNumeric(character) => {
                    innerParseExpression(~buffer=character, ~code=tail, ~state=WaitingTerminateNumber, ~stack, ~parsedTokens)
                }
                | (ProcessingWordOrExpression, "", character) when isAlphabetical(character) => {
                    innerParseExpression(~buffer=character, ~code=tail, ~state=WaitingTerminateWord, ~stack, ~parsedTokens)
                }
                | (ExpectingCloseBracketOrExpression, "", character) when isNumeric(character) => {
                    innerParseExpression(~buffer=character, ~code=tail, ~state=WaitingTerminateNumber, ~stack=list{state, ...stack}, ~parsedTokens)
                }
                | (ExpectingCloseBracketOrExpression, "", character) when isAlphabetical(character) => {
                    innerParseExpression(~buffer=character, ~code=tail, ~state=WaitingTerminateWord, ~stack=list{state, ...stack}, ~parsedTokens)
                }
                | (ExpectingCloseBracketOrExpression, "", character) when isOperator(character) => {
                    Js.log(`OPERATOR(${character})`)
                    innerParseExpression(~buffer="", ~code=tail, ~state=ProcessingWordOrExpression, ~stack=list{state, ...stack}, ~parsedTokens=list{Operator(character), ...parsedTokens})
                }
                | (ExpectingCloseBracketOrExpression, "", ")") => {
                    Js.log("CLOSEBRACKET")
                    innerParseExpression(~buffer="", ~code=tail, ~state=stack->Belt.List.headExn, ~stack=stack->Belt.List.tailExn, ~parsedTokens=list{ClosedBracket, ...parsedTokens})
                }
                // Termination
                | (WaitingTerminateNumber, number, character) when Some(character) === terminatingToken => {
                    Js.log(`NUMBER(${number})`)
                    innerParseExpression(~buffer="", ~code=list{}, ~state=stack->Belt.List.headExn, ~stack=stack->Belt.List.tailExn, ~parsedTokens=list{Number(number->Belt.Int.fromString->Belt.Option.getExn), ...parsedTokens})
                }
                | (WaitingTerminateWord, word, character) when Some(character) === terminatingToken => {
                    Js.log(`WORD(${word})`)
                    innerParseExpression(~buffer="", ~code=list{}, ~state=stack->Belt.List.headExn, ~stack=stack->Belt.List.tailExn, ~parsedTokens=list{Word(word), ...parsedTokens})
                }
                | (ProcessingWordOrExpression, "", character) when Some(character) === terminatingToken => {
                    innerParseExpression(~buffer="", ~code=list{}, ~state=stack->Belt.List.headExn, ~stack=stack->Belt.List.tailExn, ~parsedTokens)
                }
                | (ExpectingExpressionEndOrOperator(terminatingToken), "", character) when Some(character) === terminatingToken => {
                    Js.log(`END`)
                    Some({
                        tokens: parsedTokens,
                        remainingCode: tail,
                    })
                }

                // Word Parsing
                | (WaitingTerminateWord, word, character) => parseWord(word, character, tail, state, stack, parsedTokens)

                // Function parsing
                | (ExpectingCloseBracketOrFunctionArguments, word, character) => parseFunction(word, character, tail, state, stack, parsedTokens)
                | (WaitingTerminateArgument, word, character) => parseFunction(word, character, tail, state, stack, parsedTokens)
                | (WaitingTerminateNumberArgument, word, character) => parseFunction(word, character, tail, state, stack, parsedTokens)

                // Number parsing
                | (WaitingTerminateNumber, word, character) => parseNumber(word, character, tail, state, stack, parsedTokens)
                | (WaitingTerminateString, word, character) => parseString(word, character, tail, state, stack, parsedTokens)

                | (ExpectingWord, "", character) when isAlphabetical(character) => {
                    innerParseExpression(~buffer=character, ~code=tail, ~state=WaitingTerminateWord, ~stack, ~parsedTokens)
                }
                | (ProcessingWordOrExpression, "", "\"") => {
                    innerParseExpression(~buffer="", ~code=tail, ~state=WaitingTerminateString, ~stack, ~parsedTokens)
                }
                | (ExpectingExpressionEndOrOperator(terminatingToken), "", " ") when terminatingToken !== Some(" ") => innerParseExpression(~buffer="", ~code=tail, ~state=ExpectingExpressionEndOrOperator(terminatingToken), ~stack, ~parsedTokens)
                | (ExpectingExpressionEndOrOperator(terminatingToken), "", character) when terminatingToken !== Some(character) && isOperator(character) => {
                    Js.log(`OPERATOR(${character})`)
                    innerParseExpression(~buffer="", ~code=tail, ~state=ProcessingWordOrExpression, ~stack=list{state, ...stack}, ~parsedTokens=list{Operator(character), ...parsedTokens})
                }

                | _ => {
                    Js.log2(`state:`, expressionStateToString(state))
                    Js.log2("Buffer:", buffer)
                    Js.log2("Head:", head)
                    None
                }
            }
            | _ => switch (state, buffer) {
                | (WaitingTerminateNumber, number) => {
                    Js.log(`NUMBER(${number})`)
                    innerParseExpression(~buffer="", ~code=list{}, ~state=stack->Belt.List.headExn, ~stack=stack->Belt.List.tailExn, ~parsedTokens=list{Number(number->Belt.Int.fromString->Belt.Option.getExn), ...parsedTokens})
                }
                | (WaitingTerminateWord, word) => {
                    Js.log(`WORD(${word})`)
                    innerParseExpression(~buffer="", ~code=list{}, ~state=stack->Belt.List.headExn, ~stack=stack->Belt.List.tailExn, ~parsedTokens=list{Word(word), ...parsedTokens})
                }
                | (ProcessingWordOrExpression, "") => {
                    innerParseExpression(~buffer="", ~code=list{}, ~state=stack->Belt.List.headExn, ~stack=stack->Belt.List.tailExn, ~parsedTokens)
                }
                | (ExpectingExpressionEndOrOperator(terminatingToken), "") => {
                    Js.log(`END`)
                    Some({
                        tokens: parsedTokens,
                        remainingCode: list{},
                    })
                }
                | _ => {
                    Js.log2(`state:`, expressionStateToString(state))
                    Js.log2("Buffer:", buffer)
                    Js.log("list empty")
                    None
                }
            }
        }
    } and parseString = (buffer: string, currentToken: string, nextTokens: list<string>, state: expressionState, stack: list<expressionState>, parsedTokens: list<parsedToken>) => {
        switch (state, buffer, currentToken) {
            | (WaitingTerminateString, word, "\"") => {
                Js.log(`STRING(${word})`)
                innerParseExpression(~buffer="", ~code=nextTokens, ~state=stack->Belt.List.headExn, ~stack=stack->Belt.List.tailExn, ~parsedTokens=list{String(word), ...parsedTokens})
            }
            | (WaitingTerminateString, word, character) => innerParseExpression(~buffer=word++character, ~code=nextTokens, ~state=WaitingTerminateString, ~stack, ~parsedTokens)
            | _ => {
                Js.log2(`state:`, expressionStateToString(state))
                Js.log2("Buffer:", buffer)
                Js.log2("Head:", currentToken)
                None
            }
        }
    } and parseWord = (buffer: string, currentToken: string, nextTokens: list<string>, state: expressionState, stack: list<expressionState>, parsedTokens: list<parsedToken>) => {
        switch (state, buffer, currentToken) {
            | (WaitingTerminateWord, word, character) when isAlphanumeric(character) => innerParseExpression(~buffer=word++character, ~code=nextTokens, ~state=WaitingTerminateWord, ~stack, ~parsedTokens)
            | (WaitingTerminateWord, word, " ") => {
                Js.log(`WORD(${word})`)
                innerParseExpression(~buffer="", ~code=nextTokens, ~state=stack->Belt.List.headExn, ~stack=stack->Belt.List.tailExn, ~parsedTokens=list{Word(word), ...parsedTokens})
            }
            | (WaitingTerminateWord, word, character) when isOperator(character) => {
                Js.log(`WORD(${word})`)
                Js.log(`OPERATOR(${character})`)
                innerParseExpression(~buffer="", ~code=nextTokens, ~state=ProcessingWordOrExpression, ~stack, ~parsedTokens=list{Operator(character), Word(word), ...parsedTokens})
            }
            | (WaitingTerminateWord, word, ".") => {
                Js.log(`WORD(${word})`)
                Js.log(`DOT`)
                innerParseExpression(~buffer="", ~code=nextTokens, ~state=ExpectingWord, ~stack, ~parsedTokens=list{Dot, Word(word), ...parsedTokens})
            }
            | (WaitingTerminateWord, word, "(") => {
                Js.log(`FUNCTION(${word})`)
                Js.log(`OPENBRACKET`)
                innerParseExpression(~buffer="", ~code=nextTokens, ~state=ExpectingCloseBracketOrFunctionArguments, ~stack, ~parsedTokens=list{OpenBracket, Function(word), ...parsedTokens})
            }
            | _ => {
                Js.log2(`state:`, expressionStateToString(state))
                Js.log2("Buffer:", buffer)
                Js.log2("Head:", currentToken)
                None
            }
        }
    } and parseNumber = (buffer: string, currentToken: string, nextTokens: list<string>, state: expressionState, stack: list<expressionState>, parsedTokens: list<parsedToken>) => {
        switch (state, buffer, currentToken) {
            | (WaitingTerminateNumber, word, character) when isNumeric(character) => {
                innerParseExpression(~buffer=word++character, ~code=nextTokens, ~state=WaitingTerminateNumber, ~stack, ~parsedTokens)
            }
            | (WaitingTerminateNumber, word, character) when isOperator(character) => {
                Js.log(`NUMBER(${word})`)
                Js.log(`OPERATOR(${character})`)
                innerParseExpression(~buffer="", ~code=nextTokens, ~state=ProcessingWordOrExpression, ~stack, ~parsedTokens=list{Operator(character), Number(word->Belt.Int.fromString->Belt.Option.getExn), ...parsedTokens})
            }
            | (WaitingTerminateNumber, word, " ") => {
                Js.log(`NUMBER(${word})`)
                innerParseExpression(~buffer="", ~code=nextTokens, ~state=stack->Belt.List.headExn, ~stack=stack->Belt.List.tailExn, ~parsedTokens=list{Number(word->Belt.Int.fromString->Belt.Option.getExn), ...parsedTokens})
            }
            | (WaitingTerminateNumber, word, ")") => {
                Js.log(`NUMBER(${word})`)
                innerParseExpression(~buffer="", ~code=list{currentToken, ...nextTokens}, ~state=stack->Belt.List.headExn, ~stack=stack->Belt.List.tailExn, ~parsedTokens=list{Number(word->Belt.Int.fromString->Belt.Option.getExn), ...parsedTokens})
            }
            | _ => {
                Js.log2(`state:`, expressionStateToString(state))
                Js.log2("Buffer:", buffer)
                Js.log2("Head:", currentToken)
                None
            }
        }
    } and parseFunction = (buffer: string, currentToken: string, nextTokens: list<string>, state: expressionState, stack: list<expressionState>, parsedTokens: list<parsedToken>) => {
        switch (state, buffer, currentToken) {
            | (ExpectingCloseBracketOrFunctionArguments, "", " ") => {
                innerParseExpression(~buffer="", ~code=nextTokens, ~state=ExpectingCloseBracketOrFunctionArguments, ~stack, ~parsedTokens)
            }
            | (ExpectingCloseBracketOrFunctionArguments, "", ")") => {
                Js.log(`CLOSEBRACKET`)
                innerParseExpression(~buffer="", ~code=nextTokens, ~state=stack->Belt.List.headExn, ~stack=stack->Belt.List.tailExn, ~parsedTokens=list{ClosedBracket, ...parsedTokens})
            }
            | (ExpectingCloseBracketOrFunctionArguments, "", character) when isAlphabetical(character) => {
                innerParseExpression(~buffer=character, ~code=nextTokens, ~state=WaitingTerminateArgument, ~stack=list{state, ...stack}, ~parsedTokens)
            }
            | (ExpectingCloseBracketOrFunctionArguments, "", character) when isNumeric(character) => {
                innerParseExpression(~buffer=character, ~code=nextTokens, ~state=WaitingTerminateNumberArgument, ~stack=list{state, ...stack}, ~parsedTokens)
            }
            | (WaitingTerminateArgument, word, ")") => {
                Js.log(`LASTARGUMENT(${word})`)
                innerParseExpression(~buffer="", ~code=list{currentToken, ...nextTokens}, ~state=stack->Belt.List.headExn, ~stack=stack->Belt.List.tailExn, ~parsedTokens=list{LastArgument(word), ...parsedTokens})
            }
            | (WaitingTerminateNumberArgument, word, ")") => {
                Js.log(`LASTNUMBERARGUMENT(${word})`)
                innerParseExpression(~buffer="", ~code=list{currentToken, ...nextTokens}, ~state=stack->Belt.List.headExn, ~stack=stack->Belt.List.tailExn, ~parsedTokens=list{LastNumberArgument(word->Belt.Int.fromString->Belt.Option.getExn), ...parsedTokens})
            }
            | (WaitingTerminateArgument, word, character) when isAlphanumeric(character) => {
                innerParseExpression(~buffer=word++character, ~code=nextTokens, ~state=WaitingTerminateArgument, ~stack, ~parsedTokens)
            }
            | (WaitingTerminateNumberArgument, word, character) when isNumeric(character) => {
                innerParseExpression(~buffer=word++character, ~code=nextTokens, ~state=WaitingTerminateNumberArgument, ~stack, ~parsedTokens)
            }
            | (WaitingTerminateArgument, word, ",") => {
                Js.log(`ARGUMENT(${word})`)
                innerParseExpression(~buffer="", ~code=nextTokens, ~state=stack->Belt.List.headExn, ~stack=stack->Belt.List.tailExn, ~parsedTokens=list{Argument(word), ...parsedTokens})
            }
            | (WaitingTerminateNumberArgument, word, ",") => {
                Js.log(`NUMBERARGUMENT(${word})`)
                innerParseExpression(~buffer="", ~code=nextTokens, ~state=stack->Belt.List.headExn, ~stack=stack->Belt.List.tailExn, ~parsedTokens=list{NumberArgument(word->Belt.Int.fromString->Belt.Option.getExn), ...parsedTokens})
            }
            | _ => {
                Js.log2(`state:`, expressionStateToString(state))
                Js.log2("Buffer:", buffer)
                Js.log2("Head:", currentToken)
                None
            }
        }
    }
    innerParseExpression(~buffer, ~code, ~state=ExpressionStart(terminatingToken), ~stack=list{}, ~parsedTokens=list{})
}

let parsed = (parseExpression("", `"hellothere" + Method.name(5) + "hello" + (a.name.b + (5 / 2)+10) + b.getAge(hello, 123, h12),`->strToTokenList, Some(","))->Belt.Option.getUnsafe).tokens->Belt.List.reverse
parsed->Belt.List.reduceU("", (. acc, token) => acc++switch token {
    | OpenBracket => "("
    | ClosedBracket => ")"
    | Word(word) => word
    | Dot => "."
    | Operator(operator) => ` ${operator} `
    | Number(number) => Belt.Int.toString(number)
    | Function(func) => func
    | Argument(arg) => arg++", "
    | NumberArgument(argNum) => Belt.Int.toString(argNum)++", "
    | LastArgument(arg) => arg
    | LastNumberArgument(argNum) => Belt.Int.toString(argNum)
    | String(str) => `"${str}"`
})->Js.log