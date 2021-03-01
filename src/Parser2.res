let code = `
if (WorldGen.destroyObject && false || true)
        {
            return;
        }
        let y: int = j - (world.getTile({ x: i, y: j }).frameY / 18) as int;
        let fX: int = world.getTile({ x: i, y: j }).frameX as int;
        let fX2: int = 0;
        let a: string = "hello";
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

let strToTokenList = (codeStr) => {
    Js.Array2.fromMap(Js.String2.castToArrayLike(codeStr), x => x)->Belt.List.fromArray
}

type expressionState =
    | Processing
    | ProcessingAnd
    | ProcessingOr
    | ProcessingWord(string)
    | ProcessingNumber(string)
    | ProcessingString(string)

let expressionStateToString = state => switch state {
    | Processing => "Processing"
    | ProcessingAnd => "ProcessingAnd"
    | ProcessingOr => "ProcessingOr"
    | ProcessingWord(string) => "ProcessingWord"
    | ProcessingNumber(string) => "ProcessingNumber"
    | ProcessingString(string) => "ProcessingString"
}

let isTerminatingCharacter = (terminationList, character) => {
    Js.log3("isTerminatingCharacter", terminationList, character)
    let result = terminationList
        ->Belt.Option.mapWithDefaultU(list{}, (.terminationToken) => terminationToken)
        ->Belt.List.someU((. terminationToken) => {
            character === terminationToken
        })
    Js.log(result)
    result
}

let isOperator = character => switch character {
    | "+"
    | "-"
    | "/"
    | "*"
    | "<"
    | ">"
    | "!"
    | "=" => true
    | _ => false
}

type rec parsedToken =
    | OpenBracket
    | CloseBracket
    | Dot
    | Operator(string)
    | Word(string)
    | String(string)
    | Number(string)
    | Function(string)
    | Comma
    | Semicolon
    | Colon
    | OpenBrace
    | CloseBrace
    | And
    | Or
    | BitwiseAnd
    | BitwiseOr

let parsedTokenToString = (. token) => switch token {
    | OpenBracket => "OpenBracket"
    | CloseBracket => "CloseBracket"
    | Word(word) => `Word(${word})`
    | Dot => "Dot"
    | Operator(operator) => `Operator(${operator})`
    | Number(number) => `Number(${number})`
    | Function(func) => `Function(${func})`
    | String(str) => `String(${str})`
    | Comma => "Comma"
    | Semicolon => "Semicolon"
    | OpenBrace => "OpenBrace"
    | CloseBrace => "CloseBrace"
    | Colon => "Colon"
    | And => "And"
    | BitwiseAnd => "BitwiseAnd"
    | Or => "Or"
    | BitwiseOr => "BitwiseOr"
}

type parsedExpressionResult = {
    tokens: list<parsedToken>,
    remainingCode: list<string>,
}

let parseExpression = (expression: list<string>): option<parsedExpressionResult> => {
    let rec innerParseExpressions = (~state: expressionState, ~expression: list<string>, ~parsedTokens: list<parsedToken>) => {
        switch expression {
            | list{currentToken, ...otherTokens} => switch (state, currentToken) {
                | (Processing, _) when currentToken->isAlphabetical => innerParseExpressions(~state=ProcessingWord(currentToken), ~expression=otherTokens, ~parsedTokens)
                | (Processing, _) when currentToken->isNumeric => innerParseExpressions(~state=ProcessingNumber(currentToken), ~expression=otherTokens, ~parsedTokens)
                | (Processing, operator) when currentToken->isOperator => innerParseExpressions(~state=Processing, ~expression=otherTokens, ~parsedTokens=list{Operator(operator), ...parsedTokens})
                | (Processing, "\"") => innerParseExpressions(~state=ProcessingString(""), ~expression=otherTokens, ~parsedTokens)
                | (Processing, "(") => innerParseExpressions(~state=Processing, ~expression=otherTokens, ~parsedTokens=list{OpenBracket, ...parsedTokens})
                | (Processing, ")") => innerParseExpressions(~state=Processing, ~expression=otherTokens, ~parsedTokens=list{CloseBracket, ...parsedTokens})
                | (Processing, "{") => innerParseExpressions(~state=Processing, ~expression=otherTokens, ~parsedTokens=list{OpenBrace, ...parsedTokens})
                | (Processing, "}") => innerParseExpressions(~state=Processing, ~expression=otherTokens, ~parsedTokens=list{CloseBrace, ...parsedTokens})
                | (Processing, ";") => innerParseExpressions(~state=Processing, ~expression=otherTokens, ~parsedTokens=list{Semicolon, ...parsedTokens})
                | (Processing, ".") => innerParseExpressions(~state=Processing, ~expression=otherTokens, ~parsedTokens=list{Dot, ...parsedTokens})
                | (Processing, ",") => innerParseExpressions(~state=Processing, ~expression=otherTokens, ~parsedTokens=list{Comma, ...parsedTokens})
                | (Processing, ":") => innerParseExpressions(~state=Processing, ~expression=otherTokens, ~parsedTokens=list{Colon, ...parsedTokens})
                | (Processing, "&") => innerParseExpressions(~state=ProcessingAnd, ~expression=otherTokens, ~parsedTokens)
                | (Processing, "|") => innerParseExpressions(~state=ProcessingOr, ~expression=otherTokens, ~parsedTokens)
                | (Processing, " " | "\n") => innerParseExpressions(~state=Processing, ~expression=otherTokens, ~parsedTokens)
                | (Processing, expressionToken) => {
                    Js.log(`Unexpected token: '${expressionToken}'`)
                    Js.log2("in state:", state->expressionStateToString)
                    None
                }
                | (ProcessingWord(word), _) when currentToken->isAlphanumeric => innerParseExpressions(~state=ProcessingWord(word++currentToken), ~expression=otherTokens, ~parsedTokens)
                | (ProcessingWord(word), _) => innerParseExpressions(~state=Processing, ~expression=list{currentToken, ...otherTokens}, ~parsedTokens=list{Word(word), ...parsedTokens})
                | (ProcessingNumber(number), _) when currentToken->isNumeric => innerParseExpressions(~state=ProcessingNumber(number++currentToken), ~expression=otherTokens, ~parsedTokens)
                | (ProcessingNumber(number), _) => innerParseExpressions(~state=Processing, ~expression=list{currentToken, ...otherTokens}, ~parsedTokens=list{Number(number), ...parsedTokens})
                | (ProcessingString(word), "\"") => innerParseExpressions(~state=Processing, ~expression=otherTokens, ~parsedTokens=list{String(word), ...parsedTokens})
                | (ProcessingString(word), _) => innerParseExpressions(~state=ProcessingString(word++currentToken), ~expression=otherTokens, ~parsedTokens)
                | (ProcessingAnd, "&") => innerParseExpressions(~state=Processing, ~expression=otherTokens, ~parsedTokens=list{And, ...parsedTokens})
                | (ProcessingOr, "|") => innerParseExpressions(~state=Processing, ~expression=otherTokens, ~parsedTokens=list{Or, ...parsedTokens})
                | (ProcessingAnd, _) => innerParseExpressions(~state=Processing, ~expression=otherTokens, ~parsedTokens=list{BitwiseAnd, ...parsedTokens})
                | (ProcessingOr, _) => innerParseExpressions(~state=Processing, ~expression=otherTokens, ~parsedTokens=list{BitwiseOr, ...parsedTokens})
            }
            | list{} => Some({
                tokens: parsedTokens,
                remainingCode: list{}
            })
        }
    }

    innerParseExpressions(~state=Processing, ~expression, ~parsedTokens=list{})
}

let parsed = (parseExpression(code->strToTokenList)->Belt.Option.getUnsafe).tokens->Belt.List.reverse
parsed->Belt.List.toArray->Belt.Array.mapU(parsedTokenToString)->Belt.Array.joinWithU(" ", (. a) => a)->Js.log
parsed->Belt.List.reduceU("", (. acc, token) => acc++switch token {
    | OpenBracket => "("
    | CloseBracket => ")"
    | Word(word) => word++" "
    | Dot => "."
    | Operator(operator) => `${operator} `
    | Number(number) => number
    | Function(func) => func
    | String(str) => `"${str}"`
    | Comma => ", "
    | Semicolon => ";\n"
    | OpenBrace => "\n{\n"
    | CloseBrace => "\n}\n"
    | Colon => ":"
    | And => "&&"
    | BitwiseAnd => "&"
    | Or => "||"
    | BitwiseOr => "|"
})->Js.log


/*type expressionPart =
    | FieldAccess(string, string)
    | MethodCall(string, list<string>)
    | Equal(string, string)
    | NotEqual(string, string)
    | GreaterThan(string, string)
    | GreaterThanOrEqual(string, string)
    | LessThan(string, string)
    | LessThanOrEqual(string, string)

type rec ifExpression =
    | IfExpression(expressionPart, option<booleanOperator>)
and booleanOperator =
    | And(ifExpression)
    | Or(ifExpression)

type codeParts =
    | If(ifExpression)

type parserState =
    | Start
    | IfStart
    | IfStartExpression
    | IfExpressionStack(list<expressionPart>)*/

type rec secondLevelToken =
    | If(list<parsedToken>, list<parsedToken>)
    | Statement(list<parsedToken>)
    | Block(list<secondLevelToken>)

type nestedBracketCount = int

type parserState =
    | ParsingTopLevel(list<parsedToken>)
    | ParsingIfBody(list<parsedToken>)
    | ParsingBlock(list<parsedToken>)

let parserStateToString = (parserState) => switch parserState {
    | ParsingTopLevel(_) => "ParsingTopLevel"
    | ParsingIfBody(_) => "ParsingIfBody"
    | ParsingBlock(_) => "ParsingBlock"
}

type ifParserState =
    | ParsingIfNoBracketYet
    | ParsingIf(nestedBracketCount)

let ifParserStateToString = (ifParserState) => switch ifParserState {
    | ParsingIfNoBracketYet => "ParseIfNoBracketYet"
    | ParsingIf(nestedBracketCount) => `ParsingIf(${nestedBracketCount->Belt.Int.toString})`
}

type parseIfResult = {
    tokens: list<parsedToken>,
    restOfTokens: list<parsedToken>,
}

let rec parseIfExpression = (~state: ifParserState, ~expressionTokens: list<parsedToken>, ~tokenBuffer: list<parsedToken>) => {
    switch expressionTokens {
        | list{currentToken, ...otherTokens} => switch (state, currentToken) {
            | (ParsingIfNoBracketYet, OpenBracket) => parseIfExpression(~state=ParsingIf(0), ~expressionTokens=otherTokens, ~tokenBuffer)
            | (ParsingIf(nestedBracketCount), OpenBracket) => parseIfExpression(~state=ParsingIf(nestedBracketCount+1), ~expressionTokens=otherTokens, ~tokenBuffer=list{currentToken, ...tokenBuffer})
            | (ParsingIf(0), CloseBracket) => Some({
                tokens: tokenBuffer,
                restOfTokens: otherTokens,
            })
            | (ParsingIf(nestedBracketCount), CloseBracket) => parseIfExpression(~state=ParsingIf(nestedBracketCount-1), ~expressionTokens=otherTokens, ~tokenBuffer)
            | (ParsingIf(_), _) => parseIfExpression(~state, ~expressionTokens=otherTokens, ~tokenBuffer)
            | _ => {
                Js.log3(ifParserStateToString(state), "- Unexpected token:", parsedTokenToString(. currentToken))
                None
            }
        }
        | list{} =>  None
    }
}

let parseExpressionTokens = (expressionTokens: list<parsedToken>): option<unit> => {
    let rec innerParseExpressionTokens = (~state: parserState, ~expressionTokens: list<parsedToken>, ~stack: list<parserState>, ~tokenBuffer: list<parsedToken>) => {
        switch expressionTokens {
            | list{currentToken, ...otherTokens} => switch (state, currentToken) {
                | (ParsingTopLevel(_), Word("if")) => innerParseExpressionTokens(~state=ParsingTopLevel)
                | (ParsingIfNoBracketYet, OpenBracket) => innerParseExpressionTokens(~state=ParsingIf(0), ~expressionTokens=otherTokens, ~stack, ~tokenBuffer)
                | (ParsingIf(nestedBracketCount), OpenBracket) => innerParseExpressionTokens(~state=ParsingIf(nestedBracketCount+1), ~expressionTokens=otherTokens, ~stack, ~tokenBuffer=list{currentToken, ...tokenBuffer})
                | (ParsingIf(0), CloseBracket) => innerParseExpressionTokens(~state=ParsingIfBody(tokenBuffer->Belt.List.reverse), ~expressionTokens=otherTokens, ~stack, ~tokenBuffer=list{})
                | (ParsingIf(nestedBracketCount), CloseBracket) => innerParseExpressionTokens(~state=ParsingIf(nestedBracketCount-1), ~expressionTokens=otherTokens, ~stack, ~tokenBuffer)
                | (ParsingIf(_), _) => innerParseExpressionTokens(~state, ~expressionTokens=otherTokens, ~stack, ~tokenBuffer)
                | (ParsingIfBody(_), OpenBrace) => innerParseExpressionTokens(~state=ParsingBlock(list{}), ~expressionTokens=otherTokens, ~stack=list{state, ...stack}, ~tokenBuffer)
                | (ParsingIfBody(_), Semicolon) => innerParseExpressionTokens(~state=ParsingBlock(list{}), ~expressionTokens=otherTokens, ~stack=list{state, ...stack}, ~tokenBuffer)
                | _ => {
                    Js.log3(parserStateToString(state), "- Unexpected token:", parsedTokenToString(. currentToken))
                    None
                }
            }
            | list{} =>  None
        }
    }

    innerParseExpressionTokens(~state=ParsingTopLevel(list{}), ~expressionTokens, ~stack=list{}, ~tokenBuffer=list{})
}

parseExpressionTokens(parsed)->ignore