module Oblig12016 where

import Data.Char

data Ast = Number Integer | Name String | App Ast [Ast] | Block [Ast] |
    Case Ast [Ast] | Bool Ast Ast Ast | Default | Set String Ast
    | Lambda String Ast | Function String Ast Context deriving (Eq,Show,Ord)

isLambda :: Ast -> Bool
isLambda (Lambda _ _) = True
isLambda _ = False

type Memory = (Integer, Integer -> Maybe Ast)

emptyMem :: Memory
emptyMem = (0, \_ -> Nothing)

lookupMem :: Memory -> Integer -> Maybe Ast
lookupMem (num, mem) n = mem n

addToMem :: Memory -> Ast -> (Integer, Memory)
addToMem (num, func) ast = (num, (num + 1, \n -> if n == num then Just ast else func n))

modifyMem :: Memory -> Integer -> Ast -> (Integer, Memory)
modifyMem (num, func) pos ast = (pos, (num, \n -> if n == pos then Just ast else func n))

newtype Context = Context (String -> Maybe Integer)
instance Show Context where
    show x = ""
instance Eq Context where
    x == y = False
instance Ord Context where
    x < y = False
    x > y = False
    x <= y = False
    x >= y = False
    max x y = x
    min x y = y

emptyCtx :: Context
emptyCtx = Context (const Nothing)

setVar :: Context -> Memory -> String -> Ast -> (Context, Memory) -- Adds or updates a variable
setVar (Context ctxfunc) mem varname value =
    let idx = ctxfunc varname
    in 
        if idx == Nothing
        then -- Didn't have the variable, create new
            let (newIdx, newmem) = addToMem mem value
            in (Context (\s -> if s == varname then Just newIdx else ctxfunc s), newmem)
        else -- Did have the variable, update value
            let (Just i) = idx
            in let (newIdx, newmem) = modifyMem mem i value
            in ((Context ctxfunc), newmem)

getVar :: Context -> Memory -> String -> Maybe Ast -- Gets the value of a variable
getVar (Context ctxfunc) mem varname =
    let idx = ctxfunc varname
    in
        if idx == Nothing then Nothing
        else let (Just i) = idx in lookupMem mem i

addVar :: Context -> Memory -> String -> Ast -> (Context, Memory) -- Adds a variable (even if it already exists)
addVar (Context ctxfunc) mem varname value =
    let (idx, newmem) = addToMem mem value
    in (Context (\s -> if s == varname then Just idx else ctxfunc s), newmem)

copyRef :: Context -> String -> Context -> String -> Context -- Copies a reference from one context to another (or to the same context)
copyRef (Context origCtxfunc) origName (Context newCtxfunc) newName =
    let origPos = origCtxfunc origName
    in 
        if origPos == Nothing then error "No such variable."
        else Context (\s -> if s == newName then origPos else newCtxfunc s)

chars = ['A'..'Z']++['a'..'z']

-- Helper functions

-- Determines whether a character is in either range of A-Z and a-z.
-- Needed because Haskell's isAlpha (and isLetter) accept Unicode, and the
-- specified grammar doesn't.
isChar :: Char -> Bool
isChar c = c `elem` chars

-- Determines whether a string is completely composed of valid characters.
isCharString :: String -> Bool
isCharString s = all isChar s

-- Determines whether a string is a valid integer.
isInt :: String -> Bool
isInt s = all isDigit s

tokenize :: String -> [String]
tokenize [] = []
tokenize ('=':'=':xs) = "==":tokenize xs
tokenize ('!':'=':xs) = "!=":tokenize xs
tokenize ('-':'>':xs) = "->":tokenize xs
tokenize (' ':xs) = tokenize xs
tokenize ('(':xs) = "(":tokenize xs
tokenize (')':xs) = ")":tokenize xs
tokenize ('+':xs) = "+":tokenize xs
tokenize ('-':xs) = "-":tokenize xs
tokenize ('*':xs) = "*":tokenize xs
tokenize ('/':xs) = "/":tokenize xs
tokenize ('<':xs) = "<":tokenize xs
tokenize ('>':xs) = ">":tokenize xs
tokenize (';':xs) = ";":tokenize xs
tokenize (',':xs) = ",":tokenize xs
tokenize ('.':xs) = ".":tokenize xs
tokenize (s:xs)
    | isDigit s = (takeWhile isDigit (s:xs)):tokenize (dropWhile isDigit xs)
    | isChar s = (takeWhile isChar (s:xs)):tokenize (dropWhile isChar xs)
    | otherwise = error "Syntax error."

parseBlock :: [String] -> (Ast, [String])
parseBlock [] = error "Nothing left to parse."
parseBlock s =
    let (ast, str) = parseExpr s
    in
        if str == [";"]
        then (Block [ast], [])
        else
            if head str == ";"
            then
                let ((Block newast), newstr) = parseBlock $ tail str
                in (Block (ast:newast), newstr)
            else error "Block must end in semicolon."

parseExpr :: [String] -> (Ast, [String])
parseExpr [] = error "Malformed expression."
parseExpr s
    | head s == "(" = parseApp s
    | isDigit (head (head s)) = (Number (read $ head s), tail s)
    | head s == "case" = parseCase s
    | head s == "set" = parseSet s
    | otherwise =
        let (ast, xs) = if head s == "lambda" then parseLambda s else parseVar s
        in
            if head xs == "("
                then
                    let (callast, callxs) = parseCall xs
                    in (App ast [callast], callxs)
                else (ast, xs)

parseApp :: [String] -> (Ast, [String])
parseApp ("(":s) = 
    let (expr1ast, expr1str) = parseExpr s
    in
        if length expr1str == 0 then error "Malformed app string." else
        if head expr1str == ","
        then
            let (expr2ast, expr2str) = parseExpr (tail expr1str)
            in 
                if length expr2str == 0 then error "Malformed app string." else
                if head expr2str == ")"
                then
                    let (funcast, funcstr) = parseFunc(tail expr2str)
                    in (App funcast [expr1ast, expr2ast], funcstr)
                else error "Malformed app string."
        else error "Malformed app string."
parseApp _ = error "Malformed app string."

parseFunc :: [String] -> (Ast, [String])
parseFunc ("+":s) = (Name "+", s)
parseFunc ("-":s) = (Name "-", s)
parseFunc ("*":s) = (Name "*", s)
parseFunc ("/":s) = (Name "/", s)
parseFunc _ = error "Malformed function."

parseBool :: [String] -> (Ast, [String])
parseBool ("(":s) = 
    let (expr1ast, expr1str) = parseExpr s
    in
        if head expr1str == "," then
            let (expr2ast, expr2str) = parseExpr $ tail expr1str
            in
                if head expr2str == ")" then
                    let op = head $ tail expr2str
                    in let rest = tail $ tail expr2str
                    in
                        if op == "==" then (Bool (Name "==") expr1ast expr2ast, rest)
                        else if op == "!=" then (Bool (Name "!=") expr1ast expr2ast, rest)
                        else if op == "<" then (Bool (Name "<") expr1ast expr2ast, rest)
                        else if op == ">" then (Bool (Name ">") expr1ast expr2ast, rest)
                        else error "Malformed boolean test."
                else error "Malformed boolean test."
        else error "Malformed boolean test."
parseBool _ = error "Malformed boolean test."

parseCase :: [String] -> (Ast, [String])
parseCase ("case":"otherwise":"->":s) =
    let (exprast, exprstr) = parseExpr s
    in
        if head exprstr == "." then
            (Case Default [exprast], tail exprstr)
        else error "Malformed case string."
parseCase ("case":s) =
    let (boolast, boolstr) = parseBool s
    in
        if head boolstr == "->" then
            let (exprast, exprstr) = parseExpr $ tail boolstr
            in 
                if head exprstr == "," then
                    let (caseast, casestr) = parseCase $ tail exprstr
                    in (Case boolast [exprast, caseast], casestr)
                else error "Malformed case string."
        else error "Malformed case string."
parseCase _ = error "Malformed case string."

parseSet :: [String] -> (Ast, [String])
parseSet ("set":var:s) =
    let (exprast, exprstr) = parseExpr s
    in (Set var exprast, exprstr)
parseSet _ = error "Malformed set expression."

parseVar :: [String] -> (Ast, [String])
parseVar (s:xs)
    | isCharString s = (Name s, xs)
    | otherwise = error "Invalid variable name."
parseVar [] = error "Invalid variable name."

parseLambda :: [String] -> (Ast, [String])
parseLambda ("lambda":name:"(":s) = 
    let (exprast, exprstr) = parseExpr s
    in
        if head exprstr == ")"
        then (Lambda name exprast, tail exprstr)
        else error "Malformed lambda string."
parseLambda _ = error "Malformed lambda string."

parseCall :: [String] -> (Ast, [String])
parseCall ("(":s) =
    let (exprast, exprstr) = parseExpr s
    in
        if head exprstr == ")"
        then (exprast, tail exprstr)
        else error "Malformed call."
parseCall _ = error "Malformed call."

parse :: String -> Ast
parse "" = error "Nothing to parse."
parse s = let slist = tokenize s in let (ast, trash) = parseBlock slist in ast

eval :: Ast -> Context -> Memory -> (Ast, Context, Memory)
eval (Number n) ctx mem = (Number n, ctx, mem)
eval (Block [blc]) ctx mem = eval blc ctx mem
eval (Block (blc:more)) ctx mem =
    let (blcast, blcctx, blcmem) = eval blc ctx mem
    in eval (Block more) blcctx blcmem
eval (App (Name func) [(Name var)]) ctx mem =
    let ((Function varname funcast funcctx), _, _) = eval (Name func) ctx mem -- Look up the Function by the Name
    in let inctx = copyRef ctx var funcctx varname -- Create a copy reference to the reference variable
    in let (out, outctx, outmem) = eval funcast inctx mem -- Run the lambda, get the result and updated memory
    in (out, ctx, outmem) -- Discard the updated context
eval (App (Name func) [ast]) ctx mem =
    let ((Function varname funcast funcctx), _, _) = eval (Name func) ctx mem
    in let (exprast, exprctx, exprmem) = eval ast ctx mem -- Evaluate the pass-by-value
    in let (inctx, inmem) = addVar funcctx exprmem varname exprast -- Force creation of a new variable by this name
    in let (out, outctx, outmem) = eval funcast inctx inmem
    in (out, ctx, outmem)
eval (App (Name func) [sub1, sub2]) ctx mem =
    let ((Number num1), sub1ctx, sub1mem) = eval sub1 ctx mem
    in let ((Number num2), sub2ctx, sub2mem) = eval sub2 sub1ctx sub1mem
    in 
        if func == "+" then (Number (num1 + num2), sub2ctx, sub2mem)
        else if func == "-" then (Number (num1 - num2), sub2ctx, sub2mem)
        else if func == "*" then (Number (num1 * num2), sub2ctx, sub2mem)
        else if func == "/" then (Number (num1 `div` num2), sub2ctx, sub2mem)
        else error "Bad AST"
eval (App (Lambda lbdname lbdast) [(Name var)]) ctx mem = 
    let copyctx = copyRef ctx var ctx lbdname -- Create a copy reference to the reference variable
    in let (out, outctx, outmem) = eval lbdast copyctx mem-- Run the lambda, keep the modified memory but discard the context
    in (out, ctx, outmem)
eval (App (Lambda lbdname lbdast) [appast]) ctx mem = 
    let (exprast, exprctx, exprmem) = eval appast ctx mem -- Evaluate the pass-by-value
    in let (copyctx, copymem) = addVar exprctx exprmem lbdname exprast -- Force creation of a new variable by this name
    in let (out, outctx, outmem) = eval lbdast copyctx copymem -- Run the lambda, discarding the new context but keeping the result and memory
    in (out, exprctx, outmem)
eval (Bool (Name test) sub1 sub2) ctx mem =
    let ((Number num1), sub1ctx, sub1mem) = eval sub1 ctx mem
    in let ((Number num2), sub2ctx, sub2mem) = eval sub2 sub1ctx sub1mem
    in
        if test == "==" then if num1 == num2 then (Number 1, sub2ctx, sub2mem) else (Number 0, sub2ctx, sub2mem)
        else if test == "!=" then if num1 == num2 then (Number 0, sub2ctx, sub2mem) else (Number 1, sub2ctx, sub2mem)
        else if test == "<" then if num1 < num2 then (Number 1, sub2ctx, sub2mem) else (Number 0, sub2ctx, sub2mem)
        else if test == ">" then if num1 > num2 then (Number 1, sub2ctx, sub2mem) else (Number 0, sub2ctx, sub2mem)
        else error "Bad AST"
eval (Case Default [sub]) ctx mem = eval sub ctx mem
eval (Case test [sub1, sub2]) ctx mem = 
    let (outcome, testctx, testmem) = eval test ctx mem
    in if outcome == (Number 1) then eval sub1 testctx testmem else eval sub2 testctx testmem
eval (Set str ast) ctx mem =
    let (myctx, mymem) = if isLambda ast then setVar ctx mem str (Number 0) else (ctx, mem) -- Creates a dummy entry to enable self-reference, i.e. recursion (the dummy is overwritten before the reference happens)
    in let (exprval, exprctx, exprmem) = eval ast myctx mymem
    in let (newctx, newmem) = setVar exprctx exprmem str exprval
    in (exprval, newctx, newmem)
eval (Name name) ctx mem =
    let val = getVar ctx mem name
    in
        if val == Nothing then error ("Variable not set: "++name)
        else
            let (Just jval) = val
            in (jval, ctx, mem)
eval (Lambda vname lambdaast) ctx mem = (Function vname lambdaast ctx, ctx, mem)

eval ast ctx mem = error "Bad AST"

run :: String -> Ast
run s = let (ast, ctx, mem) = eval (parse s) emptyCtx emptyMem in ast