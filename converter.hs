-- lexing
data Token = AND | OR | NOT | IMPL | LPAREN | RPAREN | ID String | TRUE | FALSE;

split :: String -> [String]
split s = words s;

strToToken :: String -> Token   
strToToken s = case s of
                "and"     -> AND
                "or"      -> OR
                "not"     -> NOT
                "implies" -> IMPL
                "("       -> LPAREN
                ")"       -> RPAREN
                "t"       -> TRUE
                "f"       -> FALSE
                _         -> ID s;

tokenToStr :: Token -> String   
tokenToStr token = case token of
                    AND    -> "and"
                    OR     -> "or"
                    NOT    -> "not"
                    IMPL   -> "implies"
                    LPAREN -> "("
                    RPAREN -> ")"
                    TRUE   -> "t"
                    FALSE  -> "f"
                    ID s   -> s;

tokenize :: [String] -> [Token]
tokenize words = map strToToken words;

detokenize :: [Token] -> [String]
detokenize tokens = map tokenToStr tokens;

lexx :: String -> [Token]
lexx = tokenize . split;

-- parsing
data Expr = EAnd Expr Expr
          | EOr Expr Expr
          | EImpl Expr Expr
          | ENot Expr
          | EId String
          | EBool String
          | EErr String;

expr :: [Token] -> (Expr, [Token]);
expr (t:xt) = case t of
                AND    -> (EAnd e e', xt'') where
                          (e , xt')  = expr xt
                          (e', xt'') = expr xt'
                OR     -> (EOr e e', xt'') where
                          (e , xt')  = expr xt
                          (e', xt'') = expr xt'
                IMPL   -> (EImpl e e', xt'') where
                          (e , xt')  = expr xt
                          (e', xt'') = expr xt'
                NOT    -> (ENot e, xt') where
                          (e, xt') = expr xt
                LPAREN -> let (e, xt') = (expr xt) in 
                            case (head xt') of 
                                RPAREN -> (e, tail xt')
                                _      -> (EErr "Missing bracket", xt')
                RPAREN -> (EErr "Wrong right )", xt)
                ID s   -> (EId s, xt)
                TRUE   -> (EBool "t", xt)
                FALSE  -> (EBool "f", xt);

parse :: [Token] -> Expr;
parse = fst . expr;
          
exprToStr :: Expr -> String
exprToStr e = case e of
                EAnd  e1 e2 -> "(" ++ (exprToStr e1) ++ " and " ++ (exprToStr e2) ++ ")"
                EOr   e1 e2 -> "(" ++ (exprToStr e1) ++ " or " ++ (exprToStr e2) ++ ")"
                EImpl e1 e2 -> "(" ++ (exprToStr e1) ++ " implies " ++ (exprToStr e2) ++ ")"
                ENot  e1    -> "(not " ++ (exprToStr e1) ++ ")"
                EId s       -> s
                EBool s     -> s
                EErr s      -> s;

elimImpl :: Expr -> Expr
elimImpl e = case e of
                EAnd  e1 e2 -> EAnd (elimImpl e1) (elimImpl e2)
                EOr   e1 e2 -> EOr (elimImpl e1) (elimImpl e2)
                EImpl e1 e2 -> EOr (ENot (elimImpl e1)) (elimImpl e2)
                ENot  e1    -> ENot (elimImpl e1)
                EId s       -> EId s
                EBool s     -> EBool s
                EErr s      -> EErr s;

elimNot :: Expr -> Expr
elimNot e = case e of
                EAnd  e1 e2 -> EAnd (elimNot e1) (elimNot e2)
                EOr   e1 e2 -> EOr (elimNot e1) (elimNot e2)
                --EImpl e1 e2 -> EImpl (elimNot e1) (elimNot e2)
                ENot  e1    -> case e1 of
                                EAnd  se1 se2 -> elimNot (EOr (ENot se1) (ENot se2))
                                EOr   se1 se2 -> elimNot (EAnd (ENot se1) (ENot se2))
                                --EImpl se1 se2 -> elimNot (EOr (ENot se1) se2)
                                ENot  se1    -> se1
                                EId s       -> ENot (EId s)
                                EBool s     -> case s of
                                                    "t" -> EBool "f"
                                                    "f" -> EBool "t"
                                EErr s      -> EErr s;
                EId s       -> EId s
                EBool s     -> EBool s
                EErr s      -> EErr s;

pushDisjIn :: Expr -> Expr                
pushDisjIn e = case e of
                EAnd  e1 e2 -> EAnd (pushDisjIn e1) (pushDisjIn e2)
                --EImpl e1 e2 -> EOr (ENot (elimImpl e1)) (elimImpl e2)
                ENot  e1    -> ENot (pushDisjIn e1)
                EId s       -> EId s
                EBool s     -> EBool s
                EErr s      -> EErr s
                EOr   e1 e2 -> case e1 of
                                EAnd se1 se2 -> pushDisjIn(EAnd (EOr se1 e2) (EOr se2 e2))
                                _            -> case e2 of
                                                 EAnd se1 se2 -> pushDisjIn(EAnd (EOr e1 se1) (EOr e1 se2))
                                                 _            -> EOr (pushDisjIn(e1)) (pushDisjIn(e2));
                                                 
cnf :: Expr -> Expr
cnf = pushDisjIn . elimNot . elimImpl;

strToExpr :: String -> Expr
strToExpr = parse . lexx;

convert :: String -> String
convert = exprToStr . cnf . parse . lexx;

-- DPLL
-- DPLL parsing methods, converting from CNF expression
-- Positive and negated literal, or boolean value
data Literal = Pos String | Neg String | Bol String deriving (Eq);

flatten :: [[Literal]] -> [Literal];
flatten [] = [];
flatten (x:xs) = x ++ flatten(xs);

cnfExprToClauseSet :: Expr -> [[Literal]]
cnfExprToClauseSet e = case e of
    EAnd e1 e2  -> cnfExprToClauseSet(e1) ++ cnfExprToClauseSet(e2)
    EOr e1 e2   -> [ flatten(cnfExprToClauseSet(e1)) ++ flatten(cnfExprToClauseSet(e2)) ]
    EImpl e1 e2 -> error "not a valid cnf"
    ENot (EId s)  -> [[Neg s]]
    EId s       -> [[Pos s]]
    EBool s     -> [[Bol s]]
    EErr s      -> error s;

clauseToStr :: [Literal] -> String
clauseToStr [] = "";
clauseToStr (x:xs) = case x of 
                        Pos s  -> s ++ ", " ++ clauseToStr(xs)
                        Neg s  -> "¬" ++ s ++ ", " ++ clauseToStr(xs)
                        Bol s -> s ++ ", " ++ clauseToStr(xs);

clauseSetToStr :: [[Literal]] -> String
clauseSetToStr [] = "";
clauseSetToStr (x:xs) = "{" ++ clauseToStr(x) ++ "}" ++ clauseSetToStr(xs);

strToClauseSet :: String -> [[Literal]]
strToClauseSet = cnfExprToClauseSet . strToExpr;

-- DPLL actual algorithm

-- Deleting tautologic clauses
isTautologyClause :: [Literal] -> Bool
isTautologyClause [] = False;
isTautologyClause (x:xs) = case x of 
                                Pos s -> (elem (Neg s) xs) || (isTautologyClause(xs))
                                Neg s -> (elem (Pos s) xs) || (isTautologyClause(xs))
                                Bol s -> if s == "t" then True else isTautologyClause(xs);

deleteTautologies :: [[Literal]] -> [[Literal]]
deleteTautologies [] = [];
deleteTautologies (x:xs) = if isTautologyClause(x) then deleteTautologies(xs) else x:(deleteTautologies(xs));

-- Unit propagation for each unit claus {L}

-- Delete all clauses containing L
getUnitLiterals :: [[Literal]] -> [Literal]
getUnitLiterals [] = [];
getUnitLiterals (x:xs) = if length x == 1 then x ++ getUnitLiterals(xs) else getUnitLiterals(xs);

isUnitClause :: [Literal] -> [Literal] -> Bool
isUnitClause [] _ = False;
isUnitClause (x:xs) units = if (elem x units) then True else (isUnitClause xs units);

deleteUnitLiterals :: [[Literal]] -> [[Literal]]
deleteUnitLiterals [] = [];
deleteUnitLiterals (x:xs) = let units = (getUnitLiterals (x:xs)) in if (isUnitClause x units) then (deleteUnitLiterals xs) else x:(deleteUnitLiterals xs);

-- Delete ¬L from all clauses
--deleteNegOfUnitLiterals :: [[Literal]] -> [[]]