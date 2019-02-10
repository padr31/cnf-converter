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
                        Neg s  -> "-" ++ s ++ ", " ++ clauseToStr(xs)
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

-- Delete clauses consisting of one Bol "f"
deleteFalseClauses :: [[Literal]] -> [[Literal]]
deleteFalseClauses [] = [];
deleteFalseClauses (x:xs) = if x == [Bol "f"] then (deleteFalseClauses xs) else x:(deleteFalseClauses xs);

-- Deleting Bol "f" literals in clauses - as they have no meaning
deleteFalses :: [[Literal]] -> [[Literal]]
deleteFalses [] = [];
deleteFalses (x:xs) = (deleteLiteral (Bol "f") x):(deleteFalses xs);

-- Unit propagation for each unit claus {L}

getUnitLiterals :: [[Literal]] -> [Literal]
getUnitLiterals [] = [];
getUnitLiterals (x:xs) = if length x == 1 then x ++ getUnitLiterals(xs) else getUnitLiterals(xs);

isClauseContainingOneOf :: [Literal] -> [Literal] -> Bool
isClauseContainingOneOf [] _ = False;
isClauseContainingOneOf (x:xs) units = if (elem x units) then True else (isClauseContainingOneOf xs units);

deleteLiteral :: Literal -> [Literal] -> [Literal]
deleteLiteral _ [] = [];
deleteLiteral l (x:xs) = if x == l then (deleteLiteral l xs) else x:(deleteLiteral l xs); 

deleteAll :: [Literal] -> [Literal] -> [Literal]
deleteAll [] xs = xs;
deleteAll _ [] = [];
deleteAll (l:ls) xs = deleteAll ls (deleteLiteral l xs);

negateLiterals :: [Literal] -> [Literal]
negateLiterals [] = [];
negateLiterals (x:xs) = case x of 
                        Pos s -> (Neg s):negateLiterals(xs)
                        Neg s -> (Pos s):negateLiterals(xs)
                        Bol s -> if s == "t" then (Bol "f"):xs else (Bol "t"):xs;

negateLiteral :: Literal -> Literal
negateLiteral l = case l of 
                        Pos s -> Neg s
                        Neg s -> Pos s
                        Bol s -> if s == "t" then (Bol "f") else (Bol "t");

deleteUnitLiterals :: [[Literal]] -> [[Literal]]
deleteUnitLiterals [] = [];
deleteUnitLiterals xs = let units = (getUnitLiterals xs) in 
                                if (length units) == 0 then xs
                                else let unitL = units!!0
                                     in deleteUnitLiterals(deleteOccurrenceOfInClauses (deleteClausesContainingLiteral xs unitL) (negateLiteral unitL));
                                        

deleteClausesContainingLiteral :: [[Literal]] -> Literal -> [[Literal]]
deleteClausesContainingLiteral [] _ = [];
deleteClausesContainingLiteral (x:xs) l = if (elem l x) 
                                             then (deleteClausesContainingLiteral xs l) 
                                             else x:(deleteClausesContainingLiteral xs l);

deleteOccurrenceOfInClauses :: [[Literal]] -> Literal -> [[Literal]]
deleteOccurrenceOfInClauses [] _ = [];
deleteOccurrenceOfInClauses (x:xs) l = (deleteLiteral l x):(deleteOccurrenceOfInClauses xs l);

-- Delete all clauses containing pure literals
getAllDistinctLiterals :: [[Literal]] -> [Literal]
getAllDistinctLiterals xs = let ls = flatten xs
                                f :: [Literal] -> Literal -> [Literal]
                                f l a = if (elem a l) then l else (a:l)
                            in foldl f [] ls;

-- takes list of clauses and checks if given literal is pure in it
isPure :: [[Literal]] -> Literal -> Bool
isPure [] _ = True;
isPure (x:xs) (Neg s) = if (elem (Pos s) x) then False else (isPure xs (Neg s));
isPure (x:xs) (Pos s) = if (elem (Neg s) x) then False else (isPure xs (Pos s));
isPure (x:xs) _ = False;

getPures :: [[Literal]] -> [Literal];
getPures [] = [];
getPures xs = filter (isPure xs) (getAllDistinctLiterals xs);

deleteAllPureLiteralClausesAccumulator :: [[Literal]] -> [Literal] -> [[Literal]]
deleteAllPureLiteralClausesAccumulator [] _ = [];
deleteAllPureLiteralClausesAccumulator (x:xs) pures = if (isClauseContainingOneOf x pures) 
                                              then (deleteAllPureLiteralClausesAccumulator xs pures)
                                              else x:(deleteAllPureLiteralClausesAccumulator xs pures);

deleteAllPureLiteralClauses :: [[Literal]] -> [[Literal]]
deleteAllPureLiteralClauses xs = let pures = (getPures xs) 
                                 in (deleteAllPureLiteralClausesAccumulator xs pures);

beforeCaseSplit :: [[Literal]] -> [[Literal]]
beforeCaseSplit = deleteAllPureLiteralClauses . deleteUnitLiterals . deleteFalses . deleteFalseClauses . deleteTautologies;

-- Case splitting
replaceLiteralInClause :: [Literal] -> Literal -> Literal -> [Literal]
replaceLiteralInClause [] _ _ = [];
replaceLiteralInClause (x:xs) l n = if x == l then (n:(replaceLiteralInClause xs l n)) else x:(replaceLiteralInClause xs l n); 

replaceLiteralInSet :: [[Literal]] -> Literal -> Literal -> [[Literal]]
replaceLiteralInSet [] _ _ = [];
replaceLiteralInSet (x:xs) l n = (replaceLiteralInClause x l n):(replaceLiteralInSet xs l n);

caseSplit :: [[Literal]] -> Literal -> ([[Literal]], [[Literal]])
caseSplit xs (Pos l) = (replaceLiteralInSet (replaceLiteralInSet xs (Pos l) (Bol "t")) (Neg l) (Bol "f"),
                        replaceLiteralInSet (replaceLiteralInSet xs (Neg l) (Bol "t")) (Pos l) (Bol "f"));

caseSplit xs (Neg l) = (replaceLiteralInSet (replaceLiteralInSet xs (Pos l) (Bol "t")) (Neg l) (Bol "f"),
                        replaceLiteralInSet (replaceLiteralInSet xs (Neg l) (Bol "t")) (Pos l) (Bol "f"));

caseSplit xs (Bol _) = error "Can't do case split on boolean";

-- Driver - the loop that will keep doing things before case split and case splitting appropriately
data Result = EmptyClause | EmptyClauseSet | Unfinished deriving (Eq);

getResult :: [[Literal]] -> Result
getResult [] = EmptyClauseSet;
getResult xs = if (elem [] xs) then EmptyClause else Unfinished;

getFirstLiteral :: [[Literal]] -> Literal
getFirstLiteral [] = error "No literals, should not happen";
getFirstLiteral xs = (getAllDistinctLiterals xs)!!0;

drive :: [[Literal]] -> Result
drive xs = let before = (beforeCaseSplit xs) in
                                case (getResult before) of
                                    Unfinished -> let l = getFirstLiteral before
                                                      cases = caseSplit before l
                                                  in if (drive (fst cases)) == EmptyClauseSet 
                                                     then EmptyClauseSet  
                                                     else if (drive (snd cases)) == EmptyClauseSet
                                                          then EmptyClauseSet
                                                          else EmptyClause
                                    _ -> getResult before;

resultToStr :: Result -> String
resultToStr r = case r of
            EmptyClause    -> "Refuted"
            EmptyClauseSet -> "Satisfiable"
            Unfinished     -> "Unfinished";

