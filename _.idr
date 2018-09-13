module Main

import Effects
import Effect.StdIO
import Effect.File
import Prelude.File
import Prelude.List

data Statement_Lex = Statement String String
data StateLSS a b = Error_Statement b | Ok a 


letters : String -> List String
letters "" = []
letters x = (singleton (strHead x)) :: letters (strTail x)

unletters : List String -> String
unletters [] = ""
unletters (y :: ys) = y ++ (unletters ys)

clean_lex : List String -> String
clean_lex [] = ""
clean_lex(x :: xs) = if (x == " " || x == "\n") then clean_lex xs else (unletters (x :: xs))

lex_statement : String -> StateLSS Statement_Lex (List String)
lex_statement f = let_statement (split (== '=') f) where
  let_statement : List String -> StateLSS Statement_Lex (List String)
  let_statement xs = case xs of 
                                             (x :: y :: xs) => Ok (Statement (clean_lex (reverse (letters (clean_lex (reverse (letters x)))))) (clean_lex (reverse (letters (clean_lex (reverse (letters y)))))))
                                             _ => Error_Statement xs


readLines : File -> IO (List String)
readLines k = do
                y <- fEOF k
                case y of
                          False => do
                                      f <- fGetLine k
                                      case f of
                                                Right x => do 
                                                              t <- readLines k
                                                              if (x /= "") && (x /= "\n") then pure (x :: t) else pure t
                                                Left y => pure []
                          True => pure []


analys_lex : List String -> List (StateLSS Statement_Lex (List String))
analys_lex (x :: x_xs) = case (head' (x :: x_xs)) of 
                                       Just x_ => (lex_statement x_) :: analys_lex x_xs
                                       Nothing => []


io_list : String -> Maybe (Eff() [STDIO])
io_list x = Just (putStrLn x)

isAnalysLexOk : StateLSS Statement_Lex (List String) -> Maybe (List (Eff() [STDIO]))
isAnalysLexOk k = case k of
                             Ok a => Nothing
                             Error_Statement b => Just (mapMaybe io_list b)

isCorrectProgramLex : List (Maybe (List (Eff() [STDIO]))) -> List (Maybe (List (Eff() [STDIO])))
isCorrectProgramLex t = filter just_erros t where
  just_erros : Maybe (List (Eff() [STDIO])) -> Bool
  just_erros k = case k of 
                            Just a => True
                            Nothing => False

toAnalysLexOk : StateLSS Statement_Lex (List String) -> Maybe (Maybe (List (Eff() [STDIO])))
toAnalysLexOk f = Just (isAnalysLexOk f)

print_lexerros : List (Eff() [STDIO]) -> Eff() [STDIO]
print_lexerros [] = putStr ""
print_lexerros (x :: xs) = do
                              x
                              print_lexerros xs
                              
print_erros : List (Maybe (List (Eff() [STDIO]))) -> Eff() [STDIO]
print_erros [] = putStr ""
print_erros (x :: xs) = do 
                           case x of
                                     Nothing => print_erros xs
                                     Just y => print_lexerros y
                           print_erros xs
                                 
justExpression : List (StateLSS Statement_Lex (List String)) -> List Statement_Lex
justExpression [] = []
justExpression (x :: xs) = case x of
                                     Ok a => a :: (justExpression xs)
                                     Error_Statement a => (justExpression xs)

get_expression : String -> List Statement_Lex -> Maybe String
get_expression a [] = Nothing
get_expression a ((Statement f s) :: d_) = if (a == f) then Just s else get_expression a d_

expression__ : List String -> List Statement_Lex -> Maybe String
expression__ [] k = Just ""
expression__ (k :: k_) y = (if (k /= "_" && k /= "(" && k /= ")") then do
                                                                          case (get_expression k y) of 
                                                                                                       Just t => case (expression__ (letters t) y) of 
                                                                                                                                                    Just y_ => case (expression__ k_ y) of 
                                                                                                                                                                                            Just n => Just (y_ ++ n)
                                                                                                                                                                                            Nothing => Nothing
                                                                                                                                                    Nothing => Nothing
                                                                                                       Nothing => Nothing
                                                                       else case (expression__ k_ y) of 
                                                                                                        Just n => Just (k ++ n)
                                                                                                        Nothing => Nothing)
test : List Statement_Lex -> Eff () [STDIO]
test [] = putStrLn ""
test ((Statement f s) :: d_) = do 
                                  putStrLn (f ++ "-" ++ s)
                                  test d_
main : IO ()
main = do
         x <- (run getStr)
         file <- openFile (trim x) Read
         case file of 
                     Left f => putStrLn "File don't found"
                     Right a => do
                                  t <- readLines a
                                  let lex_struct = (analys_lex t) 
                                  let _lex = mapMaybe toAnalysLexOk lex_struct
                                  let __lex = (isCorrectProgramLex _lex)
                                  case (length __lex) of
                                                         Z => do 
                                                                 print (expression__ ["s", "k"] (justExpression lex_struct))
                                                                 putStrLn ""
                                                                 case (get_expression "s" (justExpression lex_struct)) of
                                                                   Just x => print (letters x)
                                                                   Nothing => putStrLn ""
                                                                 putStrLn ""
                                                                 run (test (justExpression lex_struct))
                                                         _ => do 
                                                                 putStrLn "Lexical error"
                                                                 run (print_erros __lex)
                                  print t
         