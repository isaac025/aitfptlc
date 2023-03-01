module LC where

-- Lambda Calculus can be treated as a universal machine code

main :: IO ()
main = do
    x <- getLine
    let res = 10 * (read x :: Int)
      in print res

{- Abstraction
 - REPLACE items IN 10*items
 - REPLACE items WITH 84 IN 10*items -> 10*84
 -
 - REPLACE cost IN
 -  REPLACE items IN cost*items
  
 - REPLACE cost WITH 32 IN
 -   REPLACE items WITH 12 IN cost*items 
 -   -> items*32
 -   -> 12*32
 -
 - REPLACE op IN
 -   REPLACE cost IN
 -     REPLACE items IN cost op items
 -
 - REPLACE op WITH * IN 
 -   REPLACE cost IN
 -     REPLACE items IN cost op items
 -     -> cost*items 
-}


{- Lambda Calculus
 - The lambda calculus is a system for manipulating lambda expressions.
 - A lambda expression is either: a name, a function or a function application
 - Name                 - identify an abstraction point
 - Function             - introduce abstraction
 - Function application - specialize abstraction 
 - BNF Grammar:
 - <expression>             ::= <name> | <function> | <function application>
 - <name>                   ::= SEQUENCE OF NON BLANK CHARACTERS
 - <function>               ::= lambda<name>.<body> *NOTE*: <name> is called the function's bound variable
 - <body>                   ::= <expression>
 - <application>            ::= (<function expression> <argument expression>)
 - <function expression>    ::= <expression> 
 - <argument expression>    ::= <expression> 
-}

-- Identity function
identity = \x -> x

idAppliedToItself = (\x -> x) (\x -> x)

-- Self-application Function
selfApplication :: (forall a. a -> a) -> b -> b
selfApplication = \s -> s s
-- The self-application function applied to itself never terminates!

-- Function application function
functionApplicationFunc = \func -> \arg -> (func arg)

-- Beta Reduction - the replacement of a bound variable with an argument in a
--                  function body
identity2 = \x -> functionApplicationFunc identity x

selfApplication2 = \s -> functionApplicationFunc s

-- Argument selecting and pairing 
selectFirst = \first -> \second -> first

selectSecond = \first -> \second -> second

makePair = \first -> \second -> \func -> ((func first) second)
