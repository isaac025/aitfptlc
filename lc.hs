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
 - <function>               ::= \<name>.<body> *NOTE*: <name> is called the function's bound variable
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

{- Bound and free variables
  
 - A variable is bound if:
 - 1. The expression is a application:
         (<function> <argument>)
      and the variable is bound in <function> or <argument>.
      Example, convict is bound in: 
         (\convict.convict fugitive)
         (\prison.prison \convict.convict)
  
 - 2. The expression is a function:
         \<name>.<body>
      and the variable's name is <name> or is bound in <body>
      Example, prisoner is bound in:
         \prisoner.(number6 prisoner)
         \prison.\prisoner.(prison prisoner)
  
 - A variable is free if:
 - 1. The expression is a single name:
         <name>
      and the variable's name is name
      Example, truant is free in:
         truant
  
 - 2. The expression is an application
          (<function> <argument>)
      and the variable is free in <function> or <argument>.
      Example, escaper is free in: 
         (\prisoner.prisoner escaper)
         (escaper \jailor.jailor)
  
 - 3. The expression is a function
          \<name>.<body>
      and the variable's name is not <name> and is free in <body>
      Example, fugitive is free in:
         \prison.(prison fugitive)
         \short.\sharp.\shock.fugitive
-} 

-- Alpha Conversion is a conversion that renames variables in order to avoid
-- confusion of repetitive variable names
  
-- Eta reduction is the simplification of some function to simple expression
-- In other words, \<name>.(<expression> <name>) => <expression> 


-- Exercise 2.1
-- (a) \a.(a \b.(b a))
--     function: 
--      bound variable: a
--      body: application: (a \b.(b a))
--        function expression: a
--        argument expression: function: \b.(b a)
--          bound variable: b
--          body: application: b a
--            function expression: b
--            argument expression: a
  
-- (b) \x.\y.\z.((z x) (z y))
--     function:
--       bound variable: x
--       body: function: \y.\z.((z x) (z y))
--         bound variable: y
--         body: function: \z.((z x) (z y))
--           bound variable: z
--           body: application: ((z x) (z y))
--             function expression: application: (z x)
--               function expression: z
--               argument expression: x
--             argument expression: application: (z y)
--               function expression: z
--               argument expression: y
--
-- (c) (\f.\g.(\h.(g h) f) \p.\q.p)
--     application:
--       function expression: 
--         application: (\f.\g.(\h.(g h) f))
--           function expression: function: \f.\g.(\h.(g h))
--             bound variable: f
--             body: function: \g.(\h.(g h))
--               bound variable: g
--               body: function: \h.(g h)
--                 bound variable: h
--                 body: application: (g h)
--                   function expression: g
--                   argument expressoin: h
--           argument expression: f
--       argument expression: 
--         function: \p.\q.p
--           bound variable: p
--           body: function: \q.p
--             bound variable: q
--             body: p
  
-- (d) \fee.\fi.\fo.\fum.(fum (fo (fi fee)))
--     function:
--      bound variable: fee
--      body: function: \fi.\fo.\fum.(fum (fo (fi fee)))
--        bound variable: fi
--        body: function: \fo.\fum.(fum (fo (fi fee)))
--          bound variable: fo
--          body: function: \fum.(fum (fo (fi fee)))
--            bound variable: fum
--            body: application: (fum (fo (fi fee)))
--              function expression: fum
--              argument expression: application: (fo (fi fee))
--                function expression: fo
--                argument expression: application: (fi fee)
--                  function expression: fi
--                  argument expression: fee
  
-- (e) (\p.(\q.p \x.(x p)) \i.\j.(j i))
--       application:
--         function expression: 
--           function: \p.(\q.p \x.(x p))
--             bound variable: p
--             body: application: (\q.p \x.(x p))
--               function expression: 
--                 function: \q.p
--                   bound variable: q
--                   body: p
--               argument expression:
--                 function: \x.(x p)
--                   bound variable: x
--                   body: application: (x p)
--                     function expression: x
--                     argument expression: p
--         argument expression:
--           function: \i.\j.(j i)
--             bound variable: i
--             body: function: \j.(j i)
--               bound variable: j
--               body: application: j i
--                 function expression: j
--                 argument expression: i
  
-- Exercise 2.2
-- (a) 
  
-- Exercise 2.3
  
-- Exercise 2.4
-- Exercise 2.5
