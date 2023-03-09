module CBI where
import Prelude hiding (and,not,or, succ, pred)
import LC

data Bit = One | Zero  
instance Show Bit where
    show One = "1"
    show Zero = "0"


-- Boolean lambda expressions
-- cond = \e1.\e2.\c.((c e1) e2)
cond = \e1 -> \e2 -> \c -> ((c e1) e2)
  
-- true = \x.\y.\x
true = selectFirst
-- false = \x.\y.y
false = selectSecond

-- not = \x.((x false) true)
-- Example:
--  not true 
--  === (\x.((x false) true) true)
--  === ((true  false) true)
--  === ((\first.\second.first false) \first.second.first)
--  === (false \first.second.first)
--  === (\first.\second.second \first.second.second)
--  === (\second.second)
not = \x -> ((x false) true)
-- and = \x.\y.(((cond y) x) false)
and = \x -> \y -> ((x y) false)

-- or = \x.\y.((x true) y)
or = \x -> \y -> ((x true) y)

-- Natural Numbers
-- zero = identity
zero = identity

succ = \n -> \s -> ((s false) n)

one = (succ zero)

two = (succ one)

three = (succ two)

iszero = \z -> (z selectFirst)

pred = \n -> (((iszero n) zero) (n selectSecond))
-- Exercise 3.1
-- implication === ~x \/ y 
-- implies = \x.\y.(or (not x) y)
-- = \x.\y.(\x1.\y1.((x1 true) y1) (not x) y)
-- = \x.\y.(\x1.\y1.((x1 true) y1) (\x2.((x2 false) true) x) y)
-- = \x.\y.(\y1.((y true) y1) ((x false) true))
-- = \x.\y.((y true) ((x false) true))
-- = \x.\y.((y true) ((x false) true))
-- implication = \x -> \y -> (\x1 -> \y1 -> (((\e1 -> \e2 -> \c -> ((c e1) e2) y1) x1) \first -> \second -> second) (\x2 -> (((\e1 -> \e2 -> \c -> ((c e1) e2) \first -> \second -> second) \first -> \second -> first) x2) y))
implies = \x -> \y -> (or (not x) y)
implies2 = \x -> \y -> ((y true) ((x false) true))

-- Exercise 3.2

