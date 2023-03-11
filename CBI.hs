module CBI where
import Prelude hiding (and,not,or,succ,pred)
import Util 
import LC

-- Boolean lambda expressions
-- cond = \e1.\e2.\c.((c e1) e2)
cond = \e1 -> \e2 -> \c -> ((c e1) e2)
  
-- true = \x.\y.\x
true = selectFirst
-- false = \x.\y.y
false = selectSecond

-- (true false) === (\f.\s.f \f.\s.s)
--              === (\s.\f.\s.s )

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

pred :: (forall a. a -> a) -> (a -> p2) -> a -> p2
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

-- Exercise 3.2 WRONG APPROACH
-- x equiv y === (~x \/ y) /\ (x \/ ~y)
-- equiv = \x.\y.(and (or (not x) y) (or x (not y)))
-- = \x.\y.(and (or (not x) y) (or x (not y)))
-- = \x.\y.((((cond (or (not x) y)) (or x (not y))) false))
-- = \x.\y.(or x (not y))
-- = \x.\y.(or x ((y false) true))
-- = \x.\y.(((((y \f.\s.s) \f.\s.f) \f.\s.f) x))
--equiv = \x -> \y -> (or x (not y))
--equiv :: (forall a b. (a -> b -> a) -> (a -> b -> b)) -> a -> b -> p1 -> p2 -> p2
--equiv = \x -> \y -> (and (or (not x) y) (or x (not y)))
--equiv = \x -> \y -> (and (((y true) ((x false) true))) (((x true) (((y false) true) y))))
--
-- CORRECT
-- x equiv y === (x /\ y) \/ (~x /\ ~y)
-- equiv = \x.\y.((or ((and x) y)) ((and (not x)) (not y)))
equiv = \x -> \y -> ((or ((and x) y)) ((and (not x)) (not y)))

-- Exericise 3.3
-- (a) (i) \x.\y.(and (not x) (not y))
--     (ii) \x.\y.(not (or x y))
-- (i) == \x.\y.(and ((x false) true) ((y false) true))
--     == \x.\y.((y false) true)
-- (ii) == \x.\y.(not (((x true) y)))
--      == \x.\y.((y false) true)
-- Now you can see that (x true) will always evaluate true
-- leaving ((y false) true)
-- You can see that by boolean algebra they are equivalent statements
-- Proofs:
ai = \x -> \y -> ((y false) true)
aii = \x -> \y -> ((y false) true)

-- (b) (i) implies === \x.\y.(or (not x) y)
--     (ii) \x.\y.(implies (not y) (not x))
-- By boolean algebra it is easy to see that they are equivalent
-- let's change (implies (not y) (not x)) to (implies y' x') where x' = not x,
-- y' = not y
-- then (implies y' 'x) === (or (not y') x') === (or (not (not y) (not x)) 
-- === (or y (not x) == implies
bi = implies
bii = \x -> \y -> (implies (not y) (not x))

-- (c) (i) not => \x.((x false) true)
--     (ii) \x.(not (not (not x)))
-- (ii) = \x.((((((x false) true) false) true) false) true)
-- By boolean algebra this is obvious 
-- (not (not (not x) === (not x)
ci = not
cii = \x -> ((((((x false) true) false) true) false) true)

-- (d) (i) implies = \x.\y.(or (not x) y)
--     (ii) \x.\y.(not (and x (not y)))
-- by boolean algebra (not (and x (not y))) => (or (not x) y)
di = implies
dii = \x -> \y -> (not (and x (not y)))

-- (e) (i) equiv === \x.\y.((or ((and x) y)) ((and (not x)) (not y)))
--     (ii) \x.\y.(and (implies x y) (implies y x))
--     = \x.\y.((implies x y) (implies y x))

ei = equiv
--eii :: (forall p2 p3. (p2 -> p3 -> p2) -> (p2 -> p3 -> p3)) -> p1 -> p4 -> p1
--eii = \x -> \y -> ((implies x y) (implies y x))

-- Exercise 3.4
-- \x.(succ (pred x)) === \x.(pred (succ x))
-- They are equivalent for non zero because you are doing the same on both ends
-- Proof by induction:
-- (succ (pred 1)) === (pred (succ 1))
-- (succ 0) === (pred 2)
-- 1 === 1
-- for (x - 1)
-- (succ (pred (x-1))) === (pred (succ (x-1)))
-- (succ (x - 1 - 1)) === (pred (x - 1 + 1))
-- (succ (x - 2)) === (pred x)
-- x - 2 + 1 === x - 1
-- x - 1 === x - 1
-- For once zero goes to the predecessor two things might happen:
--  1. undefined 
--  2. zero again
-- (succ (pred 0)) === (pred (succ 0))
-- (succ 0) === pred 1
-- 1 /=/ 0
--
