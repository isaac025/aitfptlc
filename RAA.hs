{-# LANGUAGE BlockArguments #-}

module RAA where

import Prelude hiding (succ, pred)
import CBI
import LC

rec f = selfApplication f $$ selfApplication f

-- add x y = if (iszero y) then x else add (succ x) (pred y)


-- Exercise 4.1
-- sum1 f n = if iszero n then zero else add n (f (pred n))
-- sum = rec sum1
-- sum three
-- = rec sum1 three
-- = sum1 sum1 three
-- => add three (sum1 (pred three)), evaluate pred three to two
-- = add three (add two (sum1 (pred two))), evaluate pred three to one
-- = add three (add two (add one (sum1 (pred one)))), evaluate pred three to zero
-- = add three (add two (add one zero)), evaluate add one zero to one
-- = add three (add two one), evaluate add one two to three
-- = add three three, evaluate to six
-- = six

-- Exercise 4.2
-- let mult x y = if iszero y then zero else rec add x (mult x (pred y))
--     sub x y = if iszero y then x else rec sub (pred x) (pred y) 
--     absDiff x y = add (sub x y) (sub y x)
--     equal x y = iszero (absDiff x y)
--
-- prod1 f n = if equal n one then one else mult n (f (pred n))
-- prod = rec prod1
-- prod three
-- = rec prod1 three
-- = prod1 prod1 three
-- => mult three (prod1 (pred three)), evaluate three to two
-- => mult three (mult two (prod1 (pred two))), evaluate two to one
-- => mult three (mult two one)
-- => mult three (add two (mult two (pred one))), evaluate one to zero
-- => mult three (add two zero)
-- => mult three two
-- => add three (mult three one)
-- => add three (add three (mult three zero))
-- => add three (add three zero)
-- => add three three
-- => add (succ three) (pred three)
-- => add (succ four) (pred two)
-- => add (succ five) (pred one)
-- => add six zero
-- => six
  
-- Exercise 4.3
--


