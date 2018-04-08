--{-# LANGUAGE MonoLocalBinds, NoMonomorphismRestriction #-}

-- | Interesting Haskell examples for parsing, type inference, etc

main = pure ()

f y z =
  y (g z) True

g x = (1 :: Int)

h :: a -> (Bool, Int, a)
h k =
  let
    id' x = x
    y = id' True
    z = id' (1 :: Int)
  in (id' y, id' z, id' k)

m =
      let
    x = 1
   in let
  y = 5
  z = 7
 in x + y + z


l = do
  print "hi"
  pure ()
  pure $
    let
      x = 1
    in x

-- a :: Int -> Int
-- a _ _ = 1

prefix :: a -> [[a]] -> [[a]]
prefix x yss =
  let
    -- xcons :: [a] -> [a]
    xcons ys = x : ys
  in
    map xcons yss


data Seq a
  = Nil
  | Zero (Seq (a,a))
  | One a (Seq (a,a))
  deriving (Show, Eq)

-- If type signature for cons is left off, we get this error:
--
-- • Occurs check: cannot construct the infinite type: b ~ (b, b)
--   Expected type: (b, b) -> Seq (b, b) -> Seq (b, b)
--     Actual type: b -> Seq b -> Seq b
-- • Relevant bindings include
--     cons :: (b, b) -> Seq (b, b) -> Seq (b, b)
cons :: a -> Seq a -> Seq a
cons x Nil = One x Nil
cons x (Zero ps) = One x ps
cons x (One y ps) = Zero (cons (x,y) ps)

--     • Couldn't match expected type ‘Bool’ with actual type ‘Char’
--     • In the first argument of ‘g’, namely ‘'a'’
--       In the expression: g 'a'
--       In the expression: (g True, g 'a')
--    |
-- 62 | f' g = (g True, g 'a')
--    |                   ^^^
--
-- f' g = (g True, g 'a')


data BalancedTree a
  = Zero' a
  | Succ (BalancedTree (a,a))

zig :: BalancedTree a -> a
zig (Zero' a) = a
zig (Succ t) = fst (zag t)

-- zag :: BalancedTree a -> a
zag (Zero' a) = a
zag (Succ t) = snd (zig t)


f' x = x + 1
g' x = let h y = f' y * 2
           k z = z+x
       in  h x + k x


-- Fails to type check if {-# LANGUAGE MonoLocalBinds,
-- NoMonomorphismRestriction #-} is set. The function @b@ will be assumed to be
-- of type b :: a -> Bool -> (a, Bool) instead of the more general b :: a -> b
-- -> (a, b)
a x =
  let
    --b :: _
    b y = (x,y)
  in (b 3, b False)
