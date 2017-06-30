data W a =  W a deriving (Show)

return_ :: a -> W a
return_ x = W x

fmap_ :: (a -> b) -> (W a -> W b)
fmap_ f (W x) = W (f x)

bind_ :: (a -> W b) -> (W a -> W b)
bind_ f (W x) = f x

{-
1. Define a function g :: Int -> W Int -> W Int so that g x (W y) = W (x+y). Obviously that definition won't do - the left hand side has a W y pattern so it's actually unwrapping. Rewrite this function so that the only unwrapping that happens is carried out by bind.
-}
g :: Int -> W Int -> W Int
g x y = bind_ (return_ . (+x)) y
