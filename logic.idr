-- de Morgan's laws in Idris
-- Inspired by http://www.fewbutripe.com/swift/math/2015/01/06/proof-in-functions.html
-- `Either a b` here means "a or b", `(a, b)` means "a and b".  "not a" is `a -> Void`, as in the article.

deMorgan1 : Either (a -> Void) (b -> Void) -> ((a, b) -> Void)
deMorgan1 (Left l) = \x => l (fst x)
deMorgan1 (Right r) = \x => r (snd x)

deMorgan2 : (Either a b -> Void) -> (a -> Void, b -> Void)
deMorgan2 f = (\x => f (Left x), \x => f (Right x))

deMorgan3 : (a -> Void, b -> Void) -> (Either a b -> Void)
deMorgan3 (f, g) x = case x of
                      Left l => f l
                      Right r => g r

-- This one doesn't work -- not valid in constructive logic
deMorgan4 : ((a, b) -> Void) -> Either (a -> Void) (b -> Void)
deMorgan4 f = Left (\x => f (x, ?uhh))
