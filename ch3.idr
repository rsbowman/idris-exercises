import Data.Vect

-- Chapter 3

-- Define the following functions, defined in Prelude or Data.Vect:

-- length
my_length : List a -> Nat
my_length [] = Z
my_length (x :: xs) = S $ my_length xs

-- reverse
reverse_accumulator : List a -> List a -> List a
reverse_accumulator [] acc = acc
reverse_accumulator (x :: xs) acc = reverse_accumulator xs (x :: acc)

my_reverse : List a -> List a
my_reverse l = reverse_accumulator l []

-- map for Lists
my_map : (a -> b) -> List a -> List b
my_map f [] = []
my_map f (x :: xs) = f x :: my_map f xs

-- map for Vects
my_map2 : (a -> b) -> Vect n a -> Vect n b
my_map2 f [] = []
my_map2 f (x :: xs) = f x :: my_map2 f xs

-- The matrix multiply question

addMatrix : Num n => Vect r (Vect c n) -> Vect r (Vect c n) -> Vect r (Vect c n)
addMatrix [] [] = []
addMatrix (xRow :: xRows) (yRow :: yRows) = (zipWith (+) xRow yRow) :: addMatrix xRows yRows

createEmpties : Vect c (Vect 0 n)
createEmpties = replicate _ []

transposeHelper : (x : Vect c n) -> (xsTrans : Vect c (Vect len n)) -> Vect c (Vect (S len) n)
transposeHelper [] [] = []
transposeHelper xs ys = zipWith (::) xs ys

transposeMat : Vect r (Vect c n) -> Vect c (Vect r n)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                         transposeHelper x xsTrans

dot : Num a => Vect l a -> Vect l a -> a
dot xs ys = sum $ zipWith (*) xs ys

rowhelp : Num a => (x : Vect m a) -> (ys : Vect n (Vect m a)) -> Vect n a
rowhelp x ys = map (`dot` x) ys

multHelper : Num a => (xs : Vect len (Vect m a)) -> (ys : Vect n (Vect m a)) -> Vect len (Vect n a)
multHelper [] xs = []
multHelper (x :: xs) [] = replicate _ []
multHelper (x :: xs) ys = rowhelp x ys :: multHelper xs ys

multMatrix : Num a => Vect l (Vect m a) -> Vect m (Vect n a) -> Vect l (Vect n a)
multMatrix [] [] = []
multMatrix [] xs = []
multMatrix xs ys = let ysT = transposeMat ys in
                     multHelper xs ysT
