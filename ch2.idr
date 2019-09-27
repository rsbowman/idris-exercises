import Prelude.List

-- Chapter 2

-- 1. What are the types of the following values?

--    a. ("A", "B", "C")
--    b. ["A", "B", "C"]
--    c. (('A', "B"), 'C')

-- 2. Write a `palindrome` function of type `String -> Bool` that returns
--    whether the input reads the same backwards as forwards.  (Hint: use
--    `reverse : String -> String`.)

palindrome : String -> Bool
palindrome x = x == reverse x

-- 3. Modify the `palindrome` function so that it's not case sensitive.  (Hint: use `toLower : String -> String`).

palindrome2 : String -> Bool
palindrome2 x = let y = toLower x
                in y == reverse y

-- 4.  Modify the `palindrome` function so that it only returns true for strings longer than 10 characters.

-- 5. Modify the `palindrome` function so that it only returns true for strings longer than some length given as an argument.

palindrome3 : Nat -> String -> Bool
palindrome3 n x = palindrome x && length x > n

-- 6. Write a function `counts : String -> (Nat, Nat)` that returns a pair of the number of words in the input and the number of characters in the input.

counts : String -> (Nat, Nat)
counts w = (length $ words w, length w)

-- 7. Write a function `top_ten : Ord a => List a -> List a` that returns the ten largest values in a list.

top_ten : Ord a => List a -> List a
top_ten = List.take 10 . reverse . sort

-- 8. Write a function `over_length : Nat -> List String -> Nat` that returns the number of strings in the list longer than the given number of characters.

over_length : Nat -> List String -> Nat
over_length n l = length $ filter (> n) $ map length l

-- 9. For each of `palindrome` and `counts`, write a complete program that prompts for an input, calls the function, and prints its output.

-- Extra stuff

-- Pointfree palindrome: write the palindrome function in pointfree style
