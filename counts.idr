module Main

-- Chapter 2 exercise: write a complete program that prompts for an input, calls the function, and
-- prints its output.

counts : String -> (Nat, Nat)
counts w = (length $ words w, length w)

main : IO ()
main = do
  line <- getLine
  let (ws, cs) = counts line
  putStrLn $ unwords ["There were", (show ws), "words and", (show cs), "characters."]
