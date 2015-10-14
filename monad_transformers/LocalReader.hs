-- to run:
-- *Main> runReader localExample "Fred"
-- ("First, I am Fred","Second, I am Freddy","Third, I am Fred")

import Control.Monad.Reader

myName step = do
  name <- ask
  return (step ++ ", I am " ++ name)

localExample :: Reader String (String, String, String)
localExample = do
  a <- myName "First"
  b <- local (++"dy") (myName "Second")
  c <- myName "Third"
  return (a, b, c)
