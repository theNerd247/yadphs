import Parser 
import Data.Text

main = do
	file <- readFile "todo.txt"
	todoEvents file putStrLn
	return ()
