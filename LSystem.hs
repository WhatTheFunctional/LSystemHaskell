--LSystem.hs
--Copyright Laurence Emms 2018
--Lindenmayer system tree generator example

import qualified Data.Map

data Start a = Start a deriving (Show)
data Rules a = Rules (a -> [a])

applyLSystem :: [a] -> Rules a -> [[a]]
applyLSystem [] _ = [[]]
applyLSystem (symbol : system) (Rules f)
    = (f symbol) : applyLSystem system (Rules f)

evaluateLSystem :: [a] -> Rules a -> [a]
evaluateLSystem system rules = concat (applyLSystem system rules)

getNewLSystem :: Show a => ([a] -> IO ()) -> ([a], Rules a) -> IO ([a], Rules a)
getNewLSystem draw (system, rules)
    = draw system >> putStr "-----\n" >> return (newSystem, rules)
        where newSystem = evaluateLSystem system rules

lSystemLoop :: Show a => Int -> Int -> ([a] -> IO ()) -> ([a], Rules a) -> IO ()
lSystemLoop iter maxIter draw (system, rules)
    | iter < maxIter = let newSystem = (getNewLSystem draw (system, rules)) in newSystem >>= lSystemLoop (iter + 1) maxIter draw
    | otherwise = return ()

--Lindenmayer's original Algae L-System
algaeRules :: Char -> [Char]
algaeRules 'A' = "AB"
algaeRules 'B' = "A"
algaeRules x = [x] --Any other symbol is just copied

drawAlgaeSystem :: [Char] -> IO ()
drawAlgaeSystem system = putStrLn system

--main = lSystemLoop 0 10 drawAlgaeSystem ("A", Rules algaeRules)

--BinaryTree L-System to Logo code
binaryDistance = 1

binaryRules :: Char -> [Char]
binaryRules '1' = "11"
binaryRules '0' = "1[0]0"
binaryRules x = [x] --Any other symbol is just copied

popBinaryStack :: [Char] -> IO [Char]
popBinaryStack [] = return []
popBinaryStack ('0' : stack) = putStrLn ("bk " ++ (show binaryDistance)) >> popBinaryStack stack
popBinaryStack ('1' : stack) = putStrLn ("bk " ++ (show binaryDistance)) >> popBinaryStack stack
popBinaryStack ('[' : stack) = putStrLn "rt 45" >> return stack
popBinaryStack (']' : stack) = putStrLn "lt 45" >> popBinaryStack stack

drawBinarySymbol :: [Char] -> [Char] -> IO ()
drawBinarySymbol stack [] = return ()
drawBinarySymbol stack ('0' : system) = putStrLn ("fd " ++ (show binaryDistance)) >> drawBinarySymbol ('0' : stack) system
drawBinarySymbol stack ('1' : system) = putStrLn ("fd " ++ (show binaryDistance)) >> drawBinarySymbol ('1' : stack) system
drawBinarySymbol stack ('[' : system) = putStrLn "lt 45" >> drawBinarySymbol ('[' : stack) system
drawBinarySymbol stack (']' : system) = popBinaryStack stack >>= (\newStack ->
                                        putStrLn "rt 45" >>
                                        drawBinarySymbol (']' : newStack) system)

drawBinarySystem :: [Char] -> IO ()
drawBinarySystem system = drawBinarySymbol [] system

--main = lSystemLoop 0 10 drawBinarySystem ("0", Rules binaryRules)

--Fractal Plant L-System to Logo code
plantDistance = 4

plantRules :: Char -> [Char]
plantRules 'X' = "F+[[X]-X]-F[-FX]+X"
plantRules 'F' = "FF"
plantRules x = [x] --Any other symbol is just copied

popPlantStack :: [Char] -> IO [Char]
popPlantStack [] = return []
popPlantStack ('F' : stack) = putStrLn ("bk " ++ (show plantDistance)) >> popPlantStack stack
popPlantStack ('X' : stack) = popPlantStack stack
popPlantStack ('+' : stack) = putStrLn "lt 25" >> popPlantStack stack
popPlantStack ('-' : stack) = putStrLn "rt 25" >> popPlantStack stack
popPlantStack ('[' : stack) = return stack
popPlantStack (']' : stack) = popPlantStack stack

drawPlantSymbol :: [Char] -> [Char] -> IO ()
drawPlantSymbol stack [] = return ()
drawPlantSymbol stack ('F' : system) = putStrLn ("fd " ++ (show plantDistance)) >> drawPlantSymbol ('F' : stack) system
drawPlantSymbol stack ('X' : system) = drawPlantSymbol ('X' : stack) system
drawPlantSymbol stack ('+' : system) = putStrLn "rt 25" >> drawPlantSymbol ('+' : stack) system
drawPlantSymbol stack ('-' : system) = putStrLn "lt 25" >> drawPlantSymbol ('-' : stack) system
drawPlantSymbol stack ('[' : system) = drawPlantSymbol ('[' : stack) system
drawPlantSymbol stack (']' : system) = popPlantStack stack >>= (\newStack ->
                                       drawPlantSymbol newStack system)

drawPlantSystem :: [Char] -> IO ()
drawPlantSystem system = drawPlantSymbol [] system

main = lSystemLoop 0 6 drawPlantSystem ("X", Rules plantRules)
