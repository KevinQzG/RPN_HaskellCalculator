import Data.List  
  
solveRPN :: String -> Float  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:xs) "sqrt" = sqrt x:xs
            foldingFunction (x:xs) "negate" = negate x:xs
            foldingFunction (x:xs) "condNumb" = condNumb x:xs
            foldingFunction(x:y:ys) "totalSum" = sum (x:y:ys):ys
            foldingFunction(x:y:ys) "totalProd" = product (x:y:ys):ys
            foldingFunction xs "totalProm" = (sum xs / fromIntegral (length xs)):xs 
            foldingFunction xs numberString = read numberString:xs  
condNumb:: Float -> Float
condNumb num
   | num == 3 = 100
   | num == 5 = 25
   | otherwise = 0

main = do
    putStrLn ""
    putStrLn "Welcome to the RPN calculator, RPN Example: 10 4 3 + 2 * - = -4"
    putStrLn ""
    putStrLn "1. Add (10 4 3 2 + +) :"
    putStrLn "The result of the operation is: "
    print (solveRPN "10 4 3 2  + +")
    putStrLn ""
    putStrLn "2. Subtract (20 5 4 2 - -) :"
    putStrLn "The result of the operation is: "
    print (solveRPN "20 5 4 2  - -")
    putStrLn ""
    putStrLn "3. Multiply (90 5 2 4 * *) :"
    putStrLn "The result of the operation is: "
    print (solveRPN "90 5 2 4  * *")
    putStrLn ""
    putStrLn "4. Divide (70 5 6 4 / /) :"
    putStrLn "The result of the operation is: "
    print (solveRPN "70 5 6 4 / /")
    putStrLn ""
    putStrLn "5. Negate (10 67 negate) :"
    putStrLn "The result of the operation is: "
    print (solveRPN "10 67 negate")
    putStrLn ""
    putStrLn "6. Negate (10 67 negate +) :"
    putStrLn "The result of the operation is: "
    print (solveRPN "10 67 negate +")
    putStrLn ""
    putStrLn "7. Negate (10 67 negate *) :"
    putStrLn "The result of the operation is: "
    print (solveRPN "10 67 negate *")
    putStrLn ""
    putStrLn "8. Sqrt (10 16 sqrt) :"
    putStrLn "The result of the operation is: "
    print (solveRPN "10 16 sqrt")
    putStrLn ""
    putStrLn "9. Sqrt (10 16 sqrt +) :"
    putStrLn "The result of the operation is: "
    print (solveRPN "10 16 sqrt +")
    putStrLn ""
    putStrLn "10. condNumb (10 5 condNumb) :"
    putStrLn "The result of the operation is: "
    print (solveRPN "10 5 condNumb")
    putStrLn ""
    putStrLn "11. condNumb (10 5 condNumb -) :"
    putStrLn "The result of the operation is: "
    print (solveRPN "10 5 condNumb -")
    putStrLn ""
    putStrLn "12. condNumb (10 3 condNumb +) :"
    putStrLn "The result of the operation is: "
    print (solveRPN "10 3 condNumb +")
    putStrLn ""
    putStrLn "13. condNumb (10 2 condNumb +) :"
    putStrLn "The result of the operation is: "
    print (solveRPN "10 2 condNumb +")
    putStrLn ""
    putStrLn "14. Total Sum (10 67 15 totalSum) :"
    putStrLn "The result of the operation is: "
    print (solveRPN "10 67 15 totalSum")
    putStrLn ""
    putStrLn "15. Total Prod (10 16 10  totalProd) :"
    putStrLn "The result of the operation is: "
    print (solveRPN "10 16 10 totalProd")
    putStrLn ""
    putStrLn "16. Total Promedy (120 20 40 120 totalProm) :"
    putStrLn "The result of the operation is: "
    print (solveRPN "120 20 40 120 totalProm")
    putStrLn ""
    putStrLn "Thanks for using the RPN calculator, bye!"
    putStrLn "Created by: Kevin Quiroz and Juan Esteban Garcia"
    putStrLn ""
