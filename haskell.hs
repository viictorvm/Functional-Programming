-- 1)Remove duplicates from a list

-- Union

ex1s:: [Int] -> [Int]
ex1s [] = []
ex1s (n:ns) = if elem n ns then ex1s ns else n:ex1s ns 

-- Tail recursive

ex1r:: [Int] -> [Int] -> [Int]
ex1r [] acc = acc
ex1r (n:ns) acc = if elem n ns then ex1r ns acc else ex1r ns (n:acc)

-- 2)Verify if n is prime (recursive)
ex2r:: Int -> Int -> Bool
ex2r 0 acc = False
ex2r x 1 = True
ex2r x acc = if (x `mod` acc == 0) && (acc < x) then False else ex2r x (acc-1)  

-- 3)List of primes <= lim
ex3r:: Int -> [Int] -> [Int]
ex3r 1 acc = acc
ex3r x acc = if (ex2r x (x-1)) == True then ex3r (x-1) (x:acc) else ex3r (x-1) acc

-- 4) Pow
f1s:: Int -> Int -> Int
f1s x 0 = 1
f1s x y = x * f1s x (y-1)

-- Pow using accumulator
f1t:: Int -> Int -> Int -> Int
f1t x 0 acc = acc -- if is 0, return acumulator
f1t x y acc = f1t  x (y-1) (acc * x) --

-- Fibonacci

f2s:: Int -> Int
f2s 1 = 1
f2s 2 = 1
f2s n = f2s(n-1) + f2s(n-2)

            

-- uliply all items in a list stack
f3s:: [Int] -> Int
f3s [] = 1 
f3s (n:ns) =  f3s(ns) * n

-- Muliply all items in a list (recursive)

f3t:: [Int] -> Int -> Int
f3t [] acc = acc
f3t (n:ns) acc = f3t ns (acc*n) 
                                
-- check if x is content in a list
f4s:: [Int] -> Int -> Bool
f4s [] x = False
f4s(n:ns) x = if n == x then True else f4s(ns) x



--Verify if the sum of digits of n equals k

ex4r :: Int -> Int -> Int -> Bool
ex4r 0 k sum= if k==sum then True else False
ex4r n k sum= ex4r (div n 10) k (sum+(mod n 10))

-- Length of a list
f1l:: [Int] -> Int
f1l n = f1lAux n acc where
    acc = 0
    f1lAux [] acc = acc
    f1lAux(x:xs) acc = f1lAux xs (acc+1)

-- Union of lists
f2l:: [Int] -> [Int] -> [Int]
f2l a b = f2lAux a b where
    f2lAux a [] = a
    f2lAux a (x:xs) = if elem x a then f2lAux a xs else f2lAux (x:a) xs 

-- Sum items of a list with an accumulator
rowSum::[Int] -> Int -> Int
rowSum [] acc = acc
rowSum (x:xs) acc = rowSum xs acc + x

-- Sum items of a matrix
f3l:: [[Int]] -> Int  
f3l matrix = f3lAux matrix acc where
    acc = 0
    f3lAux [] acc = acc
    f3lAux (x:xs) acc = f3lAux xs (acc + (rowSum x 0))
    

-- Preorder (bintree)
data Tree2 a = Empty | Branch a (Tree2 a) (Tree2 a) deriving Show 

preorder:: Tree2 Int-> [Int]
preorder Empty = []
preorder (Branch f ls rs) = f:preorder ls ++ preorder rs 

-- List comprehesion

ex3l :: Int -> [Int]
ex3l n = [x|x<-[2..n], ex2r x (x-1)]
