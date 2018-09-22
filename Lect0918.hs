{-
类型推导规则：

f :: A -> B, x :: A
--------------------
 f x :: B


12 :: Int
-}

double :: Int -> Int
double x = 2*x

bmi :: Float -> Float -> Float
bmi h m = m / h^2

bmi1 :: (Float,Float) -> Float
bmi1 (h,m) = m / h^2

-- double 2 
-- bmi 1.75 :: Float -> Float

g0 :: Float -> Float
g0 x = 3.14
g1 :: Float -> Float
g1 x = x

g2 :: (Float -> Float) -> Float
g2 x = 1

g3 :: (Float -> Float) -> Float
g3 x = x 0


-- elearning.sysu.edu.cn
-- "Hello, 123" :: String ([Char])
-- False || 
-- || 
-- 定义下列类型函数，至少一个
-- Bool -> Bool
-- Bool -> Bool -> Bool
-- f x y = x || y
-- f x = False ||

--(Bool, Bool) -> Bool
-- f (x,y) = x

-- Bool -> (Bool,Bool)
-- f x = (x,True)
-- (+) :: Num a => a -> a -> a

-- Variable not in scope: f

f :: Bool -> Bool
f x = True || False

f1 :: Bool -> Bool
f1 True = False
f1 False = True

-- 输入是分数，输出是其相反数
-- 1/2 ==> -1/2 1/2 <=> (1,2) -1/2 <=> (-1,2)
-- type String = [Char]
type Fraction = (Integer, Integer)
neg :: Fraction -> Fraction
neg (a,b) =  (-a,b)

add :: Fraction -> Fraction -> Fraction
add (a,b) (c,d) = (div (a*d+b*c) e, div (b*d) e)
    where
    e = gcd (a*b+b*c) (b*d)
(<+>) (a,b) (c,d) = add (a,b) (c,d)

--fraction_eq :: Fraction -> Fraction -> Bool
