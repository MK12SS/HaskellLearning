module MyFraction where
import Test.QuickCheck

type Fraction = (Integer, Integer)

simplify :: Fraction -> Fraction
simplify x = (div (fst x) e, div (snd x) e)
    where
    e = (gcd (fst x) (snd x))


ratplus (a,b) (c,d) = simplify (a*d+b*c , b*d)

ratminus (a,b) (c,d) = simplify (a*d-b*c , b*d)

prop_ratplus (a,b) (c,d) =  b/= 0 && d/=0 ==> ratplus (a,b) (c,d) == ratplus (c,d) (a,b)


-- slove equation ax^2 + bx+c=0, returns the two roots
solve :: (Float,Float, Float)  -> (Float, Float)
solve (a,b,c) 
    | e >=0 =  ((-b+e)/(2*a),(-b+e)/(2*a) )
    | otherwise = error "No roots"
    where
    e = sqrt (b*b-4*a*c)

{-
0! = 1
n!= n * (n-1)!

3! = 3 * 2! = 3*2*1!=3*2* 1*0! = 3*2*1*1 = 6
-}

fac :: Integer -> Integer
fac 0 = 1
fac n = n * (fac (n-1))

sumFacs :: Integer -> Integer
sumFacs 0 = fac 0
sumFacs n = sumFacs (n-1) + fac n

sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f 0 = f 0
sumFun f n = sumFun f (n-1) + f n

-- gcd(a,0) = a
-- gcd(a,b) = gcd (b, mod(a,b))

mygcd :: Integer -> Integer ->Integer
mygcd a 0 = a
mygcd a b = mygcd b (mod a b)




