module MyFraction where
import Test.QuickCheck
type Fraction = (Integer,Integer)

--当分母为零时抛出错误
error_1 = "divided by 0" 

--加法
ratplus::Fraction->Fraction->Fraction
ratplus (a , b) (c , d) = 
    if b*d==0 then error error_1
    else ( div (a*d+b*c) k , div (b*d) k)
        where k = gcd (a*d+b*c) (b*d)

infix 2 <+>
(<+>) :: Fraction -> Fraction -> Fraction
(<+>) (a,b) (c,d) = ratplus (a,b) (c,d)

--减法
ratminus::Fraction->Fraction->Fraction
ratminus (a , b) (c , d) = 
    if b*d==0 then error error_1
    else (div (a*d+b*c) k , div (b*d) k)
        where k = gcd (a*d-b*c) (b*d)

infix 2 <->
(<->)::Fraction->Fraction->Fraction
(<->) (a,b) (c,d) = ratminus (a,b) (c,d)

--乘法
rattimes::Fraction->Fraction->Fraction
rattimes (a,b) (c,d) = 
    if b*d==0 then error error_1
    else (div (a*c) k,div (b*d) k)
        where k = gcd (a*c) (b*d) 

infix 3 <-*->
(<-*->)::Fraction->Fraction->Fraction
(<-*->) (a,b) (c,d) = rattimes (a,b) (c,d)

--除法
ratdiv::Fraction->Fraction->Fraction
ratdiv (a,b) (c,d) = rattimes (a,b) (d,c)

infix 3 </>
(</>)::Fraction->Fraction->Fraction
(</>) (a,b) (c,d) = ratdiv (a,b) (c,d)

--转换为整型
ratfloor::Fraction->Integer
ratfloor (a,b) = 
    if b==0 then error error_1
    else div a b

--逻辑判断相等
rateq :: Fraction -> Fraction -> Bool
rateq (a,b) (c,d) = 
    if b*d==0 then error error_1
    else 
        if div a k == div c g 
            && div b k == div d g 
             then True
         else False
    where k = gcd a b
          g = gcd c d

infix 1 <==>
(<==>)::Fraction->Fraction->Bool
(<==>) (a,b) (c,d) = rateq (a,b) (c,d)

--转换为浮点型
ratfloat::Fraction->Float
ratfloat (a,b) = 
    if b == 0 then error error_1
    else fromInteger a / fromInteger b

--以下是测试内容
--测试任意分数(a,b)加上(0,1)仍是(a,b)
prop_ratplus_unit::Fraction->Property
prop_ratplus_unit (a,b) = 
    a > 0 && b > 0 
    ==> ratplus (a,b) (0,1) <==> (a,b)
--测试任意分数(a,b)乘上(1,1)仍是(a,b)
prop_rattimes_unit::Fraction->Property
prop_rattimes_unit (a,b) = 
    a > 0 && b > 0
    ==> rattimes (a,b) (1,1) <==> (a,b)
--测试优先级
prop_priority::Fraction->Fraction->Fraction->Property
prop_priority (a,b) (c,d) (e,f)=
    b*d*f>0 ==> 
    (a,b) <+> (c,d) <-*-> (e,f) <==> (a,b) <+> ((c,d) <-*-> (e,f))
--测试是否符合分配律
prop_rattimes_plus_distr::Fraction->Fraction->Fraction->Property
prop_rattimes_plus_distr (a,b) (c,d) (e,f) =
    b > 0 && d > 0 && f > 0 ==>
    (a,b) <-*-> ((c,d) <+> (e,f)) <==> ((a,b) <-*-> (c,d)) <+> ((a,b) <-*-> (e,f))