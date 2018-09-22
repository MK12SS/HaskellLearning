type Fraction = (Integer,Integer)
error_1 = "divided by 0"

ratplus::Fraction->Fraction->Fraction
ratplus (a , b) (c , d) = 
    if b*d==0 then error error_1
    else ( div (a*d+b*c) k , div (b*d) k)
        where k = gcd (a*d+b*c) (b*d)

infix 2 <+>
(<+>) :: Fraction -> Fraction -> Fraction
(<+>) (a,b) (c,d) = ratplus (a,b) (c,d)

ratminus::Fraction->Fraction->Fraction
ratminus (a , b) (c , d) = 
    if b*d==0 then error error_1
    else (div (a*d+b*c) k , div (b*d) k)
        where k = gcd (a*d-b*c) (b*d)

infix 2 <->
(<->)::Fraction->Fraction->Fraction
(<->) (a,b) (c,d) = ratminus (a,b) (c,d)

rattimes::Fraction->Fraction->Fraction
rattimes (a,b) (c,d) = 
    if b*d==0 then error error_1
    else (div (a*c) k,div (b*d) k)
        where k = gcd (a*c) (b*d) 

infix 3 <-*->
(<-*->)::Fraction->Fraction->Fraction
(<-*->) (a,b) (c,d) = rattimes (a,b) (c,d)

ratdiv::Fraction->Fraction->Fraction
ratdiv (a,b) (c,d) = rattimes (a,b) (d,c)

infix 3 </>
(</>)::Fraction->Fraction->Fraction
(</>) (a,b) (c,d) = ratdiv (a,b) (c,d)

ratfloor::Fraction->Integer
ratfloor (a,b) = 
    if b==0 then error error_1
    else div a b

rateq :: Fraction -> Fraction -> Bool
rateq (a,b) (c,d) = 
    if b*d==0 then error error_1
    else if div a k == div c g && div b k == div d g then True
         else False
    where k = gcd a b
          g = gcd c d

infix 1 <==>
(<==>)::Fraction->Fraction->Bool
(<==>) (a,b) (c,d) = rateq (a,b) (c,d)

ratfloat::Fraction->Float
ratfloat (a,b) = 
    if b == 0 then error error_1
    else fromInteger a / fromInteger b