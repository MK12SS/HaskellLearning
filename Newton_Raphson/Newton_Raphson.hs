--mk12ss@outlook.com
--每一个都手动测试过，没发现什么毛病
--没有用quickCheck因为这里不知道能怎么用
module Newton_Raphson where

--给定初值x0和迭代次数n后2的近似平方根
squareroot2 :: Float -> Integer -> Float
squareroot2 x 0 = x
squareroot2 x n = squareroot2 ((x+2/x)/2) (n-1)

--给定初值x0和迭代次数n后r的近似平方根
--原来写成：
--squareroot r x0 n = ((squareroot r x0 n-1)+r/(squareroot r x0 n-1))/2
--但在下面求平方根序列时发现这样O(N)太大，遂改为尾递归
squareroot :: Float -> Float -> Integer -> Float
squareroot r x 0 = x
squareroot r x n = squareroot r ((x+r/x)/2) (n-1)

--返回近似平凡根序列
sqrtSeq :: Float -> Float -> [Float]
sqrtSeq r x = [squareroot r x n|n<-(0:[1,2..])]

--给定r和初值x，返回上述序列相邻两项之差小于epxilon时的r的平方根近似值
squareroot' :: Float -> Float -> Float -> Float
squareroot' r epsilon x = mySquare seq epsilon
    where seq = sqrtSeq r x

--同样返回上述序列相邻两项之差小于epxilon时的近似值，但传入的参数为近似平方根序列
--写这个是因为不知道怎么用上面那个函数干这事，有谁知道怎么做的请用上面那个邮箱秀我一下？拜托了。
mySquare::[Float]->Float->Float
mySquare (x:xs) epsilon = if abs (x-(head xs))<epsilon then head xs
                        else mySquare xs epsilon 

