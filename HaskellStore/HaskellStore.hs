-- mk12ss@outlook.com
--仅对计算总价格的函数getTotalPrice::Items->Int->Float->Float作QuickCheck检查，其他函数仅作手动检查
module HaskellStore where
import Text.Printf
import System.IO

type Items = [Item]
type Item = (Name, Amount, Price)-- name, amount and price per unit of the item
type Name = String -- name of the item
type Amount = Float -- amount, like kg or number
type Price = Float  -- price per unit

{- Output section-}
--calculate the total price
getTotalPrice::Items->Int->Float->Float
getTotalPrice ((name,amount,price):xs) n x = getTotalPrice xs (n-1) (x + amount*price)
getTotalPrice _ 0 x = x

--print a single item, as its name indicates
--suppose that the length of names is now more than 10 characters
printSingleItem::Item->IO()
printSingleItem (name,amount,price) = do
    putStr name
    printSpace (11 - (length name))
    printf "%.2f    %.2f    %.2f\n" amount price (amount*price)

--calculate the amount of spaces between name and amount
printSpace::Int->IO()
printSpace 0 = return ()
printSpace n = do
    putChar ' '
    printSpace (n - 1)

--convert an item into string
--could have been used in Output section
--may be used in input section, to save the items into a file
itemToString::Item->String
itemToString (name,amount,price) = 
    name ++ " " ++ (show amount) ++ " " ++ (show price) ++ "  " ++ (show (price*amount)) ++ "\n"

--a function used in printItems::Items->IO(), to simplify the recursion
printItem::Items->Int->IO()
printItem (x:xs) 1 = printSingleItem x
printItem (x:xs) n =  do
    printSingleItem x
    printItem xs (n-1)

--print all items in the list   
printItems::Items->IO()
printItems items = do
    printf "Name"
    printSpace spaces
    printf "Amount  Price   Sum\n"
    printItem items n
    printf "Total"
    printSpace spaces
    printf "................%.2f\n" (getTotalPrice items n 0)
        where
            n = length items
            spaces = 11 - (length "Name")



{--------------unfinished main part----------------------}
printWelcome::IO()----------------------------done
printWelcome = putStr (unlines ["Welcome to store information management system"
                                ,"please select the operation you want to perform"
                                ,"1. add a deal"
                                ,"2. print all deals"
                                ,"3. delete a deal"])

mainLoop::Items->IO()
mainLoop items = do    
    input <- getLine
    if input == "-1" 
        then do  writeDeals <- openFile "information.txt" WriteMode
                 saveNewData writeDeals items (length items)
                 hClose writeDeals
                 return ()
        else do process input items
                readDeals <- openFile "information.txt" ReadMode
                let newItems = loadPreviousData readDeals []
                hClose readDeals
                mainLoop newItems

process::String->Items->IO()
process x items  
    | x == "1" = do getItem <- getLine
                    let newItems = addItemByString items getItem
                    writeDeals <- openFile "information.txt" WriteMode
                    saveNewData writeDeals newItems (length items)
                    hClose writeDeals
    | x == "2" = printItems items
    
    |otherwise = printf "Error:plz input a valid number\n"

main::IO()
main = do
    readDeals <- openFile "information.txt" ReadMode
    let items = loadPreviousData readDeals []
    hClose readDeals
    printWelcome
    mainLoop items

{--------------Input section(unfinished)-------------}
addItem::Items->Item->Items
addItem xs x = x:xs

addItemByElements::Items->Name->Amount->Price->Items
addItemByElements xs name amount price = (name,amount,price):xs

addItemByString::Items->String->Items
addItemByString xs input = (stringToItem input):xs

stringToItem::String->Item
stringToItem input = do
    let (name:amountS:priceS:xs) = words input
        amount = read amountS::Float
        price = read priceS::Float
    (name,amount,price)

loadPreviousData::Handle->Items->Items
loadPreviousData deals x= do
    ineof <- hIsEOF deals
    if ineof
    then x
    else do newLine <- hGetLine deals
            let newItem = stringToItem newLine
            loadPreviousData deals (newItem:x)

saveNewData::Handle->Items->Int->IO()
saveNewData _ _ 0 = return ()
saveNewData writeDeals (x:xs) n = do
    hPutStr writeDeals (itemToString x)
    saveNewData writeDeals xs (n-1)
