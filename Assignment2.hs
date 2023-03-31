-- Do not alter the following line
module Assignment2 (transaction_to_string, trade_report_list, stock_test, get_trades, trade_report, update_money, profit, profit_report, complex_profit_report) where


type Transaction = (Char, Int, Int, String, Int) 

test_log :: [Transaction]
test_log = [('B', 100, 1104,  "VTI",  1),
            ('B', 200,   36, "ONEQ",  3),
            ('B',  50, 1223,  "VTI",  5),
            ('S', 150, 1240,  "VTI",  9),
            ('B', 100,  229, "IWRD", 10),
            ('S', 200,   32, "ONEQ", 11), 
            ('S', 100,  210, "IWRD", 12)
            ]


-- Part A


transaction_to_string :: Transaction -> String
transaction_to_string (action, units, price,  stocks,  day) --First making objects of tuples and named them as described in assignment
 |action == 'B'= "Bought "  ++ show(units) ++ " units of " ++ stocks ++" for " ++ show(price) ++ " pounds each on day " ++ show(day) --checking if first element is B
 |action == 'S'= "Sold "  ++   show(units) ++ " units of " ++ stocks ++" for " ++ show(price) ++ " pounds each on day " ++ show(day) --checking if first element is S
 |otherwise = "  "

trade_report_list :: [Transaction] -> [String]
trade_report_list lists = map transaction_to_string lists --listing every string in one list using map function


stock_test :: String -> Transaction -> Bool
stock_test stock_present (action, units, price,  stocks,  day) --need to check the stocks present or not, rest of the types will not used so it left empty, if given stock which is stock_present is present then true
 |stock_present == stocks = True 
 |otherwise = False --if not present then false


get_trades :: String -> [Transaction] -> [Transaction]
get_trades "" [] = [] --defining empty string and trasnaction then result will be empty 
get_trades stocks x = filter (\xs -> stock_test stocks xs) x --using filter function to filter and results will be saved in x


trade_report :: String -> [Transaction] -> String
trade_report "" [] = ""  --defining empty string and trasnaction then result will be empty 
trade_report stocks x = unlines(trade_report_list (get_trades stocks x)) --unlines place next line statement in after every element in a list.

-- Part B


update_money :: Transaction -> Int -> Int
update_money (action, units, price,  stocks,  day) money 
 |action == 'B' = money - (units * price) -- bought means money spends on stock then subtract stock money from total
 |action == 'S' = money + (units * price) -- bought means money spends on stock then add stock money from total
 |otherwise = 0 

profit :: [Transaction] -> String -> Int
profit x stocks = foldr (+) 0 (map (`update_money` 0) (get_trades stocks x))
--foldr is fold right, it will will sum prices of given stocks using map function and then sum using foldr because we are having + operator.


profit_report :: [String] -> [Transaction] -> String
profit_report _ [] = "" --defining empty strings and transaction
profit_report [] _ = "" --defining empty strings and transaction
profit_report [string] x = string ++ ": " ++ show(profit x string) ++ "\n" --string is the given stock name and then show profit of that stock
profit_report (st:st_end) x = st ++ ": " ++ show(profit x st) ++ "\n" ++ profit_report st_end x --if we have more than one transaction then we do this



-- Part C


test_str_log = "BUY 100 VTI 1\nBUY 200 ONEQ 3\nBUY 50 VTI 5\nSELL 150 VTI 9\nBUY 100 IWRD 10\nSELL 200 ONEQ 11\nSELL 100 IWRD 12\n"



type Prices = [(String, [Int])]

test_prices :: Prices
test_prices = [
                ("VTI", [1689, 1785, 1772, 1765, 1739, 1725, 1615, 1683, 1655, 1725, 1703, 1726, 1725, 1742, 1707, 1688, 1697, 1688, 1675]),
                ("ONEQ", [201, 203, 199, 199, 193, 189, 189, 183, 185, 190, 186, 182, 186, 182, 182, 186, 183, 179, 178]),
                ("IWRD", [207, 211, 213, 221, 221, 222, 221, 218, 226, 234, 229, 229, 228, 222, 218, 223, 222, 218, 214])
              ]

--we need to determine the profit or loss of the stock, since it is given as value of  stock for each day, s
profit_loss :: [String] -> String -> [Int] -> Int
profit_loss [] _ _ = 0
profit_loss (x:xs) stocks price
 |(words x !! 0) == "BUY" && (words x !! 2) == stocks   = ((price !! (read (words x !! 3) - 1)) * read (words x !! 1 ) * (-1)) + profit_loss xs stocks price
 |(words x !! 0) == "SELL"  && (words x !! 2) == stocks  = ((price !! (read (words x !! 3) - 1)) * read (words x !! 1) * (1)) + profit_loss xs stocks price
 |otherwise =  0 + profit_loss xs stocks price
-- by first checking the conditions using words function (it is a function that shows the value by indexes using double exclamation marks)
-- if it is sell or buy we have two different expressions
--first we find word at 3 index (starting from zero) which is the Day when trade  took place
--then subtract 1 becase index start from zero whereas days starts with 1
--then reading this value and finding index value in Prices List
--on parallel we find word at index 1 which is price of stock, if buying then multiply with -1 and if selling then multiply with +1
--this multiplied value then multiply with price of that day and number days that we extracted from test_str_log.
-- this recurr by summing the values untill test_str_log finishes


complex_profit_report :: String -> Prices -> String
complex_profit_report _ [] = "" 
complex_profit_report x ((stocks,price):xs) = stocks ++ ": " ++ (show (profit_loss (lines x) stocks price)) ++ "\n" ++ complex_profit_report x xs
--show the stocks and ++ concatinate with colon and shows the profit_loss of stock, contactinate with next line and recurre the complex_profit_report function again