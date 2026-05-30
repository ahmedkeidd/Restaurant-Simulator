data Month = Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec   deriving (Show, Eq)

type Date = (Int, Month, Int)
type Price = Float
type Quantity = Int

type Supply = (String, Quantity, Price)
--	representing the name of the ingredient, quantity needed of that ingredient, and the total price of the needed quantity

type Delivery = (Date, [Supply])
-- date of delivery the restaurant will make and the required supply on that date.


data Ingredient = SimpleIngredient String | Recipe String [Ingredient] deriving (Show, Eq)
  -- An ingredient consisting of other ingredients


data Expense = Item String Price Date | Category String [Expense] deriving (Show, Eq)
  -- A category of expenses that could contain expenses or other categories.
  


ingredient_info :: [(String, Int, Price)]
ingredient_info = [("rice", 20, 1.2), ("apples", 5, 5), ("flour", 1, 0.5), ("eggs",1, 2), ("butter", 3, 12), ("garlic", 11, 4.5), ("salt", 0,0.25), ("pepper", 66,0.75), ("sugar", 7, 6), ("goat_meat", 20, 1.2)]

shopping_list :: [(Date, [Ingredient])]
shopping_list = [((15,Feb,2026),
                    [SimpleIngredient "flour",
                     SimpleIngredient "eggs",
                     SimpleIngredient "rice"]),
                 ((17,Feb,2026),
                    [SimpleIngredient "sugar",
                     SimpleIngredient "butter",
                     SimpleIngredient "flour",
                     SimpleIngredient "flour",
                     (Recipe "dough" [(SimpleIngredient "flour"),
                                      (SimpleIngredient "eggs")])]),
                 ((5,Mar,2026),
                    [SimpleIngredient "salt",
                     SimpleIngredient "pepper",
                     SimpleIngredient "garlic"]) ]
--these_are_our_helper_methods

monthToInt :: Month -> Int
monthToInt Jan = 1
monthToInt Feb = 2
monthToInt Mar = 3
monthToInt Apr = 4
monthToInt May = 5
monthToInt Jun = 6
monthToInt Jul = 7
monthToInt Aug = 8
monthToInt Sep = 9
monthToInt Oct = 10
monthToInt Nov = 11
monthToInt Dec = 12

intToMonth :: Int -> Month
intToMonth 1  = Jan
intToMonth 2  = Feb
intToMonth 3  = Mar
intToMonth 4  = Apr
intToMonth 5  = May
intToMonth 6  = Jun
intToMonth 7  = Jul
intToMonth 8  = Aug
intToMonth 9  = Sep
intToMonth 10 = Oct
intToMonth 11 = Nov
intToMonth 12 = Dec
intToMonth _  = Jan

daysInMonth :: Month -> Int
daysInMonth Jan = 31
daysInMonth Feb = 28
daysInMonth Mar = 31
daysInMonth Apr = 30
daysInMonth May = 31
daysInMonth Jun = 30
daysInMonth Jul = 31
daysInMonth Aug = 31
daysInMonth Sep = 30
daysInMonth Oct = 31
daysInMonth Nov = 30
daysInMonth Dec = 31


compareDates :: Date -> Date -> Ordering
compareDates (d1,m1,y1) (d2,m2,y2)
    | y1 /= y2 = compare y1 y2
    | monthToInt m1 /= monthToInt m2 = compare (monthToInt m1) (monthToInt m2)
    | otherwise = compare d1 d2
	
	
	
	
prevMonthOf :: Month -> Month
prevMonthOf m
    | monthToInt m - 1 < 1  = Dec
    | otherwise = intToMonth (monthToInt m - 1)

prevYearOf :: Month -> Int -> Int
prevYearOf m y
    | monthToInt m - 1 < 1  = y - 1
    | otherwise = y

	
	
	
	

subtractDays :: Date -> Int -> Date
subtractDays (d,m,y) 0 = (d,m,y)
subtractDays (d,m,y) n
    | n < d = (d - n, m, y)
    | otherwise = subtractDays (daysInMonth pm, pm, py) (n - d)
    where
        pm = prevMonthOf m
        py = prevYearOf m y



		
		
flattenIngredient :: Ingredient -> [String]
flattenIngredient (SimpleIngredient name) = [name]
flattenIngredient (Recipe _ ingredients)  = concatMap flattenIngredient ingredients






nameMatches :: String -> (String, Int, Price) -> Bool
nameMatches name (n, _, _) = n == name

getDays :: [(String, Int, Price)] -> Int
getDays ((_,days,_):_) = days
getDays [] = 0

getPrice :: [(String, Int, Price)] -> Price
getPrice ((_,_,price):_) = price
getPrice [] = 0.0

getIngredientDays :: String -> Int
getIngredientDays name = getDays (filter (nameMatches name) ingredient_info)

getIngredientPrice :: String -> Price
getIngredientPrice name = getPrice (filter (nameMatches name) ingredient_info)







nubSupplies :: [Supply] -> [Supply]
nubSupplies [] = []
nubSupplies ((n,q,p):rest) = (n,q,p) : nubSupplies (filter (\(n2,_,_) -> n2 /= n) rest)








sortSupplies :: [Supply] -> [Supply]
sortSupplies [] = []
sortSupplies ((n,q,p):rest)  = sortSupplies smaller ++ [(n,q,p)] ++ sortSupplies bigger
    where
        smaller = filter (\(n2,_,_) -> n2 <= n) rest
        bigger  = filter (\(n2,_,_) -> n2 > n)  rest

		
		
		
		
		
		

sortPairsByDate :: [(Date, Supply)] -> [(Date, Supply)]
sortPairsByDate [] = []
sortPairsByDate ((d,s):rest) = sortPairsByDate smaller ++ [(d,s)] ++ sortPairsByDate bigger
    where
        smaller = filter (\(d2,_) -> compareDates d2 d /= GT) rest
        bigger  = filter (\(d2,_) -> compareDates d2 d == GT) rest

		
		
		
		
		
		

sortDeliveries :: [Delivery] -> [Delivery]
sortDeliveries [] = []
sortDeliveries ((d,s):rest)  = sortDeliveries smaller ++ [(d,s)] ++ sortDeliveries bigger
    where
        smaller = filter (\(d2,_) -> compareDates d2 d /= GT) rest
        bigger  = filter (\(d2,_) -> compareDates d2 d == GT) rest

		
		
		
		
		
groupPairsByDate :: [(Date, Supply)] -> [[(Date, Supply)]]
groupPairsByDate [] = []
groupPairsByDate ((d,s):rest) = ((d,s) : same) : groupPairsByDate different
    where
        same = filter (\(d2,_) -> d2 == d) rest
        different = filter (\(d2,_) -> d2 /= d) rest

		
		
		
		
		
		
groupDeliveriesByDate :: [Delivery] -> [[Delivery]]
groupDeliveriesByDate [] = []
groupDeliveriesByDate ((d,s):rest) = ((d,s) : same) : groupDeliveriesByDate different
    where
        same = filter (\(d2,_) -> d2 == d) rest
        different = filter (\(d2,_) -> d2 /= d) rest

		
		
		
		
		
		
		
nameMatchesSupply :: String -> Supply -> Bool
nameMatchesSupply name (n, _, _) = n == name

supplyQty :: Supply -> Quantity
supplyQty (_, q, _) = q

supplyPrice :: Supply -> Price
supplyPrice (_, _, p) = p

mergeOneSupply :: [Supply] -> String -> Supply
mergeOneSupply supplies name = (name, totalQty, totalPrice)
    where
        matching = filter (nameMatchesSupply name) supplies
        totalQty = sum (map supplyQty matching)
        totalPrice = sum (map supplyPrice matching)

mergeSupplies :: [Supply] -> [Supply]
mergeSupplies supplies = map (mergeOneSupply supplies) uniqueNames
    where
        uniqueNames = map (\(n,_,_) -> n) (nubSupplies supplies)

--this_is_the_end_of_our_helper_methds
--just_to_make_actual_methods_not_long

-- e) calculateTotalExpenses  


addExpense :: Expense -> Price -> Price
addExpense e acc = calculateTotalExpenses e + acc

calculateTotalExpenses :: Expense -> Price
calculateTotalExpenses (Item _ price _) = price
calculateTotalExpenses (Category _ expenses) = foldr addExpense 0.0 expenses


-- f) countCategoryItems


addLeafCount :: Expense -> Int -> Int
addLeafCount e acc = countLeaves e + acc

countLeaves :: Expense -> Int
countLeaves (Item _ _ _) = 1
countLeaves (Category _ expenses) = foldr addLeafCount 0 expenses

addCategoryCount :: String -> Expense -> Int -> Int
addCategoryCount targetName e acc = countCategoryItems targetName e + acc

countCategoryItems :: String -> Expense -> Int
countCategoryItems _ (Item _ _ _) = 0
countCategoryItems targetName (Category name expenses)
    | name == targetName = countLeaves (Category name expenses)
    | otherwise = foldr (addCategoryCount targetName) 0 expenses

	
	
	
	
	
	
-- a) calculateDeliveryDates


makeDeliveryEntry :: Date -> String -> (Date, (String, Price))
makeDeliveryEntry requiredDate name = (deliveryDate, (name, price))
    where
        days = getIngredientDays name
        price = getIngredientPrice name
        deliveryDate = subtractDays requiredDate days

calculateDeliveryDates :: Date -> [Ingredient] -> [(Date, (String, Price))]
calculateDeliveryDates requiredDate ingredients = map (makeDeliveryEntry requiredDate) simpleNames
    where
        simpleNames = concatMap flattenIngredient ingredients


-- b) summarizeAllDeliveries


makeSupplyPair :: Date -> String -> (Date, Supply)
makeSupplyPair reqDate name = (deliveryDate, (name, 1, unitPrice))
    where
        days = getIngredientDays name
        unitPrice = getIngredientPrice name
        deliveryDate = subtractDays reqDate days

processShoppingEntry :: (Date, [Ingredient]) -> [(Date, Supply)]
processShoppingEntry (reqDate, ingredients) = map (makeSupplyPair reqDate) simpleNames
    where
        simpleNames = concatMap flattenIngredient ingredients

buildOneDelivery :: [(Date, Supply)] -> Delivery
buildOneDelivery grp = (fst (head grp), sortSupplies (mergeSupplies (map snd grp)))

buildDeliveries :: [(Date, Supply)] -> [Delivery]
buildDeliveries pairs = map buildOneDelivery grouped
    where
        sorted = sortPairsByDate pairs
        grouped = groupPairsByDate sorted

mergeOneGroup :: [Delivery] -> Delivery
mergeOneGroup grp = (fst (head grp), sortSupplies (mergeSupplies allSupplies))
    where
        allSupplies = concatMap snd grp

mergeDeliveriesByDate :: [Delivery] -> [Delivery]
mergeDeliveriesByDate deliveries = map mergeOneGroup grouped
    where
        sorted = sortDeliveries deliveries
        grouped = groupDeliveriesByDate sorted

dateIsInList :: [Date] -> (Date, [Ingredient]) -> Bool
dateIsInList dates (d, _) = d `elem` dates

summarizeAllDeliveries :: [Date] -> [Delivery]
summarizeAllDeliveries dates = mergeDeliveriesByDate (buildDeliveries flatPairs)
    where
        relevantEntries = filter (dateIsInList dates) shopping_list
        flatPairs = concatMap processShoppingEntry relevantEntries


-- c) getDeliveryExpenses


supplyToItem :: Date -> Supply -> Expense
supplyToItem date (name, _, totalPrice) = Item name totalPrice date

deliveryToItems :: Delivery -> [Expense]
deliveryToItems (date, supplies) = map (supplyToItem date) supplies

getDeliveryExpenses :: [Delivery] -> Expense
getDeliveryExpenses deliveries = Category "Food Supplies" (concatMap deliveryToItems deliveries)


-- d) mostPopularDish


countOccurrences :: String -> [String] -> Int
countOccurrences x xs = length (filter (== x) xs)

nubStrings :: [String] -> [String]
nubStrings [] = []
nubStrings (x:xs) = x : nubStrings (filter (/= x) xs)

makeDishCount :: [String] -> String -> (String, Int)
makeDishCount dishes dish = (dish, countOccurrences dish dishes)

hasMaxCount :: Int -> (String, Int) -> Bool
hasMaxCount maxCount (_, c) = c == maxCount

mostPopularDish :: [String] -> [String]
mostPopularDish [] = []
mostPopularDish dishes = map fst (filter (hasMaxCount maxCount) counts)
    where
        uniqueDishes = nubStrings dishes
        counts = map (makeDishCount dishes) uniqueDishes
        maxCount = maximum (map snd counts)


