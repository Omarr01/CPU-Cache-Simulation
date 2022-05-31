
data Item a = It Tag (Data a) Bool Int 
	      | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)
data Output a = Out (a, Int) 
	       |NoOutput deriving (Show, Eq)



convertBinToDec x = convertHelper x 0

convertHelper x y = if x ==0 then 0
		    else (((mod x 10) * (2 ^ y)) + convertHelper (div x 10) (y+1))

replaceIthItem item [] index  = []
replaceIthItem item (x:xs) 0  = item:xs
replaceIthItem item (x:xs) index = x:replaceIthItem item xs (index-1)


splitEvery n l = splitEveryHelp n l 0 [] []


splitEveryHelp n [] count acc res = res ++ [acc]

splitEveryHelp n (x:xs) count acc [] | n== count = splitEveryHelp n (x:xs) 0 [] [acc]
				     | otherwise = splitEveryHelp n xs (count+1) (acc ++[x]) []
 

splitEveryHelp n (x:xs) count acc res| n== count = splitEveryHelp n (x:xs) 0 [] (res ++ [acc])
				     | otherwise = splitEveryHelp n xs (count+1) (acc ++[x]) res


logBase2 x = logBase 2 x

getNumBits x "fullyAssoc" l = 0
getNumBits x "setAssoc" l =  round (logBase2 x)
getNumBits x "directMap" l = round (logBase2 (fromIntegral (length l)))

fillZeros x 0 = x
--fillZeros x 1 =  "0"++x
fillZeros x y = fillZeros ("0"++x) (y-1)
---------------------------------------------------------------


square x = x * x

pow x 0 = 1
pow x 1 = x
pow x y | even y = square (pow x (div y 2))
		| otherwise = x * pow x (y-1)


getDataFromCache stringAddress oldCache "setAssoc" bitsNum =  getDataFromCache (show m) newCache2 "fullyAssoc" 0
									where { n = div (length oldCache) (2^bitsNum);
									newCache = splitEvery n oldCache ;
									(m,n1) = convertAddress (read stringAddress :: Int) bitsNum "setAssoc" ;
									newCache2 = newCache !! (convertBinToDec n1) }

getDataFromCache stringAdress [] "fullyAssoc" bitsNum = NoOutput
getDataFromCache stringAdress ((It (T x) (D y) True _):xs) "fullyAssoc" bitsNum | z==stringAdress = Out ( y , bitsNum) 
									        | otherwise = getDataFromCache stringAdress xs "fullyAssoc" (bitsNum+1)
									where 
										z = fillZeros (show x) (length stringAdress- length (show x))  
getDataFromCache stringAdress ((It _ _ False _):xs) "fullyAssoc" bitsNum = getDataFromCache stringAdress xs "fullyAssoc" (bitsNum+1)
 
getDataFromCache address [] "directMap" bitsnum = NoOutput


getDataFromCache address cache "directMap" bitsnum = getDataFromCacheHelp newAdd cache "directMap" bitsnum 0 indx
											where {  newAdd = (splitEvery ((length address)-bitsnum) address) !! 0 ;   
												tmp = read ((splitEvery ((length address)-bitsnum) address) !! 1) :: Int;
												indx = convertBinToDec tmp
												}

getDataFromCacheHelp newAdd [] "directMap" bitsnum hopsNum indx = NoOutput
 
getDataFromCacheHelp newAdd ((It (T x) (D y) True z):xs) "directMap" bitsnum hopsNum indx | indx /= 0 = getDataFromCacheHelp newAdd xs "directMap" bitsnum (hopsNum+1) (indx-1)
											   
getDataFromCacheHelp newAdd ((It (T x) (D y) False z):xs) "directMap" bitsnum hopsNum indx | indx /= 0 = getDataFromCacheHelp newAdd xs "directMap" bitsnum hopsNum (indx-1)

getDataFromCacheHelp newAdd ((It (T x) (D y) True z):xs) "directMap" bitsnum hopsNum indx | indx == 0 && newAdd==newT = Out(y,hopsNum) 
											where newT = fillZeros (show x) ((length newAdd)-(length (show x)))

getDataFromCacheHelp newAdd ((It (T x) (D y) True z):xs) "directMap" bitsnum hopsNum indx | indx == 0 && newAdd/=newT = NoOutput
											where newT = fillZeros (show x) ((length newAdd)-(length (show x)))

getDataFromCacheHelp newAdd ((It (T x) (D y) False z):xs) "directMap" bitsnum hopsNum indx | indx == 0 = NoOutput


convertAddress binAddress bitsNum "directMap" = (div binAddress (pow 10 bitsNum), mod binAddress (pow 10 bitsNum))
convertAddress binAddress bitsNum "fullyAssoc" = (binAddress, 0)
convertAddress binAddress bitsNum "setAssoc" = (div binAddress (pow 10 bitsNum), mod binAddress (pow 10 bitsNum))


containInvalid [] l = (-1)
containInvalid ((It _ _ False _):xs) l= (l+1)
containInvalid ((It _ _ True _):xs) l = (containInvalid xs (l+1))


getFifo ((It _ _ _ f):xs) = getFifohelp xs f 0 0
getFifohelp [] _ _ x = x
getFifohelp ((It _ _ _ m):xs) f curr x | m>= f = getFifohelp xs m (curr +1) (curr+1)
				       | otherwise = getFifohelp xs f (curr+1) x


changeCache [] location newDataStr tag = []
changeCache ((It a b c f):xs) location newDataStr tag | location==0 = (It (T tag) (D newDataStr) True 0 ):(changeCache xs (-10) newDataStr tag)
						      | c == True =   (It a b c (f+1)):(changeCache xs (location -1) newDataStr tag)
						      | otherwise =    (It a b c f):(changeCache xs (location -1) newDataStr tag)



replaceInCacheDirectHelper tag idx memory [] "directMap" decIdx newDataStr bitsNum = []					
replaceInCacheDirectHelper tag idx memory (x:xs) "directMap" decIdx newDataStr bitsNum | decIdx /= 0 = x : replaceInCacheDirectHelper tag idx memory xs "directMap" (decIdx-1) newDataStr bitsNum
																			   | otherwise = (It (T tag) (D newDataStr) True 0) : xs
																			   
replaceInCache tag idx memory oldCache "directMap" bitsNum = (newDataStr,replaceInCacheDirectHelper tag idx memory oldCache "directMap" (convertBinToDec(idx)) newDataStr bitsNum)
												where { locMem = convertBinToDec(read (show tag ++ y) :: Int);
												x = show idx;
												y = fillZeros x (bitsNum - length x);
												newDataStr = memory !! locMem }


replaceInCache tag idx memory oldCache "fullyAssoc" bitsNum | containInvalid oldCache (-1) /= -1 = replaceInCachehelp tag idx memory oldCache "fullyAssoc" bitsNum newDataStr (containInvalid oldCache (-1))
												where { locMem = convertBinToDec(tag);
												newDataStr = memory !! locMem }
replaceInCache tag idx memory oldCache "fullyAssoc" bitsNum = replaceInCachehelp tag idx memory oldCache "fullyAssoc" bitsNum newDataStr (getFifo oldCache)
												where { locMem = convertBinToDec(tag);
												newDataStr = memory !! locMem }

replaceInCache tag idx memory oldCache "setAssoc" bitsNum = (a, (delist x))
								where
								 x = replaceIthItem z cache (convertBinToDec idx)
								 z =  tagcorrect d tag
								 (a,d) = (replaceInCache newtag idx memory newCache "fullyAssoc" bitsNum)
								 kk = show idx
								 mm = fillZeros kk (bitsNum - length kk)
								 newtag = read ( show tag ++ mm) :: Int
								 newCache = cache !! (convertBinToDec idx)
							         cache = (splitEvery n oldCache)
							         n = div (length oldCache) (2 ^ bitsNum)

delist [] = []
delist ((s:ss):xs) =  (s:ss)++ delist(xs)

tagcorrect ((It _ b True 0):xs) tag = ((It (T tag) b True 0):xs)
tagcorrect (x:xs) tag = (x:(tagcorrect xs tag))




replaceInCachehelp tag idx memory oldCache "fullyAssoc" bitsNum newDataStr location  = (newDataStr, (changeCache oldCache location newDataStr tag))
												 

-------------------------------------------------------------
getData stringAddress cache memory cacheType bitsNum
	| x == NoOutput = replaceInCache tag index memory cache cacheType bitsNum
	| otherwise = (getX x, cache)
		where
		x = getDataFromCache stringAddress cache cacheType bitsNum
		address = read stringAddress:: Int
		(tag, index) = convertAddress address bitsNum cacheType
		getX (Out (d, _)) = d

runProgram [] cache _ _ _ = ([], cache)
runProgram (addr: xs) cache memory cacheType numOfSets =
	((d:prevData), finalCache)
		where
		 bitsNum = round(logBase2 numOfSets)
 		 (d, updatedCache) = getData addr cache memory cacheType bitsNum
		 (prevData, finalCache) = runProgram xs updatedCache memory cacheType numOfSets

-----------------------------------------------------------------------------------------

								



















