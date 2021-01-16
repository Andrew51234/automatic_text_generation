import DataFile

--Extra Methods used
occursIn x [] = False
occursIn a (x:xs) = if a == x then True else occursIn a xs

freq _ [] = 0
freq f (x:xs) = if f == x 
				then 1 + freq f xs 
				else freq f xs

-- Splitting the sentences
wordTokenH :: String -> String
wordTokenH "" = ""			   
wordTokenH (x:xs)= if (x == '!'|| x=='#'||x=='$'||x=='%'||x=='&'||x==','||x=='.'||x==':'||x==';'||x=='?'||x=='@'||x=='`'||x=='|'||x=='~')
				 then " "++x : wordTokenH xs
				 else x : wordTokenH xs 
				 
wordToken :: String -> [String]
wordToken "" = []
wordToken x = words a where a = wordTokenH x

-- Generating tokens from lists
wordTokenList :: [String] -> [String]
wordTokenList [] = []
wordTokenList (x:xs) = wordToken x ++ wordTokenList xs

-- Generating unique Bigrams
uniqueBigrams :: [String] -> [(String,String)]
uniqueBigrams [] = []
uniqueBigrams [a] = [] 
uniqueBigrams (a:b:c) = if occursIn (a,b) (uniqueBigrams(b:c)) == False
						 then (a,b) : uniqueBigrams(b:c)
						 else uniqueBigrams (b:c)

-- Generating unique Trigrams
uniqueTrigrams :: [String] -> [(String,String,String)]
uniqueTrigrams [a] = []
uniqueTrigrams [a,b] = []
uniqueTrigrams (a:b:c:d) = if occursIn (a,b,c) (uniqueTrigrams (b:c:d)) == False
						   then (a,b,c) : uniqueTrigrams (b:c:d)
						   else uniqueTrigrams (b:c:d)

-- Counting unique Bigrams
bigrams [] = []
bigrams [a] = []
bigrams (a:b:c) = (a,b) : bigrams (b:c)
			
bigramsFreq :: Num a => [String] -> [((String,String),a)]
bigramsFreq [a] = []
bigramsFreq (a:b:c) = ((a,b),freq (a,b) (bigrams (a:b:c))) : bigramsFreq (b:c)

-- Counting unique Trigrams
trigrams [] = []
trigrams [a] = [] 
trigrams [a,b] = []
trigrams (a:b:c:d) = (a,b,c) : trigrams (b:c:d)

trigramsFreq :: Num a => [String] -> [((String,String,String),a)]
trigramsFreq [a] = [] 
trigramsFreq [a,b] = []
trigramsFreq (a:b:c:d) = ((a,b,c),freq(a,b,c)(trigrams(a:b:c:d))):trigramsFreq(b:c:d)

-- Counting a given item
getFreq :: (Eq a, Num b) => a -> [(a,b)] -> b
getFreq _ [] = 0
getFreq f ((a,b):xs) = if f == a then b else getFreq f xs

-- Calculating the probablity of a word 
generateOneProb :: Fractional a => ((String,String,String),a) ->[((String,String),a)] -> a
generateOneProb _ [] = 0
generateOneProb ((a,b,c),f1)(((x,y),f2):xs) = if (x,y) == (a,b) then f1/f2 
											 else generateOneProb ((a,b,c),f1)xs

-- 
genProbPairs :: Fractional a => [((String,String,String),a)] ->[((String,String),a)] -> [((String,String,String),a)]
genProbPairs _ [] = [] 
genProbPairs [] _ = []
genProbPairs (((a,b,c),f1):xs)(((x,y),f2):ys) = (((a,b,c),generateOneProb ((a,b,c),f1)(((x,y),f2):ys))): genProbPairs xs (((x,y),f2):ys)

-- generate next word
genNextH [a,b] [] = [] -- List of possible words 
genNextH [a,b] (((x,y,z),f):xs) = if (a,b) == (x,y) && f>0.03 then z:genNextH[a,b] xs
								  else genNextH [a,b] xs 

generateNextWord :: (Ord a, Fractional a) => [String] ->[((String,String,String),a)] -> String
generateNextWord [a,b] (((x,y,z),f):xs) = if genNextH [a,b] (((x,y,z),f):xs) == [] then error "Sorry, it is not possible to infer from current database"
										  else p !! randomZeroToX ((length p)-1)  where p = genNextH [a,b] (((x,y,z),f):xs)

--  generate Text 
generateText :: String -> Int -> String
generateText x 0 = x
generateText x n = generateText ( x ++ " " ++ generateNextWord ((l !! (length (l)-2)) : [l !! (length (l)-1)]) (trigramsFreq(wordTokenList docs))) (n-1) where l = wordToken x