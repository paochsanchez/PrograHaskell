
import Data.List (words ) 
import Data.Char (isSpace )
import Data.Map (findWithDefault, fromList,  Map )

type Line = [Token]
data Token = Word String | Blank | HypWord String
             deriving (Eq,Show)

-- (a)

string2line :: String -> Line
string2line [] = []
string2line str =[Word x | x <-Data.List.words str]

-- (b)
trimSpaces :: String -> String
trimSpaces = reverse . trimLeading . reverse
  where
    trimLeading [] = []
    trimLeading (x:xs) 
      | isSpace x = trimLeading xs 
      | otherwise = x:xs

token2string :: Token ->String
token2string t =
    case t of
        (Word w) ->w++" "
        Blank ->" "
        (HypWord w) -> w ++ "-"

line2string :: Line  -> String
line2string line = trimSpaces(concat[token2string x | x <- line]) 


-- (c)
tokenLength :: Token -> Int
tokenLength t =
    case t of
        (Word w) -> length w
        Blank -> 1
        (HypWord w) -> (length w )+ 1

-- (d)
lineLength:: Line -> Int
lineLength l = length(line2string l)

--(e)
breakLine :: Int -> Line -> (Line,Line)
breakLine len lista_= breakLine2 len lista_ []

breakLine2 :: Int -> Line -> Line -> ( Line,Line)
breakLine2 _ [] listaF = ( listaF,[])
breakLine2 0 lista_a listaF = (listaF,lista_a)
breakLine2 len l_a listaF  
  | len < tokenLength (head l_a )= ( listaF,l_a )
  | otherwise = breakLine2 (len - (tokenLength $head l_a)-1) (tail l_a) (listaF ++[(head l_a)]) 


--(f)
mergers :: [String] -> [(String,String)]
mergers [] = []
mergers (_:[]) =[] 
mergers (x1:x2:xs)  
  | length xs == 0 = [(x1,x2)]
  | otherwise = [(x1,concat(x2:xs))] ++ mergers([x1++x2]++xs)

--(g)
type HypMap = Data.Map.Map String [String]

enHyp :: HypMap
enHyp = Data.Map.fromList [ ("controla",["con","tro","la"]), 
                            ("futuro",["fu","tu","ro"]),
                            ("presente",["pre","sen","te"])]


findListK :: HypMap -> String-> [String]
findListK mapa xs = findWithDefault ([]) xs mapa

tuple2tokens :: String -> [(String,String)] -> [(Token, Token)]
tuple2tokens dots xs
  | length dots /=0 = [(HypWord x,Word (y++dots)) | (x,y)<-xs]
  | otherwise = [(HypWord x,Word  y) | (x,y)<-xs]


hyphenate :: HypMap -> Token ->[(Token, Token)]
hyphenate mapa (Word xs) = 
  let dots = (filter (=='.') xs)
      string = (filter (/='.') xs)
  in tuple2tokens dots ( mergers $ findListK mapa string)

--(h)
len_hyp :: HypMap -> Int ->  [Token] ->[(Token, Token)]
len_hyp mapa len line =  
  let a = hyphenate mapa (head line)
  in [(x,y) | (x,y) <-  a, tokenLength x < len ]

lineBreaks :: HypMap -> Int -> Line -> [(Line,Line)]
lineBreaks mapa len xs
  | length( line_s ) == 0 = [ breakLine len xs ]
  | otherwise = breakLine len xs : [(line_f ++ [x], [y] ++(tail line_s) ) |  (x,y) <-len_hyp mapa len_b line_s]
  where line_s = snd $ breakLine len xs
        line_f = fst $ breakLine len xs
        len_b = len - (lineLength line_f)


--(i)
split :: Int -> [Token] -> [Line]
split _ [] = []
split 0 _ = []
split length lista = [take length lista] ++ split length (drop length lista)

listOfBlanks :: Int -> Int -> [Line]
listOfBlanks blanks length = (split length $take blanks (repeat Blank))

intercalateList :: Line -> [Line] -> Line
intercalateList line blanks =(concat [ [x]++y |(x, y) <- zip line blanks] ) ++ lastPart (length blanks) line

lastPart :: Int -> Line -> Line
lastPart x orig =  snd $ splitAt x orig

insertBlanks :: Int -> Line ->  Line
insertBlanks _ [] = []
insertBlanks 0 line = line
insertBlanks b line 
  | length line == 1 = line
  | num == 0 = intercalateList line (listOfBlanks b 1)
  | otherwise = intercalateList line (listOfBlanks b num)
  where num = round ((toRational b / toRational (length line -1)) +0.01) 

--(j)
data Flag = NOSEPARAR | SEPARAR | NOAJUSTAR | AJUSTAR 
  deriving ( Eq ,  Enum )  
  
separarYalinear:: Int -> Flag -> Flag -> String -> [String]
separarYalinear num SEPARAR AJUSTAR line = s_a num (string2line line)
separarYalinear num SEPARAR NOAJUSTAR line = s_na num (string2line line)
separarYalinear num NOSEPARAR AJUSTAR line = ns_a num (string2line line)
separarYalinear num NOSEPARAR NOAJUSTAR line = ns_na num (string2line line)

s_a :: Int -> Line ->  [String]
s_a _ [] = []
s_a num line = [ addBlanks (num-(lineLength x)) x|x <-separeLine num line]
  

--(Separar NoAjustar)
s_na :: Int -> Line -> [String]
s_na _ [] = []
s_na num line = [line2string x| x<- separeLine num line]

separeLine :: Int -> Line -> [Line]
separeLine _ [] = []
separeLine num line 
  | length lista == 1 =  [(fst $ head (lista))] ++  separeLine num (snd $ head (lista))
  | otherwise = [(fst $ head $ selectLine 0 [] [] n (tail lista))]  ++ separeLine num (snd $ head $ selectLine 0 [] [] n lista) 
    where n = num - (lineLength $ fst $ head lista)
          lista = lineBreaks enHyp num line

selectLine :: Int ->[(Line, Line)]-> [(Line, Line)] -> Int -> [(Line, Line)] ->[(Line, Line)]
selectLine _ h [] _ [] = h
selectLine _ _ l _ [] = l
selectLine len h lista num (x:xs) 
  | a >= len && a <=num = selectLine a h [x] num xs
  | otherwise = selectLine len h lista num xs
    where a = tokenLength $ last $ fst x

--(NoSeparar Ajustar)
ns_a :: Int -> Line -> [String]
ns_a _ [] =[]
ns_a num line = 
  let tup = breakLine num line
  in [addBlanks (num-(lineLength $ fst tup)) (fst tup)] ++ ns_a num (snd tup)

addBlanks :: Int -> Line -> String
addBlanks 0 line = line2string line
addBlanks num line = line2string (insertBlanks num line)

--(NoSeparar NoAjustar)
ns_na :: Int -> Line -> [String]
ns_na _ [] =[]
ns_na num line = 
  let tup = breakLine num line
  in [line2string $ fst tup] ++ ns_na num (snd tup)
