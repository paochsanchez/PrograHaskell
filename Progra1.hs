import Data.Char (isSpace, toLower)
import Data.List (sort, words)
import Data.Map (Map, delete, findWithDefault, fromList, insert, member, size, toList)
import qualified Data.Text as T
import System.IO
  ( Handle,
    IOMode (ReadMode, WriteMode),
    hClose,
    hGetLine,
    hIsEOF,
    hPutStrLn,
    openFile,
  )
import Prelude hiding (lookup, null)
import qualified System.Directory as Dir

type Line = [Token]

data Token = Word String | Blank | HypWord String
  deriving (Eq, Show)

-- (a)

string2line :: String -> Line
string2line [] = []
string2line str = [Word x | x <- Data.List.words str]

-- (b)
trimSpaces :: String -> String
trimSpaces = reverse . trimLeading . reverse
  where
    trimLeading [] = []
    trimLeading (x : xs)
      | isSpace x = trimLeading xs
      | otherwise = x : xs

token2string :: Token -> String
token2string t =
  case t of
    (Word w) -> w ++ " "
    Blank -> " "
    (HypWord w) -> w ++ "-"

line2string :: Line -> String
line2string line = trimSpaces (concat [token2string x | x <- line])

-- (c)
tokenLength :: Token -> Int
tokenLength t =
  case t of
    (Word w) -> length w
    Blank -> 1
    (HypWord w) -> (length w) + 1

-- (d)
lineLength :: Line -> Int
lineLength l = length (line2string l)

--(e)
breakLine :: Int -> Line -> (Line, Line)
breakLine len lista_ = breakLine2 len lista_ []

breakLine2 :: Int -> Line -> Line -> (Line, Line)
breakLine2 _ [] listaF = (listaF, [])
breakLine2 0 lista_a listaF = (listaF, lista_a)
breakLine2 len l_a listaF
  | len < tokenLength (head l_a) = (listaF, l_a)
  | otherwise = breakLine2 (len - (tokenLength $head l_a) -1) (tail l_a) (listaF ++ [(head l_a)])

--(f)
mergers :: [String] -> [(String, String)]
mergers [] = []
mergers (_ : []) = []
mergers (x1 : x2 : xs)
  | length xs == 0 = [(x1, x2)]
  | otherwise = [(x1, concat (x2 : xs))] ++ mergers ([x1 ++ x2] ++ xs)

--(g)
type HypMap = Data.Map.Map String [String]

enHyp :: HypMap
enHyp =
  Data.Map.fromList
    [ ("controla", ["con", "tro", "la"]),
      ("futuro", ["fu", "tu", "ro"]),
      ("presente", ["pre", "sen", "te"])
    ]

findListK :: HypMap -> String -> [String]
findListK mapa xs = findWithDefault ([]) xs mapa

tuple2tokens :: String -> [(String, String)] -> [(Token, Token)]
tuple2tokens dots xs
  | length dots /= 0 = [(HypWord x, Word (y ++ dots)) | (x, y) <- xs]
  | otherwise = [(HypWord x, Word y) | (x, y) <- xs]

hyphenate :: HypMap -> Token -> [(Token, Token)]
hyphenate mapa (Word xs) =
  let dots = (filter (== '.') xs)
      string = (filter (/= '.') xs)
   in tuple2tokens dots (mergers $ findListK mapa string)

--(h)
len_hyp :: HypMap -> Int -> [Token] -> [(Token, Token)]
len_hyp mapa len line =
  let a = hyphenate mapa (head line)
   in [(x, y) | (x, y) <- a, tokenLength x < len]

lineBreaks :: HypMap -> Int -> Line -> [(Line, Line)]
lineBreaks mapa len xs
  | length (line_s) == 0 = [breakLine len xs]
  | otherwise = breakLine len xs : [(line_f ++ [x], [y] ++ (tail line_s)) | (x, y) <- len_hyp mapa len_b line_s]
  where
    line_s = snd $ breakLine len xs
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
intercalateList line blanks = (concat [[x] ++ y | (x, y) <- zip line blanks]) ++ lastPart (length blanks) line

lastPart :: Int -> Line -> Line
lastPart x orig = snd $ splitAt x orig

insertBlanks :: Int -> Line -> Line
insertBlanks _ [] = []
insertBlanks 0 line = line
insertBlanks b line
  | length line == 1 = line
  | num == 0 = intercalateList line (listOfBlanks b 1)
  | otherwise = intercalateList line (listOfBlanks b num)
  where
    num = round ((toRational b / toRational (length line -1)) + 0.01)



--(j)
data Flag = NOSEPARAR | SEPARAR | NOAJUSTAR | AJUSTAR
  deriving (Eq, Enum)

separarYalinear :: Int -> Flag -> Flag -> String -> [String]
separarYalinear num SEPARAR AJUSTAR line = s_a num (string2line line) enHyp
separarYalinear num SEPARAR NOAJUSTAR line = s_na num (string2line line) enHyp
separarYalinear num NOSEPARAR AJUSTAR line = ns_a num (string2line line)
separarYalinear num NOSEPARAR NOAJUSTAR line = ns_na num (string2line line)

separarYalinearAux :: Int -> HypMap -> (String, String) -> String -> [String]
separarYalinearAux int h a texto =
  case a of
    ("n", "n") -> separarYalinear int NOSEPARAR NOAJUSTAR texto
    ("n", "s") -> separarYalinear int NOSEPARAR AJUSTAR texto
    ("s", "s") -> separarYalinear_hype int SEPARAR AJUSTAR h texto
    ("s", "n") -> separarYalinear_hype int SEPARAR NOAJUSTAR h texto

separarYalinear_hype :: Int -> Flag -> Flag -> HypMap -> String -> [String]
separarYalinear_hype num SEPARAR AJUSTAR h line = s_a num (string2line line) h
separarYalinear_hype num SEPARAR NOAJUSTAR h line = s_na num (string2line line) h

s_a :: Int -> Line -> HypMap -> [String]
s_a _ [] a = []
s_a num line a = [addBlanks (num - (lineLength x)) x | x <- separeLine num line a]

--(Separar NoAjustar)
s_na :: Int -> Line -> HypMap -> [String]
s_na _ [] a = []
s_na num line a = [line2string x | x <- separeLine num line a]

separeLine :: Int -> Line -> HypMap -> [Line]
separeLine _ [] h = []
separeLine num line h
  | length lista == 1 = [(fst $ head (lista))] ++ separeLine num (snd $ head (lista)) h
  | otherwise =
    [(fst $ head $ selectLine 0 [] [] n (tail lista))]
      ++ separeLine num (snd $ head $ selectLine 0 [] [] n lista) h
  where
    n = num - (lineLength $ fst $ head lista)
    lista = lineBreaks h num line

selectLine :: Int -> [(Line, Line)] -> [(Line, Line)] -> Int -> [(Line, Line)] -> [(Line, Line)]
selectLine _ h [] _ [] = h
selectLine _ _ l _ [] = l
selectLine len h lista num (x : xs)
  | a >= len && a <= num = selectLine a h [x] num xs
  | otherwise = selectLine len h lista num xs
  where
    a = tokenLength $ last $ fst x

--(NoSeparar Ajustar)
ns_a :: Int -> Line -> [String]
ns_a _ [] = []
ns_a num line =
  let tup = breakLine num line
   in [addBlanks (num - (lineLength $ fst tup)) (fst tup)] ++ ns_a num (snd tup)

addBlanks :: Int -> Line -> String
addBlanks 0 line = line2string line
addBlanks num line = line2string (insertBlanks num line)

--(NoSeparar NoAjustar)
ns_na :: Int -> Line -> [String]
ns_na _ [] = []
ns_na num line =
  let tup = breakLine num line
   in [line2string $ fst tup] ++ ns_na num (snd tup)

--Conteo Palabras

type Estado = Map String [String]
-- main crea un Estado vacío e invoca a mainloop
-- el cual recibe el Estado como parámetro
main :: IO ()
main = do
  mainloop (fromList [])

mainloop :: Estado -> IO ()
mainloop estado = do
  putStr ">> "
  inpStr <- getLine
  let tokens = words inpStr
      comando = tokens !! 0
  case comando of
    "load" -> do
      let archivo = tokens !! 1
      inh <- openFile archivo ReadMode
      nuevoestado <- cargar inh estado
      hClose inh
      putStrLn $ "Diccionario cargado (" ++ (show ((Data.Map.size nuevoestado))) ++ " palabras)"
      putStrLn (show nuevoestado)
      mainloop nuevoestado
    "save" -> do
      let archivo = tokens !! 1
      outh <- openFile archivo WriteMode
      descargar outh (sort (toList estado))
      hClose outh
      putStrLn $ "Diccionario guardado (" ++ (show ((Data.Map.size estado))) ++ " palabras)"
      mainloop estado
    "ins" -> do
      let palabra = tokens !! 1
          silabaLista = silabas $ tokens !! 2
          nuevoestado = contar_token estado palabra silabaLista
      putStrLn $ "Palabra " ++ palabra ++ " agregada"
      mainloop nuevoestado
    "show" -> do
      let lista = contar_linea estado
      putStrLn (show lista)
      mainloop estado
    "split" -> do
      let texto = listatexto $ drop 4 tokens
          res = separarYalinearAux (read $ tokens !! 1 :: Int) estado (tokens !! 2, tokens !! 3) texto
      putStrLn $ resToText res
      mainloop estado
    "splitf" -> do
      inh <- openFile (tokens!!4) ReadMode
      inpStr <- hGetLine inh
      hClose inh
      let archivoFinal = if length tokens == 6 then tokens !! 5 else ""
          res = resToText $ separarYalinearAux (read $ tokens !! 1 :: Int) estado (tokens !! 2, tokens !! 3) inpStr
      case archivoFinal of 
        "" ->  putStrLn res
        _ -> do
            fileExists <- Dir.doesFileExist archivoFinal
            if fileExists == False then putStrLn "El archivo donde se ha solicitado no existe"
            else do
              outh <- openFile archivoFinal WriteMode
              hPutStrLn outh res
              hClose outh
              putStrLn res
      mainloop estado
    "borrar" -> do
      let (nuevoestado, salida) = cmd_borrar (tail tokens) estado
      putStrLn salida
      mainloop nuevoestado
    "imp" -> do
      let (nuevoestado, salida) = cmd_imp estado
      putStrLn salida
      mainloop nuevoestado
    "exit" -> do
      putStrLn "Saliendo..."
    _ -> do
      putStrLn $ "Comando desconocido (" ++ comando ++ "): '" ++ inpStr ++ "'"
      mainloop estado

-- función que implementa el comando contar_linea
resToText :: [String] -> String
resToText (x : []) = x
resToText (x : xs) = x ++ "\n" ++ resToText xs

contar_linea :: Estado -> [(String, [String])]
contar_linea estado = toList estado

contar_token :: Estado -> String -> [String] -> Estado
contar_token estado tok c = insert tok c estado

-- función que implementa el comando borrar
cmd_borrar :: [String] -> Estado -> (Estado, String)
cmd_borrar [] estado = (estado, "No se especificó qué borrar")
cmd_borrar (v : _) estado =
  if member v estado
    then (delete v estado, v ++ " borrado")
    else (estado, v ++ " no aparece")

-- función que implementa el comando imp
cmd_imp :: Estado -> (Estado, String)
cmd_imp estado = (estado, show estado)

-- función que implementa leer un archivo línea por línea
-- y contar las palabras de cada línea
cargar :: Handle -> Estado -> IO Estado
cargar inh estado = do
  ineof <- hIsEOF inh
  if ineof
    then return estado
    else do
      inpStr <- hGetLine inh
      let (x : y : _) = (words (map toLower inpStr))
          nuevoestado = contar_token estado x (silabas y)
      cargar inh nuevoestado

silabas :: String -> [String]
silabas string = [(T.unpack x) | x <- (T.splitOn (T.pack "-") (T.pack string))]

-- descargar :: Handle -> [(String,Int)] -> IO ()
descargar :: Handle -> [(String, [String])] -> IO ()
descargar _ [] = return ()
descargar outh ((k, v) : kvs) = do
  hPutStrLn outh $ k ++ " " ++ (listaSilabas v)
  descargar outh kvs

listaSilabas :: [String] -> String
listaSilabas (x : []) = x
listaSilabas (x : xs) = x ++ "-" ++ listaSilabas xs

listatexto :: [String] -> String
listatexto (x : []) = x
listatexto (x : xs) = x ++ " " ++ listatexto xs