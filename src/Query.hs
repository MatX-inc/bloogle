{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
-- otherwise it takes a lot of memory to compile on the haskell.org server
{-# OPTIONS_GHC -O0 #-}

module Query
  ( Query (..),
    isQueryName,
    isQueryType,
    isQueryScope,
    parseQuery,
    renderQuery,
    query_test,
  )
where

import Control.Applicative
import Data.Char
import Data.Generics.Uniplate.Data
import Data.List
import Data.List.Extra
import Data.Maybe
import Data.Monoid
import General.Util
import Language.Haskell.Exts
import Text.Blaze
import qualified Text.Blaze.XHtml5 as H
import Prelude

---------------------------------------------------------------------
-- DATA TYPE

data Query
  = QueryName {fromQueryName :: String}
  | QueryType {fromQueryType :: Type ()}
  | QueryScope {scopeInclude :: Bool, scopeCategory :: String, scopeValue :: String}
  | QueryNone String -- part of the query that is ignored
  deriving (Show, Eq)

isQueryName, isQueryType, isQueryScope :: Query -> Bool
isQueryName QueryName {} = True; isQueryName _ = False
isQueryType QueryType {} = True; isQueryType _ = False
isQueryScope QueryScope {} = True; isQueryScope _ = False

renderQuery :: [Query] -> Markup
renderQuery [] = H.i "No query"
renderQuery xs = do
  string $
    unwords $
      [x | QueryName x <- xs]
        ++ [":: " ++ pretty x | QueryType x <- xs]
        ++ [['-' | not scopeInclude] ++ scopeCategory ++ ":" ++ scopeValue | QueryScope {..} <- xs]
  mconcat [" " <> H.del (string x) | QueryNone x <- xs]

---------------------------------------------------------------------
-- PARSER

parseQuery :: String -> [Query]
parseQuery x = map QueryName nam ++ map QueryType (maybeToList typ) ++ scp
  where
    (scp, rest) = scope_ $ lexer x
    (nam, typ) = divide rest

openBrackets :: [[Char]]
openBrackets = ["(#", "[:", "(", "["]

shutBrackets :: [[Char]]
shutBrackets = ["#)", ":]", ")", "]"]

isBracket :: [Char] -> Bool
isBracket x = x `elem` (openBrackets ++ shutBrackets)

isBracketPair :: [Char] -> Bool
isBracketPair x = x `elem` zipWith (++) openBrackets shutBrackets

isSym :: Char -> Bool
isSym x = ((isSymbol x || isPunctuation x) && x `notElem` special) || x `elem` ascSymbol
  where
    special = "(),;[]`{}\"'" :: String
    ascSymbol = "!#$%&*+./<=>?@\\^|-~" :: String

isSyms :: [Char] -> Bool
isSyms xs | isBracket xs || isBracketPair xs = False
isSyms (x : _xs) = isSym x
isSyms [] = False

-- | Split into small lexical chunks.
--
-- > "Data.Map.(!)" ==> ["Data",".","Map",".","(","!",")"]
lexer :: String -> [String]
lexer ('(' : ',' : xs) | (a, ')' : b) <- span (== ',') xs = ("(," ++ a ++ ")") : lexer b
lexer x | Just s <- (bs !!) <$> findIndex (`isPrefixOf` x) bs = s : lexer (drop (length s) x)
  where
    bs = zipWith (++) openBrackets shutBrackets ++ openBrackets ++ shutBrackets
lexer (x : xs)
  | isSpace x = " " : lexer (trimStart xs)
  | isAlpha x || x == '_' =
      let (a, b) = span (\c -> isAlphaNum c || c `elem` ("_'#-" :: String)) xs
          (a1, a2) = spanEnd (== '-') a
       in (x : a1) : lexer (a2 ++ b)
  | isSym x = let (a, b) = span isSym xs in (x : a) : lexer b
  | x == ',' = "," : lexer xs
  | otherwise = lexer xs -- drop invalid bits
lexer [] = []

-- | Find and extract the scope annotations.
--
-- > +package
-- > +module
-- > name.bar
-- > name.++ name.(++) (name.++)
-- > +foo -foo
-- > +scope:foo -scope:foo scope:foo
scope_ :: [String] -> ([Query], [String])
scope_ xs = case xs of
  (readPM -> Just pm) : (readCat -> Just cat) : ":" : (readMod -> Just (modl, rest)) -> add pm cat modl rest
  (readPM -> Just _pm) : (readCat -> Just cat) : ":-" : (readMod -> Just (modl, rest)) -> add False cat modl rest
  (readPM -> Just pm) : (readMod -> Just (modl, rest)) -> add_ pm modl rest
  (readCat -> Just cat) : ":" : (readMod -> Just (modl, rest)) -> add True cat modl rest
  (readCat -> Just cat) : ":." : (readMod -> Just (modl, rest)) -> add True cat ('.' : modl) rest
  (readCat -> Just cat) : ":-" : (readMod -> Just (modl, rest)) -> add False cat modl rest
  (readCat -> Just cat) : ":-." : (readMod -> Just (modl, rest)) -> add False cat ('.' : modl) rest
  "(" : (readDots -> Just (scp, x : ")" : rest)) -> out ["(", x, ")"] $ add_ True scp rest
  (readDots -> Just (scp, rest)) -> add_ True scp rest
  "(" : "." : (readDots -> Just (scp, x : ")" : rest)) -> out ["(", x, ")"] $ add_ True ('.' : scp) rest
  "." : (readDots -> Just (scp, rest)) -> add_ True ('.' : scp) rest
  x : xs' -> out [x] $ scope_ xs'
  [] -> ([], [])
  where
    out xs' (a, b) = (a, xs' ++ b)
    add a b c rest = let (x, y) = scope_ rest in (QueryScope a b c : x, y)
    add_ a c rest = add a b c rest
      where
        b = if '.' `elem` c || any isUpper (take 1 c) then "module" else "package"

    readPM x = case x of "+" -> Just True; "-" -> Just False; _ -> Nothing

    readCat x
      | isAlpha1 x = Just x
      | otherwise = Nothing

    readMod (x : xs') | isAlpha1 x = Just $ case xs' of
      "." : ys | Just (a, b) <- readMod ys -> (x ++ "." ++ a, b)
      "." : [] -> (x ++ ".", [])
      "." : " " : ys -> (x ++ ".", " " : ys)
      _ -> (x, xs')
    readMod _ = Nothing

    readDots (x : xs') | isAlpha1 x = case xs' of
      "." : ys | Just (a, b) <- readDots ys -> Just (x ++ "." ++ a, b)
      ('.' : y) : ys -> Just (x, [y | y /= ""] ++ ys)
      _ -> Nothing
    readDots _ = Nothing

-- | If everything is a name, or everything is a symbol, then you only have names.
divide :: [String] -> ([String], Maybe (Type ()))
divide xs
  | all isAlpha1 ns = (ns, Nothing)
  | all isSyms ns = (ns, Nothing)
  | length ns == 1 = (ns, Nothing)
  | otherwise = case break (== "::") xs of
      (nam, _ : rest) -> (names_ nam, typeSig_ rest)
      _ -> ([], typeSig_ xs)
  where
    ns = names_ xs

-- | Ignore brackets around symbols, and try to deal with tuple names.
names_ :: [String] -> [String]
names_ ("(" : x : ")" : xs) = [x | x /= " "] ++ names_ xs
names_ ["(", x] = [x]
names_ (x : xs) = [x | x /= " "] ++ names_ xs
names_ [] = []

typeSig_ :: [String] -> Maybe (Type ())
typeSig_ xs = case parseTypeWithMode parseMode $ unwords $ fixup $ filter (not . all isSpace) xs of
  ParseOk x -> Just $ transformBi (\v -> if v == Ident () "__" then Ident () "_" else v) $ fmap (const ()) x
  _ -> Nothing
  where
    fixup = underscore . closeBracket . completeFunc . completeArrow

    completeArrow (unsnoc -> Just (a, b)) | b `elem` ["-", "="] = snoc a (b ++ ">")
    completeArrow x = x

    completeFunc (unsnoc -> Just (a, b)) | b `elem` ["->", "=>"] = a ++ [b, "_"]
    completeFunc x = x

    closeBracket xs' = xs' ++ foldl f [] xs'
      where
        f stack x | Just c <- lookup x (zip openBrackets shutBrackets) = c : stack
        f (s : tack) x | x == s = tack
        f stack _x = stack

    underscore = replace ["_"] ["__"]

query_test :: IO ()
query_test = testing "Query.parseQuery" $ do
  let want s p (_bad, q) = (["missing " ++ s | not $ any p q], filter (not . p) q)
      wantEq v = want (show v) (== v)
      qname = wantEq . QueryName
      scope b c v = wantEq $ QueryScope b c v
      typ = wantEq . QueryType . fmap (const ()) . fromParseResult . parseTypeWithMode parseMode
      typpp x = want ("type " ++ x) (\v -> case v of QueryType s -> pretty s == x; _ -> False)
  let infixl 0 ===
      a === f
        | bad@(_ : _) <- fst $ f ([], q) = error $ show (a, q, bad :: [String])
        | otherwise = putChar '.'
        where
          q = parseQuery a

  "" === id
  "map" === qname "map"
  "#" === qname "#"
  "c#" === qname "c#"
  "-" === qname "-"
  "/" === qname "/"
  "->" === qname "->"
  "foldl'" === qname "foldl'"
  "fold'l" === qname "fold'l"
  "Int#" === qname "Int#"
  "concat map" === qname "concat" . qname "map"
  "a -> b" === typ "a -> b"
  "a->b" === typ "a -> b"
  "(a b)" === typ "(a b)"
  "map :: a -> b" === typ "a -> b"
  "+Data.Map map" === scope True "module" "Data.Map" . qname "map"
  "a -> b package:foo" === scope True "package" "foo" . typ "a -> b"
  "a -> b package:foo-bar" === scope True "package" "foo-bar" . typ "a -> b"
  "Data.Map.map" === scope True "module" "Data.Map" . qname "map"
  "[a]" === typ "[a]"
  "++" === qname "++"
  "(++)" === qname "++"
  ":+:" === qname ":+:"
  "bytestring-cvs +hackage" === scope True "package" "hackage" . qname "bytestring-cvs"
  "m => c" === typ "m => c"
  "[b ()" === typ "[b ()]"
  "[b (" === typ "[b ()]"
  "_ -> a" === typpp "_ -> a"
  "(a -> b) ->" === typpp "(a -> b) -> _"
  "(a -> b) -" === typpp "(a -> b) -> _"
  "Monad m => " === typpp "Monad m => _"
  "map is:exact" === qname "map" . scope True "is" "exact"
  "sort set:hackage" === qname "sort" . scope True "set" "hackage"
  "sort -set:hackage" === qname "sort" . scope False "set" "hackage"
  "sort set:-hackage" === qname "sort" . scope False "set" "hackage"
  "sort -set:-hackage" === qname "sort" . scope False "set" "hackage"
  "package:bytestring-csv" === scope True "package" "bytestring-csv"
  "(>>=)" === qname ">>="
  "(>>=" === qname ">>="
  ">>=" === qname ">>="
  "Control.Monad.mplus" === qname "mplus" . scope True "module" "Control.Monad"
  "Control.Monad.>>=" === qname ">>=" . scope True "module" "Control.Monad"
  "Control.Monad.(>>=)" === qname ">>=" . scope True "module" "Control.Monad"
  "(Control.Monad.>>=)" === qname ">>=" . scope True "module" "Control.Monad"
  "Control.Monad.(>>=" === qname ">>=" . scope True "module" "Control.Monad"
  "(Control.Monad.>>=" === qname ">>=" . scope True "module" "Control.Monad"
  "foo.bar" === qname "bar" . scope True "package" "foo"
  "insert module:.Map" === qname "insert" . scope True "module" ".Map"
  "insert module:Map." === qname "insert" . scope True "module" "Map."
  "insert module:.Map." === qname "insert" . scope True "module" ".Map."
  ".Map.insert" === qname "insert" . scope True "module" ".Map"
  ".Map." === scope True "module" ".Map"
  --  FIXME: ".Map" === scope True "module" ".Map" -- probably should work, but really needs to rewrite a fair bit
  "(.Monad.>>=" === qname ">>=" . scope True "module" ".Monad"
  --  FIXME: "author:Taylor-M.-Hedberg" === scope True "author" "Taylor-M.-Hedberg"
  "author:Bryan-O'Sullivan" === scope True "author" "Bryan-O'Sullivan"
  "\8801" === qname "\8801"
  "( )" === id -- FIXME: Should probably be ()
