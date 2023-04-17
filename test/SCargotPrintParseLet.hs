{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Data.Either
import           Data.List
import           Data.SCargot
import           Data.SCargot.Comments
import           Data.SCargot.LetBind
import           Data.SCargot.Repr
import           Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Exit
import           Test.HUnit
import           Text.Parsec as P
import           Text.Parsec.Text (Parser)
import           Text.Printf ( printf )


main = do
  srcs <- mapM (\n -> (,) n <$> TIO.readFile n) [ "test/small-sample.sexp"
                                                , "test/med-sample.sexp"
                                                , "test/med2-sample.sexp"
                                                , "test/big-sample.sexp"
                                                ]
  counts <- runTestTT $ TestList
            [ TestLabel "basic checks" $ TestList
              [ TestLabel "flat print" $ TestList
                [ TestLabel "flatprint SNil" $ "()" ~=? printSExpr SNil
                , TestLabel "flatprint SAtom" $ "hi" ~=? printSExpr (SAtom (AIdent "hi"))
                , TestLabel "flatprint pair" $ "(hi . world)" ~=?
                  printSExpr (SCons (SAtom (AIdent "hi")) (SAtom (AIdent "world")))
                , TestLabel "flatprint list of 1" $ "(hi)" ~=?
                  printSExpr (SCons (SAtom (AIdent "hi")) SNil)
                , TestLabel "flatprint list of 2" $ "(hi world)" ~=?
                  printSExpr (SCons (SAtom (AIdent "hi"))
                                    (SCons (SAtom (AIdent "world"))
                                           SNil))
                , TestLabel "flatprint list of 2 pairs" $ "((hi . hallo) world . welt)" ~=?
                  printSExpr (SCons (SCons (SAtom (AIdent "hi"))
                                           (SAtom (AIdent "hallo")))
                                    (SCons (SAtom (AIdent "world"))
                                           (SAtom (AIdent "welt"))))
                , TestLabel "flatprint list of 3 ending in a pair" $ "(hi world hallo . welt)" ~=?
                  printSExpr (SCons (SAtom (AIdent "hi"))
                                    (SCons (SAtom (AIdent "world"))
                                           (SCons (SAtom (AIdent "hallo"))
                                                  (SAtom (AIdent "welt")))))
                , TestLabel "flatprint list of 3" $ "(hi world hallo)" ~=?
                  printSExpr (SCons (SAtom (AIdent "hi"))
                                    (SCons (SAtom (AIdent "world"))
                                           (SCons (SAtom (AIdent "hallo"))
                                                  SNil)))
                ]
              , TestLabel "pretty print" $
                let pprintIt = pprintSExpr 40 Swing in TestList
                [ TestLabel "pretty print SNil" $ "()" ~=? pprintIt SNil
                , TestLabel "pretty print SAtom" $ "hi" ~=? pprintIt (SAtom (AIdent "hi"))
                , TestLabel "pretty print pair" $ "(hi . world)" ~=?
                  pprintIt (SCons (SAtom (AIdent "hi")) (SAtom (AIdent "world")))
                , TestLabel "pretty print list of 1" $ "(hi)" ~=?
                  pprintIt (SCons (SAtom (AIdent "hi")) SNil)
                , TestLabel "pretty print list of 2" $ "(hi world)" ~=?
                  pprintIt (SCons (SAtom (AIdent "hi"))
                                  (SCons (SAtom (AIdent "world"))
                                         SNil))
                , TestLabel "pretty print list of 2 pairs" $
                  "((hi . hallo) world . welt)" ~=?
                  pprintIt (SCons (SCons (SAtom (AIdent "hi"))
                                         (SAtom (AIdent "hallo")))
                                  (SCons (SAtom (AIdent "world"))
                                         (SAtom (AIdent "welt"))))
                , TestLabel "pretty print list of 3 ending in a pair" $
                  "(hi world hallo . welt)" ~=?
                  pprintIt (SCons (SAtom (AIdent "hi"))
                                  (SCons (SAtom (AIdent "world"))
                                         (SCons (SAtom (AIdent "hallo"))
                                                (SAtom (AIdent "welt")))))
                , TestLabel "pretty print list of 3" $ "(hi world hallo)" ~=?
                  pprintIt (SCons (SAtom (AIdent "hi"))
                                  (SCons (SAtom (AIdent "world"))
                                         (SCons (SAtom (AIdent "hallo"))
                                                SNil)))
                ]
              ]

            , TestLabel "let bind corner cases" $
              let pprintIt = pprintSExpr 40 Swing
                  widePrintIt = encodeOne (setIndentStrategy (const Swing) $
                                           setIndentAmount 1 $
                                           unconstrainedPrint printAtom)
                  guide = (nativeGuide AIdent (\n _ -> AIdent n))
                          { extractStr = Just . T.unpack . printAtom
                          }
                  samenames = guide
                              { labelMaker = \_ _ -> AIdent "samevar"
                              , minExprSize = 2
                              }
                  peoplenames = guide
                                { labelMaker = \s e -> AIdent
                                               $ case e of
                                                   (SCons (SAtom (AIdent "welt")) _) -> "people"
                                                   _ -> s
                                , minExprSize = 2
                                , weighting = \e c ->
                                              case e of
                                                   (SCons (SAtom (AIdent "welt")) _) -> 1000000
                                                   _ -> weighting guide e c
                                }
                  hinames = peoplenames
                                { labelMaker = \s e -> AIdent
                                               $ case e of
                                                   (SCons (SAtom (AIdent "welt")) _) -> "hi"
                                                   _ -> s
                                }
                  nobindGuide = guide { weighting = \_ _ -> 0 }
                  throws t estrs = catch (evaluate t >> pure False) $
                                   \(ErrorCall e) -> (mapM (assertInString e) estrs >> pure True)
                                   -- \(ErrorCall e) -> assertInString estrs e
                                   -- \(ErrorCall e) -> mapM (assertBool "chk" . isInfixOf e) estrs
                                   -- \(ErrorCall e) -> pure $ and $ fmap (assertBool "chk" . isInfixOf e) estrs
                  assertInString whole part = assertBool ("String \"" <> part <> "\" not in \"" <> whole <> "\"") $
                                              part `isInfixOf` whole
                  checkStrs :: [String] -> String -> IO Bool
                  checkStrs estrs e = mapM (assertBool "chk" . isInfixOf e) estrs >> pure True
                  sexpr f = (SCons (SCons (SAtom (AIdent "hi"))
                                          (SCons (SAtom (AIdent "world"))
                                                 (SCons (SAtom (AIdent "and"))
                                                        (SCons f
                                                               SNil))))
                                   (SCons (SAtom (AIdent "hallo"))
                                          (SCons (SAtom (AIdent "welt"))
                                                 (SCons (SAtom (AIdent "und"))
                                                        (SCons (SAtom (AIdent "leute"))
                                                               SNil)))))
                  normalf = (SAtom (AIdent "people"))
              in TestList
              [ TestLabel "trivial let binding" $
                "((hi world and people)\n  hallo\n  welt\n  und\n  leute)" ~=?
                (pprintIt $ discoverLetBindings nobindGuide $ sexpr normalf)
              , TestLabel "trivial let binding (wide)" $
                "((hi world and people)\n  hallo\n  welt\n  und\n  leute)" ~=?
                (widePrintIt $ discoverLetBindings nobindGuide $ sexpr normalf)

              , TestLabel "duplicate names" $
                TestCase (assertBool "No error on duplicate bindings" =<<
                          throws (pprintIt $ discoverLetBindings samenames $ sexpr normalf)
                         [ "duplicated let variable"
                         , "people"
                         , "   let variable"
                         ])

              , TestLabel "expected bindings for these tests" $
                              "(let\n\
                              \  ((people (welt und leute))\n\
                              \    (var1 (world and last)))\n\
                              \  ((hi var1) hallo people))" ~=?
                              (pprintIt $ discoverLetBindings peoplenames
                                            $ sexpr (SAtom (AIdent "last")))
              , TestLabel "expected bindings for these tests" $
                              "(let\n\
                              \  ((people\n    (welt und leute))\n\
                              \    (var1\n      (world and last)))\n\
                              \  ((hi var1)\n    hallo\n    people))" ~=?
                              (widePrintIt $ discoverLetBindings peoplenames
                                            $ sexpr (SAtom (AIdent "last")))

              , TestLabel "expression ident names collision above bind point" $
                TestCase (assertBool "No expected error on duplicate bindings" =<<
                          throws (pprintIt $ discoverLetBindings hinames
                                           $ sexpr normalf)
                          [ "Too many failed attempts"
                          , "unique let var name"
                          , "hi"
                          ])
              , TestLabel "expression ident names collision below bind point" $
                TestCase (assertBool "No expected error on duplicate bindings" =<<
                          throws (pprintIt $ discoverLetBindings peoplenames
                                           $ sexpr normalf)
                          [ "Too many failed attempts"
                          , "unique let var name"
                          , "people"
                          ])
              , TestLabel "expression string-only collision names" $
                TestCase (assertBool "No expected error on duplicate bindings" =<<
                          throws (pprintIt $ discoverLetBindings peoplenames
                                           $ sexpr (SAtom (AMarker "people")))
                          [ "duplicated let variable"
                          , "people"
                          , "other portion of S-expression"
                          ])
              ]

            , TestLabel "round-trip" $
              let pprintIt = pprintSExpr 40 Swing in TestList $
              concatMap (\t -> map t srcs)
              [ testParsePrint
              ]
            ]
  if errors counts + failures counts > 0
  then exitFailure
  else exitSuccess


testParsePrint :: (String, T.Text) -> Test
testParsePrint (n,s) = TestList
                       [ testParseFlatPrint n s

                       , testParsePPrint 80 Swing n s
                       , testParsePPrint 60 Swing n s
                       , testParsePPrint 40 Swing n s
                       , testParsePPrint 20 Swing n s
                       , testParsePPrint 15 Swing n s
                       , testParsePPrint 10 Swing n s

                       , testParsePPrint 80 Align n s
                       , testParsePPrint 40 Align n s
                       , testParsePPrint 10 Align n s

                       , testParseFlatPrintLetBound False n s
                       , testParseFlatPrintLetBound True n s
                       , testParsePPrintLetBound 80 Align False n s
                       , testParsePPrintLetBound 80 Align True n s
                       , testParsePPrintLetBound 40 Swing False n s
                       , testParsePPrintLetBound 40 Swing True n s
                       , testParsePPrintLetBound 60 Align False n s
                       , testParsePPrintLetBound 60 Align True n s
                       ]


testParseFlatPrint testName src =
    testRoundTrip (testName <> " flat print")
                      (fromRight (error "Failed parse") . parseSExpr)
                      printSExpr
                      stripAllText
                      src

testParseFlatPrintLetBound recursiveBound testName src =
    let guide = (nativeGuide AIdent (\n _ -> AIdent n)) { allowRecursion = recursiveBound }
    in testRoundTrip (testName <> " flat print")
           (discoverLetBindings guide . fromRight (error "Failed parse") . parseSExpr)
           (printSExpr . letExpand getIdent)
           stripAllText
           src

testParsePPrint width indentStyle testName src =
    testRoundTrip (testName <> " pretty print")
                      (fromRight (error "Failed parse") . parseSExpr)
                      (pprintSExpr width indentStyle)
                      stripAllText
                      src

testParsePPrintLetBound width indentStyle recursiveBound testName src =
    let guide = (nativeGuide AIdent (\n _ -> AIdent n)) { allowRecursion = recursiveBound }
    in testRoundTrip (testName <> " pretty print")
           (discoverLetBindings guide . fromRight (error "Failed parse") . parseSExpr)
           (pprintSExpr width indentStyle . letExpand getIdent)
           stripAllText
           src

stripAllText = T.unwords . concatMap T.words . T.lines

testRoundTrip nm there back prep src = TestList
  [ TestLabel (nm <> " round trip") $
    TestCase $ (prep src) @=? (prep $ back $ there src)

  , TestLabel (nm <> " round trip twice") $
    TestCase $ (prep src) @=? (prep $ back $ there $ back $ there src)
  ]


------------------------------------------------------------------------

data FAtom = AIdent String
           | AQuoted String
           | AString String
           | AInt Integer
           | AMarker String  -- ambiguous, but duplicates AIdent in
                             -- output; used for collision tests
           | ABV Int Integer
           deriving (Eq, Show)


string :: String -> SExpr FAtom
string = SAtom . AString

-- | Lift an unquoted identifier.
ident :: String -> SExpr FAtom
ident = SAtom . AIdent

-- | Lift a quoted identifier.
quoted :: String -> SExpr FAtom
quoted = SAtom . AQuoted

-- | Lift an integer.
int :: Integer -> SExpr FAtom
int = SAtom . AInt


printAtom :: FAtom -> T.Text
printAtom a =
  case a of
    AIdent s -> T.pack s
    AMarker s -> T.pack s
    AQuoted s -> T.pack ('\'' : s)
    AString s -> T.pack (show s)
    AInt i -> T.pack (show i)
    ABV w val -> formatBV w val


printSExpr :: SExpr FAtom -> T.Text
printSExpr = encodeOne (flatPrint printAtom)

pprintSExpr :: Int -> Indent -> SExpr FAtom -> T.Text
pprintSExpr w i = encodeOne (setIndentStrategy (const i) $
                             setMaxWidth w $
                             setIndentAmount 1 $
                             basicPrint printAtom)

getIdent :: FAtom -> Maybe String
getIdent (AIdent s) = Just s
getIdent _ = Nothing

formatBV :: Int -> Integer -> T.Text
formatBV w val = T.pack (prefix ++ printf fmt val)
  where
    (prefix, fmt)
      | w `rem` 4 == 0 = ("#x", "%0" ++ show (w `div` 4) ++ "x")
      | otherwise = ("#b", "%0" ++ show w ++ "b")

parseIdent :: Parser String
parseIdent = (:) <$> first <*> P.many rest
  where first = P.letter P.<|> P.oneOf "+-=<>_"
        rest = P.letter P.<|> P.digit P.<|> P.oneOf "+-=<>_"

parseString :: Parser String
parseString = do
  _ <- P.char '"'
  s <- P.many (P.noneOf ['"'])
  _ <- P.char '"'
  return s

parseBV :: Parser (Int, Integer)
parseBV = P.char '#' >> ((P.char 'b' >> parseBin) P.<|> (P.char 'x' >> parseHex))
  where parseBin = P.oneOf "10" >>= \d -> parseBin' (1, if d == '1' then 1 else 0)

        parseBin' :: (Int, Integer) -> Parser (Int, Integer)
        parseBin' (bits, x) = do
          P.optionMaybe (P.oneOf "10") >>= \case
            Just d -> parseBin' (bits + 1, x * 2 + (if d == '1' then 1 else 0))
            Nothing -> return (bits, x)

        parseHex = (\s -> (length s * 4, read ("0x" ++ s))) <$> P.many1 P.hexDigit

parseAtom :: Parser FAtom
parseAtom
  =   AIdent      <$> parseIdent
  P.<|> AQuoted     <$> (P.char '\'' >> parseIdent)
  P.<|> AString     <$> parseString
  P.<|> AInt . read <$> P.many1 P.digit
  P.<|> uncurry ABV <$> parseBV

parserLL :: SExprParser FAtom (SExpr FAtom)
parserLL = withLispComments (mkParser parseAtom)

parseSExpr :: T.Text -> Either String (SExpr FAtom)
parseSExpr = decodeOne parserLL
