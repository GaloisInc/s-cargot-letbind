-- | This is a helper module that enables the use of let-bound
-- variables in your S-expression.


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.SCargot.LetBind
    ( -- $intro
      -- * Automatically finding let bindings
      discoverLetBindings
    , DiscoveryGuide(..)
    , nativeGuide
      -- * Expanding
    , letExpand
    )
    where

import           Control.Applicative
import qualified Data.Foldable as F
import           Data.Function (on)
import           Data.List ( sortBy, intercalate )
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import           Data.Monoid
import           Data.SCargot.Repr
import           Data.String
import           Data.Traversable ( mapAccumL )
import           Data.Tuple


-- | This object provides guidance to the 'discoverLetBindings'
-- function, establishing various parameters for the discovery
-- process.
data DiscoveryGuide a str = Guide
    { maxLetBinds :: Int -> Int
      -- ^ Maximum number of let bindings to generate.  Given the
      -- total number discovered as input to allow the maximum number
      -- to be intelligently determined.

    , minExprSize :: Int
      -- ^ Minimum sexpr size to be considered for a let variable
      -- binding.  Expressions shorter than this will not be
      -- let-bound.

    , allowRecursion :: Bool
      -- ^ Allow rec bindings, or just direct let bindings?

    , weighting :: SExpr a -> Int -> Int
      -- ^ Given an SExpr sub-expression and the count of occurrences
      -- of that sub-expression, return a weighting value that is used
      -- for sorting the discovered let bindings to choose the most
      -- weighty 'maxLetBinds' for substitution.  A sub-expression
      -- with a weight of zero will be ignored (i.e. not let-bound);
      -- one with a weight of 1000000 or more will always be bound.

    , letMaker :: (IsString str) => str -> a
      -- ^ Called to generate the "let" statement token itself.

    , labelMaker :: (IsString str, Monoid str) => str -> SExpr a -> a
      -- ^ Called to generate the binding variable name token given
      -- the name. Passed the suggested name that will be used for
      -- this binding and also the sub-expression that will be
      -- referenced.  The return will be placed in an SAtom and used
      -- as the variable to reference the bound sub-expression.

    , extractStr :: (IsString str) => a -> Maybe str
    -- ^ Called to extract a string value.  The returned string should
    -- be the string that will be written when the enclosing
    -- S-expression is printed.  This is used to verify that the
    -- variables names extracted for let-binding are unique with
    -- respect to all other printed references.  A return value of
    -- Nothing permits continuation without uniqueness verification,
    -- but at the risk that variable names will be captured in the
    -- result.
    }


-- | Returns a default 'DiscoveryGuide'.
nativeGuide :: (str -> a) -> (str -> SExpr a -> a) -> DiscoveryGuide a str
nativeGuide letMk labelMk = Guide { maxLetBinds = const 8
                                  , minExprSize = 5
                                  , allowRecursion = False
                                  , weighting = defaultWeighting
                                  , letMaker = letMk
                                  , labelMaker = labelMk
                                  , extractStr = const Nothing
                                  }


-- | Provides a default weighting function for evaluating
-- S-expressions.  The general algorithm here is:
--
--   1. S-expressions beginning with an atom on the left probably use
--      that atom as a function name, and are therefore good
--      candidates for clarity.  These sub-expressions get a baseline
--      value of 100.
--
--   2. The frequency of occurrence matters.  It is 4 times more
--      important than the size of the sub-expression.
--
--   3. Bigger sub-expressions are better candidates than smaller
--      sub-expressions.
defaultWeighting :: SExpr a -> Int -> Int
defaultWeighting subexpr cnt =
    let h = F.length subexpr
        baseline = case subexpr of
                     (SCons (SAtom _) _) -> 100
                     _ -> 0
    in (baseline + h + (cnt * 4))


-- | Called to convert a plain S-expression into one with let-bound
-- variables.  The let bindings are "discovered" with the assistance
-- of the guide.
discoverLetBindings :: (Monoid str, IsString str, Eq str, Eq a, Show a) =>
                        DiscoveryGuide a str -> SExpr a -> SExpr a
discoverLetBindings guide inp =
    let (inpMap,annotInp) = explore guide startingLoc inp
        locs = bestBindings guide annotInp $ points inpMap
        lbn = assignLBNames guide inp locs
        varNameCollisions = verifyNamesUnique guide lbn inp
        letPart = SAtom $ letMaker guide "let"
        (lbvdefs, subsInp) = substLBRefs guide lbn annotInp
    in if null varNameCollisions
       then if null lbn
            then inp
            else SCons letPart $ SCons lbvdefs (SCons subsInp SNil)
       else error $ verificationFailureReport locs varNameCollisions

{- $intro

This module allows let bindings to be introduced into the S-Expression
syntax.

For example, instead of:

>    (concat (if (enabled x) (+ (width x) (width y)) (width y))
>            " meters")

this can be re-written with let bindings:

>    (let ((wy    (width y))
>          (wboth (+ (width x) wy))
>          (wide  (if (enabled x) wboth wy))
>         )
>      (concat wide " meters"))

As S-expressions grow larger, let-binding can help readability for
those expressions.  This module provides the 'discoverLetBindings'
function that will convert an S-expression into one containing
let-bound variables, and the inverse function 'letExpand' which will
expand let-bound variables back into the expression.

>    id = letExpand . discoverLetBindings guide

The typical use is to add let bindings before serializing to
disk, and then expand the bindings after reading from the disk but
before passing to other processing; this process allows the
application using the S-Expressions to be unaware of the let-binding
compression, although it does not obtain corresponding advantages of
the re-use of let-bound variables.

The 'discoverLetBindings' function can be called to automatically
assign let bindings based on a weighting algorithm of discovered
S-expression phrases.  The discovery is guided by parameters provided
by the caller in the 'DiscoveryGuide'; this guide also provides the
functions used to create the variables and the top-level let statement
in the language of the current S-expression.

The 'weighting' function of the 'DiscoveryGuide' can be used to assign
weights to various S-expression phrases: the S-expressions with the
highest weights will be let-bound to variables (up to the
'maxLetBinds' limit).  A weighting value of 0 will cause the
sub-expression to be ignored (never let-bound) and a value equal to or
greater than 1000000 will *always* insert a let-binding, ignoring all
other limits.

-}

alwaysBindWeight :: Int
alwaysBindWeight = 1000000

bestBindings :: DiscoveryGuide a str -> ExprInfo a -> [Location a] -> [Location a]
bestBindings guide exprs locs = getMaxBest
    where getMaxBest = NE.head $
                       -- Sometimes a lengthy binding "swallows"
                       -- everything else; skipping over it would
                       -- result in more available bindings.  Try the
                       -- first 3 combinations and take the one
                       -- yielding the most bindings.
                       NE.sortBy (compare `on` length) $
                       fmap getBestSkipping $
                       0 NE.:| [1, 2]
          getBestSkipping n = snd $ snd $  -- extract list of Locations
                              -- determine top-set of best bindings to apply
                              foldl bestB (n, (maxbinds, [])) $
                              -- sorted by heaviest -> lightest
                              reverse $
                              sortBy (compare `on` fst) $
                              filter ((/=) 0 . fst) $  -- remove weights of 0
                              -- add weights
                              fmap (\l -> (uncurry (weighting guide) $ lwi l, l)) $
                              locs
          -- bestB picks the best N bindings, where the bindings are
          -- already sorted by weight, and optionally skipping an
          -- initial count.  As an override, any binding whose weight
          -- is 1_000_000 or above is *always* included in the
          -- results.
          bestB :: (Int, (Int, [Location a]))
                -> (Int, Location a)
                -> (Int, (Int, [Location a]))
                   -- ^ ((skipcnt, ?), (numRemaining, selectedBinds))
          bestB acc@(_, (numRemaining, binds)) (w,e) =
              let subs = subBindings e binds
              in if numRemaining > 0 &&
                     (null subs || allowRecursion guide || w >= alwaysBindWeight)
                 then addUnlessSkipping acc w e
                 else acc
          subBindings x = catMaybes . fmap (isSub x)
          isSub x startingFrom = do sloc <- findLocation (locId startingFrom) exprs
                                    findLocation (locId x) sloc
          addUnlessSkipping (skip, (numRemaining, binds)) w e =
              let addE = (minExprSize guide, (numRemaining-1, e:binds))
                  skipE = (skip-1, (numRemaining, binds))
              in if w >= alwaysBindWeight
                 then addE
                 else if numRemaining > 0 && skip == 0
                      then addE
                      else skipE
          lwi l = (locExpr l, locCount l)
          maxbinds = maxLetBinds guide (length locs)


type LocationId = Int

data Location a = Location { locExpr :: SExpr a
                           , locCount :: Int
                           , locId :: LocationId
                           }
                deriving Show

data NamedLoc a = NamedLoc { nlocId :: LocationId
                           , nlocVar :: SExpr a
                           }
                deriving Show

data MyMap a = MyMap { points :: [Location a]
                     }

startingLoc :: MyMap a
startingLoc = MyMap []

data ExprInfo a = EINil | EIAtom a | EICons LocationId (ExprInfo a) (ExprInfo a)


explore :: Eq a => DiscoveryGuide a str -> MyMap a -> SExpr a -> (MyMap a, ExprInfo a)
explore _ mymap SNil = (mymap, EINil)
explore _ mymap (SAtom a) = (mymap, EIAtom a)
explore guide mymap h@(SCons l r) =
    let (lc,le) = explore guide mymap l
        (rc,re) = explore guide lc r
        (hm,hi) = updateMap guide h rc
    in (hm, EICons hi le re)


updateMap :: Eq a => DiscoveryGuide a str -> SExpr a -> MyMap a -> (MyMap a, LocationId)
updateMap guide point mymap =
    let (p, i) = addOrUpdate (points mymap)
    in (mymap { points = p }, i)
    where addOrUpdate [] = ([ Location { locExpr=point, locCount=succCnt(0), locId=lId} ], lId)
          addOrUpdate (p:ps) = let (sm,si) = addOrUpdate ps
                               in if locExpr p /= point
                                  then (p : sm, si)
                                  else (p { locCount = succCnt(locCount p) } : ps, locId p)
          lId = length (points mymap)
          succCnt n = if F.length point > (minExprSize guide) then n + 1 else n  -- ignore short SExprs


findLocation :: LocationId -> ExprInfo a -> Maybe (ExprInfo a)
findLocation loc = fndLoc
    where fndLoc EINil = Nothing
          fndLoc (EIAtom _) = Nothing
          fndLoc e@(EICons el l r) = if el == loc then Just e else fndLoc l <|> fndLoc r


assignLBNames :: (Show a, Eq a, IsString str, Monoid str) =>
                 DiscoveryGuide a str -> SExpr a -> [Location a] -> [NamedLoc a]
assignLBNames guide inp = snd . mapAccumL mkNamedLoc (1::Int, 0::Int)
    where mkNamedLoc (i,t) l = let nm = labelMaker guide suggestedName $ locExpr l
                                   suggestedName = "var" <> fromString (show i)
                               in case F.find ((==) nm) inp of
                                    Nothing -> ((i+1,0), NamedLoc { nlocId = locId l
                                                                  , nlocVar = SAtom nm
                                                                  })
                                    Just _ -> if t < 100
                                              then mkNamedLoc (i+1,t+1) l  -- collision, try another varname
                                              else error $ "Too many failed attempts \
                                                           \to generate a unique let var name: " <> show nm


type UniquenessResult a = [(NamedLoc a, [Either (NamedLoc a) (SExpr a)])]

verifyNamesUnique :: (IsString str, Eq str, Eq a) =>
                     DiscoveryGuide a str
                  -> [NamedLoc a]
                  -> SExpr a
                  -> UniquenessResult a
verifyNamesUnique guide names sexpr =
    foldr checkUniqueInExpr (checkUniqueNames names) names
    where
          varname (SAtom a) = atom2str a
          varname _ = Nothing
          atom2str = extractStr guide
          checkUniqueInExpr nloc dups =
              let locname = varname $ nlocVar nloc
                  addDup [] otherexp = [(nloc, [Right otherexp])]
                  addDup ((l,dl):dls) subexp = if nlocId l == nlocId nloc
                                               then (nloc, Right subexp : dl) : dls
                                               else addDup dls subexp
                  matchExpHead s e@(SAtom a) = if Just s == atom2str a
                                               then Just e
                                               else Nothing
                  matchExpHead s e@(SCons (SAtom a) r) = if Just s == atom2str a
                                                         then Just e
                                                         else matchExpHead s r
                  matchExpHead _ SNil = Nothing
                  matchExpHead s (SCons l r) = matchExpHead s l <|> matchExpHead s r
              in case locname of
                   Nothing -> dups
                   Just nstr -> maybe dups (addDup dups) $ matchExpHead nstr sexpr

          checkUniqueNames = fmap (fmap (fmap Left)) . snd
                             . splitDups . foldr combineDups []
          combineDups nloc [] = [(nloc, [])]
          combineDups nloc ((d,ls):ds) = if nlocVar nloc == nlocVar d
                                         then (d,nloc:ls):ds
                                         else (d,ls) : combineDups nloc ds
          splitDups = let isDup (nloc, []) (u,d) = (nloc:u, d)
                          isDup e (u,d) = (u, e:d)
                      in foldr isDup ([],[])


verificationFailureReport :: Show a => [Location a] -> UniquenessResult a -> String
verificationFailureReport locs = intercalate "\n" . fmap vfRep
    where vfRep (l, vf) =
              let fs = fmap fl vf
                  fl (Left nloc) = var nloc
                  fl (Right e) = "other portion of S-expression: "
                                 <> (show $ truncateExpr 4 e)
                  var v = "let variable \"" <> (show $ nlocVar v)
                          <> "\" ["
                          <> (show $ (truncateExpr 2 . locExpr) <$>
                                   F.find ((==) (nlocId v) . locId) locs)
                          <> " ...]"
              in intercalate "\n    " $
                     ("ERR: duplicated " <> (var l) <> " at: ") : fs

truncateExpr :: Int -> SExpr a -> SExpr a
truncateExpr _ SNil = SNil
truncateExpr _ e@(SAtom _) = e
truncateExpr 0 _ = SNil
truncateExpr n (SCons l r) = let trunc = truncateExpr (n - 1)
                             in SCons (trunc l) (trunc r)

substLBRefs :: Eq a =>
               DiscoveryGuide a str -> [NamedLoc a] -> ExprInfo a
            -> (SExpr a, SExpr a)
               -- ^ (varbindings, exprwithvars)
substLBRefs _ nlocs = swap . fmap declVars . swap . subsRefs []
    where subsRefs b EINil = (b, SNil)
          subsRefs b (EIAtom a) = (b, SAtom a)
          subsRefs b (EICons i l r) = let (b',l') = subsRefs b l
                                          (c',r') = subsRefs b' r
                                          here = SCons l' r'
                                      in case hasBinding i of
                                           Nothing -> (c', here)
                                           Just loc -> (((nlocVar loc), here) : c', (SCons (nlocVar loc) SNil))
          hasBinding i = F.find ((==) i . nlocId) nlocs
          declVars = foldl addVar SNil . foldl addVarIfUnique []
          addVarIfUnique vl v@(vn,_) = case lookup vn vl of
                                         Nothing -> v : vl
                                         Just _ -> vl
          addVar vl (vn,vv) = SCons (SCons vn (SCons vv SNil)) vl


-- ----------------------------------------------------------------------

-- | The 'letExpand' function is passed an S-expression that (may)
-- contain let-bound variables and will return an equivalent
-- S-expression that does not contain any let bindings, where let
-- bindings have been expanded into the expression.
letExpand :: (Eq a, Show a, Eq str, IsString str) =>
             (a -> Maybe str) -> SExpr a -> SExpr a
letExpand atomToText = findExpLet
    where findExpLet (SCons (SAtom a) (SCons lbvdefs (SCons subsInp SNil))) =
              if atomToText a == Just "let"
              then expLet lbvdefs subsInp
              else SCons (SAtom a) (SCons (findExpLet lbvdefs) (SCons (findExpLet subsInp) SNil))
          findExpLet e = e
          expLet lb = expandWith (bindings lb)
          bindings = parseVar []
          parseVar vdefs (SCons (SCons vn (SCons vv SNil)) r) = (vn, vv) : parseVar vdefs r
          parseVar vdefs SNil = vdefs
          parseVar _ e = error $ "Expected a var, got: " <> show e
          expandWith _ SNil = SNil
          expandWith vdefs e@(SCons v@(SAtom _) SNil) =
              case lookup v vdefs of
                Nothing -> e
                Just vv -> expandWith vdefs vv
          expandWith vdefs e@(SCons l r) =
              case lookup e vdefs of
                Nothing -> SCons (expandWith vdefs l) (expandWith vdefs r)
                Just vv -> expandWith vdefs vv
          expandWith _ e@(SAtom _) = e
