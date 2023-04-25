{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

-- | This module implements the check that the transformed code is
-- typable in the single tick calculus.

module Rattus.Plugin.CheckSingleTick
  (checkExpr, CheckExpr (..)) where



import GHC.Plugins




import Rattus.Plugin.Utils
import qualified Rattus.Plugin.PrimExpr as Prim
import Prelude hiding ((<>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Control.Monad (foldM, when)
import GHC.Types.Tickish
import Control.Applicative ((<|>))

type LCtx = Set Var
data HiddenReason = BoxApp | AdvApp | NestedRec Var | FunDef | DelayApp
type Hidden = Map Var HiddenReason


data TypeError = TypeError SrcSpan SDoc


data Ctx = Ctx
  { current :: LCtx,
    hidden :: Hidden,
    earlier :: Maybe LCtx,
    srcLoc :: SrcSpan,
    recDef :: Set Var, -- ^ recursively defined variables 
    stableTypes :: Set Var,
    allowRecursion :: Bool,
    --inDelay :: Bool,
    fresh :: Maybe Var
    }

hasTick :: Ctx -> Bool
hasTick = isJust . earlier


stabilize :: HiddenReason -> Ctx -> Ctx
stabilize hr c = c
  {current = Set.empty,
   earlier = Nothing,
   hidden = hidden c `Map.union` Map.fromSet (const hr) ctxHid
  }
  where ctxHid = maybe (current c) (Set.union (current c)) (earlier c)


data Scope = Hidden SDoc | Visible

getScope  :: Ctx -> Var -> Scope
getScope c v =
    if v `Set.member` recDef c then
      if hasTick c || allowRecursion c then Visible
      else Hidden ("(Mutually) recursive call to " <> ppr v <> " must occur under delay")
    else case Map.lookup v (hidden c) of
      Just reason ->
        if (isStable (stableTypes c) (varType v)) then Visible
        else case reason of
          NestedRec rv ->
            if allowRecursion c then Visible
            else Hidden ("Variable " <> ppr v <> " is no longer in scope:"
                         $$ "It appears in a local recursive definition (namely of " <> ppr rv <> ")"
                         $$ "and is of type " <> ppr (varType v) <> ", which is not stable.")
          BoxApp -> Hidden ("Variable " <> ppr v <> " is no longer in scope:" $$
                       "It occurs under " <> keyword "box" $$ "and is of type " <> ppr (varType v) <> ", which is not stable.")
          AdvApp -> Hidden ("Variable " <> ppr v <> " is no longer in scope: It occurs under adv.")

          FunDef -> Hidden ("Variable " <> ppr v <> " is no longer in scope: It occurs in a function that is defined under a delay, is a of a non-stable type " <> ppr (varType v) <> ", and is bound outside delay")
          DelayApp -> Hidden ("Variable " <> ppr v <> " is no longer in scope: It occurs under two occurrences of delay and is a of a non-stable type " <> ppr (varType v))
      Nothing
          | maybe False (Set.member v) (earlier c) ->
            if isStable (stableTypes c) (varType v) then Visible
            else Hidden ("Variable " <> ppr v <> " is no longer in scope:" $$
                         "It occurs under delay" $$ "and is of type " <> ppr (varType v) <> ", which is not stable.")
          | Set.member v (current c) -> Visible
          | otherwise -> Visible



pickFirst :: SrcSpan -> SrcSpan -> SrcSpan
pickFirst s@RealSrcSpan{} _ = s
pickFirst _ s = s

typeError :: Ctx -> Var -> SDoc -> TypeError
typeError ctx var = TypeError (pickFirst (srcLoc ctx) (nameSrcSpan (varName var)))

instance Outputable TypeError where
  ppr (TypeError srcLoc sdoc) = text "TypeError at " <> ppr srcLoc <> text ": " <> ppr sdoc

emptyCtx :: CheckExpr -> Ctx
emptyCtx c =
  Ctx { current =  Set.empty,
        earlier = Nothing,
        hidden = Map.empty,
        srcLoc = noLocationInfo,
        recDef = recursiveSet c,
        stableTypes = Set.empty,
        allowRecursion = allowRecExp c,
        -- inDelay = False,
        fresh = Nothing
        }

inDelay :: Ctx -> Bool
inDelay = isJust . earlier

stabilizeLater :: Ctx -> Ctx
stabilizeLater c =
  case earlier c of
    Just earl -> c {earlier = Nothing,
                    hidden = hidden c `Map.union` Map.fromSet (const FunDef) earl}
    Nothing -> c

isStableConstr :: Type -> CoreM (Maybe Var)
isStableConstr t =
  case splitTyConApp_maybe t of
    Just (con,[args]) ->
      case getNameModule con of
        Just (name, mod) ->
          if isRattModule mod && name == "Stable"
          then return (getTyVar_maybe args)
          else return Nothing
        _ -> return Nothing
    _ ->  return Nothing

-- should be equatable
type SymbolicClock = Set Var

mkClock1 :: Var -> SymbolicClock
mkClock1 = Set.singleton

mkClock2 :: Var -> Var -> SymbolicClock
mkClock2 v1 v2 = Set.fromList [v1, v2]

newtype CheckResult = CheckResult{
  -- if present, contains the variable of the primitive applied so we can report its position
  -- in case of an error, and the clock for the primitive
  prim :: Maybe (Var, SymbolicClock)
}

instance Outputable CheckResult where
  ppr (CheckResult prim) = text "CheckResult {prim = " <> ppr prim <> text "}"

emptyCheckResult :: CheckResult
emptyCheckResult = CheckResult {prim = Nothing}

data CheckExpr = CheckExpr{
  recursiveSet :: Set Var,
  oldExpr :: Expr Var,
  fatalError :: Bool,
  verbose :: Bool,
  allowRecExp :: Bool
  }

checkExpr :: CheckExpr -> Expr Var -> CoreM Bool
checkExpr c e = do
  when (verbose c) $ putMsg $ text "checkExpr: " <> ppr e
  res <- checkExpr' (emptyCtx c) e
  case res of
    Right _ -> return True
    Left (TypeError src doc) ->
      let sev = if fatalError c then SevError else SevWarning
      in if verbose c then do
        printMessage sev src ("Internal error in Rattus Plugin: single tick transformation did not preserve typing." $$ doc)
        liftIO $ putStrLn "-------- old --------"
        liftIO $ putStrLn (showSDocUnsafe (ppr (oldExpr c)))
        liftIO $ putStrLn "-------- new --------"
        liftIO $ putStrLn (showSDocUnsafe (ppr e))
        return $ not (fatalError c)
         else do
        printMessage sev noSrcSpan ("Internal error in Rattus Plugin: single tick transformation did not preserve typing." $$
                             "Compile with flags \"-fplugin-opt Rattus.Plugin:debug\" and \"-g2\" for detailed information")
        return $ not (fatalError c)


checkExpr' :: Ctx -> Expr Var -> CoreM (Either TypeError CheckResult)
checkExpr' c (App e e') | isType e' || (not $ tcIsLiftedTypeKind $ typeKind $ exprType e')
  = checkExpr' c e
checkExpr' c@Ctx{current = cur} expr@(App e e') =
  case Prim.isPrimExpr expr of
    Just (Prim.BoxApp _) ->
      checkExpr' (stabilize BoxApp c) e'
    Just (Prim.ArrApp _) ->
      checkExpr' (stabilize BoxApp c) e'
    Just (Prim.DelayApp f _ _) ->
      if inDelay c then return $ Left $ typeError c f (text "Nested delays not allowed")
      else do
        eRes <- checkExpr' c{current = Set.empty, earlier = Just cur} e'
        putMsg $ text "Delay: right checkResult = " <> ppr eRes $$ text "For expr: " $$ ppr e'
        case eRes of
          Left err -> return $ Left err
          Right (CheckResult {prim = Nothing}) -> return $ Left $ typeError c f (text "Each delay must contain an adv or select")
          Right _ -> return $ Right emptyCheckResult
    Just (Prim.AdvApp f _) | not (inDelay c) -> return $ Left $ typeError c f (text "can only use adv under delay")
    Just (Prim.AdvApp f (arg, _)) -> return $ Right $ CheckResult {prim = Just (f, mkClock1 arg)}
    Just (Prim.SelectApp f _ _) | not (inDelay c) -> return $ Left $ typeError c f (text "can only use select under delay")
    Just (Prim.SelectApp f (arg1, _) (arg2, _))-> return $ Right $ CheckResult {prim = Just (f, mkClock2 arg1 arg2)}
    Nothing -> checkBoth c e e'
checkExpr' c (Case e v _ alts) = do
    res <- checkExpr' c' e
    resAll <- mapM (\(Alt _ _ altE) -> checkExpr' c altE) alts
    foldM (fmap return . combine c) res resAll
    {-
    let maybePrimVar = foldl (\acc (Alt _ _ altE) -> acc <|> recursiveIsPrimExpr altE) Nothing alts
    case maybePrimVar of
      Just _ -> return $ Left $ typeError c v "Primitives in case expressions are not allowed"
      Nothing -> return res -}
  where c' = addVars [v] c
checkExpr' c (Lam v e)
  | isTyVar v || (not $ tcIsLiftedTypeKind $ typeKind $ varType v) = do
      is <- isStableConstr (varType v)
      let c' = case is of
            Nothing -> c
            Just t -> c{stableTypes = Set.insert t (stableTypes c)}
      checkExpr' c' e
  | otherwise = checkExpr' (addVars [v] (stabilizeLater c)) e
checkExpr' _ (Type _)  = return $ Right emptyCheckResult
checkExpr' _ (Lit _)  = return $ Right emptyCheckResult
checkExpr' _ (Coercion _)  = return $ Right emptyCheckResult
checkExpr' c (Tick (SourceNote span _name) e) =
  checkExpr' c{srcLoc = fromRealSrcSpan span} e
checkExpr' c (Tick _ e) = checkExpr' c e
checkExpr' c (Cast e _) = checkExpr' c e
checkExpr' c (Let (NonRec v e1) e2) = do
  res1 <- checkExpr' c e1
  res2 <- checkExpr' c e2
  return $ combine c res1 res2
checkExpr' c (Let (Rec binds) e2) = do
    resAll <- mapM (\ (v,e) -> checkExpr' (c' v) e) binds
    res <- checkExpr' (addVars vs c) e2
    foldM (fmap return . combine c) res resAll
  where vs = map fst binds
        ctxHid = maybe (current c) (Set.union (current c)) (earlier c)
        c' v = c {current = Set.empty,
                  earlier = Nothing,
                  hidden =  hidden c `Map.union`
                   Map.fromSet (const (NestedRec v)) ctxHid,
                  recDef = recDef c `Set.union` Set.fromList vs }
checkExpr' c  (Var v)
  | tcIsLiftedTypeKind $ typeKind $ varType v =  case getScope c v of
             Hidden reason -> return $ Left $ typeError c v reason
             Visible -> return $ Right emptyCheckResult
  | otherwise = return $ Right emptyCheckResult

addVars :: [Var] -> Ctx -> Ctx
addVars v c = c{current = Set.fromList v `Set.union` current c }

checkBoth :: Ctx -> CoreExpr -> CoreExpr -> CoreM (Either TypeError CheckResult)
checkBoth c e e' = do
  c1 <- checkExpr' c e
  c2 <- checkExpr' c e'
  return $ combine c c1 c2

-- Combines two CheckResults such that the clocks therein are compatible.
-- If both CheckResults have PrimVars, one is picked arbitrarily.
combine :: Ctx -> Either TypeError CheckResult -> Either TypeError CheckResult -> Either TypeError CheckResult
combine c eRes1 eRes2 = do
  res1 <- eRes1
  res2 <- eRes2
  case (res1, res2) of
    (CheckResult (Just (_, cl1)), CheckResult (Just (_, cl2))) | cl1 == cl2 -> Right res2
    (CheckResult (Just _), CheckResult (Just (p, _))) -> Left $ typeError c p "Only one adv/select allowed in a delay"
    (CheckResult maybeP, CheckResult maybeP') -> Right $ CheckResult {prim = maybeP <|> maybeP'}


{-
countAdvSelect' :: Ctx -> Expr Var -> Either String CheckResult
countAdvSelect' ctx (App e e') = case isPrimExpr ctx e of
      Just (p, _) -> case D.trace ("we have met a prim: " ++ showSDocUnsafe (ppr p)) p of
        Adv | not (isVar e') -> Left "Can only adv on variables"
            | hasSeenAdvSelect ctx -> Left "Only one adv/select allowed in a delay"
            | otherwise -> Right CheckResult { foundClock = Just (Clock (getVar e')) }
        Select -> Left "Select not implemented"
                    -- | not $ all isVar args -> Left "Can only select on variables"
                    -- | hasSeenAdvSelect ctx -> Left "Only one adv/select allowed in a delay"
                    -- | otherwise -> Right CheckResult { foundClock = let [first, second] = map (Clock . getVar) args in Just $ Union first second}
        Delay | inDelay ctx -> Left "Nested delays not allowed"
              | otherwise -> case countAdvSelect' (ctx {inDelay = True}) e' of
                Right r | isJust (foundClock r) -> D.trace ("Delay: found correct clock " ++ show r) (Right r)
                Left s -> Left s
                _ -> Left "Each delay must contain an adv or select"
        _ -> Right emptyCheckResult   -- what about box/unbox?
      _ -> case checkAndUpdate ctx e of
          Left s -> Left s
          Right ctx' -> countAdvSelect' ctx' e'
countAdvSelect' ctx (Lam _ rhs) = countAdvSelect' ctx rhs
countAdvSelect' ctx (Let (NonRec _ e') e) =
  case fmap (updateCtxFromResult ctx) (countAdvSelect' ctx e) of
        Left s -> Left s
        Right ctx' -> countAdvSelect' ctx' e'
countAdvSelect' ctx (Case e _ _ alts) = case countAdvSelect' ctx e of
  Left s -> Left s
  Right res -> snd <$> foldM (\(c, r) (Alt _ _ e') ->
    case countAdvSelect' c e' of
      Left s -> Left s
      Right r' | advSelect r && advSelect r' -> Left "Only one adv/select allowed in a delay"
      Right r' -> Right (updateCtxFromResult c r', r')) (updateCtxFromResult ctx res, res) alts
countAdvSelect' ctx (Cast e _) = countAdvSelect' ctx e
countAdvSelect' ctx (Tick _ e) = countAdvSelect' ctx e
countAdvSelect' _ _ = Right emptyCheckResult
-}