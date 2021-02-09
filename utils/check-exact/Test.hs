{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.List
import Data.Data
import Data.Typeable
-- import GHC.Types.SrcLoc
import GHC.Types.Name.Occurrence
import GHC.Types.Name.Reader
import GHC hiding (moduleName)
import GHC.Driver.Ppr
import GHC.Driver.Session
import GHC.Hs.Dump
import GHC.Data.Bag
-- import GHC.Types.SourceText
-- import GHC.Hs.Exact hiding (ExactPrint())
-- import GHC.Utils.Outputable hiding (space)
import System.Environment( getArgs )
import System.Exit
import System.FilePath

import Types
import Utils
import ExactPrint
import Transform
import Parsers
import qualified Parsers as P
-- exactPrint = undefined
-- showPprUnsafe = undefined

import GHC.Parser.Lexer
import GHC.Data.FastString
import GHC.Types.SrcLoc
import GHC

-- ---------------------------------------------------------------------

tt :: IO ()
-- tt = testOneFile "/home/alanz/mysrc/git.haskell.org/ghc/_build/stage1/lib"
tt = testOneFile "/home/alanz/mysrc/git.haskell.org/worktree/exactprint/_build/stage1/lib"
 -- "cases/RenameCase1.hs" changeRenameCase1
 -- "cases/LayoutLet2.hs" changeLayoutLet2
 -- "cases/LayoutLet3.hs" changeLayoutLet3
 -- "cases/LayoutLet4.hs" changeLayoutLet3
 -- "cases/Rename1.hs" changeRename1
 -- "cases/Rename2.hs" changeRename2
 -- "cases/LayoutIn1.hs" changeLayoutIn1
 -- "cases/LayoutIn3.hs" changeLayoutIn3
 -- "cases/LayoutIn3a.hs" changeLayoutIn3
 -- "cases/LayoutIn3b.hs" changeLayoutIn3
 -- "cases/LayoutIn4.hs" changeLayoutIn4
 -- "cases/LocToName.hs" changeLocToName
 -- "cases/LetIn1.hs" changeLetIn1
 -- "cases/WhereIn4.hs" changeWhereIn4
 -- "cases/AddDecl1.hs" changeAddDecl1
 -- "cases/AddDecl2.hs" changeAddDecl2
 -- "cases/AddDecl3.hs" changeAddDecl3
 -- "cases/LocalDecls.hs" changeLocalDecls
 -- "cases/LocalDecls2.hs" changeLocalDecls2
 -- "cases/WhereIn3a.hs" changeWhereIn3a
 -- "cases/WhereIn3b.hs" changeWhereIn3b
 -- "cases/AddLocalDecl1.hs" addLocaLDecl1
 -- "cases/AddLocalDecl2.hs" addLocaLDecl2
 -- "cases/AddLocalDecl3.hs" addLocaLDecl3
 -- "cases/AddLocalDecl4.hs" addLocaLDecl4
 -- "cases/AddLocalDecl5.hs" addLocaLDecl5
 -- "cases/AddLocalDecl6.hs" addLocaLDecl6
 -- "cases/RmDecl1.hs" rmDecl1
 -- "cases/RmDecl2.hs" rmDecl2
 -- "cases/RmDecl3.hs" rmDecl3
 -- "cases/RmDecl4.hs" rmDecl4
 -- "cases/RmDecl5.hs" rmDecl5
 -- "cases/RmDecl6.hs" rmDecl6
 -- "cases/RmDecl7.hs" rmDecl7
 -- "cases/RmTypeSig1.hs" rmTypeSig1
 -- "cases/RmTypeSig2.hs" rmTypeSig2
 -- "cases/AddHiding1.hs" addHiding1
 "cases/AddHiding2.hs" addHiding2

-- cloneT does not need a test, function can be retired


-- exact = ppr

-- ---------------------------------------------------------------------

usage :: String
usage = unlines
    [ "usage: check-ppr (libdir) (file)"
    , ""
    , "where libdir is the GHC library directory (e.g. the output of"
    , "ghc --print-libdir) and file is the file to parse."
    ]

main :: IO()
main = do
  args <- getArgs
  case args of
   [libdir,fileName] -> testOneFile libdir fileName noChange
   _ -> putStrLn usage

deriving instance Data Token
deriving instance Data PsSpan
deriving instance Data BufSpan
deriving instance Data BufPos

testOneFile :: FilePath -> String -> Changer -> IO ()
testOneFile libdir fileName changer = do
       (p,toks) <- parseOneFile libdir fileName
       putStrLn $ "\n\ngot p" ++ showAst (take 4 $ reverse toks)
       let
         origAst = ppAst (pm_parsed_source p)
         anns'   = pm_annotations p
         -- pped    = pragmas ++ "\n" ++ (exactPrint $ pm_parsed_source p)
         pped    = exactPrint (pm_parsed_source p) anns'
         -- pragmas = getPragmas anns'

         newFile         = dropExtension fileName <.> "ppr"      <.> takeExtension fileName
         newFileChanged  = dropExtension fileName <.> "changed"  <.> takeExtension fileName
         newFileExpected = dropExtension fileName <.> "expected" <.> takeExtension fileName
         astFile        = fileName <.> "ast"
         newAstFile     = fileName <.> "ast.new"
         changedAstFile = fileName <.> "ast.changed"

       -- pped' <- exactprintWithChange changeRenameCase1 (pm_parsed_source p) anns'
       (pped', ast') <- exactprintWithChange libdir changer (pm_parsed_source p) anns'
       -- putStrLn $ "\n\nabout to writeFile"
       writeFile changedAstFile (ppAst ast')
       writeFile astFile origAst
       -- putStrLn $ "\n\nabout to pp"
       writeFile newFile        pped
       writeFile newFileChanged pped'

       -- putStrLn $ "anns':" ++ showPprUnsafe (apiAnnRogueComments anns')

       (p',_) <- parseOneFile libdir newFile

       let newAstStr :: String
           newAstStr = ppAst (pm_parsed_source p')
       writeFile newAstFile newAstStr
       expectedSource <- readFile newFileExpected
       changedSource  <- readFile newFileChanged

       -- putStrLn $ "\n\nanns':" ++ showPprUnsafe (apiAnnRogueComments anns')

       let
         origAstOk       = origAst == newAstStr
         changedSourceOk = expectedSource == changedSource
       if origAstOk && changedSourceOk
         then do
           -- putStrLn "ASTs matched"
           exitSuccess
         else if not origAstOk
           then do
             putStrLn "AST Match Failed"
             -- putStrLn "\n===================================\nOrig\n\n"
             -- putStrLn origAst
             putStrLn "\n===================================\nNew\n\n"
             putStrLn newAstStr
             exitFailure
           else do
             putStrLn "Changed AST Source Mismatch"
             putStrLn "\n===================================\nExpected\n\n"
             putStrLn expectedSource
             putStrLn "\n===================================\nChanged\n\n"
             putStrLn changedSource
             putStrLn "\n===================================\n"
             putStrLn $ show changedSourceOk
             exitFailure

ppAst ast = showSDocUnsafe $ showAstData BlankSrcSpanFile NoBlankApiAnnotations ast

parseOneFile :: FilePath -> FilePath -> IO (ParsedModule, [Located Token])
parseOneFile libdir fileName = do
       let modByFile m =
             case ml_hs_file $ ms_location m of
               Nothing -> False
               Just fn -> fn == fileName
       runGhc (Just libdir) $ do
         dflags <- getSessionDynFlags
         let dflags2 = dflags `gopt_set` Opt_KeepRawTokenStream
         _ <- setSessionDynFlags dflags2
         addTarget Target { targetId = TargetFile fileName Nothing
                          , targetAllowObjCode = True
                          , targetContents = Nothing }
         _ <- load LoadAllTargets
         graph <- getModuleGraph
         let
           modSum = case filter modByFile (mgModSummaries graph) of
                     [x] -> x
                     xs -> error $ "Can't find module, got:"
                              ++ show (map (ml_hs_file . ms_location) xs)
         pm <- GHC.parseModule modSum
         toks <- getTokenStream (ms_mod modSum)
         return (pm, toks)

         -- getTokenStream :: GhcMonad m => Module -> m [Located Token]

-- getPragmas :: ApiAnns -> String
-- getPragmas anns' = pragmaStr
--   where
--     tokComment (L _ (AnnBlockComment s)) = s
--     tokComment (L _ (AnnLineComment  s)) = s
--     tokComment _ = ""

--     comments' = map tokComment $ sortRealLocated $ apiAnnRogueComments anns'
--     pragmas = filter (\c -> isPrefixOf "{-#" c ) comments'
--     pragmaStr = intercalate "\n" pragmas

-- pp :: (Outputable a) => a -> String
-- pp a = showPpr unsafeGlobalDynFlags a

-- ---------------------------------------------------------------------

exactprintWithChange :: FilePath -> Changer -> ParsedSource -> ApiAnns -> IO (String, ParsedSource)
exactprintWithChange libdir f p anns = do
  debugM $ "exactprintWithChange:anns=" ++ showGhc (apiAnnRogueComments anns)
  (anns',p') <- f libdir anns p
  return (exactPrint p' anns', p')


-- First param is libdir
type Changer = FilePath -> (ApiAnns -> ParsedSource -> IO (ApiAnns,ParsedSource))

noChange :: Changer
noChange libdir ans parsed = return (ans,parsed)

changeRenameCase1 :: Changer
changeRenameCase1 libdir ans parsed = return (ans,rename "bazLonger" [((3,15),(3,18))] parsed)

changeLayoutLet2 :: Changer
changeLayoutLet2 libdir ans parsed = return (ans,rename "xxxlonger" [((7,5),(7,8)),((8,24),(8,27))] parsed)

changeLayoutLet3 :: Changer
changeLayoutLet3 libdir ans parsed = return (ans,rename "xxxlonger" [((7,5),(7,8)),((9,14),(9,17))] parsed)

changeLayoutIn1 :: Changer
changeLayoutIn1 libdir ans parsed = return (ans,rename "square" [((7,17),(7,19)),((7,24),(7,26))] parsed)

changeLayoutIn3 :: Changer
changeLayoutIn3 libdir ans parsed = return (ans,rename "anotherX" [((7,13),(7,14)),((7,37),(7,38)),((8,37),(8,38))] parsed)

changeLayoutIn4 :: Changer
changeLayoutIn4 libdir ans parsed = return (ans,rename "io" [((7,8),(7,13)),((7,28),(7,33))] parsed)

changeLocToName :: Changer
changeLocToName libdir ans parsed = return (ans,rename "LocToName.newPoint" [((20,1),(20,11)),((20,28),(20,38)),((24,1),(24,11))] parsed)


changeRename1 :: Changer
changeRename1 libdir ans parsed = return (ans,rename "bar2" [((3,1),(3,4))] parsed)

changeRename2 :: Changer
changeRename2 libdir ans parsed = return (ans,rename "joe" [((2,1),(2,5))] parsed)

rename :: (Data a) => String -> [(Pos, Pos)] -> a -> a
rename newNameStr spans' a
  = everywhere (mkT replaceRdr) a
  where
    newName = mkRdrUnqual (mkVarOcc newNameStr)

    cond :: SrcSpan -> Bool
    cond ln = ss2range ln `elem` spans'

    replaceRdr :: LocatedN RdrName -> LocatedN RdrName
    replaceRdr (L ln _)
        | cond (locA ln) = L ln newName
    replaceRdr x = x

-- ---------------------------------------------------------------------

changeWhereIn4 :: Changer
changeWhereIn4 libdir ans parsed
  = return (ans,everywhere (mkT replace) parsed)
  where
    replace :: LocatedN RdrName -> LocatedN RdrName
    replace (L ln _n)
      | ss2range (locA ln) == ((12,16),(12,17)) = L ln (mkRdrUnqual (mkVarOcc "p_2"))
    replace x = x

-- ---------------------------------------------------------------------

changeLetIn1 :: Changer
changeLetIn1 libdir ans parsed
  = return (ans,everywhere (mkT replace) parsed)
  where
    replace :: HsExpr GhcPs -> HsExpr GhcPs
    replace (HsLet an localDecls expr)
      =
         let (HsValBinds x (ValBinds xv bagDecls sigs)) = localDecls
             decls@(l1:l2:ls) = reverse $ bagToList bagDecls
             l2' = remove l2 l1
             bagDecls' = listToBag $ reverse (l2':ls)
         in (HsLet an (HsValBinds x (ValBinds xv bagDecls' sigs)) expr)

    replace x = x
-- ---------------------------------------------------------------------

-- | Add a declaration to AddDecl
changeAddDecl1 :: Changer
changeAddDecl1 libdir ans top = do
  Right (declAnns, decl) <- withDynFlags libdir (\df -> parseDecl df "<interactive>" "nn = n2")
  let decl' = setEntryDP' decl (DP (2,0))

  let (p',(ans',_),_) = runTransform mempty doAddDecl
      doAddDecl = everywhereM (mkM replaceTopLevelDecls) top
      replaceTopLevelDecls :: ParsedSource -> Transform ParsedSource
      replaceTopLevelDecls m = insertAtStart m decl'
  return (ans,p')

changeAddDecl2 :: Changer
changeAddDecl2 libdir ans top = do
  Right (declAnns, decl) <- withDynFlags libdir (\df -> parseDecl df "<interactive>" "nn = n2")
  let decl' = setEntryDP' decl (DP (2,0))
  let top' = anchorEof top

  let (p',(ans',_),_) = runTransform mempty doAddDecl
      doAddDecl = everywhereM (mkM replaceTopLevelDecls) top'
      replaceTopLevelDecls :: ParsedSource -> Transform ParsedSource
      replaceTopLevelDecls m = insertAtEnd m decl'
  return (ans,p')

changeAddDecl3 :: Changer
changeAddDecl3 libdir ans top = do
  Right (declAnns, decl) <- withDynFlags libdir (\df -> parseDecl df "<interactive>" "nn = n2")
  let decl' = setEntryDP' decl (DP (2,0))

  let (p',(ans',_),_) = runTransform mempty doAddDecl
      doAddDecl = everywhereM (mkM replaceTopLevelDecls) top
      f d (l1:l2:ls) = l1:d:l2':ls
        where
          l2' = setEntryDP' l2 (DP (2,0))
      replaceTopLevelDecls :: ParsedSource -> Transform ParsedSource
      replaceTopLevelDecls m = insertAt f m decl'
  return (ans,p')

-- ---------------------------------------------------------------------

-- | Add a local declaration with signature to LocalDecl
changeLocalDecls :: Changer
changeLocalDecls libdir ans (L l p) = do
  Right (_, s@(L ls (SigD _ sig)))  <- withDynFlags libdir (\df -> parseDecl df "sig"  "nn :: Int")
  Right (_, d@(L ld (ValD _ decl))) <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  let decl' = setEntryDP' (L ld decl) (DP (1, 0))
  let  sig' = setEntryDP' (L ls sig)  (DP (0, 0))
  let (p',(ans',_),_w) = runTransform mempty doAddLocal
      doAddLocal = everywhereM (mkM replaceLocalBinds) p
      replaceLocalBinds :: LMatch GhcPs (LHsExpr GhcPs)
                        -> Transform (LMatch GhcPs (LHsExpr GhcPs))
      replaceLocalBinds m@(L lm (Match an mln pats (GRHSs _ rhs (HsValBinds van (ValBinds _ binds sigs))))) = do
        let oldDecls = sortLocatedA $ map wrapDecl (bagToList binds) ++ map wrapSig sigs
        let decls = s:d:oldDecls
        let oldDecls' = captureLineSpacing oldDecls
        let oldBinds     = concatMap decl2Bind oldDecls'
            (os:oldSigs) = concatMap decl2Sig  oldDecls'
            os' = setEntryDP' os (DP (2, 0))
        let sortKey = captureOrder decls
        let (ApiAnn anc (AnnList (Just (Anchor anc2 _)) a b c d) cs) = van
        let van' = (ApiAnn anc (AnnList (Just (Anchor anc2 (MovedAnchor (DP (1,4))))) a b c d) cs)
        let binds' = (HsValBinds van'
                          (ValBinds sortKey (listToBag $ decl':oldBinds)
                                          (sig':os':oldSigs)))
        return (L lm (Match an mln pats (GRHSs noExtField rhs binds')))
      replaceLocalBinds x = return x
  return (ans,L l p')

-- ---------------------------------------------------------------------

-- | Add a local declaration with signature to LocalDecl, where there was no
-- prior local decl. So it adds a "where" annotation.
changeLocalDecls2 :: Changer
changeLocalDecls2 libdir ans (L l p) = do
  Right (_, d@(L ld (ValD _ decl))) <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  Right (_, s@(L ls (SigD _ sig)))  <- withDynFlags libdir (\df -> parseDecl df "sig"  "nn :: Int")
  let decl' = setEntryDP' (L ld decl) (DP (1, 0))
  let  sig' = setEntryDP' (L ls  sig) (DP (0, 2))
  let (p',(ans',_),_w) = runTransform mempty doAddLocal
      doAddLocal = everywhereM (mkM replaceLocalBinds) p
      replaceLocalBinds :: LMatch GhcPs (LHsExpr GhcPs)
                        -> Transform (LMatch GhcPs (LHsExpr GhcPs))
      replaceLocalBinds m@(L lm (Match ma mln pats (GRHSs _ rhs EmptyLocalBinds{}))) = do
        newSpan <- uniqueSrcSpanT
        let anc = (Anchor (rs newSpan) (MovedAnchor (DP (1,2))))
        let anc2 = (Anchor (rs newSpan) (MovedAnchor (DP (1,4))))
        let an = ApiAnn anc
                        (AnnList (Just anc2) Nothing Nothing
                                 [(undeltaSpan (rs newSpan) AnnWhere (DP (0,0)))] [])
                        noCom
        let decls = [s,d]
        let sortKey = captureOrder decls
        let binds = (HsValBinds an (ValBinds sortKey (listToBag $ [decl'])
                                    [sig']))
        return (L lm (Match ma mln pats (GRHSs noExtField rhs binds)))
      replaceLocalBinds x = return x
  return (ans,L l p')

-- ---------------------------------------------------------------------

-- | Check that balanceCommentsList is idempotent
changeWhereIn3a :: Changer
changeWhereIn3a libdir ans (L l p) = do
  let decls0 = hsmodDecls p
      (decls,(ans',_),w) = runTransform mempty (balanceCommentsList decls0)
      (d0:_:d1:d2:_) = decls
      -- d0 : sumSquares
      -- skip signature
      -- d1 : sq
      -- d2 : anotherFun
  debugM $ unlines w
  debugM $ "changeWhereIn3a:d1:" ++ showAst d1
  let p2 = p { hsmodDecls = decls}
  return (ans,L l p2)

-- ---------------------------------------------------------------------

changeWhereIn3b :: Changer
changeWhereIn3b libdir ans (L l p) = do
  let decls0 = hsmodDecls p
      (decls,(ans',_),w) = runTransform mempty (balanceCommentsList decls0)
      (d0:_:d1:d2:_) = decls
      -- d0 : sumSquares
      -- skip signature
      -- d1 : sq
      -- d2 : anotherFun
      d0' = setEntryDP' d0 (DP (2, 0))
      d1' = setEntryDP' d1 (DP (2, 0))
      d2' = setEntryDP' d2 (DP (2, 0))
      decls' = d2':d1':d0':(tail decls)
  debugM $ unlines w
  debugM $ "changeWhereIn3b:d1':" ++ showAst d1'
  let p2 = p { hsmodDecls = decls'}
  return (ans,L l p2)

-- ---------------------------------------------------------------------

addLocaLDecl1 :: Changer
addLocaLDecl1 libdir ans lp = do
  Right (_, (L ld (ValD _ decl))) <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  -- let declAnns' = setPrecedingLines newDecl 1 4 declAnns
  let decl' = setEntryDP' (L ld decl) (DP (1, 4))
      doAddLocal = do
        (d1:d2:d3:_) <- hsDecls lp
        (d1'',d2') <- balanceComments d1 d2
        (d1',_) <- modifyValD (getLocA d1'') d1'' $ \_m d -> do
          return ((wrapDecl decl' : d),Nothing)
        replaceDecls lp [d1', d2', d3]

  (lp',(ans',_),w) <- runTransformT mempty doAddLocal
  -- putStrLn $ "log:\n" ++ intercalate "\n" _w
  debugM $ "addLocaLDecl1:" ++ intercalate "\n" w
  return (ans,lp')

-- ---------------------------------------------------------------------

addLocaLDecl2 :: Changer
addLocaLDecl2 libdir ans lp = do
  Right (_, newDecl@(L ld (ValD _ decl))) <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  let
      doAddLocal = do
         -- tlDecs <- hsDecls lp
         -- let parent = head tlDecs
         -- balanceComments parent (head $ tail tlDecs)
         (d1:d2:_) <- hsDecls lp
         (d1'',d2') <- balanceComments d1 d2

         (parent',_) <- modifyValD (getLocA d1) d1'' $ \_m (d:ds) -> do
           newDecl' <- transferEntryDP' d newDecl
           let d' = setEntryDP' d (DP (1, 0))
           return ((newDecl':d':ds),Nothing)

         replaceDecls lp [parent',d2']
         -- replaceDecls lp [d1'',d2']

  (lp',(ans',_),_w) <- runTransformT mempty doAddLocal
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return (ans,lp')

-- ---------------------------------------------------------------------

addLocaLDecl3 :: Changer
addLocaLDecl3 libdir ans lp = do
  Right (_, newDecl@(L ld (ValD _ decl))) <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  -- Right (_, newDecl@(L ld (ValD _ decl))) <- withDynFlags libdir (\df -> parseDecl df "decl" "jj = 2")
  let
      doAddLocal = do
         (d1:d2:_) <- hsDecls lp
         (d1'',d2') <- balanceComments d1 d2

         (parent',_) <- modifyValD (getLocA d1) d1'' $ \_m (d:ds) -> do
           let newDecl' = setEntryDP' newDecl (DP (1, 0))
           let d' = setEntryDP' d (DP (0,0))
           return (((d:ds) ++ [newDecl']),Nothing)
           -- return (((d':ds) ++ [newDecl']),Nothing)
           -- return (d':ds,Nothing)

         replaceDecls (anchorEof lp) [parent',d2']

  (lp',(ans',_),_w) <- runTransformT mempty doAddLocal
  -- putStrLn $ "log\n" ++ intercalate "\n" _w
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return (ans,lp')

-- ---------------------------------------------------------------------

addLocaLDecl4 :: Changer
addLocaLDecl4 libdir ans lp = do
  Right (_, newDecl@(L ld (ValD _ decl))) <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  Right (_, newSig@(L ld (SigD _ sig)))   <- withDynFlags libdir (\df -> parseDecl df "sig"  "nn :: Int")
  -- putStrLn $ "addLocaLDecl4:lp=" ++ showGhc lp
  let
      doAddLocal = do
         (parent:ds) <- hsDecls lp

         let newDecl' = setEntryDP' newDecl (DP (1, 0))
         let newSig'  = setEntryDP' newSig  (DP (1, 4))

         (parent',_) <- modifyValD (getLocA parent) parent $ \_m decls -> do
           return ((decls++[newSig',newDecl']),Nothing)

         replaceDecls (anchorEof lp) (parent':ds)

  (lp',(ans',_),_w) <- runTransformT mempty doAddLocal
  -- putStrLn $ "log\n" ++ intercalate "\n" _w
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return (ans,lp')


-- ---------------------------------------------------------------------

addLocaLDecl5 :: Changer
addLocaLDecl5 libdir ans lp = do
  let
      doAddLocal = do
         decls <- hsDecls lp
         [s1,d1,d2,d3] <- balanceCommentsList decls

         let d3' = setEntryDP' d3 (DP (2,0))

         (d1',_) <- modifyValD (getLocA d1) d1 $ \_m _decls -> do
           let d2' = setEntryDP' d2 (DP (1,0))
           return ([d2'],Nothing)
         replaceDecls lp [s1,d1',d3']

  (lp',(ans',_),_w) <- runTransformT mempty doAddLocal
  -- putStrLn $ "log\n" ++ intercalate "\n" _w
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return (ans,lp')
-- ---------------------------------------------------------------------

addLocaLDecl6 :: Changer
addLocaLDecl6 libdir ans lp = do
  Right (_, newDecl@(L ld (ValD _ decl))) <- withDynFlags libdir (\df -> parseDecl df "decl" "x = 3")
  let
      newDecl' = setEntryDP' newDecl (DP (1, 4))
      doAddLocal = do
        decls <- hsDecls lp
        [d1'',d2] <- balanceCommentsList decls

        let d1 = captureMatchLineSpacing d1''
        let L _ (ValD _ (FunBind _ _ (MG _ (L _ ms) _) _)) = d1
        let [m1,m2] = ms

        (d1',_) <- modifyValD (getLocA m1) d1 $ \_m decls -> do
           return ((newDecl' : decls),Nothing)
        replaceDecls lp [d1', d2]

  (lp',(ans',_),_w) <- runTransformT mempty doAddLocal
  -- putStrLn $ "log:\n" ++ intercalate "\n" _w
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return (ans,lp')

-- ---------------------------------------------------------------------

rmDecl1 :: Changer
rmDecl1 libdir ans lp = do
  let doRmDecl = do
         tlDecs0 <- hsDecls lp
         tlDecs <- balanceCommentsList $ captureLineSpacing tlDecs0
         let (d1:s1:d2:ds) = tlDecs

         replaceDecls lp (d1:ds)

  (lp',(ans',_),_w) <- runTransformT mempty doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return (ans,lp')

-- ---------------------------------------------------------------------

rmDecl2 :: Changer
rmDecl2 libdir ans lp = do
  let
      doRmDecl = do
        let
          go :: GHC.LHsExpr GhcPs -> Transform (GHC.LHsExpr GhcPs)
          go e@(GHC.L _ (GHC.HsLet{})) = do
            decs0 <- hsDecls e
            decs <- balanceCommentsList $ captureLineSpacing decs0
            e' <- replaceDecls e (init decs)
            return e'
          go x = return x

        everywhereM (mkM go) lp

  let (lp',(ans',_),_w) = runTransform mempty doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return (ans,lp')

-- ---------------------------------------------------------------------

rmDecl3 :: Changer
rmDecl3 libdir ans lp = do
  let
      doRmDecl = do
         [d1,d2] <- hsDecls lp

         (d1',Just sd1) <- modifyValD (getLocA d1) d1 $ \_m [sd1] -> do
           let sd1' = setEntryDP' sd1 (DP (2,0))
           return ([],Just sd1')

         replaceDecls lp [d1',sd1,d2]

  (lp',(ans',_),_w) <- runTransformT mempty doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return (ans,lp')

-- ---------------------------------------------------------------------

rmDecl4 :: Changer
rmDecl4 libdir ans lp = do
  let
      doRmDecl = do
         [d1] <- hsDecls lp

         (d1',Just sd1) <- modifyValD (getLocA d1) d1 $ \_m [sd1,sd2] -> do
           -- [sd1,sd2] <- hsDecls d1
           sd2' <- transferEntryDP' sd1 sd2

           -- setPrecedingLinesDeclT sd1 2 0
           let sd1' = setEntryDP' sd1 (DP (2,0))
           -- let sd2' = setEntryDP' sd2 (DP (0,0))
           -- d1' <- replaceDecls d1 [sd2]
           return ([sd2'],Just sd1')

         replaceDecls (anchorEof lp) [d1',sd1]

  (lp',(ans',_),_w) <- runTransformT mempty doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return (ans,lp')

-- ---------------------------------------------------------------------

rmDecl5 :: Changer
rmDecl5 libdir ans lp = do
  let
      doRmDecl = do
        let
          go :: HsExpr GhcPs -> Transform (HsExpr GhcPs)
          go e@(HsLet a lb expr) = do
            decs <- hsDeclsValBinds lb
            let dec = last decs
            transferEntryDPT (head decs) dec
            lb' <- replaceDeclsValbinds WithoutWhere lb [dec]
            return (HsLet a lb' expr)
          go x = return x

        everywhereM (mkM go) lp

  let (lp',(ans',_),_w) = runTransform mempty doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return (ans,lp')
-- ---------------------------------------------------------------------

rmDecl6 :: Changer
rmDecl6 libdir ans lp = do
  let
      doRmDecl = do
         [d1] <- hsDecls lp

         (d1',_) <- modifyValD (getLocA d1) d1 $ \_m subDecs -> do
           let (ss1:_sd1:sd2:sds) = subDecs
           sd2' <- transferEntryDP' ss1 sd2

           -- logTr $ "rmDecl6:locs=" ++ show (map (rs . getLocA) subDecs)
           return (sd2':sds,Nothing)

         replaceDecls lp [d1']

  (lp',(ans',_),_w) <- runTransformT mempty doRmDecl
  -- putStrLn $ "log:" ++ intercalate "\n" _w
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return (ans,lp')
-- ---------------------------------------------------------------------

rmDecl7 :: Changer
rmDecl7 libdir ans lp = do
  let
      doRmDecl = do
         tlDecs <- hsDecls lp
         -- [s1,d1,d2,d3] <- balanceCommentsList $ captureLineSpacing tlDecs
         [s1,d1,d2,d3] <- balanceCommentsList tlDecs
         -- let [s1,d1,d2,d3] = captureLineSpacing tlDecs

         -- balanceComments d1 d2
         -- balanceComments d2 d3

         d3' <- transferEntryDP' d2 d3

         replaceDecls lp [s1,d1,d3']
         -- replaceDecls lp [s1,d1,d2,d3]

  (lp',(ans',_),_w) <- runTransformT mempty doRmDecl
  -- putStrLn $ "log:" ++ intercalate "\n" _w
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return (ans,lp')

-- ---------------------------------------------------------------------

rmTypeSig1 :: Changer
rmTypeSig1 libdir ans lp = do
  let doRmDecl = do
         tlDecs <- hsDecls lp
         let (s0:d1:d2) = tlDecs
             s1 = captureTypeSigSpacing s0
             (L l (SigD x1 (TypeSig x2 [n1,n2] typ))) = s1
         n2' <- transferEntryDP n1 n2
         let s1' = (L l (SigD x1 (TypeSig x2 [n2'] typ)))
         replaceDecls lp (s1':d1:d2)

  let (lp',(ans',_),_w) = runTransform mempty doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return (ans,lp')

-- ---------------------------------------------------------------------

rmTypeSig2 :: Changer
rmTypeSig2 libdir ans lp = do
  let doRmDecl = do
         tlDecs <- hsDecls lp
         let [d1] = tlDecs

         (d1',_) <- modifyValD (getLocA d1) d1 $ \_m [s,d] -> do
           d' <- transferEntryDPT s d
           return ([d'],Nothing)
         replaceDecls lp [d1']

  let (lp',(ans',_),_w) = runTransform mempty doRmDecl
  -- putStrLn $ "log:" ++ intercalate "\n" _w
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return (ans,lp')
-- ---------------------------------------------------------------------

addHiding1 :: Changer
addHiding1 libdir ans (L l p) = do
  let doTransform = do
        l0 <- uniqueSrcSpanT
        l1 <- uniqueSrcSpanT
        l2 <- uniqueSrcSpanT
        let
          [L li imp1,imp2] = hsmodImports p
          n1 = L (noAnnSrcSpanDP0 l1) (mkVarUnqual (mkFastString "n1"))
          n2 = L (noAnnSrcSpanDP0 l2) (mkVarUnqual (mkFastString "n2"))
          v1 = L (addComma $ noAnnSrcSpanDP0 l1) (IEVar noExtField (L (noAnnSrcSpanDP0 l1) (IEName n1)))
          v2 = L (           noAnnSrcSpanDP0 l2) (IEVar noExtField (L (noAnnSrcSpanDP0 l2) (IEName n2)))
          impHiding = L (SrcSpanAnn (ApiAnn (Anchor (realSrcSpan l0) m0)
                                     (AnnList Nothing
                                              (Just (AddApiAnn AnnOpenP  d1))
                                              (Just (AddApiAnn AnnCloseP d0))
                                              [(AddApiAnn AnnHiding d1)]
                                              [])
                                       noCom) l0) [v1,v2]
          imp1' = imp1 { ideclHiding = Just (True,impHiding)}
          p' = p { hsmodImports = [L li imp1',imp2]}
        return (L l p')

  let (lp',(ans',_),_w) = runTransform mempty doTransform
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return (ans,lp')
-- ---------------------------------------------------------------------

addHiding2 :: Changer
addHiding2 libdir ans (L l p) = do
  let doTransform = do
        l1 <- uniqueSrcSpanT
        l2 <- uniqueSrcSpanT
        let
          [L li imp1] = hsmodImports p
          Just (_,L lh ns) = ideclHiding imp1
          lh' = (SrcSpanAnn (ApiAnn (Anchor (realSrcSpan (locA lh)) m0)
                                     (AnnList Nothing
                                              (Just (AddApiAnn AnnOpenP  d1))
                                              (Just (AddApiAnn AnnCloseP d0))
                                              [(AddApiAnn AnnHiding d1)]
                                              [])
                                       noCom) (locA lh))
          n1 = L (noAnnSrcSpanDP0 l1) (mkVarUnqual (mkFastString "n1"))
          n2 = L (noAnnSrcSpanDP0 l2) (mkVarUnqual (mkFastString "n2"))
          v1 = L (addComma $ noAnnSrcSpanDP0 l1) (IEVar noExtField (L (noAnnSrcSpanDP0 l1) (IEName n1)))
          v2 = L (           noAnnSrcSpanDP0 l2) (IEVar noExtField (L (noAnnSrcSpanDP0 l2) (IEName n2)))
          L ln n = last ns
          n' = L (addComma ln) n
          imp1' = imp1 { ideclHiding = Just (True,L lh' (init ns ++ [n',v1,v2]))}
          p' = p { hsmodImports = [L li imp1']}
        -- addSimpleAnnT n1        (DP (0,0)) [((G AnnVal),DP (0,0))]
        -- addSimpleAnnT v1        (DP (0,0)) [((G AnnComma),DP (0,0))]
        -- addSimpleAnnT n2        (DP (0,0)) [((G AnnVal),DP (0,0))]
        -- addTrailingCommaT (last ns)
        return (L l p')

  let (lp',(ans',_),_w) = runTransform mempty doTransform
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return (ans,lp')

-- ---------------------------------------------------------------------
-- Next section to be moved to the appropriate library

remove :: (Monoid t) => LocatedAn t a -> LocatedAn u b -> LocatedAn t a
remove (L (SrcSpanAnn ApiAnnNotUsed l) v) lb
  = L (SrcSpanAnn (ApiAnn (Anchor (realSrcSpan l) (DeletedAnchor $ locatedAnAnchor lb)) mempty noCom) l) v
remove (L (SrcSpanAnn (ApiAnn a an cs) l) v) lb
  = L (SrcSpanAnn (ApiAnn (Anchor (anchor a) (DeletedAnchor $ locatedAnAnchor lb)) an cs) l) v

locatedAnAnchor :: LocatedAn a t -> RealSrcSpan
locatedAnAnchor (L (SrcSpanAnn ApiAnnNotUsed l) _) = realSrcSpan l
locatedAnAnchor (L (SrcSpanAnn (ApiAnn a _ _) _) _) = anchor a

-- End of section to be moved to the appropriate library
-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- From SYB

-- | Apply transformation on each level of a tree.
--
-- Just like 'everything', this is stolen from SYB package.
everywhere :: (forall a. Data a => a -> a) -> (forall a. Data a => a -> a)
everywhere f = f . gmapT (everywhere f)

-- | Create generic transformation.
--
-- Another function stolen from SYB package.
mkT :: (Typeable a, Typeable b) => (b -> b) -> (a -> a)
mkT f = case cast f of
    Just f' -> f'
    Nothing -> id

-- ---------------------------------------------------------------------
