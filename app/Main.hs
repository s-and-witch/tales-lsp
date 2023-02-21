{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Main  where

import Language.LSP.Server
import qualified Language.LSP.Types as LSP hiding (TextDocumentSyncClientCapabilities(..))
import Language.LSP.VFS
import Control.Monad.IO.Class
import Control.Lens hiding (Iso)
import qualified Language.LSP.Types.Lens as L
import qualified Data.Text as T
import Control.Exception
import Data.Maybe
import Data.Char
import System.FilePath
import Colog.Core
import Language.LSP.Logging

main :: IO Int
main = runServer definition

logger :: LogAction (LspM ()) (WithSeverity T.Text)
logger = defaultClientLogger

definition :: ServerDefinition ()
definition = ServerDefinition
  {defaultConfig  = ()
  , onConfigurationChange = \ _ _ -> Right ()
  , doInitialize = \ lc _ -> pure (Right lc)
  , staticHandlers = mconcat
    [ notificationHandler LSP.SInitialized $ \_ -> pure ()
    , requestHandler LSP.STextDocumentDefinition searchDefinition
    , notificationHandler LSP.STextDocumentDidChange $ \_ -> pure ()
    , notificationHandler LSP.STextDocumentDidOpen $ \_ -> pure ()
    ]
  , interpretHandler = \lc -> Iso (runLspT lc) liftIO
  , options = defaultOptions
    { textDocumentSync = Just LSP.TextDocumentSyncOptions
      { LSP._openClose = Just True
      , LSP._change = Just LSP.TdSyncFull
      , LSP._willSave = Nothing
      , LSP._willSaveWaitUntil = Nothing
      , LSP._save = Nothing
      }
    }
  }

searchDefinition
  :: LSP.RequestMessage LSP.TextDocumentDefinition
  -> (Either LSP.ResponseError (LSP.Location LSP.|? (LSP.List LSP.Location LSP.|? LSP.List LSP.LocationLink)) -> LspM () ())
  -> LspM () ()
searchDefinition req callback = do
  file <- unwrap
    "no such file"
    =<< getVirtualFile ntd
  let fileContent = virtualFileText file
  pos <-
    unwrap
    "can't convert position from UTF-16 to UTF-8"
    $ positionToCodePointPosition file pos'
  let path = getRequestedPath fileContent pos
  requestedPath <- unwrap
    "can't parse path"
    (parsePath path)
  case requestedPath of
    LocalProp pr ->
      callback (Right (makeResponse ntd (getMatchingPositions file pr)))
    AbsPath fp pr -> do
      root <- unwrap
        "can't get root path"
        =<< getRootPath
      locs <- resolveLocation (LSP.toNormalizedUri $ LSP.filePathToUri (root </> fp)) pr
      callback (Right (makeSumFromLocList locs))
    RelPath fp pr -> do
      origin <- unwrap
        "unable to convert uri to file path"
        (LSP.uriToFilePath td)
      locs <-
        resolveLocation
        (LSP.toNormalizedUri $ LSP.filePathToUri (origin `replaceFileName` (dropDrive fp)))
        pr
      callback (Right (makeSumFromLocList locs))

  where
    searchFile = req ^. L.params
    ntd = LSP.toNormalizedUri td
    td  = (searchFile ^. L.textDocument . L.uri)
    pos' = searchFile ^. L.position

    unwrap _ (Just a)     = pure a
    unwrap reason Nothing = throwErr reason

    throwErr reason = do
      callback (Left (LSP.ResponseError LSP.RequestFailed reason Nothing))
      liftIO (throwIO (ErrorCall "hopefully unreachable"))

resolveLocation :: LSP.NormalizedUri -> Maybe PropertyReq -> LspM () [LSP.Location]
resolveLocation nuri = \case
  Nothing -> pure [ LSP.Location uri nullRange ]
  Just req -> do
    file <- getVirtualFile nuri
    case file of
      Nothing -> pure [ LSP.Location uri nullRange ]
      Just f -> do -- This path actually doesn't fire at all
        case getMatchingPositions f req of
          [] -> pure [ LSP.Location uri nullRange ]
          ranges -> pure $ map (LSP.Location uri) ranges
  where
    uri = LSP.fromNormalizedUri nuri
    nullRange = LSP.Range (LSP.Position 0 0) (LSP.Position 0 1)

makeSumFromLocList :: [LSP.Location] -> LSP.Location LSP.|? (LSP.List LSP.Location LSP.|? LSP.List LSP.LocationLink)
makeSumFromLocList [loc] = LSP.InL loc
makeSumFromLocList locs = LSP.InR (LSP.InL . LSP.List $ locs)

makeResponse :: LSP.NormalizedUri -> [LSP.Range] -> LSP.Location LSP.|? (LSP.List LSP.Location LSP.|? LSP.List LSP.LocationLink)
makeResponse nuri ranges = makeSumFromLocList $ map (LSP.Location uri) ranges
  where
    uri = LSP.fromNormalizedUri nuri

getMatchingPositions :: VirtualFile -> PropertyReq -> [LSP.Range]
getMatchingPositions vf pr = map makeRange $ mapMaybe (codePointPositionToPosition vf) positions
  where
    positions = concatMap (getMatchingCodePoints pr) (parsePropMap fileContent)
    fileContent = virtualFileText vf

makeRange :: LSP.Position -> LSP.Range
makeRange pos = LSP.Range pos (pos & L.character +~ 1)

parsePath :: T.Text -> Maybe Path
parsePath (T.break (== '!') -> (path, T.toLower -> props))
  | T.null path
  , T.null props = Nothing
  | T.null props = parseFilePath path Nothing
  | T.null path  = Just $ LocalProp (parseProps props)
  | otherwise    = parseFilePath path (Just (parseProps props))

parseFilePath :: T.Text -> Maybe PropertyReq -> Maybe Path
parseFilePath (T.unpack -> t)
  | ('/' : path) <- t = Just . AbsPath path
  | ('.' : path) <- t = Just . RelPath path
  | otherwise = const Nothing

parseProps :: T.Text -> PropertyReq
parseProps = go . T.splitOn "." . T.drop 1
  where
    go [] = None
    go ("" : props) = Wildcard $ go props
    go (prop : props) = Steps (T.toLower prop) [] $ go props


getRequestedPath :: T.Text -> CodePointPosition -> T.Text
getRequestedPath file pos = T.take (endPath - startPath) . T.drop startPath $ l
  where
    l = T.lines file !! fromIntegral (pos ^. line)
    ind = fromIntegral (pos ^. character)
    endPath = goAhead ind
    startPath = goBack ind

    goAhead i | i == T.length l = i
    goAhead i | isSpace (T.index l i) = i
    goAhead i = goAhead (i + 1)

    goBack 0 = 0
    goBack i | isSpace (T.index l i) = i + 1
    goBack i = goBack (i - 1)

data PropertyReq = None | Steps T.Text [T.Text] PropertyReq | Wildcard PropertyReq

data PropertyMap = PropertyStep T.Text CodePointPosition [PropertyMap] deriving Show

normalizePropertyReq :: PropertyReq -> PropertyReq
normalizePropertyReq (Wildcard (Wildcard pr)) = Wildcard (normalizePropertyReq pr)
normalizePropertyReq (Wildcard pr) = Wildcard (normalizePropertyReq pr)

normalizePropertyReq (Steps p ps (Steps p' ps' pr)) = Steps p (ps <> (p' : ps')) (normalizePropertyReq pr)
normalizePropertyReq (Steps p ps pr) = Steps p ps (normalizePropertyReq pr)

normalizePropertyReq None = None

minLength :: PropertyReq -> Int
minLength None = 0
minLength (Steps _ ps pr) = minLength pr + (Prelude.length ps + 1)
minLength (Wildcard pr) = minLength pr

removeLessThen :: Int -> PropertyMap -> Maybe PropertyMap
removeLessThen n (PropertyStep _ _ [])  | n > 1  = Nothing
removeLessThen n p@(PropertyStep _ _ _) | n <= 1 = Just p
removeLessThen n (PropertyStep pr pos nodes) =
  if any (isJust) nodes' then Just $ PropertyStep pr pos (catMaybes nodes') else Nothing
  where
    nodes' = map (removeLessThen (n - 1)) nodes

allCodePoints :: PropertyMap -> [CodePointPosition]
allCodePoints (PropertyStep _ pos nodes) = pos : concatMap allCodePoints nodes

getMatchingCodePoints :: PropertyReq -> PropertyMap -> [CodePointPosition]
getMatchingCodePoints r m' = case avil of
  Nothing -> []
  Just m -> searchMatchings nr m
  where
    nr = normalizePropertyReq r
    avil = removeLessThen (minLength nr) m'

    searchMatchings (Steps prop' [] None) (PropertyStep prop pos _)
      | prop == prop' = [pos]
    searchMatchings (Steps prop' (p:ps) pr) (PropertyStep prop _ nodes)
      | prop == prop' = concatMap (searchMatchings (Steps p ps pr)) nodes
    searchMatchings (Steps prop' [] pr) (PropertyStep prop _ nodes)
      | prop == prop' = concatMap (searchMatchings pr) nodes
    searchMatchings (Steps _ _ _) _
        = []

    searchMatchings (Wildcard None) pm
      = allCodePoints pm
    searchMatchings (Wildcard pr@(Steps prop' _ _)) pm@(PropertyStep prop _ _)
      | prop == prop' = searchMatchings pr pm
    searchMatchings pr@(Wildcard _) (PropertyStep _ _ nodes)
      = concatMap (searchMatchings pr) nodes

    searchMatchings None _ = error "searchMatchings: impossible"

parsePropMap :: T.Text -> [PropertyMap]
parsePropMap
  = unflat
  . map (\(i, (c, p)) -> (CodePointPosition i c, p))
  . mapMaybe (\(i, l) -> (i,) <$> getProp l)
  . zip [0..]
  . T.lines
  where
    getProp :: T.Text -> Maybe (LSP.UInt, T.Text)
    getProp t
      | isJust ind
      , notAComment
      , validProp = Just (wsCount, T.toLower (T.replace " " "-" res))
      where
        ind = T.findIndex (== ':') t
        i = fromJust ind
        raw = T.take i t
        validProp = T.takeEnd 1 raw /= T.pack " "
        (ws, res) = T.break (not . isSpace) raw
        wsCount = fromIntegral $ T.length ws
        notAComment = isNothing . T.find (== '#') $ res

    getProp _ = Nothing

    unflat :: [(CodePointPosition, T.Text)] -> [PropertyMap]
    unflat [] = []
    unflat ( (pos, prop) : props ) = PropertyStep prop pos (unflat nestedProps) : unflat unnestedProps
      where
        (nestedProps, unnestedProps) = break (\(pos', _) -> pos ^. character >= pos' ^. character) props

pPrintMaps :: [PropertyMap] -> T.Text
pPrintMaps  = T.unlines . map (pPrintMap 0)
  where
    ws x = T.replicate x " "
    pPrintMap x (PropertyStep n _ []) = ws x <> n
    pPrintMap x (PropertyStep n _ nodes) = ws x <> n <> ":\n" <> T.unlines (map (pPrintMap (x + 2)) nodes)

data Path
  = AbsPath FilePath (Maybe PropertyReq)
  | RelPath FilePath (Maybe PropertyReq)
  | LocalProp PropertyReq
