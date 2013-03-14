{-# LANGUAGE BangPatterns,ForeignFunctionInterface,CPP #-}
import Graphics.UI.Gtk
import Control.Monad
import qualified Data.Map as M
import Data.Map ((!))
import Data.IORef
import Data.List
import Data.Char
import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Control.Exception (bracket_)
import Text.Printf
import Control.Monad.Trans (lift)
import System.Directory
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Token
import System.FilePath
import qualified Data.ByteString.Char8 as BS
import Text.Printf
import Prelude hiding (catch)
import Control.Exception
import System.IO.Error (ioeGetErrorString)
import Foreign.C.String
import Foreign.C.Types
import System.Environment
import Control.Concurrent
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
import Graphics.UI.AppIndicator
#endif

foreign import ccall unsafe "LaunchBrowser" launchBrowser' :: CString -> IO CInt
launchBrowser s = withCString s launchBrowser'

foreign import ccall unsafe "asynchronous_playback" asynchronous_playback' :: CString -> IO ()
asynchronousPlayback s = withCString s asynchronous_playback'

data WorkStatus = Work | ShortBreak | LongBreak deriving (Eq, Ord)
initialStatus = Work

getDataFileName n = do
  exe <- liftM (fromMaybe "./prodtimer") $ findExecutable "prodtimer"
  let dir  = dropFileName exe
  return $ dir </> n
  
statusToInternalName = [(Work, "work-time"), (ShortBreak, "short-break"), (LongBreak, "long-break")]
statusToDisplayName = [(Work, "Work"), (ShortBreak, "Short break"), (LongBreak, "Long break")]
statusToDefaultSound = [(Work, "sounds/WorkTime.wav"), (ShortBreak, "sounds/ShortBreak.wav"), (LongBreak, "sounds/LongBreak.wav")]
defaultStatusPeriods = [(Work, 25 * 60), (ShortBreak, 5 * 60), (LongBreak, 15 * 60)]
statusSuccession = M.fromList [(Work, ShortBreak)]
defaultLongBreakGap :: Int
defaultLongBreakGap = 60 * 60 * 2

updateFrequency = 1000

statusToInternalNameM = M.fromList statusToInternalName
statusToDisplayNameM = M.fromList $ statusToDisplayName


data HPTGlobals = HPTGlobals {
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
       statusIcon :: StatusIcon,
#else
       statusIcon :: AppIndicator,
#endif
       playSound :: String -> IO (),
       iconsByStatus :: M.Map WorkStatus FilePath,
       mainWindow :: Window,
       contextStatusItems :: [(WorkStatus, CheckMenuItem)],
       mainStatusSettings :: [(WorkStatus, (RadioButton, Entry, Entry))],
       longFrequency :: Entry,
       changingStatus :: IORef Bool,
       timeOfLastChange :: IORef UTCTime,
       lastLongBreak :: IORef UTCTime,
       statusNow :: IORef WorkStatus,
       configuration :: IORef (M.Map [String] String)
     }

charToken = (char '\\' >> ((char 'n' >> return '\n') <|> (char 'r' >> return '\r') <|> (char 't' >> return '\t') <|> (char '\\' >> return '\\') <|> (char '"' >> return '"') <|> (char '\'' >> return '\''))) <|> noneOf "\\\"\r\n"

fqnParser = (sepBy (many (noneOf ". =\t\r\n")) (char '.')) <|> (return [])
settingsParser :: Parser [([String], String)]
settingsParser =
  liftM catMaybes $ many $
    (char ';' >> many (noneOf "\r\n") >> return Nothing) <|>
    (do
      many (oneOf " \t")
      fqn <- fqnParser
      many (oneOf " \t")
      char '='
      many (oneOf " \t")
      char '"'
      value <- many charToken
      char '"'
      optional (char '\r')
      char '\n'
      return $ Just (fqn, value))

defaultSettings = do
  files <- mapM (\(s, n) -> do { fn <- getDataFileName n; return (["sound", statusToInternalNameM!s], fn) }) statusToDefaultSound
  return $ M.fromList $ (["longtrigger"], show defaultLongBreakGap):
    (
      (map (\(stat,per) ->
             let
               statname = statusToInternalNameM!stat
             in
              (["period", statname], show per)
           ) defaultStatusPeriods) ++ files)
                    
getSettingsFile = do
  appDir <- getAppUserDataDirectory "prodtimer"
  return $ appDir </> "settings.dat"

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

loadSettings :: IO (M.Map [String] String)
loadSettings = do
  sf <- getSettingsFile
  d <- catch (liftM eitherToMaybe $ parseFromFile settingsParser sf) ((const :: a -> IOException -> a) $ return Nothing)
  ds <- defaultSettings
  return $ case d of
    Nothing -> ds
    Just v -> M.union (M.fromList v) ds

saveSettings s = do
  let dat = flip concatMap (M.toList s) $ \(key, value) ->
        (showString (intercalate "." key) . showString "=" . shows value) "\n"
  appDir <- getAppUserDataDirectory "prodtimer"
  createDirectoryIfMissing True appDir
  sf <- getSettingsFile
  catch (writeFile sf dat >> return True) ((const :: a -> IOException -> a) $ return False)

main = do
  initGUI
  mainWithInit

mainWithInit =
  mainCore (\v -> asynchronousPlayback v >> return ())

mainCore playSound' = do
  
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  statusIcon' <- statusIconNew
#else
  iconPath <- (getDataFileName "icons")
  statusIcon' <- appIndicatorNewWithPath "prodtimer" "work-time" AppIndicatorCategoryApplicationStatus iconPath
#endif

  initialConfigurationValues <- loadSettings
  initialConfiguration <- newIORef initialConfigurationValues
  initialStatusNow <- newIORef initialStatus

  (mainWindow', mainStatusSettingWidgets', longFrequencyWidget', addApply) <- setupMainWindow initialConfiguration initialStatusNow playSound'
  (menu, itemList) <- makeStatusMenu mainWindow'

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  on statusIcon' statusIconPopupMenu (menuStatusPopup menu)
#else
  appIndicatorSetMenu statusIcon' menu
#endif
  iconsByStatus' <- liftM M.fromList (mapM (\(stat, p) -> liftM ((,)stat) $ getDataFileName ("icons/" ++ p ++ ".png")) statusToInternalName)

  changingStatus' <- newIORef False
  timeNow <- getCurrentTime
  initialLastChange <- newIORef timeNow
  initialLastLongBreak <- newIORef timeNow

  let hptg = HPTGlobals { statusIcon = statusIcon', mainWindow = mainWindow', contextStatusItems = itemList, mainStatusSettings = mainStatusSettingWidgets', longFrequency = longFrequencyWidget', iconsByStatus = iconsByStatus', changingStatus = changingStatus', timeOfLastChange = initialLastChange, lastLongBreak = initialLastLongBreak, statusNow = initialStatusNow, configuration = initialConfiguration, playSound = playSound' }
  timeoutAdd (doTimerTick hptg) updateFrequency

  addApply $ \status -> changeActiveStatus hptg status

  changeActiveStatus hptg initialStatus
  forM itemList $ \(stat, item) ->
    after item menuItemActivate (changeActiveStatus hptg stat)

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  on statusIcon' statusIconActivate $ do
    widgetShow mainWindow'
    cur <- readIORef (statusNow hptg)
    windowSetIconFromFile (mainWindow hptg) ((iconsByStatus hptg)!cur)

  statusIconSetVisible statusIcon' True
#else
  appIndicatorSetStatus statusIcon' AppIndicatorStatusActive
#endif
  mainGUI

showHMS s =
  let
    (hv, mv, sv) = (s `div` 3600, (s `div` 60) `mod` 60, s `mod` 60)
  in
   if hv == 0
   then
     if mv == 0
     then
       show sv
     else
       printf "%02d:%02d" mv sv
   else
     printf "%02d:%02d:%02d" hv mv sv

hmsParsec !a1 !a2 = (
  do
    d <- liftM digitToInt digit
    hmsParsec a1 (a2 * 10 + d)) <|> (
  do
    char ':'
    hmsParsec (a1 * 60 + a2) 0) <|> (return $ a1 * 60 + a2)
                    
readDefault def s = case (reads s)
                    of
                      [] -> def
                      (v, []):_ -> v
                      _ -> def

parseHMS s = either (const $ error "parseHMS Left - should be impossible")
                    id $ parse (hmsParsec 0 0) "HMS" s

setupMainWindow settings statusF playSound = do
  w <- windowNew
  on w deleteEvent $ lift (widgetHide w) >> return True

  sentinel <- radioButtonNew
  table <- tableNew ((length statusToInternalName) + 1) 4 False
  mapM (\(v, c) -> do
          lab <- labelNew (Just v)
          tableAttachDefaults table lab c (c+1) 0 1)
    [("Active mode:", 0), ("Duration (h:m:s)", 1), ("Minimum gap (h:m:s):", 2),
     ("Sound (if any)", 3)]
  
  longFreqWidget <- entryNew
  set longFreqWidget [ entryAlignment := 1 ]

  radioItems <- forM (zip3 [1..] statusToDisplayName statusToInternalName) $ \(row, (status, name), (_, iname)) -> do
    radio <- radioButtonNewWithLabelFromWidget sentinel name
    entry <- entryNew
    set entry [ entryAlignment := 1 ]
    soundbox <- hBoxNew False 1
    soundentry <- entryNew
    soundbrowse <- buttonNewWithLabel "Browse..."
    soundbrowse `on` buttonActivated $ do
      fc <- fileChooserDialogNew (Just "Pick a sound") (Just w)
              FileChooserActionOpen [("OK", ResponseOk), ("Cancel", ResponseCancel)]
      cur <- get soundentry entryText
      fileChooserSetFilename fc cur
      soundFilter <- fileFilterNew
      set soundFilter [ fileFilterName := "Sound files"]
      mapM (fileFilterAddPattern soundFilter) ["*.wav", "*.au", "*.snd", "*.raw"]
      fileChooserAddFilter fc soundFilter
      fc `on` response $ \resp -> do
        case resp
          of
            ResponseOk -> do
              mname <- fileChooserGetFilename fc
              case mname of
                Just name ->
                  set soundentry [ entryText := name ]
                Nothing -> return ()
            _ -> return ()
        widgetDestroy fc
      widgetShowAll fc
      return ()
        
    soundtest <- buttonNewWithLabel "Test"
    soundtest `on` buttonActivated $ do
      sf <- get soundentry entryText
      playSound sf
    boxPackStart soundbox soundentry PackNatural 0
    boxPackEnd soundbox soundtest PackNatural 0
    boxPackEnd soundbox soundbrowse PackNatural 0
    tableAttachDefaults table radio 0 1 row (row+1)
    tableAttachDefaults table entry 1 2 row (row+1)
    tableAttachDefaults table soundbox 3 4 row (row+1)
    if status == LongBreak
      then tableAttachDefaults table longFreqWidget 2 3 row (row+1)
      else
        do l <- labelNew (Just "No")
           tableAttachDefaults table l 2 3 row (row+1)
    return (status, (radio, entry, soundentry))

  vb <- vBoxNew False 1
  
  containerAdd vb table

  hb <- hBoxNew False 1
  applyButton <- buttonNewWithLabel "Apply"
  resetButton <- buttonNewWithLabel "Reset to saved"
  defaultButton <- buttonNewWithLabel "Reset to defaults"
  spacer <- labelNew Nothing
  boxPackEnd hb spacer PackNatural 5
  boxPackEnd hb defaultButton PackNatural 0
  boxPackEnd hb resetButton PackNatural 0
  boxPackEnd hb applyButton PackNatural 0
  boxPackEnd vb hb PackNatural 0

  containerAdd w vb

  let populateFields = do
        cur <- readIORef statusF
        settingsNow <- readIORef settings
        forM_ radioItems $ \(status, (radio, entry, soundentry)) -> do
          when (status == cur) $ set radio [toggleButtonActive := True]
          let intName = statusToInternalNameM!status
          set entry [ entryText :=
                      showHMS (readDefault (600 :: Int) $ settingsNow!["period", intName]) ]
          set soundentry [ entryText := settingsNow!["sound", intName]]
        set longFreqWidget [ entryText := showHMS (readDefault (3600 :: Int) $ settingsNow!["longtrigger"]) ]
  populateFields
  
  resetButton `on` buttonActivated $ populateFields
  defaultButton `on` buttonActivated $ do
    cd <- messageDialogNew (Just w) [DialogModal, DialogDestroyWithParent] MessageQuestion ButtonsYesNo "Do you really want to reset all settings to the defaults, losing the settings you have entered permanently?"
    cd `on` response $ \resp -> do
      case resp
        of
          ResponseYes -> do
            ds <- defaultSettings            
            writeIORef settings ds
            saveSettings ds
            populateFields
          ResponseNo -> return ()
      widgetDestroy cd
    widgetShowAll cd

  let addApply changeActive = applyButton `on` buttonActivated $ do
        lft <- get longFreqWidget entryText
        modifyIORef settings (M.insert ["longtrigger"] (show $ parseHMS lft))
    
        forM_ radioItems $ \(status, (radio, entry, soundentry)) -> do
          cur <- readIORef statusF
          tba <- get radio toggleButtonActive
          when (tba && cur /= status) (changeActive status)
          secs <- liftM parseHMS $ get entry entryText
          sound <- get soundentry entryText
          let intName = statusToInternalNameM!status
          modifyIORef settings (M.insert ["period", intName] (show secs))
          modifyIORef settings (M.insert ["sound", intName] sound)
        settingsNow <- readIORef settings
        saveSettings settingsNow
        populateFields
        
  widgetShowAll w
  widgetHide w
  return (w, radioItems, longFreqWidget, addApply)

menuStatusPopup menu maybeButton timestamp = do
  menuPopup menu (liftM (flip (,) timestamp) maybeButton)

makeStatusMenu mainWindow = do
  m <- menuNew
  statItems <- forM statusToDisplayName $ \(status, name) -> do
    it <- checkMenuItemNewWithLabel name
    menuShellAppend m it
    return (status, it)
  separatorMenuItemNew >>= menuShellAppend m
  
#if !defined(mingw32_HOST_OS) && !defined(__MINGW32__)
  raiseMI <- menuItemNewWithLabel "Settings"
  raiseMI `on` menuItemActivate $ widgetShow mainWindow
  menuShellAppend m raiseMI
#endif
  
  aboutMI <- menuItemNewWithLabel "About"
  menuShellAppend m aboutMI
  aboutMI `on` menuItemActivate $ do
    aboutd <- aboutDialogNew
    aboutd `on` response $ \_ -> widgetDestroy aboutd
    fn <- getDataFileName "COPYING"
    licenseText <- readFile fn
    set aboutd [ aboutDialogProgramName := "Lanthaps ProdTimer",
                 aboutDialogName := "Lanthaps ProdTimer",
                 aboutDialogVersion := "2012",
                 aboutDialogCopyright := "Copyright (C) 2012 Lanthaps Limited",
                 aboutDialogComments := "Boost your productivity by concentrating for short periods and having regular breaks",
                 aboutDialogWrapLicense := True,
                 aboutDialogLicense := Just licenseText,
                 aboutDialogWebsite := "http://www.lanthaps.com/prodtimer" ]
    widgetShowAll aboutd
  
  exitMI <- menuItemNewWithLabel "Exit"
  menuShellAppend m exitMI
  
  on exitMI menuItemActivate mainQuit
  widgetShowAll m
  return (m, statItems)

computeNextStatus hptg = do
  cur <- readIORef (statusNow hptg)
  if cur /= Work
    then
      return Work
    else
      do
        lastLong <- readIORef (lastLongBreak hptg)
        timeNow <- getCurrentTime
        let timeSince = diffUTCTime timeNow lastLong
        conf <- readIORef $ configuration hptg
        return $ if timeSince < (fromIntegral $ readDefault (3600 :: Int) (conf!["longtrigger"])) then ShortBreak else LongBreak

prettyDisplayDiffTime :: NominalDiffTime -> String
prettyDisplayDiffTime diff =
  let
    h :: Int
    h = floor (diff / 3600)
    m :: Int
    m = (floor (diff / 60)) `mod` 60
    s :: Int
    s = (floor diff) `mod` 60
  in
    printf "%02d:%02d:%02d" h m s

computeEndTime hptg = do
  curConf <- readIORef (configuration hptg)
  curStat <- readIORef (statusNow hptg)
  let curStatName = statusToInternalNameM!curStat
  let curStatPer = readDefault 600 $ curConf!["period", curStatName]
  curStatStart <- readIORef (timeOfLastChange hptg)
  return $ addUTCTime (fromIntegral curStatPer) curStatStart

doTimerTick hptg = do
  timeNow <- getCurrentTime
  endTime <- computeEndTime hptg
  let timeLeft = diffUTCTime endTime timeNow
  let prettyTimeLeft = prettyDisplayDiffTime timeLeft
  statusNow <- readIORef (statusNow hptg)
  let statusName = (statusToDisplayNameM!statusNow)
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  statusIconSetTooltip (statusIcon hptg) prettyTimeLeft
#else
  set (statusIcon hptg) [appIndicatorLabel := Just prettyTimeLeft ]
#endif
  set (mainWindow hptg) [ windowTitle := prettyTimeLeft ++ " - " ++ statusName ++ " - Productivity Timer"]

  when (timeLeft <= 0) $ do
    computeNextStatus hptg >>= changeActiveStatus hptg
    windowPresent (mainWindow hptg)

  return True

protectFromReentrancy r f = do
  v <- readIORef r
  if v
    then
      return ()
    else
      bracket_ (writeIORef r True) (writeIORef r False) f

changeActiveStatus hptg newStatus = protectFromReentrancy (changingStatus hptg) $ do
  let statusName = (statusToDisplayNameM!newStatus)
  let statusInternal = (statusToInternalNameM!newStatus)
  set (mainWindow hptg) [ windowTitle := statusName ++ " - Productivity Timer" ]
  let iconFile = (iconsByStatus hptg)!newStatus
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  statusIconSetFromFile (statusIcon hptg) iconFile
#else
  let iconName = statusToInternalNameM!newStatus
  set (statusIcon hptg) [ appIndicatorIconName := Just iconName ]
#endif
  windowSetIconFromFile (mainWindow hptg) iconFile
  forM_ (contextStatusItems hptg) $ \(stat, cmi) -> do
    set cmi [checkMenuItemActive := stat == newStatus]
  forM_ (mainStatusSettings hptg) $ \(stat, (radio, _, _)) -> do
    set radio [toggleButtonActive := (stat == newStatus)]
  timeNow <- getCurrentTime
  writeIORef (timeOfLastChange hptg) timeNow
  writeIORef (statusNow hptg) newStatus
  when (newStatus == LongBreak) $ writeIORef (lastLongBreak hptg) timeNow
  conf <- readIORef (configuration hptg)
  playSound hptg (conf!["sound", statusInternal])
