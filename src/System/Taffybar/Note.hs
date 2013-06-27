--------------------------------------------------------------------------------
-- Module      : System.Taffybar.Note
-- Copyright   : (c) Eleazar Díaz Delgado
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Eleazar Díaz Delgado
-- Stability   : Unstable
-- Portability : None
--
-- | Simple manager for note
--
--------------------------------------------------------------------------------

module System.Taffybar.Note where

import Graphics.UI.Gtk
import System.Directory

type Time = Double

data Note = Note String -- ^ Simple note with string to show
          | Timer String Time -- ^ Note but limit of time
          | DoIn String Time (IO ()) -- ^ Note do any to finish time

data ConfigNote = ConfigNote { file :: IO FilePath}
-- | default value
defaultNote :: ConfigNote
defaultNote = ConfigNote { file = getHomeDirectory ++ "/.list.tasks"}                     

showNotes :: IO Window
showNotes = do
        container <- windowNew
        list <- TODO: List notes and options over them
        containerAdd con lis
        -- update the date on show
        _ <- onShow container $ liftIO $ resetCalendarDate cal
        -- prevent calendar from being destroyed, it can be only hidden:
        _ <- on container deleteEvent $ do
                liftIO (widgetHideAll container)
                return True
        return container

toggle :: WidgetClass w => w -> Window -> IO Bool
toggle w c = do
        vis <- get c widgetVisible
        if vis 
        then widgetHideAll c
        else do attachPopup w "Notes" c
                displayPopup w c
        return True

getNoTasks cfg = do
        str <- readFile $ file cfg
        return $ split "\n" str

noteNew :: ClockConfig -> String -> Double -> IO Widget
noteNew cfg fmt updateSeconds = do
  l    <- pollingLabelNew ""  (getCurrentTime' timeLocale fmt timeZone)
  ebox <- eventBoxNew
  containerAdd ebox l
  eventBoxSetVisibleWindow ebox False
  cal <- makeCalendar
  _ <- on ebox buttonPressEvent $ onClick [SingleClick] (toggleCalendar l cal)
  widgetShowAll ebox
  return (toWidget ebox)