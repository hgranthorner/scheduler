module Lib
  ( executeCommand,
  )
where

import Data.Maybe (fromJust)
import Data.Time (UTCTime, defaultTimeLocale, parseTimeM)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)

data Activity
  = DueActivity {name :: String, dueDate :: UTCTime}
  | SpanActivity {name :: String, startDate :: UTCTime, endDate :: UTCTime}
  deriving (Show, Eq, Read)

executeCommand :: [String] -> IO ()
executeCommand ["add", activityName, activityDueDate] = do
  activities <- maybeReadActivities
  case maybeParseUTCTime "%Y-%m-%d %H:%M" activityDueDate of
    Just date -> writeToCache $ DueActivity {name = activityName, dueDate = date} : fromJust activities
    Nothing -> putStrLn "Failed to parse date"

executeCommand ["add", activityName, activityStartDate, activityEndDate] = do
  activities <- maybeReadActivities
  case (maybeParseUTCTime "%Y-%m-%d %H:%M" activityStartDate, maybeParseUTCTime "%Y-%m-%d" activityEndDate) of
    (Just start, Just end) -> writeToCache $ SpanActivity {name = activityName, startDate = start, endDate = end} : fromJust activities
    _ -> putStrLn "Failed to parse date"

executeCommand ["get"] = do
  maybeActivities <- maybeReadActivities
  case maybeActivities of
    Just activities -> mapM_ (putStrLn . displayActivity) activities
    Nothing -> putStrLn "Failed to read activities"

executeCommand ["create-cache"] = do
  writeFile "cache.txt" "[]"

executeCommand _ = putStrLn "Unknown command"

maybeReadActivities :: IO (Maybe [Activity])
maybeReadActivities = do
  contents <- TIO.readFile "cache.txt"
  return (readMaybe (T.unpack contents) :: Maybe [Activity])

writeToCache :: Show a => [a] -> IO ()
writeToCache = writeFile "cache.txt" . show

maybeParseUTCTime :: String -> String -> Maybe UTCTime
maybeParseUTCTime format str = parseTimeM True defaultTimeLocale format str :: Maybe UTCTime

displayActivity :: Activity -> String
displayActivity a =
  case a of
    DueActivity {name=n, dueDate=d} ->
      "Activity: " ++ n ++ "\n" ++
      "Due: " ++ show d ++ "\n"
      
    SpanActivity {name=n, startDate=s, endDate=e} ->
      "Activity: " ++ n ++ "\n" ++
      "Start: " ++ show s ++ "\n" ++
      "end: " ++ show e ++ "\n"


throwAway :: a -> ()
throwAway _ = ()