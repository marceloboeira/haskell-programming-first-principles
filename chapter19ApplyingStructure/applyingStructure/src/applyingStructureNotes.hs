{-# LANGUAGE OverloadedStrings #-}

-- import           Data.Map.Lazy
import           Data.Monoid
import           Data.Time.Clock
import           Web.Scotty
-- import           XMonad
-- import           XMonad.Actions.Volume


scottyExample :: IO ()
scottyExample = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    html (mconcat
      [ "<h1>Scotty, "
      , beam
      , " me up!</h1>"])

-- Concatenating connection parameters

-- 19.3 Functor

offsetCurrentTime :: NominalDiffTime -> IO UTCTime
offsetCurrentTime offset =
  fmap (addUTCTime (offset * 24 * 3600)) $
    getCurrentTime


