module Program.FetchDay where

import Advent
import Data.Text
import Servant.Client
import Control.Monad.Except

getInput :: Int -> ExceptT String IO String
getInput n = do
  opts <- liftIO $ readFile "options"
  case Prelude.lines opts of
    (year : session : _) -> do
      r <- liftIO $ runAoC (AoCOpts session (read year) (Just "cache") False 3000000) $ AoCInput (mkDay_ (toEnum n))
      case r of
        Left e -> throwError $ show e
        Right a -> return $ unpack a
    _ -> throwError "failed to load opts first line is year then the second year is the session key"
