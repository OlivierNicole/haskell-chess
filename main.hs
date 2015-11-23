import ChessBoard
import Move
import ChessRules
import Color
import Decisions
import System.IO (hFlush, stdout)

depth = 4

main :: IO ()
main = do
   print initialPosition
   prompt initialPosition

prompt :: ChessBoard -> IO ()
prompt cb = do
   putStr "input> "
   hFlush stdout
   cmd <- getLine
   maybeCb' <- execute cmd cb
   maybe (return ()) prompt maybeCb'

execute :: String -> ChessBoard -> IO (Maybe ChessBoard)
execute "quit" _ = putStrLn "Bye!" >> return Nothing
execute "eval" cb = do
   print $ advantage White depth cb
   return $ Just cb
execute str cb = case readMaybe str of
   Nothing -> do putStrLn "Invalid input."
                 return $ Just cb
   Just m -> if not $ legal cb m
             then do
                putStrLn "Illegal move."
                return $ Just cb
             else do
                let cb' = doMove cb m
                print cb'
                putStrLn "I'm thinking..."
                let myMove = bestMove depth cb'
                    newCb  = doMove cb' myMove
                print newCb
                putStrLn $ "My move: " ++ show myMove
                return $ Just newCb

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                   [(x, "")] -> Just x
                   _ -> Nothing

