import ChessBoard
import Move
import ChessRules
import Color
import Decisions
import System.IO (hFlush, stdout)

depth = 5

main :: IO ()
main = do
   print initialPosition
   prompt initialPosition

prompt :: ChessBoard -> IO ()
prompt cb = do
   putStr "input> "
   hFlush stdout
   cmd <- getLine
   execute cmd cb

execute :: String -> ChessBoard -> IO ()
execute "quit" _ = return ()
execute "eval" cb = do
   print $ advantage White depth cb
   prompt cb
execute str cb =
   case readMaybe str of
      Nothing -> do
         putStrLn "Invalid input."
         prompt cb
      Just m -> if not $ legal cb m
                   then do
                      putStrLn "Illegal move."
                      prompt cb
                   else do
                      let cb' = doMove cb m
                      print cb'
                      putStrLn "I'm thinking..."
                      newCb <- play $ doMove cb m
                      prompt newCb

play :: ChessBoard -> IO ChessBoard
play initial = do
   let myMove = bestMove depth initial
   let final = doMove initial myMove
   print final
   putStrLn $ "My move: " ++ show myMove
   return final

readMaybe :: (Read a) => String -> Maybe a
readMaybe s = case reads s of
                   [(x, "")] -> Just x
                   _ -> Nothing

