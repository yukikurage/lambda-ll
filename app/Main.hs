module Main where

import Lib

main :: IO ()
main = do
  putStrLn "Running Danos-Regnier Contractibility Check..."

  let
    checks =
      [
        ( "User Example (Valid)"
        ,
          [ Strong ["x", "y", "z"]
          , Weak "w" ["x", "y"]
          , Weak "res" ["w", "z"]
          ]
        , True
        )
      ,
        ( "Cycle Error Test"
        ,
          [ Strong ["x", "y"]
          , Strong ["x", "y"] -- Redundant merge? Or cycle loop if logic is strictly "already same".
          -- My implementation: "if rootX == rootY then Cycle Error".
          -- [x,y] merges x-y.
          -- Next [x,y]: find x -> rootY (or vice versa), find y -> rootY. Same. Error.
          ]
        , False
        )
      ,
        ( "Disconnected Test"
        ,
          [ Weak "w" ["x"] -- x is isolated. w becomes x-group.
          , Weak "z" ["y"] -- y is isolated. z becomes y-group.
          -- Result: 2 components {w,x} and {z,y}.
          ]
        , False
        )
      ]

  mapM_ runTest checks

runTest :: (String, [Link], Bool) -> IO ()
runTest (name, links, expectedValid) = do
  putStrLn $ "--- " ++ name ++ " ---"
  let
    result = validateContractibility links
  case result of
    Right () -> do
      if expectedValid
        then putStrLn "Result: VALID (Expected)"
        else putStrLn "Result: VALID (Unexpected!)"
    Left err -> do
      if not expectedValid
        then putStrLn $ "Result: INVALID (Expected) - " ++ err
        else putStrLn $ "Result: INVALID (Unexpected!) - " ++ err
  putStrLn ""
