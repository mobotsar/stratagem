module LibInternal (someFunc) where

someFunc :: () -> IO ()
someFunc () = putStrLn "LibInternal, checking in!"
