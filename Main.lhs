> module Main where
>
> import Paths_CheatSheet
> import CheatSheet
>
> main = do
>  pdfLoc <- getDataFileName "CheatSheet.pdf"
>  putStrLn $ "Your cheatsheet is at: " ++ pdfLoc
>
