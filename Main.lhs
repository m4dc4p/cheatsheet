> module Main where
>
> import Paths_CheatSheet
> import CheatSheet
>
> main = do
>  pdfLoc <- getDataFileName "CheatSheet.pdf"
>  lhsLoc <- getDataFileName "CheatSheet.lhs"
>  putStrLn $ "Your cheatsheet is at: " ++ pdfLoc
>  putStrLn $ "Its literate source is at: " ++ lhsLoc
