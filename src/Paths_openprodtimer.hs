module Paths_hsprodtimer
where
  
import System.FilePath
import System.Directory
  
getDataFileName :: FilePath -> IO FilePath
getDataFileName fn = do
  dir <- getCurrentDirectory
  return $ dir </> fn
