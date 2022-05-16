module Shoggoth.Prelude.FilePath
  ( regularFile,
    (~~*?),
    (==*?),
    extensions,
    module Find,
  )
where

import System.FilePath.Find as Find

--------------------------------------------------------------------------------
-- Find files
--------------------------------------------------------------------------------

regularFile :: FilterPredicate
regularFile = fileType ==? RegularFile

(~~*?) :: FindClause FilePath -> [GlobPattern] -> FilterPredicate
file ~~*? globPatterns = foldr (\globPattern -> (file ~~? globPattern ||?)) (return False) globPatterns

(==*?) :: FindClause FilePath -> [GlobPattern] -> FilterPredicate
file ==*? globPatterns = foldr (\globPattern -> (file ==? globPattern ||?)) (return False) globPatterns

extensions :: FindClause FilePath
extensions = takeExtensions <$> fileName
