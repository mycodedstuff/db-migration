module Database.Migration.Utils.Common where

import qualified Data.Tree as DT

import Database.Migration.Types

{-
Preorder traversal of tree
-}
traverseTree :: (TreeNode -> IO [String]) -> DT.Tree TreeNode -> IO [String]
traverseTree fn (DT.Node value forest) = do
  result <- fn value
  results <- concat <$> mapM (traverseTree fn) forest
  return $ result ++ results
