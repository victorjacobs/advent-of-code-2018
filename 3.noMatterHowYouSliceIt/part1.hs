{-# LANGUAGE QuasiQuotes #-}

import Text.Scanf
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type Position = (Int, Int)
type Size = (Int, Int)
type Claim = (Int, Position, Size)
type Fabric = Map.Map Position (Set.Set Int)

main :: IO ()
main = do
    content <- readFile "input.txt"
    print $ Map.size $ Map.filter (> 1) $ Map.map Set.size $ Prelude.foldl fillClaim (Map.empty) $ Prelude.map parseLine $ lines content

parseLine :: String -> Claim
parseLine str = (id, (x, y), (w, h))
    where Just (id :+ x :+ y :+ w :+ h :+ ()) = scanf [fmt|#%d @ %d,%d: %dx%d|] str

fillClaim :: Fabric -> Claim -> Fabric
fillClaim f c@(_, _, s) = fillClaimStep f c s

fillClaimStep :: Fabric -> Claim -> Size -> Fabric
fillClaimStep f _ (0, 1) = f
fillClaimStep f claim@(_, _, (w, _)) (0, currentH) = fillClaimStep f claim (w, nextH)
    where nextH = currentH - 1
fillClaimStep f claim@(id, (x, y), _) (w, h) = fillClaimStep nextFabric claim (nextW, h)
    where nextW = w - 1
          nextFabric = Map.alter (updateFabricCell id) (x + w - 1, y + h - 1) f

updateFabricCell :: Int -> Maybe (Set.Set Int) -> Maybe (Set.Set Int)
updateFabricCell id Nothing = Just $ Set.singleton id
updateFabricCell id (Just ids) = Just $ Set.insert id ids