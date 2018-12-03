import qualified Data.Map.Strict as Map

type Occurrences = Map.Map Char Int

main :: IO ()
main = do
    content <- readFile "input.txt"
    print $ fileChecksum $ lines content

fileChecksum :: [String] -> Int
fileChecksum lines = totalChecksum
    where allChecksums = map checksum lines
          doubles = Prelude.length $ Prelude.filter fst allChecksums
          triples = Prelude.length $ Prelude.filter snd allChecksums
          totalChecksum = doubles * triples

checksum :: String -> (Bool, Bool)
checksum str = (hasDoubleLetter, hasTripleLetter)
    where hasDoubleLetter = stringHasCharacterWithCount str 2
          hasTripleLetter = stringHasCharacterWithCount str 3

stringHasCharacterWithCount :: String -> Int -> Bool
stringHasCharacterWithCount str count = numberOfCharsWithCount /= 0
    where numberOfCharsWithCount = Map.size $ Map.filter (== count) $ occurrences str (Map.empty)

occurrences :: String -> Occurrences -> Occurrences
occurrences [] o = o
occurrences (x:xs) o = Map.alter countOccurrence x (occurrences xs o)

countOccurrence :: Maybe Int -> Maybe Int
countOccurrence Nothing = Just 1
countOccurrence (Just c) = Just (c + 1)