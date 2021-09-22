module Utils where

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:xs) = Just x

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe (x:[]) = Just x
lastMaybe (x:xs) = lastMaybe xs

indexFilter :: (Int -> Bool) -> [a] -> [a]
indexFilter indexPred xs = fmap snd $ filter (\(i, _) -> indexPred i) $ zip [0..] xs