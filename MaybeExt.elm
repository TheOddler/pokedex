module MaybeExt where

allOf : List (Maybe a) -> Maybe (List a)
allOf maybies =
    let f mayb list = --Maybe a -> Maybe (List a) -> Maybe (List a)
        case list of
            Just l -> case mayb of
                Just m -> Just (m :: l)
                Nothing -> Nothing
            Nothing -> Nothing
    in List.foldr f (Just []) maybies
