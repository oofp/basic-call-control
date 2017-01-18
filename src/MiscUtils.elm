module MiscUtils exposing(..)

-- type alias FutureData = String
start_l : List a
start_l =
    []

al : a -> List a -> List a
al a lst =
    lst ++ [ a ]

al_if : Bool -> a -> List a -> List a
al_if cond a lst =
    if cond then
        al a lst
    else
        lst

al_if_just : Maybe a -> List a -> List a
al_if_just maybe_a lst = case maybe_a of
    Nothing -> lst
    (Just a) -> al a lst

al_if_just_map : Maybe b -> (b->a) -> List a -> List a
al_if_just_map maybe_b b2a = al_if_just (Maybe.map b2a maybe_b)

extractDigits : String -> Maybe String
extractDigits str = case String.filter (\ch -> not (ch=='(' || ch==')' || ch==' '))  str of
  "" -> Nothing
  digitsStr -> Just digitsStr

isJust : Maybe a -> Bool
isJust maybe_a = not (maybe_a==Nothing)

isNothing : Maybe a -> Bool
isNothing maybe_a = not (isJust maybe_a)
