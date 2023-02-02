module Amyfy exposing (..)

import Regex


type Amyfication
    = DoNotAmyfy
    | Amyfy


fromBool : Bool -> Amyfication
fromBool b =
    if b then
        Amyfy

    else
        DoNotAmyfy


amyfyName : String -> String
amyfyName originalName =
    -- We'll reverse twice to easily get the "last" match
    -- That means the regex string given here has to be "reversed" as well
    -- Basically we look for "some vowels, some consonants, and then optionally an e"
    -- and replace the last occurance with "amy"
    case Regex.fromStringWith { caseInsensitive = True, multiline = False } "e*[b-df-hj-np-tv-xz]*[aeiouy]+" of
        Nothing ->
            "Amygex failed"

        Just regex ->
            -- First reverse
            String.reverse <|
                Regex.replace
                    regex
                    (\m ->
                        if m.number == 1 then
                            -- Reversed "amy" as we'll reverse the whole string again
                            "yma"

                        else
                            m.match
                    )
                    -- Second reverse
                    (String.reverse originalName)
