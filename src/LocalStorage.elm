port module LocalStorage exposing (modeKey, save)


type alias Key =
    String


type alias Value =
    String


modeKey : Key
modeKey =
    "mode"


save : Key -> Value -> Cmd msg
save key value =
    saveLocalStorage { key = key, value = value }


port saveLocalStorage : { key : Key, value : Value } -> Cmd msg
