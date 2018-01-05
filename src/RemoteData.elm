module RemoteData
    exposing
        ( RemoteData(..)
        , map
        , unwrap
        , fromResult
        , isLoading
        , isFailed
        , isSuccess
        )


type RemoteData data error
    = Loading
    | Failed error
    | Success data


unwrap : a -> (error -> a) -> (data -> a) -> RemoteData data error -> a
unwrap loading failed success remoteData =
    case remoteData of
        Loading ->
            loading

        Failed error ->
            failed error

        Success data ->
            success data


map : (data -> a) -> RemoteData data error -> RemoteData a error
map fn remoteData =
    case remoteData of
        Loading ->
            Loading

        Failed error ->
            Failed error

        Success data ->
            Success <| fn data


fromResult : Result error data -> RemoteData data error
fromResult result =
    case result of
        Ok data ->
            Success data

        Err error ->
            Failed error


isLoading : RemoteData data error -> Bool
isLoading =
    unwrap True (always False) (always False)


isFailed : RemoteData data error -> Bool
isFailed =
    unwrap False (always True) (always False)


isSuccess : RemoteData data error -> Bool
isSuccess =
    unwrap False (always False) (always True)
