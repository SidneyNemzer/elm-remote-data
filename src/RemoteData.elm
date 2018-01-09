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

{-| The basic RemoteData type.


# Types

@docs RemoteData


# Functions

@docs map, unwrap, fromResult, isLoading, isFailed, isSuccess

-}


{-| The basic RemoteData type. Represents data that can be downloaded from a
server. Note that unlike many representations, this version does not have a
`NotAsked` state.
-}
type RemoteData data error
    = Loading
    | Failed error
    | Success data


{-| Runs a function on the data if possible, otherwise returns the default
-}
unwrap : a -> (error -> a) -> (data -> a) -> RemoteData data error -> a
unwrap loading failed success remoteData =
    case remoteData of
        Loading ->
            loading

        Failed error ->
            failed error

        Success data ->
            success data


{-| Modify the data inside if possible, otherwise do nothing
-}
map : (data -> a) -> RemoteData data error -> RemoteData a error
map fn remoteData =
    case remoteData of
        Loading ->
            Loading

        Failed error ->
            Failed error

        Success data ->
            Success <| fn data


{-| Creates a RemoteData from a Result. Use this to turn an HTTP request result
into RemoteData.
-}
fromResult : Result error data -> RemoteData data error
fromResult result =
    case result of
        Ok data ->
            Success data

        Err error ->
            Failed error


{-| Check if a RemoteData is downloading. This will return `false` when
the data is being *uploaded*.
-}
isLoading : RemoteData data error -> Bool
isLoading =
    unwrap True (always False) (always False)


{-| Check if a RemoteData has failed to download
-}
isFailed : RemoteData data error -> Bool
isFailed =
    unwrap False (always True) (always False)


{-| Check if a RemoteData has been successfully downloaded
-}
isSuccess : RemoteData data error -> Bool
isSuccess =
    unwrap False (always False) (always True)
