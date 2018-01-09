module EditableRemoteData
    exposing
        ( EditableRemoteData
        , LocalRemote
        , edit
        , unwrapLocalRemote
        , unwrapLocal
        , fromResult
        , upload
        , uploaded
        , isSaved
        , isLoading
        , isFailed
        , isSuccess
        , isUploading
        )

{-| Represents data that is downloaded from a server, edited on the client, then
uploaded back to the server. It's really an extension of `RemoteData`.


# Types

@docs LocalRemote, EditableRemoteData


# Functions

@docs edit, unwrapLocalRemote, unwrapLocal, fromResult, upload, uploaded, isSaved, isLoading, isFailed, isSuccess, isUploading

-}

import RemoteData exposing (RemoteData(..))
import UploadStatus exposing (UploadStatus(..))


{-| Represents data that is on the client and server. This allows us to check if
the data needs to be synced.
-}
type alias LocalRemote data =
    { local : data
    , remote : data
    }


{-| Represents data that is downloaded from a server, edited on the client, then
uploaded back to the server.
-}
type alias EditableRemoteData data downloadError uploadError =
    RemoteData ( UploadStatus data uploadError, LocalRemote data ) downloadError


{-| Modifies the local data if possible, otherwise does nothing
-}
edit : (data -> data) -> EditableRemoteData data de ue -> EditableRemoteData data de ue
edit fn =
    RemoteData.map
        (Tuple.mapSecond
            (\localRemote ->
                LocalRemote
                    (fn localRemote.local)
                    localRemote.remote
            )
        )


{-| Runs a function on the LocalRemote inside an EditableRemoteData if possible,
otherwise returns the default
-}
unwrapLocalRemote : a -> (LocalRemote data -> a) -> EditableRemoteData data de ue -> a
unwrapLocalRemote default fn =
    RemoteData.unwrap default (always default) (Tuple.second >> fn)


{-| Runs a function on the local data inside an EditableRemoteData if possible,
otherwise returns the default
-}
unwrapLocal : a -> (data -> a) -> EditableRemoteData data de ue -> a
unwrapLocal default fn =
    unwrapLocalRemote default (.local >> fn)


{-| Creates an EditableRemoteData from a Result. Designed to be used to turn an
HTTP request result into EditableRemoteData. The data in the Result is considered
syncronized, so the local and remote data is the same at this point.
-}
fromResult : Result error data -> EditableRemoteData data error ue
fromResult =
    RemoteData.fromResult
        >> RemoteData.map
            (\remote -> ( NotUploading, LocalRemote remote remote ))


{-| Try to put the EditableRemoteData into the "Uploading" state. This only works
if

  - The data has been successfully downloaded
  - The EditableRemoteData isn't already uploading
  - The local and remote data is desyncronized

If this is successful, the data is returned as a `Just LocalRemote`. Otherwise
`Nothing` is returned. It's up to your code to turn the `Just LocalRemote` into
a Cmd that sends the request to the server.

-}
upload :
    EditableRemoteData data de ue
    -> ( Maybe (LocalRemote data), EditableRemoteData data de ue )
upload remoteData =
    let
        noUpload =
            ( Nothing, remoteData )
    in
        RemoteData.unwrap
            noUpload
            (always noUpload)
            (\( uploadStatus, { local, remote } as localRemote ) ->
                if local == remote || UploadStatus.isUploading uploadStatus then
                    noUpload
                else
                    ( Just localRemote
                    , Success <| ( Uploading local, localRemote )
                    )
            )
            remoteData


{-| Use this after an upload has completed. This will update the EditableRemoteData
to refelect the result: either an error occured and the upload failed OR the data
was successfully saved and the remote value of the LocalRemote can be updated.

Note that edits can still be made while uploading, after the upload completes the
remote will be updated to refelect the data that was uploaded rather than the
current local data.

-}
uploaded :
    Result uploadError ()
    -> EditableRemoteData data de uploadError
    -> EditableRemoteData data de uploadError
uploaded result =
    RemoteData.map
        (\( uploadStatus, { local, remote } as localRemote ) ->
            case result of
                Ok () ->
                    case uploadStatus of
                        NotUploading ->
                            ( NotUploading
                            , localRemote
                            )

                        Uploading data ->
                            ( NotUploading
                            , LocalRemote local data
                            )

                        LastUploadFailed error ->
                            ( LastUploadFailed error
                            , localRemote
                            )

                Err error ->
                    ( LastUploadFailed error
                    , localRemote
                    )
        )


{-| Check if an EditableRemoteData has its local and remote data syncronized
-}
isSaved : EditableRemoteData date de uploadError -> Bool
isSaved =
    RemoteData.unwrap
        True
        (always True)
        (\( _, { local, remote } ) ->
            local == remote
        )


{-| Check if an EditableRemoteData is downloading. This will return `false` when
the data is being *uploaded*.
-}
isLoading : EditableRemoteData date de uploadError -> Bool
isLoading =
    RemoteData.isLoading


{-| Check if an EditableRemoteData has failed to download
-}
isFailed : EditableRemoteData date de uploadError -> Bool
isFailed =
    RemoteData.isFailed


{-| Check if an EditableRemoteData has been successfully downloaded
-}
isSuccess : EditableRemoteData date de uploadError -> Bool
isSuccess =
    RemoteData.isSuccess


{-| Check if an EditableRemoteData is currently uploading to sync its data
-}
isUploading : EditableRemoteData date de uploadError -> Bool
isUploading =
    RemoteData.unwrap
        True
        (always True)
        (Tuple.first >> UploadStatus.isUploading)
