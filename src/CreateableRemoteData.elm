module CreateableRemoteData
    exposing
        ( CreateableRemoteData
        , create
        , edit
        , set
        , delete
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
uploaded back to the server. It's really an extension of `EditableRemoteData`,
using a `Maybe data` to represent data that might or might not exist locally or
remotely.


# Types

@docs CreateableRemoteData


# Functions

@docs create, edit, set, delete, unwrapLocalRemote, unwrapLocal, fromResult, upload, uploaded, isSaved, isLoading, isFailed, isSuccess, isUploading

-}

import RemoteData exposing (RemoteData(..))
import UploadStatus exposing (UploadStatus(..))
import EditableRemoteData exposing (EditableRemoteData, LocalRemote)


{-| Represents data that can be created, edited, and deleted on the client, then
uploaded to the server.
-}
type alias CreateableRemoteData data downloadError uploadError =
    EditableRemoteData (Maybe data) downloadError uploadError


{-| Construct a new CreateableRemoteData that only exists locally
-}
create : data -> CreateableRemoteData data de ue
create data =
    Success <| ( NotUploading, LocalRemote (Just data) Nothing )


{-| Modifies the local data if possible, otherwise does nothing
-}
edit : (data -> data) -> CreateableRemoteData data de ue -> CreateableRemoteData data de ue
edit fn =
    EditableRemoteData.edit (Maybe.map fn)


{-| Similar to `edit`, but it sets the data without caring about the previous value
-}
set : data -> CreateableRemoteData data de ue -> CreateableRemoteData data de ue
set =
    always >> edit


{-| Modify a CreateableRemoteData to show that the data no longer exists locally
-}
delete : CreateableRemoteData data de ue -> CreateableRemoteData data de ue
delete =
    EditableRemoteData.edit (always Nothing)


{-| Runs a function on the LocalRemote inside a CreateableRemoteData if possible,
otherwise returns the default
-}
unwrapLocalRemote :
    a
    -> (LocalRemote (Maybe data) -> a)
    -> CreateableRemoteData data de ue
    -> a
unwrapLocalRemote =
    EditableRemoteData.unwrapLocalRemote


{-| Runs a function on the local data inside a CreateableRemoteData if possible,
otherwise returns the default
-}
unwrapLocal : a -> (Maybe data -> a) -> CreateableRemoteData data de ue -> a
unwrapLocal =
    EditableRemoteData.unwrapLocal


{-| Creates a CreateableRemoteData from a Result. Designed to be used to turn an
HTTP request result into CreateableRemoteData. The data in the Result is considered
syncronized, so the local and remote data is the same at this point.
-}
fromResult : Result error data -> CreateableRemoteData data error ue
fromResult =
    Result.map Just >> EditableRemoteData.fromResult


{-| Try to put the CreateableRemoteData into the "Uploading" state. This only works
if

  - The data has been successfully downloaded
  - The CreateableRemoteData isn't already uploading
  - The local and remote data is desyncronized

If this is successful, the data is returned as a `Just LocalRemote`. Otherwise
`Nothing` is returned. It's up to your code to turn the `Just LocalRemote` into
a Cmd that sends the request to the server.

-}
upload :
    CreateableRemoteData data de ue
    -> ( Maybe (LocalRemote (Maybe data)), CreateableRemoteData data de ue )
upload =
    EditableRemoteData.upload


{-| Use this after an upload has completed. This will update the CreateableRemoteData
to refelect the result: either an error occured and the upload failed OR the data
was successfully saved and the remote value of the LocalRemote can be updated.

Note that edits can still be made while uploading, after the upload completes the
remote will be updated to refelect the data that was uploaded rather than the
current local data.

-}
uploaded :
    Result uploadError ()
    -> CreateableRemoteData data de uploadError
    -> CreateableRemoteData data de uploadError
uploaded =
    EditableRemoteData.uploaded


{-| Check if a CreateableRemoteData has its local and remote data syncronized
-}
isSaved : CreateableRemoteData date de uploadError -> Bool
isSaved =
    EditableRemoteData.isSaved


{-| Check if a CreateableRemoteData is downloading. This will return `false` when
the data is being *uploaded*.
-}
isLoading : CreateableRemoteData date de uploadError -> Bool
isLoading =
    RemoteData.isLoading


{-| Check if a CreateableRemoteData has failed to download
-}
isFailed : CreateableRemoteData date de uploadError -> Bool
isFailed =
    RemoteData.isFailed


{-| Check if a CreateableRemoteData has been successfully downloaded
-}
isSuccess : CreateableRemoteData date de uploadError -> Bool
isSuccess =
    RemoteData.isSuccess


{-| Check if a CreateableRemoteData is currently uploading to sync its data
-}
isUploading : CreateableRemoteData date de uploadError -> Bool
isUploading =
    EditableRemoteData.isUploading
