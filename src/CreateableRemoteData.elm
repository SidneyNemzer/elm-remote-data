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

import RemoteData exposing (RemoteData(..))
import UploadStatus exposing (UploadStatus(..))
import EditableRemoteData exposing (EditableRemoteData, LocalRemote)


type alias CreateableRemoteData data downloadError uploadError =
    EditableRemoteData (Maybe data) downloadError uploadError


create : data -> CreateableRemoteData data de ue
create data =
    Success <| ( NotUploading, LocalRemote (Just data) Nothing )


edit : (data -> data) -> CreateableRemoteData data de ue -> CreateableRemoteData data de ue
edit fn =
    EditableRemoteData.edit
        (\maybeData ->
            case maybeData of
                Just data ->
                    fn data |> Just

                Nothing ->
                    Nothing
        )


set : data -> CreateableRemoteData data de ue -> CreateableRemoteData data de ue
set data =
    edit (always data)


delete : CreateableRemoteData data de ue -> CreateableRemoteData data de ue
delete =
    EditableRemoteData.edit (always Nothing)


unwrapLocalRemote :
    a
    -> (LocalRemote (Maybe data) -> a)
    -> CreateableRemoteData data de ue
    -> a
unwrapLocalRemote =
    EditableRemoteData.unwrapLocalRemote


unwrapLocal : a -> (Maybe data -> a) -> CreateableRemoteData data de ue -> a
unwrapLocal =
    EditableRemoteData.unwrapLocal


fromResult : Result error data -> CreateableRemoteData data error ue
fromResult =
    Result.map Just >> EditableRemoteData.fromResult


upload :
    CreateableRemoteData data de ue
    -> ( Maybe (LocalRemote (Maybe data)), CreateableRemoteData data de ue )
upload =
    EditableRemoteData.upload


uploaded :
    Result uploadError ()
    -> CreateableRemoteData data de uploadError
    -> CreateableRemoteData data de uploadError
uploaded =
    EditableRemoteData.uploaded


isSaved : CreateableRemoteData date de uploadError -> Bool
isSaved =
    EditableRemoteData.isSaved


isLoading : CreateableRemoteData date de uploadError -> Bool
isLoading =
    RemoteData.isLoading


isFailed : CreateableRemoteData date de uploadError -> Bool
isFailed =
    RemoteData.isFailed


isSuccess : CreateableRemoteData date de uploadError -> Bool
isSuccess =
    RemoteData.isSuccess


isUploading : CreateableRemoteData date de uploadError -> Bool
isUploading =
    EditableRemoteData.isUploading
