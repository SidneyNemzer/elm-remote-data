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

import RemoteData exposing (RemoteData(..))
import UploadStatus exposing (UploadStatus(..))


type alias LocalRemote data =
    { local : data
    , remote : data
    }


type alias EditableRemoteData data downloadError uploadError =
    RemoteData ( UploadStatus data uploadError, LocalRemote data ) downloadError


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


unwrapLocalRemote : a -> (LocalRemote data -> a) -> EditableRemoteData data de ue -> a
unwrapLocalRemote default fn =
    RemoteData.unwrap
        default
        (always default)
        (\( _, localRemote ) ->
            fn localRemote
        )


unwrapLocal : a -> (data -> a) -> EditableRemoteData data de ue -> a
unwrapLocal default fn =
    unwrapLocalRemote
        default
        (.local >> fn)


fromResult : Result error data -> EditableRemoteData data error ue
fromResult =
    RemoteData.fromResult
        >> RemoteData.map
            (\remote -> ( NotUploading, LocalRemote remote remote ))


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
                if local == remote then
                    noUpload
                else
                    let
                        willUpload =
                            ( Just localRemote
                            , Success <| ( Uploading local, localRemote )
                            )
                    in
                        case uploadStatus of
                            NotUploading ->
                                willUpload

                            Uploading data ->
                                noUpload

                            LastUploadFailed error ->
                                willUpload
            )
            remoteData


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


isSaved : EditableRemoteData date de uploadError -> Bool
isSaved =
    RemoteData.unwrap
        True
        (always True)
        (\( uploadStatus, { local, remote } ) ->
            local == remote
        )


isLoading : EditableRemoteData date de uploadError -> Bool
isLoading =
    RemoteData.isLoading


isFailed : EditableRemoteData date de uploadError -> Bool
isFailed =
    RemoteData.isFailed


isSuccess : EditableRemoteData date de uploadError -> Bool
isSuccess =
    RemoteData.isSuccess


isUploading : EditableRemoteData date de uploadError -> Bool
isUploading =
    RemoteData.unwrap
        True
        (always True)
        (\( uploadStatus, _ ) ->
            case uploadStatus of
                NotUploading ->
                    False

                Uploading _ ->
                    True

                LastUploadFailed _ ->
                    False
        )
