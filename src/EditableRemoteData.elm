module EditableRemoteData exposing (..)

import RemoteData exposing (RemoteData(..))
import UploadStatus exposing (UploadStatus(..))


type alias LocalRemote data =
    { local : data
    , remote : data
    }


type alias EditableRemoteData data downloadError uploadError =
    RemoteData ( UploadStatus data uploadError, LocalRemote data ) downloadError
