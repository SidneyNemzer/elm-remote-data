module CreateableRemoteData exposing (..)

import EditableRemoteData exposing (EditableRemoteData)


type alias CreateableRemoteData data downloadError uploadError =
    EditableRemoteData (Maybe data) downloadError uploadError
