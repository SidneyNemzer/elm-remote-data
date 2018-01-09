module UploadStatus exposing (UploadStatus(..), isUploading)

{-| This module helps represent data that can be sent to a server


# Type

@docs UploadStatus


# Function

@docs isUploading

-}


{-| Represents the three states of uploadable data
-}
type UploadStatus data error
    = NotUploading
    | Uploading data
    | LastUploadFailed error


{-| Check if the given UploadStatus is Uploading
-}
isUploading : UploadStatus data error -> Bool
isUploading uploadStatus =
    case uploadStatus of
        NotUploading ->
            False

        Uploading _ ->
            True

        LastUploadFailed _ ->
            False
