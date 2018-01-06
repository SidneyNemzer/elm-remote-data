module UploadStatus exposing (UploadStatus(..), isUploading)


type UploadStatus data error
    = NotUploading
    | Uploading data
    | LastUploadFailed error


isUploading : UploadStatus data error -> Bool
isUploading uploadStatus =
    case uploadStatus of
        NotUploading ->
            False

        Uploading _ ->
            True

        LastUploadFailed _ ->
            False
