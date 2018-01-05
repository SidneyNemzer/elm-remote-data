module UploadStatus exposing (..)


type UploadStatus data error
    = NotUploading
    | Uploading data
    | LastUploadFailed error
