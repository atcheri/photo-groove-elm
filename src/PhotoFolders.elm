module PhotoFolders exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import PhotoGroove exposing (view)


type Folder
    = Folder
        { name : String
        , photoUrls : List String
        , subFolders : List Folder
        , expanded : Bool
        }


type FolderPath
    = End
    | SubFolder Int FolderPath


toggleExpand : FolderPath -> Folder -> Folder
toggleExpand path (Folder folder) =
    case path of
        End ->
            Folder { folder | expanded = not folder.expanded }

        SubFolder targetIndex remainingPath ->
            let
                subFolders : List Folder
                subFolders =
                    List.indexedMap transform folder.subFolders

                transform : Int -> Folder -> Folder
                transform currentIndex currentSubFolder =
                    if currentIndex == targetIndex then
                        toggleExpand remainingPath currentSubFolder

                    else
                        currentSubFolder
            in
            Folder { folder | subFolders = subFolders }


type alias Model =
    { selectedPhotoUrl : Maybe String
    , photos : Dict String Photo
    , root : Folder
    }


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing
    , photos = Dict.empty
    , root = Folder { name = "Loading", photoUrls = [], subFolders = [], expanded = True }
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Http.get
        { url = "http://elm-in-action.com/folders/list"
        , expect = Http.expectJson GotInitialModel modelDecoder
        }
    )


modelDecoder : Decoder Model
modelDecoder =
    Decode.succeed
        { selectedPhotoUrl = Just "trevi"
        , photos =
            Dict.fromList
                [ ( "trevi"
                  , { title = "Trevi"
                    , relatedUrls = [ "coli", "fresco" ]
                    , size = 34
                    , url = "trevi"
                    }
                  )
                , ( "fresco"
                  , { title = "Fresco"
                    , relatedUrls = [ "trevi" ]
                    , size = 46
                    , url = "fresco"
                    }
                  )
                , ( "coli"
                  , { title = "Coliseum"
                    , relatedUrls = [ "trevi", "fresco" ]
                    , size = 36
                    , url = "coli"
                    }
                  )
                ]
        , root =
            Folder
                { name = "Photos"
                , photoUrls = []
                , subFolders =
                    [ Folder
                        { name = "2016"
                        , photoUrls = [ "trevi", "coli" ]
                        , subFolders =
                            [ Folder
                                { name = "outdoors"
                                , photoUrls = []
                                , subFolders = []
                                , expanded = True
                                }
                            , Folder
                                { name = "indoors"
                                , photoUrls = [ "fresco" ]
                                , subFolders = []
                                , expanded = True
                                }
                            ]
                        , expanded = True
                        }
                    , Folder
                        { name = "2017"
                        , photoUrls = []
                        , subFolders =
                            [ Folder
                                { name = "outdoors"
                                , photoUrls = []
                                , subFolders = []
                                , expanded = True
                                }
                            , Folder
                                { name = "indoors"
                                , photoUrls = []
                                , subFolders = []
                                , expanded = True
                                }
                            ]
                        , expanded = True
                        }
                    ]
                , expanded = True
                }
        }


type Msg
    = ClickedPhoto String
    | GotInitialModel (Result Http.Error Model)
    | ClickedFolder FolderPath


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )

        GotInitialModel (Ok newModel) ->
            ( newModel, Cmd.none )

        GotInitialModel (Err _) ->
            ( model, Cmd.none )

        ClickedFolder path ->
            ( { model | root = toggleExpand path model.root }, Cmd.none )


type alias Photo =
    { title : String
    , size : Int
    , relatedUrls : List String
    , url : String
    }


viewSelectedPhoto : Photo -> Html Msg
viewSelectedPhoto photo =
    div [ class "selected-photo" ]
        [ h2 [] [ text photo.title ]
        , img [ src (urlPrefix ++ "photos/" ++ photo.url ++ "/full") ] []
        , span [] [ text (String.fromInt photo.size ++ "KB") ]
        , h3 [] [ text "Related photos" ]
        , div [ class "related-photos" ]
            (List.map viewRelatedPhoto photo.relatedUrls)
        ]


viewRelatedPhoto : String -> Html Msg
viewRelatedPhoto url =
    img
        [ class "related-photo"
        , onClick (ClickedPhoto url)
        , src (urlPrefix ++ "photos/" ++ url ++ "/thumb")
        ]
        []


viewFolder : FolderPath -> Folder -> Html Msg
viewFolder path (Folder folder) =
    let
        viewSubFolder : Int -> Folder -> Html Msg
        viewSubFolder index subFolder =
            viewFolder (appendIndex index path) subFolder

        folderLabel =
            label [ onClick (ClickedFolder path) ] [ text folder.name ]
    in
    if folder.expanded then
        let
            contents =
                List.indexedMap viewSubFolder folder.subFolders
        in
        div [ class "folder expanded" ]
            [ folderLabel
            , div [ class "subfolders" ] contents
            ]

    else
        div [ class "folder collapsed" ] [ folderLabel ]


appendIndex : Int -> FolderPath -> FolderPath
appendIndex index path =
    case path of
        End ->
            SubFolder index End

        SubFolder subFolderIndex remainingPath ->
            SubFolder subFolderIndex (appendIndex index remainingPath)


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


view : Model -> Html Msg
view model =
    let
        photoByUrl : String -> Maybe Photo
        photoByUrl url =
            Dict.get url model.photos

        selectedPhoto : Html Msg
        selectedPhoto =
            case Maybe.andThen photoByUrl model.selectedPhotoUrl of
                Just photo ->
                    viewSelectedPhoto photo

                Nothing ->
                    text ""
    in
    div [ class "content" ]
        [ div [ class "folders" ]
            [ h1 [] [ text "Folders" ]
            , viewFolder End model.root
            ]
        , div [ class "selected-photo" ] [ selectedPhoto ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
