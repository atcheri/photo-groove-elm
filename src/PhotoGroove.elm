module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (button, div, h1, img, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Random


type ThumbnailSize
    = Small
    | Medium
    | Large


type alias Photo =
    { url : String }


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { status : Status, chosenSize : ThumbnailSize }


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GetPhotos (Result Http.Error String)


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list"
        , expect = Http.expectString GetPhotos

        -- , expect = Http.expectString (\result -> GetPhotos result)
        }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail : String -> Photo -> Html.Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> Html.Html Msg
viewSizeChooser size =
    label
        []
        [ input
            [ type_ "radio"
            , name "size"
            , onClick (ClickedSize size)
            ]
            []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"



-- getPhotoUrl : Int -> String
-- getPhotoUrl index =
--     case Array.get index photoArray of
--         Just photo ->
--             photo.url
--         Nothing ->
--             ""


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored errorMessage ->
            status


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )

        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    ( model
                    , Random.generate GotRandomPhoto
                        (Random.uniform firstPhoto otherPhotos)
                    )

                -- Random.uniform firstPhoto otherPhotos
                --     |> Random.generate GotRandomPhoto
                --     |> Tuple.pair model
                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored errorMessage ->
                    ( model, Cmd.none )

        GetPhotos result ->
            case result of
                Ok responseStr ->
                    case String.split "," responseStr of
                        (firstUrl :: _) as urls ->
                            let
                                photos =
                                    List.map Photo urls
                            in
                            ( { model | status = Loaded photos firstUrl }, Cmd.none )

                        [] ->
                            ( { model | status = Errored "No photos found" }, Cmd.none )

                Err httError ->
                    ( { model | status = Errored "Server error!" }, Cmd.none )


viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html.Html Msg)
viewLoaded photos selectedUrl chosenSize =
    [ h1 [] [ text "Photo Groove" ]
    , div [ id "choose-size" ]
        (List.map viewSizeChooser [ Small, Medium, Large ])
    , button [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , div [ id "thumbnails", class (sizeToString chosenSize) ]
        (List.map
            (viewThumbnail selectedUrl)
            photos
        )
    , img
        [ class "large"
        , src (urlPrefix ++ "large/" ++ selectedUrl)
        ]
        []
    ]


view : Model -> Html.Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.chosenSize

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]



-- main : Program Never Model Msg


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
