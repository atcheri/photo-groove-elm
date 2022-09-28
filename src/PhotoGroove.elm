module PhotoGroove exposing (main)

import Html exposing (div, h1, img, text)
import Html.Attributes exposing (..)


initialModel : { photos : List { url : String }, selectedUrl : String }
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        , { url = "4.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail : String -> { a | url : String } -> Html.Html msg
viewThumbnail selectedUrl thumb =
    if selectedUrl == thumb.url then
        img
            [ src (urlPrefix ++ thumb.url)
            , classList [ ( "selected", selectedUrl == thumb.url ) ]
            ]
            []

    else
        img
            [ src (urlPrefix ++ thumb.url) ]
            []


view : { photos : List { url : String }, selectedUrl : String } -> Html.Html msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "thumbnails" ]
            (List.map
                (\photo -> viewThumbnail model.selectedUrl photo)
                model.photos
            )
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]


main : Html.Html msg
main =
    view initialModel
