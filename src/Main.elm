module Main exposing (..)

import Html exposing (Html, h1, text)


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    {}


initialModel : Model
initialModel =
    {}


type Msg
    = MyMessage


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []


view : Model -> Html Msg
view model =
    h1 [] [ text "hello world" ]
