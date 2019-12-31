module Element exposing (Element, none, px, styled)

import Html exposing (Attribute, Html, text)
import Html.Attributes exposing (style)


type alias Element msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


styled : Element msg -> List ( String, String ) -> Element msg
styled el css =
    \attrs children ->
        el (List.map (\( k, v ) -> style k v) css ++ attrs) children


px : Int -> String
px x =
    String.fromInt x ++ "px"


none : Html msg
none =
    text ""
