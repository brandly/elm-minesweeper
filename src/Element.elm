module Element exposing (..)

import Html exposing (Attribute, Html)
import Html.Attributes exposing (style)
import Debug exposing (toString)



type alias Element msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


styled : Element msg -> List ( String, String ) -> Element msg
styled el css =
    \attrs children ->
        el (List.map (\( k, v ) -> style k v) css ++ attrs) children


px : Int -> String
px x =
    toString x ++ "px"
