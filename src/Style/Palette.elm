module Style.Palette exposing (black, blue, brown, charcoal, colorConv, darkBlue, darkBrown, darkCharcoal, darkGray, darkGreen, darkGrey, darkOrange, darkPurple, darkRed, darkYellow, gray, green, grey, hexToColor, lightBlue, lightBrown, lightCharcoal, lightGray, lightGreen, lightGrey, lightOrange, lightPurple, lightRed, lightYellow, orange, purple, red, webColors, white, yellow)

import Color as Color
import Dict exposing (fromList)
import Element as Element
import Hex exposing (fromString)


colorConv : Color.Color -> Element.Color
colorConv c =
    Color.toRgba c
        |> (\c_ ->
                Element.rgba c_.red c_.green c_.blue c_.alpha
           )


red =
    colorConv Color.red


orange =
    colorConv Color.orange


yellow =
    colorConv Color.yellow


green =
    colorConv Color.green


blue =
    colorConv Color.blue


purple =
    colorConv Color.purple


brown =
    colorConv Color.brown


lightRed =
    colorConv Color.lightRed


lightOrange =
    colorConv Color.lightOrange


lightYellow =
    colorConv Color.lightYellow


lightGreen =
    colorConv Color.lightGreen


lightBlue =
    colorConv Color.lightBlue


lightPurple =
    colorConv Color.lightPurple


lightBrown =
    colorConv Color.lightBrown


darkRed =
    colorConv Color.darkRed


darkOrange =
    colorConv Color.darkOrange


darkYellow =
    colorConv Color.darkYellow


darkGreen =
    colorConv Color.darkGreen


darkBlue =
    colorConv Color.darkBlue


darkPurple =
    colorConv Color.darkPurple


darkBrown =
    colorConv Color.darkBrown


white =
    colorConv Color.white


lightGrey =
    colorConv Color.lightGrey


grey =
    colorConv Color.grey


darkGrey =
    colorConv Color.darkGrey


lightCharcoal =
    colorConv Color.lightCharcoal


charcoal =
    colorConv Color.charcoal


darkCharcoal =
    colorConv Color.darkCharcoal


black =
    colorConv Color.black


lightGray =
    colorConv Color.lightGray


gray =
    colorConv Color.gray


darkGray =
    colorConv Color.darkGray


colA : Color.Color -> Float -> Element.Color
colA c a =
    Color.toRgba c
        |> (\c_ ->
                Element.rgba c_.red c_.green c_.blue a
           )



-------------------------------------------------------------------------------


hexToColor : String -> Element.Color
hexToColor hexColor =
    let
        hexColor_ =
            String.toLower hexColor

        red_ =
            String.left 2 hexColor_
                |> Hex.fromString
                |> Result.withDefault 0
                |> toFloat

        green_ =
            String.dropLeft 2 hexColor_
                |> String.left 2
                |> Hex.fromString
                |> Result.withDefault 0
                |> toFloat

        blue_ =
            String.dropLeft 4 hexColor_
                |> String.left 2
                |> Hex.fromString
                |> Result.withDefault 0
                |> toFloat
    in
    Element.rgb (red_ / 255) (green_ / 255) (blue_ / 255)



-------------------------------------------------------------------------------


webColors =
    Dict.fromList
        [ ( "marron", "800000" )
        , ( "rouge foncé", "8B0000" )
        , ( "brun", "A52A2A" )
        , ( "briques réfractaires", "B22222" )
        , ( "crimson", "DC143C" )
        , ( "rouge", "FF0000" )
        , ( "tomate", "FF6347" )
        , ( "corail", "FF7F50" )
        , ( "rouge indien", "CD5C5C" )
        , ( "corail clair", "F08080" )
        , ( "saumon noir", "E9967A" )
        , ( "saumon", "FA8072" )
        , ( "saumon léger", "FFA07A" )
        , ( "rouge orange", "FF4500" )
        , ( "orange foncé", "FF8C00" )
        , ( "orange", "FFA500" )
        , ( "or", "FFD700" )
        , ( "tige dorée sombre", "B8860B" )
        , ( "tige d'or", "DAA520" )
        , ( "tige dorée pâle", "EEE8AA" )
        , ( "kaki foncé", "BDB76B" )
        , ( "kaki", "F0E68C" )
        , ( "olive", "808000" )
        , ( "jaune", "FFFF00" )
        , ( "jaune vert", "9ACD32" )
        , ( "vert olive foncé", "556B2F" )
        , ( "olive terne", "6B8E23" )
        , ( "vert de pelouse", "7CFC00" )
        , ( "réutilisation des cartes", "7FFF00" )
        , ( "vert jaune", "ADFF2F" )
        , ( "vert foncé", "006400" )
        , ( "vert", "008000" )
        , ( "vert forêt", "228B22" )
        , ( "chaux", "00FF00" )
        , ( "vert lime", "32CD32" )
        , ( "vert clair", "90EE90" )
        , ( "vert pâle", "98FB98" )
        , ( "vert foncé de la mer", "8FBC8F" )
        , ( "vert printanier moyen", "00FA9A" )
        , ( "printemps vert", "0F0FF7F" )
        , ( "vert de mer", "2E8B57" )
        , ( "milieu aqua marine", "66CDAA" )
        , ( "vert de mer moyen", "3CB371" )
        , ( "vert d'eau claire", "20B2AA" )
        , ( "gris ardoise foncé", "2F4F4F" )
        , ( "sarcelle", "008080" )
        , ( "cyan foncé", "008B8B" )
        , ( "aqua", "00FFFF" )
        , ( "cyan", "00FFFF" )
        , ( "cyan clair", "E0FFFF" )
        , ( "turquoise foncé", "00CED1" )
        , ( "turquoise", "40E0D0" )
        , ( "turquoise moyen", "48D1CC" )
        , ( "turquoise pâle", "AFEEEE" )
        , ( "aqua marine", "7FFFD4" )
        , ( "bleu poudre", "B0E0E6" )
        , ( "bleu cadet", "5F9EA0" )
        , ( "bleu acier", "4682B4" )
        , ( "fleur de maïs bleue", "6495ED" )
        , ( "bleu ciel profond", "00BFFF" )
        , ( "bleu dodger", "1E90FF" )
        , ( "bleu clair", "ADD8E6" )
        , ( "bleu ciel", "87CEEB" )
        , ( "ciel bleu clair", "87CEFA" )
        , ( "bleu nuit", "191970" )
        , ( "marine", "000080" )
        , ( "bleu foncé", "00008B" )
        , ( "bleu moyen", "0000CD" )
        , ( "bleu", "0000FF" )
        , ( "bleu royal", "4169E1" )
        , ( "bleu violet", "8A2BE2" )
        , ( "indigo", "4B0082" )
        , ( "bleu ardoise foncé", "483D8B" )
        , ( "bleu ardoise", "6A5ACD" )
        , ( "bleu ardoise moyenne", "7B68EE" )
        , ( "violet moyen", "9370DB" )
        , ( "magenta foncé", "8B008B" )
        , ( "violet foncé", "9400D3" )
        , ( "orchidée noire", "9932CC" )
        , ( "orchidée moyenne", "BA55D3" )
        , ( "mauve", "800080" )
        , ( "chardon", "D8BFD8" )
        , ( "prune", "DDA0DD" )
        , ( "violet", "EE82EE" )
        , ( "magenta / fuchsia", "FF00FF" )
        , ( "orchidée", "DA70D6" )
        , ( "rouge violet moyen", "C71585" )
        , ( "rouge violet pâle", "DB7093" )
        , ( "rose profond", "FF1493" )
        , ( "rose chaud", "FF69B4" )
        , ( "rose clair", "FFB6C1" )
        , ( "rose", "FFC0CB" )
        , ( "blanc antique", "FAEBD7" )
        , ( "beige", "F5F5DC" )
        , ( "bisque", "FFE4C4" )
        , ( "amande blanchie", "FFEBCD" )
        , ( "blé", "F5DEB3" )
        , ( "soie de maïs", "FFF8DC" )
        , ( "mousseline de citron", "FFFACD" )
        , ( "tige dorée légère", "FAFAD2" )
        , ( "jaune clair", "FFFFE0" )
        , ( "selle brune", "8B4513" )
        , ( "sienna", "A0522D" )
        , ( "chocolat", "D2691E" )
        , ( "pérou", "CD853F" )
        , ( "brun sableux", "F4A460" )
        , ( "bois robuste", "DEB887" )
        , ( "bronzage", "D2B48C" )
        , ( "brun rosé", "BC8F8F" )
        , ( "mocassin", "FFE4B5" )
        , ( "blanc navajo", "FFDEAD" )
        , ( "bouffée de pêche", "FFDAB9" )
        , ( "rose brumeux", "FFE4E1" )
        , ( "blush lavande", "FFF0F5" )
        , ( "lin", "FAF0E6" )
        , ( "vieille dentelle", "FDF5E6" )
        , ( "fouet à la papaye", "FFEFD5" )
        , ( "coquillage", "FFF5EE" )
        , ( "crème à la menthe", "F5FFFA" )
        , ( "gris ardoise", "708090" )
        , ( "gris ardoise clair", "778899" )
        , ( "bleu acier clair", "B0C4DE" )
        , ( "lavande", "E6E6FA" )
        , ( "blanc floral", "FFFAF0" )
        , ( "bleu alice", "F0F8FF" )
        , ( "blanc fantôme", "F8F8FF" )
        , ( "miellat", "F0FFF0" )
        , ( "ivoire", "FFFFF0" )
        , ( "azur", "F0FFFF" )
        , ( "neige", "FFFAFA" )
        , ( "noir", "000000" )
        , ( "gris pâle", "696969" )
        , ( "gris / gris", "808080" )
        , ( "gris foncé / gris foncé", "A9A9A9" )
        , ( "argent", "C0C0C0" )
        , ( "gris clair", "D3D3D3" )
        , ( "gainsboro", "DCDCDC" )
        , ( "fumée blanche", "F5F5F5" )
        , ( "blanc", "FFFFFF" )
        ]
