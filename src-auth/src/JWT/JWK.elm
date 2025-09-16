module JWT.JWK exposing (JWK, decoder)

import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)


type alias JWK =
    { kty : String
    , use : Maybe String
    , key_ops : Maybe (List String)
    , alg : Maybe String
    , kid : Maybe String
    , x5u : Maybe String
    , x5c : Maybe (List String)
    , x5t : Maybe String
    , x5t_S256 : Maybe String
    }


decoder : Decode.Decoder JWK
decoder =
    Decode.succeed JWK
        |> required "kty" Decode.string
        |> optional "use" (Decode.maybe Decode.string) Nothing
        |> optional "key_ops" (Decode.maybe <| Decode.list Decode.string) Nothing
        |> optional "alg" (Decode.maybe Decode.string) Nothing
        |> optional "kid" (Decode.maybe Decode.string) Nothing
        |> optional "x5u" (Decode.maybe Decode.string) Nothing
        |> optional "x5c" (Decode.maybe <| Decode.list Decode.string) Nothing
        |> optional "x5t" (Decode.maybe Decode.string) Nothing
        |> optional "x5t#S256" (Decode.maybe Decode.string) Nothing
