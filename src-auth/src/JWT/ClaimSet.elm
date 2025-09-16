module JWT.ClaimSet exposing (ClaimSet, VerificationError(..), decoder)

import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (custom, optional)


type alias ClaimSet =
    { iss : Maybe String
    , sub : Maybe String
    , aud : Maybe String
    , exp : Maybe Int
    , nbf : Maybe Int
    , iat : Maybe Int
    , jti : Maybe String
    , metadata : Dict String Decode.Value
    }


decoder : Decode.Decoder ClaimSet
decoder =
    Decode.succeed ClaimSet
        |> optional "iss" (Decode.maybe Decode.string) Nothing
        |> optional "sub" (Decode.maybe Decode.string) Nothing
        |> optional "aud" (Decode.maybe Decode.string) Nothing
        |> optional "exp" (Decode.maybe Decode.int) Nothing
        |> optional "nbf" (Decode.maybe Decode.int) Nothing
        |> optional "iat" (Decode.maybe Decode.int) Nothing
        |> optional "jti" (Decode.maybe Decode.string) Nothing
        |> custom (Decode.dict Decode.value)


type VerificationError
    = NotYetValid
