module JWT.JWS exposing (DecodeError(..), Header, JWS, VerificationError(..), fromParts)

import Base64.Decode as B64Decode
import Bytes
import Bytes.Decode
import JWT.ClaimSet as ClaimSet
import JWT.JWK as JWK
import JWT.UrlBase64 as UrlBase64
import Json.Decode as JDecode
import Json.Decode.Pipeline exposing (optional, required)
import Result exposing (andThen, map, mapError)


type alias JWS =
    { signature : List Int
    , header : Header
    , claims : ClaimSet.ClaimSet
    }


type alias Header =
    { alg : String
    , jku : Maybe String
    , jwk : Maybe JWK.JWK
    , kid : Maybe String
    , x5u : Maybe String
    , x5c : Maybe (List String)
    , x5t : Maybe String
    , x5t_S256 : Maybe String
    , typ : Maybe String
    , cty : Maybe String
    , crit : Maybe (List String)
    }


type DecodeError
    = Base64DecodeError
    | MalformedSignature
    | InvalidHeader JDecode.Error
    | InvalidClaims JDecode.Error


fromParts : String -> String -> String -> Result DecodeError JWS
fromParts header claims signature =
    let
        decode_ d part =
            UrlBase64.decode (B64Decode.decode d) part

        bytesDecoder len =
            Bytes.Decode.loop ( len, [] ) <|
                \( n, xs ) ->
                    if n <= 0 then
                        Bytes.Decode.succeed (Bytes.Decode.Done xs)

                    else
                        Bytes.Decode.map (\x -> Bytes.Decode.Loop ( n - 1, x :: xs )) Bytes.Decode.unsignedInt8

        decodeBytes bytes =
            Bytes.Decode.decode (bytesDecoder (Bytes.width bytes)) bytes
                |> Maybe.map List.reverse
    in
    case
        ( decode_ B64Decode.string header
        , decode_ B64Decode.string claims
        , decode_ B64Decode.bytes signature
        )
    of
        ( Ok header_, Ok claims_, Ok signature_ ) ->
            case decodeBytes signature_ of
                Just sig ->
                    decode header_ claims_ sig

                Nothing ->
                    Err MalformedSignature

        _ ->
            Err Base64DecodeError


decode : String -> String -> List Int -> Result DecodeError JWS
decode header claims signature =
    JDecode.decodeString headerDecoder header
        |> mapError InvalidHeader
        |> andThen
            (\header_ ->
                JDecode.decodeString ClaimSet.decoder claims
                    |> mapError InvalidClaims
                    |> map (JWS signature header_)
            )


headerDecoder : JDecode.Decoder Header
headerDecoder =
    JDecode.succeed Header
        |> required "alg" JDecode.string
        |> optional "jku" (JDecode.maybe JDecode.string) Nothing
        |> optional "jwk" (JDecode.maybe JWK.decoder) Nothing
        |> optional "kid" (JDecode.maybe JDecode.string) Nothing
        |> optional "x5u" (JDecode.maybe JDecode.string) Nothing
        |> optional "x5c" (JDecode.maybe <| JDecode.list JDecode.string) Nothing
        |> optional "x5t" (JDecode.maybe JDecode.string) Nothing
        |> optional "x5t#S256" (JDecode.maybe JDecode.string) Nothing
        |> optional "typ" (JDecode.maybe JDecode.string) Nothing
        |> optional "cty" (JDecode.maybe JDecode.string) Nothing
        |> optional "crit" (JDecode.maybe <| JDecode.list JDecode.string) Nothing


type VerificationError
    = UnsupportedAlgorithm
