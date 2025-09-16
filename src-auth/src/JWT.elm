module JWT exposing
    ( JWT(..), DecodeError(..), fromString
    , VerificationError(..)
    )

{-|


# JWT

@docs JWT, DecodeError, fromString


# Verification

@docs VerificationError

-}

import JWT.JWS as JWS


{-| A JSON Web Token.

Can be either a JWS (signed) or a JWE (encrypted). The latter is not yet implemented.

-}
type JWT
    = JWS JWS.JWS


{-| A structured error describing exactly how the decoder failed.
-}
type DecodeError
    = TokenTypeUnknown
    | JWSError JWS.DecodeError


{-| Decode a JWT from string.

    fromString "eyJhbGciOi..." == Ok ...
    fromString "" == Err ...
    fromString "definitelyNotAJWT" == Err ...

-}
fromString : String -> Result DecodeError JWT
fromString string =
    case String.split "." string of
        [ header, claims, signature ] ->
            JWS.fromParts header claims signature
                |> Result.mapError JWSError
                |> Result.map JWS

        -- TODO [ header_, encryptedKey, iv, ciphertext, authenticationTag ]
        _ ->
            Err TokenTypeUnknown


{-| A structured error describing all verification errors.
-}
type VerificationError
    = JWSVerificationError JWS.VerificationError
