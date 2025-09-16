module JWT.UrlBase64 exposing (decode)

{-| A couple of functions for use with a base64 encoder and decoder that convert the
base64 alphabet to and from the url alphabet.

They can be composed with encode and decode in truqu/elm-base64 like this:

    b64e =
        UrlBase64.encode Base64.encode

    b64d =
        UrlBase64.decode Base64.decode

Applying these to url base64 converts to and from standard base64 into and out
of the decoders underneath.


    base64_1 =
        b64e "aÿÿ"

    -- Ok "Yf__"
    base64_t =
        b64e "aÿÿ" |> Result.andThen b64d

    -- Ok "aÿÿ"
    base64_2 =
        b64e "aÿ"

    -- Ok "Yf8"
    base64_u =
        b64e "aÿ" |> Result.andThen b64d

    -- Ok "aÿ"

@docs decode

-}

import Maybe
import Regex exposing (Regex)


replaceFromUrl : Regex
replaceFromUrl =
    Regex.fromString "[-_]" |> Maybe.withDefault Regex.never


{-| Expose the given function to the standard base64 alphabet form of the given
string with padding restored.

Compose this with a base64 decoder to make a url-base64 decoder.

    b64d =
        UrlBase64.decode Base64.decode

-}
decode : (String -> Result err a) -> String -> Result err a
decode dec e =
    let
        replaceChar rematch =
            case rematch.match of
                "-" ->
                    "+"

                _ ->
                    "/"

        strlen =
            String.length e

        hanging =
            modBy strlen 4

        ilen =
            if hanging == 0 then
                0

            else
                4 - hanging
    in
    dec (Regex.replace replaceFromUrl replaceChar (e ++ String.repeat ilen "="))
