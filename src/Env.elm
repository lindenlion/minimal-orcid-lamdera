module Env exposing (orcidAppClientId, orcidAppClientSecret, useOrcidSandbox)

-- The Env.elm file is for per-environment configuration.
-- See https://dashboard.lamdera.app/docs/environment for more info.
-- Do not enter production secrets here!
-- Production values will be injected during deployment.


orcidAppClientId : String
orcidAppClientId =
    "APP-KASX8EPTRUT9I5HX"


orcidAppClientSecret : String
orcidAppClientSecret =
    "c2c9ce54-dec5-4ad4-85e9-be54ba8cf44a"


sandboxString : String
sandboxString =
    "sandbox"


useOrcidSandbox : Bool
useOrcidSandbox =
    -- set to False on lamdera dashboard
    if sandboxString == "sandbox" then
        True

    else
        False
