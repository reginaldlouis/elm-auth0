module Url.Extra exposing (empty)

import Url exposing (Protocol(..), Url)


empty : Url
empty =
    Url Http "" Nothing "" Nothing Nothing
