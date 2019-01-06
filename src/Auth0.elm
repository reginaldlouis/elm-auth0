module Auth0 exposing
    ( Audience(..)
    , ClientId(..)
    , Config(..)
    , Connection(..)
    , Nonce(..)
    , Prompt(..)
    , RedirectUri(..)
    , ResponseType(..)
    , Scope(..)
    , State(..)
    , authorize
    , config
    , decode
    )

import Array
import Url exposing (Url)
import Url.Builder
import Url.Extra


type Config
    = Config
        { baseUrl : String
        , clientId : String
        }


type Audience
    = Audience String


type Scope
    = Scope String


type ResponseType
    = Code -- For server side flow
    | Token -- For application side flow
    | IdToken


type ClientId
    = ClientId String


type State
    = State String


type RedirectUri
    = RedirectUri String


type Nonce
    = Nonce String


type Connection
    = Connection String


type Prompt
    = Prompt String


config : String -> ClientId -> Config
config baseUrl (ClientId clientId) =
    Config
        { baseUrl = baseUrl
        , clientId = clientId
        }


authorize :
    Config
    ->
        { responseType : List ResponseType
        , audience : Maybe Audience
        , scope : Maybe (List Scope)
        , state : Maybe State
        , redirectUri : Maybe RedirectUri
        , nonce : Maybe Nonce
        , connection : Maybe Connection
        , prompt : Maybe Prompt
        }
    -> String
authorize (Config { baseUrl, clientId }) p =
    let
        q =
            Url.Builder.string

        responseType_ =
            p.responseType
                |> List.map (\x -> responseTypeToString x)
                |> String.join " "

        querys =
            [ q "response_type" responseType_ |> Just
            , q "client_id" clientId |> Just
            , p.audience |> Maybe.andThen (\x -> x |> audience |> q "audience" |> Just)
            , p.scope |> Maybe.andThen (\x -> x |> List.map (\s -> s |> scope) |> String.join " " |> q "scope" |> Just)
            , p.state |> Maybe.andThen (\x -> x |> state |> q "state" |> Just)
            , p.redirectUri |> Maybe.andThen (\x -> x |> redirectUri |> q "redirect_uri" |> Just)
            , p.nonce |> Maybe.andThen (\x -> x |> nonce |> q "nonce" |> Just)
            , p.connection |> Maybe.andThen (\x -> x |> connection |> q "connection" |> Just)
            , p.prompt |> Maybe.andThen (\x -> x |> prompt |> q "prompt" |> Just)
            ]
                |> List.filterMap (\x -> x)
    in
    Url.Builder.crossOrigin
        baseUrl
        [ "authorize" ]
        querys



{--Helper function --}


type alias CallbackInfo =
    { accessToken : Maybe String
    , expiresIn : Maybe String
    , tokenType : Maybe String
    , idToken : Maybe String
    , code : Maybe String
    }


urlCallbackParser : Maybe Url -> CallbackInfo
urlCallbackParser url =
    let
        toKeyValue lst =
            lst
                |> List.map (\e -> String.split "=" e |> Array.fromList)
                |> List.map
                    (\e ->
                        ( Array.get 0 e |> Maybe.withDefault ""
                        , Array.get 1 e |> Maybe.withDefault ""
                        )
                    )

        getValue key lst =
            lst
                |> List.filter (\( k, v ) -> k == key)
                |> List.head
                |> Maybe.map (\x -> Tuple.second x)

        url_ =
            Maybe.withDefault Url.Extra.empty url

        query =
            url_.query |> Maybe.withDefault ""

        fragment =
            url_.fragment |> Maybe.withDefault ""

        qKeyValues =
            query |> String.split "&" |> toKeyValue

        fKeyValues =
            fragment |> String.split "&" |> toKeyValue

        getAccessToken =
            getValue "access_token"

        getExpiresIn =
            getValue "expires_in"

        getTokenType =
            getValue "token_type"

        getIdToken =
            getValue "id_token"

        getCode =
            getValue "code"
    in
    { accessToken = either (getAccessToken qKeyValues) (getAccessToken fKeyValues)
    , expiresIn = either (getExpiresIn qKeyValues) (getExpiresIn fKeyValues)
    , tokenType = either (getTokenType qKeyValues) (getTokenType fKeyValues)
    , idToken = either (getIdToken qKeyValues) (getIdToken fKeyValues)
    , code = either (getCode qKeyValues) (getCode fKeyValues)
    }



{--Utils functions --}


either : Maybe a -> Maybe a -> Maybe a
either left right =
    case left of
        Just x ->
            left

        Nothing ->
            right


audience (Audience a) =
    a


scope (Scope s) =
    s


state (State s) =
    s


redirectUri (RedirectUri r) =
    r


nonce (Nonce n) =
    n


connection (Connection c) =
    c


prompt (Prompt p) =
    p


responseTypeToString : ResponseType -> String
responseTypeToString rt =
    case rt of
        Code ->
            "code"

        Token ->
            "token"

        IdToken ->
            "id_token"
