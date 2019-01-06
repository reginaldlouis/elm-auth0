module Example1 exposing (Model, Msg, init, update, view)

import Auth0
import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Html exposing (Html, a, div, input, span, text)
import Html.Attributes exposing (href, name, style, target, type_)
import Html.Events exposing (onCheck, onInput)
import Url exposing (Url)


type alias Model =
    { cfg : Auth0.Config
    , baseUrl : String
    , clientId : String
    , rtCode : Bool
    , rtToken : Bool
    , rtIdToken : Bool
    , audience : Maybe Auth0.Audience
    , scope : Maybe (List Auth0.Scope)
    , state : Maybe Auth0.State
    , redirectUri : Maybe Auth0.RedirectUri
    , nonce : Maybe Auth0.Nonce
    , connection : Maybe Auth0.Connection
    , prompt : Maybe Auth0.Prompt
    , key : Key
    , url : Maybe Url
    }


type Msg
    = NoOp
    | OnUrlRequest UrlRequest
    | OnUrlChange Url
    | SetBaseUrl String
    | SetClientId String
    | SetAudience String
    | SetScope String
    | SetReponseTypeCode Bool
    | SetReponseTypeToken Bool
    | SetReponseTypeIdToken Bool
    | SetState String
    | SetRedirectUri String
    | SetNonce String
    | SetConnection String
    | SetPrompt String


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


init : () -> Url -> Key -> ( Model, Cmd Msg )
init _ url key =
    let
        mdl =
            { cfg = Auth0.config "" (Auth0.ClientId "")
            , baseUrl = ""
            , clientId = ""
            , rtCode = False
            , rtToken = False
            , rtIdToken = False
            , audience = Nothing
            , scope = Nothing
            , state = Nothing
            , redirectUri = Nothing
            , nonce = Nothing
            , connection = Nothing
            , prompt = Nothing
            , key = key
            , url = Just url
            }
    in
    ( mdl, Cmd.none )


view : Model -> Document Msg
view model =
    let
        textInput label msg =
            div [ style "padding-bottom" "10px" ]
                [ span
                    [ style "width" "100px"
                    , style "display" "inline-block"
                    ]
                    [ text (label ++ ": ") ]
                , input
                    [ style "min-width" "300px"
                    , onInput msg
                    ]
                    []
                ]

        choice title choices =
            div [ style "padding-bottom" "10px" ]
                (div [] [ text title ]
                    :: (choices
                            |> List.map
                                (\( str, msg ) ->
                                    div
                                        [ style "padding-bottom" "5px"
                                        , style "padding-left" "25px"
                                        ]
                                        [ input [ type_ "checkbox", onCheck msg ] [], text str ]
                                )
                       )
                )

        authUrl =
            Auth0.authorize model.cfg
                { responseType = makeResponseType model.rtCode model.rtToken model.rtIdToken
                , audience = model.audience
                , scope = model.scope
                , state = model.state
                , redirectUri = model.redirectUri
                , nonce = model.nonce
                , connection = model.connection
                , prompt = model.prompt
                }

        inputParameters =
            div []
                [ textInput "Base url*" SetBaseUrl
                , textInput "Client id*" SetClientId
                , textInput "Audience" SetAudience
                , textInput "Scope" SetScope
                , choice "Response Type"
                    [ ( "Code", SetReponseTypeCode )
                    , ( "Token", SetReponseTypeToken )
                    , ( "Id Token", SetReponseTypeIdToken )
                    ]
                , textInput "State" SetState
                , textInput "Redirect Uri" SetRedirectUri
                , textInput "Nonce" SetNonce
                , textInput "Connection" SetConnection
                , textInput "Prompt" SetPrompt
                , div
                    [ style "padding-top" "50px" ]
                    [ text "Auth/login url: "
                    , a
                        [ href authUrl
                        , target "_blank"
                        ]
                        [ text authUrl ]
                    ]
                ]

        authCallbackInfo =
            let
                callbackInfo =
                    Auth0.parseCallbackUrl model.url

                s =
                    span [ style "width" "100px", style "display" "inline-block" ]

                showText txt1 txt2 =
                    div []
                        [ s [ text txt1 ]
                        , s [ text txt2 ]
                        ]

                default =
                    Maybe.withDefault "N/A"
            in
            div [ style "padding-top" "25px" ]
                [ showText "Access token: " (callbackInfo.accessToken |> default)
                , showText "Expires in: " (callbackInfo.expiresIn |> default)
                , showText "Token Type: " (callbackInfo.tokenType |> default)
                , showText "Id token: " (callbackInfo.idToken |> default)
                , showText "Code: " (callbackInfo.code |> default)
                ]

        body =
            div [] [ inputParameters, authCallbackInfo ]
    in
    Document "" [ body ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        OnUrlChange url ->
            ( { model | url = Just url }, Cmd.none )

        SetBaseUrl baseUrl ->
            ( { model
                | baseUrl = baseUrl
                , cfg = Auth0.config baseUrl (Auth0.ClientId model.clientId)
              }
            , Cmd.none
            )

        SetClientId clientId ->
            ( { model
                | clientId = clientId
                , cfg = Auth0.config model.baseUrl (Auth0.ClientId clientId)
              }
            , Cmd.none
            )

        SetAudience audience ->
            ( { model | audience = Auth0.Audience audience |> Just }, Cmd.none )

        SetScope scope ->
            let
                scope_ =
                    scope
                        |> String.trim
                        |> String.split " "
                        |> List.map Auth0.Scope
                        |> Just
            in
            ( { model | scope = scope_ }, Cmd.none )

        SetReponseTypeCode b ->
            ( { model
                | rtCode = b
              }
            , Cmd.none
            )

        SetReponseTypeToken b ->
            ( { model
                | rtToken = b
              }
            , Cmd.none
            )

        SetReponseTypeIdToken b ->
            ( { model
                | rtIdToken = b
              }
            , Cmd.none
            )

        SetState state ->
            ( { model | state = Auth0.State state |> Just }, Cmd.none )

        SetRedirectUri redirectUri ->
            ( { model | redirectUri = Auth0.RedirectUri redirectUri |> Just }, Cmd.none )

        SetNonce nonce ->
            ( { model | nonce = Auth0.Nonce nonce |> Just }, Cmd.none )

        SetConnection connection ->
            ( { model | connection = Auth0.Connection connection |> Just }, Cmd.none )

        SetPrompt prompt ->
            ( { model | prompt = Auth0.Prompt prompt |> Just }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



{--Help function --}


makeResponseType b1 b2 b3 =
    let
        f a b =
            case a of
                True ->
                    Just b

                False ->
                    Nothing
    in
    List.map2 f [ b1, b2, b3 ] [ Auth0.Code, Auth0.Token, Auth0.IdToken ]
        |> List.filterMap (\x -> x)
