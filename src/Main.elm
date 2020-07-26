module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Url
import Url.Parser as UrlParser exposing ((</>))
import Element exposing (..)
import Element.Font as Font
import Element.Background as Bg
import Element.Border as Br
import Element.Input as In
import Element.Events exposing (..)
import Http
import Conf
import Json.Decode as D
import Json.Encode as E

-- ANCHOR Model
type alias Model =
  { key : Nav.Key
  , route : Route
  , device : Element.Device
  , rootFontSize : Int
  , errorPopup : ErrorPopup
  , confirmPopup : ConfirmPopup
  , user : RemoteUser
  , loginEmail : String
  , loginPassword : String
  , signupEmail : String
  , signupPassword : String
  , signupAcceptTerms : Bool
  }

type RemoteUser = Loading | LoggedOut | LoggedIn User
type alias User =
  { accountID : String
  , email : String
  , fullName : String
  }
userDecoder : D.Decoder User
userDecoder =
 D.map3 User
  (D.field "account_id" D.string)
  (D.field "email" D.string)
  (D.field "full_name" D.string)

type alias ErrorPopup =
  { message : String
  , isOpen : Bool
  }

type ConfirmPopup = ConfirmClosed | ConfirmOpen Msg

-- ANCHOR Msg
type Msg
  = OnUrlRequest Browser.UrlRequest
  | OnUrlChange Url.Url
  | OnResize Int Int
  | CloseErrorPopup
  | CloseConfirmPopup
  | Confirm Msg
  | UserReceived (Result HttpError User)
  | OnLogout
  | LogoutResponse (Result HttpError String)
  | OnLoginEmailInput String
  | OnLoginPasswordInput String
  | OnLogin
  | LoginResponse (Result HttpError User)
  | OnSignupEmailInput String
  | OnSignupPasswordInput String
  | OnSignupAcceptTermsCheckbox Bool
  | OnSignup
  | SignupResponse (Result HttpError User)

-- ANCHOR Route
type Route
  = Home
  | Login
  | Signup
  | ContactsPage
  | TermsPage
  | NotFound
  | ProtectedRouteTag ProtectedRoute

type ProtectedRoute
  = ProtectedPage String

route : UrlParser.Parser (Route -> a) a
route =
  UrlParser.oneOf
    [ UrlParser.map Home UrlParser.top
    , UrlParser.map Login (UrlParser.s "login")
    , UrlParser.map Signup (UrlParser.s "signup")
    , UrlParser.map ContactsPage (UrlParser.s "contacts")
    , UrlParser.map TermsPage (UrlParser.s "terms")
    , UrlParser.map ProtectedRouteTag protectedRoute
    ]

protectedRoute : UrlParser.Parser (ProtectedRoute -> a) a
protectedRoute =
  UrlParser.oneOf
    [ UrlParser.map ProtectedPage (UrlParser.s "protected-page" </> UrlParser.string)
    ]

toRoute : Url.Url -> Route
toRoute url =
  Maybe.withDefault NotFound (UrlParser.parse route url)

-- ANCHOR View.layout
view : Model -> Browser.Document Msg
view model =
  { title = "Elm application starter"
  , body =
    [
      layout
        [ Font.family
          [ Font.typeface "Nanum Gothic"
          ]
        , Font.size model.rootFontSize
        , Font.light
        , Br.widthEach {bottom=0,left=0,right=0,top=3}
        , Br.color colors.red
        , inFront (if model.errorPopup.isOpen then
          column
              [ width fill
              , height fill
              , Bg.color <| rgba255 0 0 0 0.8
              , spacing 50
              ]
              [ paragraph
                  [ width fill
                  , centerY
                  , Font.center
                  , padding 50
                  , Font.color white
                  ] [text model.errorPopup.message]
              , In.button
                  [ centerY
                  , centerX
                  -- , paddingXY 30 10
                  -- , Bg.color white
                  -- , Font.color colors.blue
                  , Font.color colors.red
                  , Font.heavy
                  , Font.size <| biggerFont model 2
                  ] {onPress = Just CloseErrorPopup, label = text "Close"}
              ]
      else
          text "")
      , inFront (case model.confirmPopup of
          ConfirmClosed ->
              text ""
          ConfirmOpen msg ->
              column
                  [ centerY
                  , centerX
                  , Bg.color white
                  , Br.width 5
                  , Br.color colors.blue
                  , width <| px 500
                  ]
                  [ el
                      [ padding 50
                      , centerY
                      , centerX
                      , Bg.color white
                      , Font.color colors.blue
                      , Font.bold
                      ] <| text "Are you sure?"
                  , row
                      [ width fill
                      ]
                      [ In.button
                          [ paddingXY 30 10
                          , Bg.color colors.red
                          , Font.color white
                          , alignLeft
                          , Font.heavy
                          ] {onPress = Just msg, label = text "Yes"}
                      , In.button
                          [ paddingXY 30 10
                          , Bg.color white
                          , Font.color colors.blue
                          , alignRight
                          , Font.hairline
                          ] {onPress = Just CloseConfirmPopup, label = text "Cancel"}
                      ]
                  ])
      ] <|
      column
          [ width fill
          , height fill
          , Bg.color colors.light
          , Font.color colors.blue
          ]
          [ header model
          , mainView model
          ]
    ]
  }

-- ANCHOR View.header
header : Model -> Element Msg
header model =
    row
        [ width fill
        -- , Bg.color <| dimmer colorMainBg 1.1
        , paddingXY 30 5
        , spacing 50
        ] <|
        [ row
          [spacing 5]
          [ column
              [ Font.family [Font.monospace]
              ]
              [ text "╔═══╗"
              , text "║≡≡≡║"
              , text "║▀  ║"
              , text "╚═══╝"
              ]
          , el [Font.size <| biggerFont model 1.5] <| text "Formsnap"
          ]
        , menuLink [] {url = "/", label = text "Home"}
        , menuLink [] {url = "/contacts", label = text "Contacts"}
        , menuLink [] {url = "/terms", label = text "Terms & Privacy"}
        , row [alignRight, spacing 15]
          (case model.route of
            Login ->
              [ text "Don't have an account yet?"
              , menuLink [] {url = "/signup", label = text "Signup"}
              ]
            Signup ->
              [ text "Already have an account?"
              , menuLink [alignRight] {url = "/login", label = text "Login"}
              ]
            ProtectedRouteTag protectedRoute_ ->
              [ case model.user of
                  LoggedIn user ->
                      row
                          [ alignRight
                          , spacing 20
                          ]
                          [ paragraph [moveUp 2]
                              [ el [Font.hairline] <| text <| "Logged in as " ++ user.fullName ++ " "
                              , el [Font.regular] <| text user.email
                              ]
                          , menuLink [onClick OnLogout] {url = "#", label = text "Logout"}
                          ]
                      
                  _ ->
                      text ""
              ]
            _ ->
              [ menuLink [alignRight] {url = "/login", label = text "Login"}
              , menuLink [] {url = "/signup", label = text "Signup"}
              ])
        ]

-- ANCHOR View.main
mainView : Model -> Element Msg
mainView model =
    case model.route of
        Home ->
          homeView model
        Login ->
          loginView model
        Signup ->
          signupView model
        ContactsPage ->
            contactsView model
        TermsPage ->
            termsView model
        NotFound ->
            notFoundView model
        ProtectedRouteTag protectedRoute_ ->
            case model.user of
                Loading ->
                    el
                        [ centerX
                        , centerY
                        , spacing 20
                        , moveUp 100
                        , Font.size <| biggerFont model 1.4
                        ] <| text "Loading your user profile"
                LoggedOut ->
                    column
                        [ centerX
                        , centerY
                        , spacing 20
                        , moveUp 100
                        , Font.size <| biggerFont model 1.4
                        ]
                        [ el
                            [ centerX
                            ] <| text "You are not logged in"
                        , menuLink 
                            [ centerX
                            , Font.bold
                            ]
                            { url = ""
                            , label = text "Click here to login"
                            }
                        ]
                LoggedIn user ->
                    case protectedRoute_ of
                        ProtectedPage arg ->
                            protectedPageView model user arg

homeView : Model -> Element Msg
homeView model =
    boldLabel [centerX, centerY] model "Home page"

-- ANCHOR View.login
loginView : Model -> Element Msg
loginView model =
    el
      [ centerX
      , centerY
      ] <|
      column
        [ spacing 20
        , Font.size <| smallerFont model 1.2
        , width <| px 250
        ]
        [ paragraph
          [ Font.center
          , Font.size <| biggerFont model 1
          , moveUp 10
          ]
          [text "Hello, who's this?"]
        , In.email
          []
          { onChange = OnLoginEmailInput
          , text = model.loginEmail
          , placeholder = Just <| In.placeholder [] <| text "aaa@bb.cc"
          , label = In.labelAbove [] <| text "Email"
          }
        , In.currentPassword
            []
            { onChange = OnLoginPasswordInput
            , text = model.loginPassword
            , placeholder = Just <| In.placeholder [] <| text "At least 8 characters"
            , label = In.labelAbove [] <| text "Password"
            , show = False
            }
        , link
          [ moveUp 15
          , Font.underline
          , Font.size <| smallerFont model 1.5
          ]
          { url = "/restore-password"
          , label = text "I forgot my password"
          }
        , In.button
          [ width fill
          , Bg.color black
          , Font.color white
          , Font.center
          , paddingXY 0 10
          ]
          { onPress = Just OnLogin
          , label = text "Log in to Formsnap"
          }
        ]

-- ANCHOR View.signup
signupView : Model -> Element Msg
signupView model =
    el
      [ centerX
      , centerY
      ] <|
      column
        [ spacing 20
        , Font.size <| smallerFont model 1.2
        , width <| px 300
        ]
        [ paragraph
          [ centerX
          , Font.size <| biggerFont model 1
          , moveUp 10
          ]
          [text "Get better data with conversational forms, surveys, quizzes & more"]
        , In.email
          []
          { onChange = OnSignupEmailInput
          , text = model.signupEmail
          , placeholder = Just <| In.placeholder [] <| text "aaa@bb.cc"
          , label = In.labelAbove [] <| text "Email"
          }
        , In.newPassword
            []
            { onChange = OnSignupPasswordInput
            , text = model.signupPassword
            , placeholder = Just <| In.placeholder [] <| text "At least 8 characters"
            , label = In.labelAbove [] <| text "Password"
            , show = False
            }
        , In.checkbox
          [ moveUp 10
          ]
          { onChange = OnSignupAcceptTermsCheckbox
          , icon = In.defaultCheckbox
          , checked = model.signupAcceptTerms
          , label = In.labelRight [width fill] <|
            paragraph
              []
              [ text "I agree to Formsnap’s "
              , newTabLink [Font.underline] {url = "/terms", label = text "Terms of Service"}
              ]
          }
        , In.button
          [ width fill
          , Bg.color black
          , Font.color white
          , Font.center
          , paddingXY 0 10
          ]
          { onPress = Just OnSignup
          , label = text "Create my free account"
          }
        ]

contactsView : Model -> Element Msg
contactsView model =
    boldLabel [centerX, centerY] model "Contacts page"

termsView : Model -> Element Msg
termsView model =
    boldLabel [centerX, centerY] model "Terms & privacy"

notFoundView : Model -> Element Msg
notFoundView model =
    boldLabel [centerX, centerY] model "Page not found"

protectedPageView : Model -> User -> String -> Element Msg
protectedPageView model user arg =
    boldLabel [centerX, centerY] model "Protected page"

-- ANCHOR Update
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    OnUrlRequest urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          (model, Nav.replaceUrl model.key url.path)
        Browser.External href ->
          (model, Nav.load href)
    OnUrlChange url ->
      ({model | route = toRoute url}, Cmd.none)
    OnResize width height ->
      let
        device = classifyDevice {width = width, height = height }
      in
      ({ model | device = device, rootFontSize = getRootFontSize device }, Cmd.none)
    CloseErrorPopup ->
      ({ model | errorPopup = {message = "", isOpen = False}}, Cmd.none)
    CloseConfirmPopup ->
      ({ model | confirmPopup = ConfirmClosed}, Cmd.none)
    Confirm msg_ ->
      ({ model | confirmPopup = ConfirmOpen msg_ }, Cmd.none)
    UserReceived result ->
      case result of
        (Ok user) ->
          ({ model | user = LoggedIn user}, Cmd.none)
        (Err error) ->
          updateOnHttpError model error
    OnLogout ->
      (model, logout)
    LogoutResponse result ->
      case result of
        (Ok _) ->
          ({model | user = LoggedOut}, Cmd.none)
        (Err error) ->
          updateOnHttpError model error
    OnLoginEmailInput value ->
      ({model | loginEmail = value}, Cmd.none)
    OnLoginPasswordInput value ->
      ({model | loginPassword = value}, Cmd.none)
    OnLogin ->
      (model, login {email = model.loginEmail, password = model.loginPassword})
    LoginResponse result ->
      case result of
        (Ok user) ->
          ({ model | user = LoggedIn user}, Nav.pushUrl model.key <| "/protected-page/" ++ user.accountID)
        (Err error) ->
          updateOnHttpError model error
    OnSignupEmailInput value ->
      ({model | signupEmail = value}, Cmd.none)
    OnSignupPasswordInput value ->
      ({model | signupPassword = value}, Cmd.none)
    OnSignupAcceptTermsCheckbox value ->
      ({model | signupAcceptTerms = value}, Cmd.none)
    OnSignup ->
      (model, signup {email = model.signupEmail, password = model.signupPassword, acceptTerms = model.signupAcceptTerms})
    SignupResponse result ->
      case result of
        (Ok user) ->
          ({ model | user = LoggedIn user}, Nav.pushUrl model.key <| "/protected-page/" ++ user.accountID)
        (Err error) ->
          updateOnHttpError model error


-- ANCHOR Sub
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Browser.Events.onResize OnResize
    ]

-- ANCHOR Main
type alias Flags =
    { width : Int
    , height : Int
    }

init : Flags -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key =
  let
    r = toRoute url
    device = classifyDevice {width = flags.width, height = flags.height }
  in
  ( { key = key
    , route = r
    , device = device
    , rootFontSize = getRootFontSize device
    , errorPopup = {message = "", isOpen = False}
    , confirmPopup = ConfirmClosed
    , user = Loading
    , loginEmail = ""
    , loginPassword = ""
    , signupEmail = ""
    , signupPassword = ""
    , signupAcceptTerms = False
    }
  , loadUser
  )

main : Program Flags Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = OnUrlRequest
    , onUrlChange = OnUrlChange
    }

-- ANCHOR Commands
loadUser : Cmd Msg
loadUser =
  Http.riskyRequest
    { method = "GET"
    , headers = []
    , url = Conf.apiHost ++ "/whoami"
    , body = Http.emptyBody
    , expect = expectJson UserReceived userDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

login : {email : String, password : String} -> Cmd Msg
login data =
  Http.riskyRequest
    { method = "POST"
    , headers = []
    , url = Conf.apiHost ++ "/login"
    , body = Http.jsonBody <| E.object
        [ ("email", data.email |> E.string)
        , ("password", data.password |> E.string)
        ]
    , expect = expectJson LoginResponse userDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

signup : {email : String, password : String, acceptTerms : Bool} -> Cmd Msg
signup data =
  Http.riskyRequest
    { method = "PUT"
    , headers = []
    , url = Conf.apiHost ++ "/account"
    , body = Http.jsonBody <| E.object
        [ ("email", data.email |> E.string)
        , ("password", data.password |> E.string)
        ]
    , expect = expectJson SignupResponse userDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

logout : Cmd Msg
logout =
  Http.riskyRequest
    { method = "GET"
    , headers = []
    , url = Conf.apiHost ++ "/logout"
    , body = Http.emptyBody
    , expect = expectString LogoutResponse
    , timeout = Nothing
    , tracker = Nothing
    }

-- ANCHOR Utils
colors : { blue : Color, light : Color, lightblue : Color, mint : Color, red : Color}
colors =
    { red = rgb255 230 57 70
    , light = rgb255 241 250 238
    , mint = rgb255 168 218 220
    , lightblue = rgb255 69 123 157
    , blue = rgb255 29 53 87
    }
black : Color
black = rgb255 0 0 0
white : Color
white = rgb255 255 255 255
colorMainBg : Color
colorMainBg = colors.light

dimmer : Color -> Float -> Color
dimmer color magnitude =
    let
        r = toRgb color
    in
    fromRgb {r | red = r.red / magnitude, green = r.green / magnitude, blue = r.blue / magnitude}

lighter : Color -> Float -> Color
lighter color magnitude =
    let
        r = toRgb color
    in
    fromRgb {r | red = r.red * magnitude, green = r.green * magnitude, blue = r.blue * magnitude}

biggerFont : Model -> Float -> Int
biggerFont model magnitude =
    toFloat model.rootFontSize * magnitude |> round
smallerFont : Model -> Float -> Int
smallerFont model magnitude =
    toFloat model.rootFontSize / magnitude |> round

menuLink : List (Attribute Msg) -> {url : String, label : Element Msg} -> Element Msg
menuLink attrs body =
    link
        ( attrs ++
            [ Br.widthEach {top=0, left=0, right=0, bottom=3}
            , mouseOver [Font.color colors.lightblue]
            ]
        ) body

boldLabel : List (Attribute Msg) -> Model -> String -> Element Msg
boldLabel attrs model message =
    el ([Font.color colors.mint, Font.size <| biggerFont model 2, Font.heavy]++attrs) <| text message

getRootFontSize : Device -> Int
getRootFontSize device =
  case device.class of
      Phone ->
        10
      Tablet ->
        12
      Desktop ->
        16
      BigDesktop ->
        18

type HttpError
    = HttpBadUrl String
    | HttpTimeout
    | HttpNetworkError
    | HttpBadStatus Int String
    | HttpBadBody String

httpErrorToShortString : HttpError -> String
httpErrorToShortString error =
  case error of
    HttpBadUrl text ->
      "Bad URL: " ++ text

    HttpTimeout ->
      "HTTP timeout"

    HttpNetworkError ->
      "Network error"

    HttpBadStatus code body ->
      "Bad HTTP status: " ++ String.fromInt code

    HttpBadBody message ->
      "Bad HTTP payload: " ++ message

httpErrorToFullString : HttpError -> String
httpErrorToFullString error =
  case error of
    HttpBadUrl text ->
      "Bad URL: " ++ text

    HttpTimeout ->
      "HTTP timeout"

    HttpNetworkError ->
      "Network error"

    HttpBadStatus code body ->
      "Bad HTTP status: " ++ String.fromInt code ++ " " ++ body

    HttpBadBody message ->
      "Bad HTTP payload: " ++ message

httpErrorCode : HttpError -> Int
httpErrorCode error =
  case error of
    HttpBadStatus code body ->
      code
    _ ->
      0

updateOnHttpError : Model -> HttpError -> (Model, Cmd Msg)
updateOnHttpError model error =
  let
    statusCode = httpErrorCode error
  in
  case statusCode of
    401 ->
      case model.route of
        ProtectedRouteTag _ ->
          ({ model | user = LoggedOut}, Nav.pushUrl model.key "/")
        _ ->
          ({ model | user = LoggedOut}, Cmd.none)
    _ ->
      ({ model | errorPopup = {message = httpErrorToFullString error, isOpen = True}}, Cmd.none)

-- Custom implementation of expectJson
expectJson : (Result HttpError a -> msg) -> D.Decoder a -> Http.Expect msg
expectJson toMsg decoder =
  Http.expectStringResponse toMsg <|
    \response ->
      case response of
        Http.BadUrl_ url ->
          Err (HttpBadUrl url)

        Http.Timeout_ ->
          Err HttpTimeout

        Http.NetworkError_ ->
          Err HttpNetworkError

        Http.BadStatus_ metadata body ->
          Err (HttpBadStatus metadata.statusCode body)

        Http.GoodStatus_ metadata body ->
          case D.decodeString decoder body of
            Ok value ->
              Ok value

            Err err ->
              Err (HttpBadBody (D.errorToString err))

expectString : (Result HttpError String -> msg) -> Http.Expect msg
expectString toMsg =
  Http.expectStringResponse toMsg <|
    \response ->
      case response of
        Http.BadUrl_ url ->
          Err (HttpBadUrl url)

        Http.Timeout_ ->
          Err HttpTimeout

        Http.NetworkError_ ->
          Err HttpNetworkError

        Http.BadStatus_ metadata body ->
          Err (HttpBadStatus metadata.statusCode body)

        Http.GoodStatus_ metadata body ->
          Ok body


expectWhatever : (Result HttpError () -> msg) -> Http.Expect msg
expectWhatever toMsg =
  Http.expectStringResponse toMsg <|
    \response ->
      case response of
        Http.BadUrl_ url ->
          Err (HttpBadUrl url)

        Http.Timeout_ ->
          Err HttpTimeout

        Http.NetworkError_ ->
          Err HttpNetworkError

        Http.BadStatus_ metadata body ->
          Err (HttpBadStatus metadata.statusCode body)

        Http.GoodStatus_ metadata body ->
          Ok ()