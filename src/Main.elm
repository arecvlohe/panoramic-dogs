port module Main exposing (main)

import Array
import Browser
import Debug exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, button, div, img, li, text, ul)
import Html.Attributes exposing (disabled, src, style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Json.Encode as E



-- Helper functions


flattenBreeds : DogsDictList -> List String
flattenBreeds =
    Dict.foldl
        (\breed subBreeds list ->
            if List.isEmpty subBreeds then
                breed :: list

            else
                List.map
                    (\subBreed -> subBreed ++ " " ++ breed)
                    subBreeds
                    ++ list
        )
        []
        >> List.sort


getBreedAndSubBreed : String -> String
getBreedAndSubBreed =
    String.split " " >> List.reverse >> String.join "/"


getDogsList : Cmd Msg
getDogsList =
    Http.get
        { url = dogsBaseUrl ++ "/breeds/list/all"
        , expect = Http.expectJson GetDogsList dogsListResponseDecoder
        }


getBreedPics : String -> Cmd Msg
getBreedPics name =
    Http.get
        { url = dogsBaseUrl ++ "/breed/" ++ name ++ "/images"
        , expect = Http.expectJson (GetBreedPics name) breedPicsResponseDecoder
        }



-- Constants


offset : Int
offset =
    20


dogsBaseUrl : String
dogsBaseUrl =
    "https://dog.ceo/api"



-- API Types


type alias DogsListResponse =
    { message : Dict String (List String), status : String }


type alias DogsDictList =
    Dict String (List String)


type alias BreedPicsResponse =
    { message : List String, status : String }



-- Encoders & Decoders


dogsListResponseDecoder : D.Decoder DogsListResponse
dogsListResponseDecoder =
    D.map2 DogsListResponse
        (D.field "message" (D.dict (D.list D.string)))
        (D.field "status" D.string)


breedPicsResponseDecoder : D.Decoder BreedPicsResponse
breedPicsResponseDecoder =
    D.map2 BreedPicsResponse
        (D.field "message" (D.list D.string))
        (D.field "status" D.string)


listDecoder : D.Decoder (List String)
listDecoder =
    D.list D.string


listEncoder : List String -> E.Value
listEncoder =
    E.list E.string



-- Program types


type alias Data =
    { current : String -- either "all" or breed name
    , data : List String
    }


type View
    = NoData
    | Loading
    | Failure
    | Success Data


type alias Model =
    { view : View
    , page :
        { cursor : Int
        , total : Int
        }
    }


type Direction
    = Back
    | Forward


type Msg
    = GetDogsList (Result Http.Error DogsListResponse)
    | GetBreedPics String (Result Http.Error BreedPicsResponse)
    | ChangeView String
    | Go Direction
    | GetLocalData ( String, Maybe (List String) )



-- Program logic


port setStorage : ( String, E.Value ) -> Cmd msg


port requestStorage : String -> Cmd msg


port returnStorage : (( String, Maybe (List String) ) -> msg) -> Sub msg


main : Program E.Value Model Msg
main =
    Browser.element
        { init = init
        , update = updateWithStorage
        , view = view
        , subscriptions = subscriptions
        }


init : E.Value -> ( Model, Cmd Msg )
init flags =
    case D.decodeValue listDecoder flags of
        Ok list ->
            ( { view =
                    Success
                        { current = "all"
                        , data = list
                        }
              , page =
                    { cursor = 0
                    , total = 0
                    }
              }
            , Cmd.none
            )

        Err _ ->
            ( { view = Loading
              , page = { cursor = 0, total = 0 }
              }
            , getDogsList
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ returnStorage GetLocalData ]


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg oldModel =
    let
        ( newModel, cmds ) =
            update msg oldModel
    in
    case msg of
        GetDogsList _ ->
            case newModel.view of
                Success v ->
                    ( newModel, Cmd.batch [ setStorage ( "all", listEncoder v.data ), cmds ] )

                _ ->
                    ( newModel, cmds )

        GetBreedPics name _ ->
            case newModel.view of
                Success v ->
                    ( newModel, Cmd.batch [ setStorage ( name, listEncoder v.data ), cmds ] )

                _ ->
                    ( newModel, cmds )

        _ ->
            ( newModel, cmds )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetDogsList response ->
            case response of
                Ok result ->
                    let
                        list =
                            flattenBreeds result.message
                    in
                    if result.status /= "success" then
                        ( { model | view = Failure }, Cmd.none )

                    else
                        ( { model
                            | view =
                                Success
                                    { current = "all"
                                    , data = list
                                    }
                          }
                        , Cmd.none
                        )

                Err _ ->
                    ( { model | view = Failure }, Cmd.none )

        ChangeView name ->
            ( { model | view = Loading }
            , requestStorage name
            )

        GetLocalData ( breed, data ) ->
            case data of
                Just d ->
                    ( { model
                        | view =
                            Success
                                { current = breed
                                , data = d
                                }
                        , page =
                            { cursor = 0
                            , total = List.length d
                            }
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model
                    , if breed == "all" then
                        getDogsList

                      else
                        getBreedPics breed
                    )

        GetBreedPics name response ->
            case response of
                Ok result ->
                    let
                        images =
                            result.message
                    in
                    if result.status /= "success" then
                        ( { model | view = Failure }, Cmd.none )

                    else
                        ( { model
                            | view =
                                Success
                                    { current = name
                                    , data = images
                                    }
                            , page = { cursor = 0, total = List.length images }
                          }
                        , Cmd.none
                        )

                Err _ ->
                    ( { model | view = Failure }, Cmd.none )

        Go direction ->
            let
                page =
                    model.page
            in
            ( { model
                | page =
                    { page
                        | cursor =
                            case direction of
                                Back ->
                                    page.cursor - offset

                                Forward ->
                                    page.cursor + offset
                    }
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case model.view of
        NoData ->
            div
                [ style "width" "100%"
                , style "min-height" "100%"
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                ]
                [ text "Loading" ]

        Loading ->
            div
                [ style "width" "100%"
                , style "height" "100%"
                , style "display" "flex"
                , style "justify-content" "center"
                , style "align-items" "center"
                ]
                [ text "Loading" ]

        Failure ->
            div [] [ text "Sorry, there was an error :(" ]

        Success r ->
            if r.current == "all" then
                ul
                    [ style "list-style" "none" ]
                    (List.map
                        (\name ->
                            li []
                                [ button
                                    [ style "font-size" "16px"
                                    , style "background" "white"
                                    , style "border" "none"
                                    , style "cursor" "pointer"
                                    , style "text-decoration" "underline"
                                    , onClick (ChangeView (getBreedAndSubBreed name))
                                    ]
                                    [ text name ]
                                ]
                        )
                        r.data
                    )

            else
                let
                    isFirstPage =
                        model.page.cursor == 0

                    isLastPage =
                        model.page.cursor >= ((model.page.total // offset) * offset)
                in
                div
                    [ style "display" "flex"
                    , style "justify-content" "center"
                    , style "flex-wrap" "wrap"
                    , style "gap" "8px"
                    , style "padding" "16px"
                    ]
                    (List.map
                        (\url ->
                            img
                                [ src url
                                , style "object-fit" "cover"
                                , style "width" "300px"
                                , style "height" "300px"
                                ]
                                []
                        )
                        r.data
                        |> Array.fromList
                        |> Array.slice model.page.cursor (model.page.cursor + offset)
                        |> Array.toList
                        |> List.append
                            [ div
                                [ style "display" "flex"
                                , style "width" "100%"
                                , style "justify-content" "center"
                                , style "padding" "8px 8px"
                                , style "gap" "8px"
                                ]
                                [ button
                                    [ disabled isFirstPage
                                    , onClick (Go Back)
                                    , style "font-size" "16px"
                                    , style "background-color" "white"
                                    , style "padding" "8px 16px"
                                    , style "border"
                                        ("1px solid "
                                            ++ (if isFirstPage then
                                                    "gray"

                                                else
                                                    "black"
                                               )
                                        )
                                    , style "border-radius" "5%"
                                    , style "cursor"
                                        (if isFirstPage then
                                            "not-allowed"

                                         else
                                            "pointer"
                                        )
                                    ]
                                    [ text "Back" ]
                                , button
                                    [ disabled isLastPage
                                    , onClick (Go Forward)
                                    , style "font-size" "16px"
                                    , style "background-color" "white"
                                    , style "padding" "8px 16px"
                                    , style "border"
                                        ("1px solid "
                                            ++ (if isLastPage then
                                                    "gray"

                                                else
                                                    "black"
                                               )
                                        )
                                    , style "border-radius" "5%"
                                    , style "cursor"
                                        (if isLastPage then
                                            "not-allowed"

                                         else
                                            "pointer"
                                        )
                                    ]
                                    [ text "Next" ]
                                ]
                            ]
                        |> List.append
                            [ div
                                [ style "width" "100%"
                                , style "text-align" "center"
                                , style "text-transform" "uppercase"
                                ]
                                [ text ("Total Images: " ++ String.fromInt model.page.total)
                                ]
                            ]
                        |> List.append
                            [ div
                                [ style "display" "flex"
                                , style "justify-content" "center"
                                , style "align-items" "center"
                                ]
                                [ button
                                    [ onClick (ChangeView "all")
                                    , style "font-size" "16px"
                                    , style "background-color" "white"
                                    , style "padding" "8px 16px"
                                    , style "border" "1px solid black "
                                    , style "border-radius" "5%"
                                    , style "cursor" "pointer"
                                    ]
                                    [ text "Return Home" ]
                                ]
                            ]
                    )
