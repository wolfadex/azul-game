module Main exposing (..)

import Array exposing (Array)
import Browser
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Random
import Random.List


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { players : Dict String Player
    , tileStores : Dict Int (List Tile)
    , tilePool : { firstPlayerToken : Bool, tiles : List Tile }
    , seed : Random.Seed
    , tileBag : List Tile
    , discardedTiles : List Tile
    , playerTurn : String
    }


type alias Player =
    { board : Board
    , staging :
        { rowOne : List TileOrEmpty
        , rowTwo : List TileOrEmpty
        , rowThree : List TileOrEmpty
        , rowFour : List TileOrEmpty
        , rowFive : List TileOrEmpty
        }
    , negatives : List Tile
    , score : Int
    , toPlace : List Tile
    , isFirstPlayer : Bool
    }


initPlayer : Player
initPlayer =
    { board =
        List.repeat 25 Empty
            |> Array.fromList
    , staging =
        { rowOne = [ Empty ]
        , rowTwo = [ Empty, Empty ]
        , rowThree = [ Empty, Empty, Empty ]
        , rowFour = [ Empty, Empty, Empty, Empty ]
        , rowFive = [ Empty, Empty, Empty, Empty, Empty ]
        }
    , negatives = []
    , score = 0
    , toPlace = []
    , isFirstPlayer = False
    }


type alias Board =
    Array TileOrEmpty


type TileOrEmpty
    = Empty
    | Tile Tile


from2dPointToIndex : Int -> ( Int, Int ) -> Int
from2dPointToIndex width ( x, y ) =
    y * width + x


fromIndexTo2dPoint : Int -> Int -> ( Int, Int )
fromIndexTo2dPoint width index =
    ( modBy width index, index // width )


type Tile
    = TileOne
    | TileTwo
    | TileThree
    | TileFour
    | TileFive


init : Int -> ( Model, Cmd Msg )
init initialSeed =
    ( { players =
            Dict.fromList
                [ ( "player1", initPlayer )
                , ( "player2", initPlayer )
                ]
      , tileStores =
            List.repeat 9 []
                |> List.indexedMap Tuple.pair
                |> Dict.fromList
      , tilePool =
            { firstPlayerToken = False
            , tiles = []
            }
      , tileBag = initialTiles
      , discardedTiles = []
      , seed = Random.initialSeed initialSeed
      , playerTurn = "player1"
      }
        |> newGameModel
        |> fillStores
        |> assignFirstPlayerToken
    , Cmd.none
    )


initialTiles : List Tile
initialTiles =
    List.repeat 20 TileOne
        ++ List.repeat 20 TileTwo
        ++ List.repeat 20 TileThree
        ++ List.repeat 20 TileFour
        ++ List.repeat 20 TileFive


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type Msg
    = NewGame
    | StoreTileClicked String Int Tile
    | PoolTileClicked String Tile
    | PlayerPoolTileClicked String Tile


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model
                |> newGameModel
                |> fillStores
                |> assignFirstPlayerToken
            , Cmd.none
            )

        StoreTileClicked playerId storeIndex tile ->
            let
                ( playerTiles, pileTiles ) =
                    model.tileStores
                        |> Dict.get storeIndex
                        |> Maybe.withDefault []
                        |> List.partition (\t -> t == tile)

                tilePool =
                    model.tilePool
            in
            ( { model
                | tileStores = Dict.insert storeIndex [] model.tileStores
                , tilePool =
                    { tilePool
                        | tiles = List.sortBy tileToComparable (model.tilePool.tiles ++ pileTiles)
                    }
                , players =
                    Dict.update
                        playerId
                        (Maybe.map
                            (\player ->
                                { player
                                    | toPlace = List.sortBy tileToComparable playerTiles
                                }
                            )
                        )
                        model.players
              }
            , Cmd.none
            )

        PoolTileClicked playerId tile ->
            Debug.todo ""

        PlayerPoolTileClicked playerId tile ->
            Debug.todo ""


newGameModel : Model -> Model
newGameModel model =
    let
        ( shuffledTiles, seed ) =
            Random.step
                (Random.List.shuffle initialTiles)
                model.seed
    in
    { model
        | players =
            Dict.fromList
                [ ( "player1", initPlayer )
                , ( "player2", initPlayer )
                ]
        , tileStores =
            List.repeat 9 []
                |> List.indexedMap Tuple.pair
                |> Dict.fromList
        , tilePool =
            { firstPlayerToken = False
            , tiles = []
            }
        , tileBag = shuffledTiles
        , discardedTiles = []
        , seed = seed
    }


fillStores : Model -> Model
fillStores model =
    let
        ( ( tileStores, tileBag ), nextSeed ) =
            Random.step
                (randomDoN 9
                    (\i ( stores, bag ) ->
                        Random.List.choices 4 bag
                            |> Random.map
                                (\( tiles, newBag ) ->
                                    ( Dict.insert i (List.sortBy tileToComparable tiles) stores, newBag )
                                )
                    )
                    ( Dict.empty, model.tileBag )
                )
                model.seed
    in
    { model
        | seed = nextSeed
        , tileStores = tileStores
        , tileBag = tileBag
    }


assignFirstPlayerToken : Model -> Model
assignFirstPlayerToken model =
    let
        ( firstPlayerId, nextSeed ) =
            Random.step
                (Random.List.shuffle (Dict.keys model.players)
                    |> Random.map List.head
                )
                model.seed
    in
    case firstPlayerId of
        Nothing ->
            model

        Just playerId ->
            { model
                | players =
                    Dict.update
                        playerId
                        (Maybe.map (\player -> { player | isFirstPlayer = True }))
                        model.players
                , playerTurn = playerId
            }


tileToComparable : Tile -> Int
tileToComparable tile =
    case tile of
        TileOne ->
            1

        TileTwo ->
            2

        TileThree ->
            3

        TileFour ->
            4

        TileFive ->
            5


randomDoN : Int -> (Int -> a -> Random.Generator a) -> a -> Random.Generator a
randomDoN count fn a =
    if count < 2 then
        fn count a

    else
        Random.andThen
            (\b ->
                Random.lazy
                    (\_ ->
                        randomDoN (count - 1) fn b
                    )
            )
            (fn count a)


view : Model -> Browser.Document Msg
view model =
    { title = "Azul"
    , body =
        [ model.tileStores
            |> Dict.toList
            |> List.map (viewTileStore model.playerTurn)
            |> Html.div []
        , model.tilePool.tiles
            |> List.map (viewTile (PoolTileClicked model.playerTurn))
            |> Html.div []
        , model.players
            |> Dict.toList
            |> List.map viewPlayer
            |> Html.div []
        ]
    }


viewTileStore : String -> ( Int, List Tile ) -> Html Msg
viewTileStore playerId ( index, tiles ) =
    Html.div
        [ Html.Attributes.style "padding" "1rem"
        , Html.Attributes.style "border" "2px solid black"
        , Html.Attributes.style "display" "inline-block"
        ]
        (List.map (viewTile (StoreTileClicked playerId index)) tiles)


viewTile : (Tile -> Msg) -> Tile -> Html Msg
viewTile clickHandler tile =
    Html.div
        [ Html.Attributes.style "padding" "1rem"
        , Html.Attributes.style "border" "2px solid black"
        , Html.Attributes.style "display" "inline-block"
        , Html.Events.onClick (clickHandler tile)
        ]
        [ Html.text (tileToString tile) ]


tileToString : Tile -> String
tileToString tile =
    case tile of
        TileOne ->
            "1"

        TileTwo ->
            "2"

        TileThree ->
            "3"

        TileFour ->
            "4"

        TileFive ->
            "5"


viewPlayer : ( String, Player ) -> Html Msg
viewPlayer ( id, player ) =
    let
        stagingWrapper : List TileOrEmpty -> List (Html Msg)
        stagingWrapper =
            List.indexedMap
                (\i tileOrEmpty ->
                    Html.div
                        [ Html.Attributes.style "grid-column" (String.fromInt (5 - i))
                        , Html.Attributes.style "grid-row" "1"
                        ]
                        [ viewTileOrEmpty tileOrEmpty ]
                )

        stagingGrid : List (Html Msg) -> Html Msg
        stagingGrid =
            Html.div
                [ Html.Attributes.style "display" "grid"
                , Html.Attributes.style "grid-template-columns" "repeat(5, 1fr)"
                ]
    in
    Html.div
        [ Html.Attributes.style "padding" "1rem"
        , Html.Attributes.style "border" "2px solid black"
        , Html.Attributes.style "display" "grid"
        , Html.Attributes.style "grid-template-columns" "repeat(2, 1fr)"
        ]
        [ Html.div
            []
            [ if player.isFirstPlayer then
                Html.div
                    [ Html.Attributes.style "padding" "1rem"
                    , Html.Attributes.style "border" "2px solid black"
                    , Html.Attributes.style "display" "inline-block"
                    ]
                    [ Html.text "Fst" ]

              else
                Html.text ""
            , player.staging.rowOne
                |> stagingWrapper
                |> stagingGrid
            , player.staging.rowTwo
                |> stagingWrapper
                |> stagingGrid
            , player.staging.rowThree
                |> stagingWrapper
                |> stagingGrid
            , player.staging.rowFour
                |> stagingWrapper
                |> stagingGrid
            , player.staging.rowFive
                |> stagingWrapper
                |> stagingGrid
            ]
        , player.board
            |> Array.toList
            |> List.map viewTileOrEmpty
            |> Html.div
                [ Html.Attributes.style "display" "grid"
                , Html.Attributes.style "grid-template-columns" "repeat(5, 1fr)"
                ]
        , player.toPlace
            |> List.map (viewTile (PlayerPoolTileClicked id))
            |> Html.div []
        ]


viewTileOrEmpty : TileOrEmpty -> Html Msg
viewTileOrEmpty tileOrEmpty =
    case tileOrEmpty of
        Empty ->
            Html.div
                [ Html.Attributes.style "padding" "1rem"
                , Html.Attributes.style "border" "2px solid black"
                , Html.Attributes.style "display" "inline-block"
                ]
                [ Html.text "E" ]

        Tile tile ->
            Html.div
                [ Html.Attributes.style "padding" "1rem"
                , Html.Attributes.style "border" "2px solid black"
                , Html.Attributes.style "display" "inline-block"
                ]
                [ Html.text (tileToString tile) ]
