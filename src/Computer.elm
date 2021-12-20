module Computer exposing (..)

-- Computer.elm contains functions for finding the computers move

import Board exposing (..)
import List
import Play exposing (..)



-- the number of moves ahead the computer looks


dep : Int
dep =
    5



-- gets the computer move using minMax algorithm


computerMove : Game -> Maybe Col
computerMove g =
    Tuple.second <| minMax g dep -inf inf True



-- Player who is maximizing their points (yellow)


maxPlayer : Sqr
maxPlayer =
    O


inf : Float
inf =
    1 / 0



-- check the number of n in a row vertically at a given position and assign score


evalVertical : Game -> Pos -> Sqr -> Int -> Float
evalVertical game ( i, j ) turn n =
    List.all (\x -> get ( i + x, j ) game.board == Just turn) (List.range 0 (n - 1))
        |> rowToScore n



-- check the number of n in a row horizontally at a given position and assign score


evalHorizontal : Game -> Pos -> Sqr -> Int -> Float
evalHorizontal game ( i, j ) turn n =
    List.all (\x -> get ( i, j + x ) game.board == Just turn) (List.range 0 (n - 1))
        |> rowToScore n



-- check the number of n in a row diagonally at a given position and assign score


evalDiagonal1 : Game -> Pos -> Sqr -> Int -> Float
evalDiagonal1 game ( i, j ) turn n =
    List.all (\x -> get ( i + x, j + x ) game.board == Just turn) (List.range 0 (n - 1))
        |> rowToScore n



-- check the number of n in a row diagonally in other direction  at a given position and assign score


evalDiagonal2 : Game -> Pos -> Sqr -> Int -> Float
evalDiagonal2 game ( i, j ) turn n =
    List.all (\x -> get ( i + x, j - x ) game.board == Just turn) (List.range 0 (n - 1))
        |> rowToScore n



-- takes a function and checks score for 2,3, and 4 in a row,


evalDirection : (Int -> Float) -> Float
evalDirection f =
    List.foldl (\n acc -> f n + acc) 0 [ 4, 3, 2 ]



-- If there is n in a row, assign points


rowToScore : Int -> Bool -> Float
rowToScore n b =
    case ( b, n ) of
        ( True, 4 ) ->
            inf

        ( True, 3 ) ->
            100

        ( True, 2 ) ->
            30

        _ ->
            0



-- Evaluates the score at a given position


evaluatePos : Game -> Pos -> Float
evaluatePos game pos =
    let
        checkList =
            [ evalVertical, evalHorizontal, evalDiagonal1, evalDiagonal2 ]

        maxPlayerVal =
            List.foldl
                (\f acc ->
                    evalDirection (f game pos maxPlayer) + acc
                )
                0
                checkList

        minPlayerVal =
            List.foldl
                (\f acc ->
                    evalDirection (f game pos (opsSqr maxPlayer)) + acc
                )
                0
                checkList
    in
    maxPlayerVal - minPlayerVal



-- finds score of entire board


evaluateBoard : Game -> Float
evaluateBoard game =
    List.foldl
        (\i acc1 ->
            List.foldl
                (\j acc2 ->
                    evaluatePos game ( i, j ) + acc2
                )
                acc1
                [ 0, 1, 2, 3, 4, 5, 6 ]
        )
        0
        [ 0, 1, 2, 3, 4, 5, 6 ]



-- Returns the opposite value the player is trying to achieve


playerMin : Bool -> Float
playerMin is_max_player =
    if is_max_player then
        -inf

    else
        inf



-- Using alpha-beta pruning to optimize minMax
--this function updates the new alpha and beta values


getAlphaBeta : Float -> Float -> Float -> Bool -> ( Float, Float )
getAlphaBeta score alpha beta is_max_player =
    case ( is_max_player, score < alpha, score < beta ) of
        ( True, True, _ ) ->
            ( alpha, beta )

        ( True, False, _ ) ->
            ( score, beta )

        ( False, _, True ) ->
            ( alpha, score )

        ( False, _, False ) ->
            ( alpha, beta )



-- gets the best score and between the previous best score and the current score


getBestScore : Float -> Maybe Col -> Float -> Maybe Col -> Bool -> ( Float, Maybe Col )
getBestScore score move bestScore bestMove is_max_player =
    case ( is_max_player, score > bestScore, bestMove ) of
        ( True, True, _ ) ->
            ( score, move )

        ( True, False, Nothing ) ->
            ( bestScore, move )

        ( True, False, _ ) ->
            ( bestScore, bestMove )

        ( False, False, _ ) ->
            ( score, move )

        ( False, True, Nothing ) ->
            ( bestScore, move )

        ( False, True, _ ) ->
            ( bestScore, bestMove )



-- checks if the game has winner


hasWinner : Game -> Bool
hasWinner game =
    case game.winner of
        Nothing ->
            False

        _ ->
            True



-- Min max function with alpha-beta pruning


minMax : Game -> Int -> Float -> Float -> Bool -> ( Float, Maybe Col )
minMax game depth a b is_max_player =
    if (depth == 0) || hasWinner game then
        ( evaluateBoard game, Nothing )

    else
        let
            -- checks all valid moves and finds the best move
            loop : List Col -> ( Float, Maybe Col ) -> Float -> Float -> ( Float, Maybe Col )
            loop lst ( bestScore, bestMove ) alpha beta =
                case lst of
                    [] ->
                        ( bestScore, bestMove )

                    col :: restCol ->
                        case makeMove game col of
                            Nothing ->
                                loop restCol ( bestScore, bestMove ) alpha beta

                            Just futureGame ->
                                let
                                    score =
                                        minMax futureGame (depth - 1) alpha beta (not is_max_player) |> Tuple.first

                                    ( newScore, newMove ) =
                                        getBestScore score (Just col) bestScore bestMove is_max_player

                                    ( newAlpha, newBeta ) =
                                        getAlphaBeta newScore alpha beta is_max_player
                                in
                                if newBeta <= newAlpha then
                                    ( newScore, newMove )

                                else
                                    loop restCol ( newScore, newMove ) newAlpha newBeta
        in
        -- check valid positions in this order
        loop [ 3, 4, 2, 5, 1, 6, 0 ] ( playerMin is_max_player, Nothing ) a b
