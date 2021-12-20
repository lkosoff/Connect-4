module Play exposing (..)

-- Play file contain the game logic for placing a piece and determining a winner

import Board exposing (..)



-- A column on the board


type alias Col =
    Int


newGame : Game
newGame =
    { board = emptyBoard, moves = [], turn = X, winner = Nothing }


type alias Game =
    { board : Board, moves : List Pos, turn : Sqr, winner : Maybe Sqr }



-- Finds the first free space in a given column


getFreeSpace : Board -> Col -> Maybe Pos
getFreeSpace board col =
    let
        setInColumn i =
            if i < 0 then
                Nothing

            else
                case get ( i, col ) board of
                    Just E ->
                        Just ( i, col )

                    _ ->
                        setInColumn (i - 1)
    in
    setInColumn 6



-- Insert a piece into a column in the board.
-- If invalid move, return Nothing


placePiece : Game -> Col -> Maybe Game
placePiece game col =
    case game.winner of
        Just _ ->
            Just game

        Nothing ->
            case getFreeSpace game.board col of
                Nothing ->
                    Nothing

                Just ( i, j ) ->
                    Just
                        { game
                            | board = set ( i, j ) game.turn game.board
                            , turn = opsSqr game.turn
                            , moves = ( i, j ) :: game.moves
                        }



-- check if the there is a winner yet


checkGameStatus : Game -> Game
checkGameStatus game =
    case game.moves of
        [] ->
            game

        ( i, j ) :: xs ->
            if winAtPosition (opsSqr game.turn) ( i, j ) game.board then
                { game | winner = Just (opsSqr game.turn) }

            else if List.all (\c -> get ( 0, c ) game.board /= Just E) (List.range 0 5) then
                { game | winner = Just E }

            else
                game



--sets piece and checks if winner


makeMove : Game -> Col -> Maybe Game
makeMove game col =
    Maybe.map checkGameStatus (placePiece game col)



-- checks if there is n in a row a given position


nInARow : Sqr -> Pos -> Int -> Board -> Bool
nInARow sqr ( i, j ) n board =
    let
        nList =
            List.range 0 (n - 1)

        verticalWin =
            --List.all (\x -> get ( i + x, j ) board == Just sqr) nList
            List.any
                (\row ->
                    List.all (\x -> get ( i + x - row, j ) board == Just sqr) nList
                 -- Checks if the next n color in the row are the same
                )
                --checks starting at a different position
                nList

        horizontalWin =
            List.any
                (\col ->
                    List.all (\x -> get ( i, col + x ) board == Just sqr) nList
                 -- Checks if the next n color in the row are the same
                )
                --checks starting at a different position
                (List.map (\x -> j - x) nList)

        diagonalWin1 =
            List.any
                (\offset ->
                    List.all (\x -> get ( offset + i + x, offset + j + x ) board == Just sqr) nList
                 -- Checks if the next n color in the row are the same
                )
                --checks starting at a different position
                (List.map (\x -> -x) nList)

        diagonalWin2 =
            List.any
                (\offset ->
                    List.all (\x -> get ( i - offset + x, j + offset - x ) board == Just sqr) [ 0, 1, 2, 3 ]
                 -- Checks if the next n color in the row are the same
                )
                --checks starting at a different position
                nList
    in
    horizontalWin || verticalWin || diagonalWin1 || diagonalWin2



-- checks if there is 4 in a row at a given position


winAtPosition : Sqr -> Pos -> Board -> Bool
winAtPosition sqr pos board =
    nInARow sqr pos 4 board


getBoard : Game -> Board
getBoard game =
    game.board



-- sets the last move to empty and changes turn


makeUndo : Game -> Game
makeUndo game =
    case ( game.winner, game.moves ) of
        ( Just _, _ ) ->
            game

        ( Nothing, [] ) ->
            game

        ( Nothing, lastMove :: moves ) ->
            { game
                | board = set lastMove E game.board
                , turn = opsSqr game.turn
                , moves = moves
            }
