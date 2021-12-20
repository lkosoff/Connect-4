module Board exposing (Board, Pos, Sqr(..), emptyBoard, get, opsSqr, set)

-- Board file  contains board representation and function for accessing the board
-- Sqr : different game pieces, player X, Player O, and empty E


type Sqr
    = X
    | O
    | E



-- Position on the board


type alias Pos =
    ( Int, Int )


type alias Board =
    List (List Sqr)


emptyBoard : Board
emptyBoard =
    let
        row =
            [ E, E, E, E, E, E, E ]
    in
    [ row, row, row, row, row, row ]



-- returns the opposite Sqr value


opsSqr : Sqr -> Sqr
opsSqr sqr =
    case sqr of
        X ->
            O

        _ ->
            X



-- helper function that sets a piece in a giver row


setRow : Int -> Sqr -> List Sqr -> List Sqr
setRow j sqr row =
    case row of
        [] ->
            []

        r :: rs ->
            if j == 0 then
                sqr :: rs

            else
                r :: setRow (j - 1) sqr rs



-- sets a piece in a given board


set : Pos -> Sqr -> Board -> Board
set pos sqr board =
    case ( pos, board ) of
        ( _, [] ) ->
            []

        ( ( i, j ), row :: rows ) ->
            if i == 0 then
                setRow j sqr row :: rows

            else
                row :: set ( i - 1, j ) sqr rows



-- helper function to get a piece at a given row


getRow : Int -> List Sqr -> Maybe Sqr
getRow j row =
    case row of
        [] ->
            Nothing

        r :: rs ->
            if j == 0 then
                Just r

            else
                getRow (j - 1) rs



-- helper function to get a piece in a given board


get : Pos -> Board -> Maybe Sqr
get pos board =
    case ( pos, board ) of
        ( _, [] ) ->
            Nothing

        ( ( i, j ), row :: rows ) ->
            if i == 0 then
                getRow j row

            else
                get ( i - 1, j ) rows
