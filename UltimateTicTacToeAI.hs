-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\
module UltimateTicTacToeAI(State, author, nickname, initial, think) where

import UltimateTicTacToe (Move (..), Player (..))
-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

import Control.Monad (foldM, when)
import Data.Function (on)
import Data.List (sortBy)

{- The core logic while significantly enhancing code readability through improved variable naming and structural organization. The primary data types include GameState a for representing the game state, BigGame for the nested structure of the overall game, State to hold information about the AI's current state, and Game a for individual subgames. The refactoring introduces clear and descriptive names for functions, such as updateDirections, which appropriately handles the update of direction states based on player moves. Error handling functions like mapError and isRestricted effectively manage issues related to unavailable or restricted moves.

The central makeMove function orchestrates the AI's move, updating the game state and appropriately handling move restrictions. The updateGame function modifies the game state based on a player's move, ensuring consistency in the game's representation. The simulate function is a key component, simulating potential game outcomes using different decision-making algorithms (oAi and xAi) for the opponent and AI. Two distinct decision-making strategies, namely randomMove and minmaxMove, have been implemented, reflecting simpler and minimax-based approaches, respectively.
-}

-- Represents the state of a game, either ongoing, a win for a player, or a draw
data GameState a = Ongoing a | Win Player | Draw deriving (Show)

-- Represents a game within a game
type BigGame = Game (Game ())

-- Represents the state of the overall game
data State = State
  { gameState :: GameState BigGame,   -- State of the current game
    player :: Player                  -- Current player
  }
  deriving (Show)

-- | 'opposite' function returns the opposite player of the given player.
--
-- The function takes one argument:
--   1. 'player' - The player for whom the opposite player is being determined.
--
-- The function returns the opposite player: 'O' if the input is 'X' and 'X' if the input is 'O'.
--
-- Examples:
--   let oppositePlayer = opposite O
--   -- Returns 'X' as the opposite player of 'O'.
opposite :: Player -> Player
opposite O = X
opposite X = O

-- Represents a game with available moves, direction states, and an optional index restriction
data Game a = Game
  { available :: ![(Int, a)],          -- List of available moves with associated data
    dstates :: !DStates,               -- States of different directions in the game
    index_restriction :: !(Maybe Int)  -- optional restriction for index
  }
  deriving (Show)

-- Represents different directions in the game
data Direction = H1 | H2 | H3 | V1 | V2 | V3 | S1 | S2

-- Represents the state of a particular direction
data DState = Empty | One Player | Two Player | Both deriving (Show)

-- Represents the states of different directions in the game
data DStates = DStates
  { h1 :: DState,
    h2 :: DState,
    h3 :: DState,
    v1 :: DState,
    v2 :: DState,
    v3 :: DState,
    s1 :: DState,
    s2 :: DState
  }
  deriving (Show)

author :: String
author = "Shivam Srivastava" -- replace `undefined' with your first and last name

nickname :: String
nickname = "Player Mojo" -- replace `undefined' with a nickname for your AI

-- Represents the empty state for all directions
emptyDirectionStates =
  DStates
    { h1 = Empty,
      h2 = Empty,
      h3 = Empty,
      v1 = Empty,
      v2 = Empty,
      v3 = Empty,
      s1 = Empty,
      s2 = Empty
    }

-- | 'emptyGame' function creates an empty 'Game' with a specified initial subgame state.
--
-- The function takes one argument:
--   1. 'subgame' - The initial state of the subgames.
--
-- The function returns a newly created 'Game' with an initial list of available subgames,
-- empty direction states, and no index restriction.
--
-- Examples:
--   let emptyBigGame = emptyGame ()
--   -- Returns a new 'Game' with an initial state of empty subgames and no index restriction.
emptyGame :: a -> Game a
emptyGame subgame =
  Game
    { available = map (\i -> (i, subgame)) [1 .. 9],
      dstates = emptyDirectionStates,
      index_restriction = Nothing
    }

-- | 'updateDirections' function updates the 'DStates' based on a player's move in a 'BigGame'.
--
-- The function takes three arguments:
--   1. 'player' - The player making the move.
--   2. 'dss' - The current 'DStates'.
--   3. 'index' - The index representing the subgame where the move is made.
--
-- The function returns 'Just updatedDStates' if the move is valid, and 'Nothing' otherwise.
--
-- Examples:
--   let updatedDStates = updateDirections X someDStates 5
--   -- Returns 'Just updatedDStates' after updating the 'DStates' based on player X's move in subgame 5.
updateDirections :: Player -> DStates -> Int -> Maybe DStates
updateDirections player dss = foldM updateDirection dss . directions
  where
    updateDirection states d = go (get d states) >>= Just . set d states
    get H1 = h1
    get H2 = h2
    get H3 = h3
    get V1 = v1
    get V2 = v2
    get V3 = v3
    get S1 = s1
    get S2 = s2
    set H1 r v = r {h1 = v}
    set H2 r v = r {h2 = v}
    set H3 r v = r {h3 = v}
    set V1 r v = r {v1 = v}
    set V2 r v = r {v2 = v}
    set V3 r v = r {v3 = v}
    set S1 r v = r {s1 = v}
    set S2 r v = r {s2 = v}
    go Empty = Just (One player)
    go (One p) = if player == p then Just (Two p) else Just Both
    go (Two p) = if player == p then Nothing else Just (Two p)
    go Both = Just Both
    directions 1 = [H1, V1, S1]
    directions 2 = [H1, V2]
    directions 3 = [H1, V3, S2]
    directions 4 = [H2, V1]
    directions 5 = [H2, V2, S1, S2]
    directions 6 = [H2, V3]
    directions 7 = [H3, V1, S2]
    directions 8 = [H3, V2]
    directions 9 = [H3, V3, S1]

data Error = IndexUnavailable | IndexRestricted deriving (Show)

-- | 'mapError' function maps a specific error to a 'Maybe' value and converts it to an 'Either' monad.
--
-- The function takes two arguments:
--   1. 'error' - The error to map if the 'Maybe' value is 'Nothing'.
--   2. 'maybeValue' - The 'Maybe' value to convert to an 'Either' monad.
--
-- The function returns 'Left error' if the 'Maybe' value is 'Nothing', and 'Right x' otherwise,
-- where 'x' is the value inside the 'Just' constructor.
--
-- Examples:
--   let eitherValue = mapError IndexUnavailable (Just 42)
--   -- Returns 'Right 42' as the 'Maybe' value is 'Just 42'.
mapError :: Error -> Maybe a -> Either Error a
mapError error Nothing = Left error
mapError _ (Just x) = Right x

-- | 'isRestricted' function checks if a specified index is restricted in the given 'Game'.
--
-- The function takes two arguments:
--   1. 'index' - The index to check for restriction.
--   2. 'game' - The current state of the game, represented as 'Game a'.
--
-- The function returns 'True' if the specified index is restricted, and 'False' otherwise.
--
-- Examples:
--   let restricted = isRestricted 2 someGame
--   -- Returns 'True' if the index 2 is restricted in the specified 'Game'.
isRestricted :: Int -> Game a -> Bool
isRestricted index game = case index_restriction game of
  Just i -> index /= i
  _ -> False

-- | 'move' function represents a move in a generic game.
--
-- The function takes four arguments:
--   1. 'playOnSubgame' - A function that plays on a subgame and returns a new state and an integer.
--   2. 'index' - The index representing the subgame to make the move on.
--   3. 'player' - The player making the move.
--   4. 'game' - The current state of the game, represented as 'Game a'.
--
-- The function returns an 'Either' monad with an 'Error' indicating any issues during the move
-- or a tuple containing the updated game state ('GameState (Game a)') and an integer value.
--
-- Examples:
--   let playOnSubgame subgame = Right (Ongoing newSubgame, 42)
--   move playOnSubgame 1 X someGame
--   -- Returns 'Right (Ongoing updatedGame, 1)' representing the successful move and the updated game state.
move ::
  (a -> Either Error (GameState a, Int)) ->
  Int ->
  Player ->
  Game a ->
  Either Error (GameState (Game a), Int)
move playOnSubgame index player game = do
  when (isRestricted index game) (Left IndexRestricted)
  subgame <- mapError IndexUnavailable $ lookup index (available game)
  let without_index = filter (\(key, _) -> key /= index) (available game)
  (subgameState, subindex) <- playOnSubgame subgame
  let unavailable new_dstates =
        if null without_index
          then Draw
          else
            Ongoing $
              game
                { available = without_index,
                  index_restriction = restrict subindex without_index,
                  dstates = new_dstates
                }
  let gameState = case subgameState of
        Ongoing newSubgame ->
          Ongoing
            game
              { available = (index, newSubgame) : without_index,
                index_restriction = restrict subindex (available game)
              }
        Win player -> case updateDirections player (dstates game) index of
          Just newDstates -> unavailable newDstates
          _ -> Win player
        Draw -> unavailable (dstates game)
  Right (gameState, index)

-- | 'restrict' function restricts the available subgames based on a specified index.
--
-- The function takes two arguments:
--   1. 'index' - The index to restrict the available subgames.
--   2. 'available' - A list of tuples where each tuple contains an index and a corresponding subgame.
--
-- The function returns 'Just index' if the specified index is found in the list of available subgames;
-- otherwise, it returns 'Nothing'.
--
-- Examples:
--   let index = restrict 2 [(1, subgame1), (2, subgame2), (3, subgame3)]
--   -- Returns 'Just 2' as the specified index is found in the list of available subgames.
restrict :: Int -> [(Int, subgame)] -> Maybe Int
restrict index available = (\_ -> index) <$> lookup index available

-- | 'updateGame' function updates the game state based on a player's move in a 'BigGame'.
--
-- The function takes three arguments:
--   1. 'm' - The move made by the player, represented as a 'Move'.
--   2. 'player' - The player making the move.
--   3. 'gameState' - The current state of the game, represented as 'GameState BigGame'.
--
-- The function returns the updated game state after applying the player's move.
--
-- Examples:
--   let updatedGameState = updateGame (Move 1 2) X (Ongoing someBigGame)
--   -- Returns the updated game state after player X makes the move (1, 2) in the ongoing 'BigGame'.
updateGame :: Move -> Player -> GameState BigGame -> GameState BigGame
updateGame m player gameState = case gameState of
  Ongoing game -> case big_move m player game of
    Right (newGameState, _) -> newGameState
    Left x -> error (show x)
  _ -> gameState
  where
    small_move f player = move (\() -> Right (Win player, f)) f player
    big_move (Move b f) player = move (small_move f player) b player

-- | 'moves' function returns a list of all possible moves in a 'BigGame'.
--
-- The function takes one argument:
--   1. 'game' - The 'BigGame' for which the possible moves are being determined.
--
-- The function returns a list of 'Move' values representing all possible moves in the given 'BigGame'.
--
-- Examples:
--   let possibleMoves = moves someBigGame
--   -- Returns a list of all possible moves in the specified 'BigGame'.
moves :: BigGame -> [Move]
moves game =
  [Move b f | (b, subgame) <- available' game, (f, _) <- available subgame]
  where
    available' game = case index_restriction game of
      Just index -> case lookup index (available game) of
        Just subgame -> [(index, subgame)]
        _ -> []
      _ -> available game

-- | 'initial' function initializes a new 'State' for a given player.
--
-- The function takes one argument:
--   1. 'player' - The player for whom the initial state is being created.
--
-- The function returns a newly created 'State' with an initial game state of 'Ongoing initGame' for the specified player.
--
-- Examples:
--   let initialState = initial O
--   -- Returns a new 'State' with an initial game state of 'Ongoing initGame' for player O.
initial :: Player -> State
initial player = State {gameState = Ongoing initGame, player}

-- | 'initGame' function initializes a new 'BigGame'.
--
-- The function returns a newly created 'BigGame' with an initial state of an empty 'BigGame' within an empty 'BigGame'.
--
-- Examples:
--   let initialBigGame = initGame
--   -- Returns a new 'BigGame' with an initial state of an empty 'BigGame' within an empty 'BigGame'.
initGame :: BigGame
initGame = emptyGame (emptyGame ())

-- | 'think' helper function represents a decision-making process for a player in a game.
--
-- The function takes three arguments:
--   1. 'state' - The current state of the game encapsulated in a 'State' data structure.
--   2. 'maybeOpponentMove' - An optional 'Move' made by the opponent; 'Nothing' if it's the player's first move.
--   3. 'timeLeft' - The remaining time available for the player to make a decision.
--
-- The function returns a tuple containing the chosen 'Move' and the updated game 'State' after the player's move.
--
-- The decision-making process involves:
--   - Determining the current player ('p') based on the 'state'.
--   - Using the 'minmaxMove' function to decide the player's move with a depth of 4 in the decision tree.
--   - Updating the game state based on the opponent's move ('maybeOpponentMove').
--   - Returning the chosen move and the updated game state.
--
-- Examples:
--   let initialState = State { gameState = initialGameState, player = O }
--   let (chosenMove, newState) = think initialState Nothing 60.0
--   -- Represents the decision-making process for player O in the initial state with no opponent move and 60.0 seconds remaining.
--   -- Returns the chosen move and the updated game state after the player's move.
think :: State -> Maybe Move -> Double -> (Move, State)
think state maybeOpponentMove timeLeft =
  (currMove, state {gameState = afterPlayer})
  where
    p = player state
    decide = minmaxMove 4
    (afterOpponent, currMove) = case maybeOpponentMove of
      Just move -> case updateGame move (opposite p) (gameState state) of
        Ongoing game -> (Ongoing game, move)
          where
            (move, _) = decide game p timeLeft
      _ -> (gameState state, Move 5 5)
    afterPlayer = updateGame currMove p afterOpponent

type Decide score = BigGame -> Player -> Double -> (Move, score)

-- | 'simulate' function simulates a series of moves in a game based on the decisions made by two AI players.
--
-- The function takes five arguments:
--   1. 'o_ai' - The decision-making function for player O.
--   2. 'x_ai' - The decision-making function for player X.
--   3. 'initialState' - A tuple containing the initial list of moves and the initial game state ('GameState BigGame').
--   4. 'startingPlayer' - The player who starts the simulation.
--   5. 'simulations' - The number of simulations to perform.
--   6. 'clockTimeLeft' - The initial clock time available for each decision.
--
-- The function returns a tuple containing the list of moves and the final game state after the specified number of simulations.
--
-- The simulation alternates between the two AI players, making decisions based on their respective decision functions.
-- Each decision contributes to the list of moves, and the game state is updated accordingly.
--
-- Examples:
--   let initialState = ([], initialBigGameState)
--   let (finalMoves, finalState) = simulate o_ai x_ai initialState O 100 60.0
--   -- Simulates 100 moves with players O and X making decisions based on 'o_ai' and 'x_ai' functions,
--   -- starting with player O, and with an initial clock time of 60.0 seconds.
--   -- Returns the list of moves and the final game state after the simulations.
simulate ::
  Decide a ->
  Decide a ->
  ([(Move, a)], GameState BigGame) ->
  Player ->
  Int ->
  Double ->
  ([(Move, a)], GameState BigGame)
simulate o_ai x_ai = go
  where
    go (moves, Ongoing game) player n clockTimeLeft
      | n > 0 =
          go newState (opposite player) (n - 1) clockTimeLeft
      where
        newState = (moveAndScore : moves, newGameState)
        decide = if player == O then o_ai else x_ai
        moveAndScore = decide game player clockTimeLeft
        newGameState = updateGame (fst moveAndScore) player (Ongoing game)
    go (moves, gameState) _ _ _ = (reverse moves, gameState)

-- | 'arbitraryMove' function provides an arbitrary move selection based on the available valid moves and remaining time.
--
-- The function takes three arguments:
--   1. 'game' - The current state of the game.
--   2. 'player' - The player for whom the move is being determined.
--   3. 'timeLeft' - The remaining time available for the decision.
--
-- The function returns a tuple containing the selected move (of type 'Int') and a unit value '()' as the result of the decision.
-- The move is chosen randomly from the available valid moves based on a seed derived from the remaining time.
--
-- Examples:
--   let decideMove = arbitraryMove someGame currentPlayer remainingTime
--   decideMove someGame currentPlayer remainingTime
--      -- Returns a randomly selected move from the available valid moves along with '()' as the result of the decision.
arbitraryMove :: Decide ()
arbitraryMove game player timeLeft = (availableValidMoves !! index, ())
  where
    (_, seed) = properFraction timeLeft
    availableValidMoves = moves game
    index = floor $ seed * fromIntegral (length availableValidMoves)

-- | 'minmaxMove' function initializes the minimax algorithm with a specific depth for decision making.
--
-- The function takes an 'Int' parameter 'depth' and returns a 'Decide Int' function.
-- The returned function is configured for the minimax algorithm with the specified depth, starting as a maximizing move.
--
-- The returned function can be applied to a 'GameState', 'Player', and 'Int' (time remaining) to determine the best move.
--
-- Examples:
--   let decideMove = minmaxMove 3
--   decideMove someGame currentPlayer remainingTime
--      -- Returns the best move for the current player in the given game state, considering a depth of 3 and alpha-beta pruning.
minmaxMove :: Int -> Decide Int
minmaxMove depth = minmax (depth, True, minBound, maxBound)

-- | 'minmax' function performs the minimax algorithm to determine the best move in a game.
--
-- The function takes a tuple of parameters and returns a 'Decide Int' function.
-- The tuple consists of:
--   1. 'depth' - The maximum depth to explore in the game tree.
--   2. 'maxmizing' - A boolean indicating whether the current move is a maximizing move.
--   3. 'a' - The alpha value for alpha-beta pruning.
--   4. 'b' - The beta value for alpha-beta pruning.
--
-- The returned function 'Decide Int' takes the following arguments:
--   1. 'game' - The current state of the game.
--   2. 'player' - The player for whom the best move is being determined.
--   3. 'timeLeft' - The remaining time available for the decision.
--
-- The function returns an 'Int' representing the best move based on the minimax algorithm within the given constraints.
--
-- The minimax algorithm is enhanced with alpha-beta pruning to reduce the number of nodes evaluated in the search tree.
--
-- The 'evaluateState' function is used to evaluate the game state at leaf nodes of the search tree.
--
-- Examples:
--   let decideMove = minmax (3, True, minBound, maxBound) someGame currentPlayer remainingTime
--   decideMove someGame currentPlayer remainingTime
--      -- Returns the best move for the current player in the given game state, considering a depth of 3 and alpha-beta pruning.
minmax :: (Int, Bool, Int, Int) -> Decide Int
minmax (depth, maximizing, a, b) game player timeLeft = bestMove
  where
    cmp = if maximizing then flip compare else compare
    bestMove = case moves game of
      first : rest -> go a b (eval a b first) rest
      _ -> error "no valid moves in current game"
    go a b best (move : rest) = if prune then newBest else f newBest rest
      where
        prune = if maximizing then score > b else score < a
        f = if maximizing then go (max a score) b else go a (min b score)
        newBest = minBy (cmp `on` snd) (eval a b move) best
        score = snd newBest
    go _ _ best [] = best
    maxPlayer = if maximizing then player else opposite player
    eval a b move = case newGameState of
      Ongoing game ->
        if depth == 0
          then (move, score)
          else (move, s)
        where
          newMinMax = minmax (depth - 1, not maximizing, a, b)
          (_, s) = newMinMax game (opposite player) timeLeft
      _ -> (move, score)
      where
        score = evaluateState maxPlayer newGameState
        newGameState = updateGame move player (Ongoing game)

{-
\| 'minBy' function returns the minimum of two values based on a custom ordering function.

The function takes three arguments:
 1. 'f' - A custom comparison function that compares two values of type 'a' and returns an 'Ordering'.
 2. 'a' - The first value to be compared.
 3. 'b' - The second value to be compared.

The function returns the minimum value between 'a' and 'b' based on the ordering defined by the function 'f'.

The custom comparison function 'f' should return one of the following 'Ordering' values: 'LT' (less than), 'EQ' (equal), or 'GT' (greater than).

Examples:
 minBy compare 5 10      Returns 5, as 5 is less than 10 according to the default comparison function.
-}
minBy :: (a -> a -> Ordering) -> a -> a -> a
minBy f a b = case f a b of LT -> a; _ -> b

{-
 |'evaluateState' function evaluates the current state of a game in progress or completed.

 This function takes two arguments:
  1. 'player' - The player for whom the evaluation is being performed.
  2. 'gameState' - The current state of the game, which can be either 'Ongoing game', 'Win p', or 'Draw'.

 The function returns an 'Int' value representing the evaluation of the current game state for the specified player.

 Evaluation criteria:
  - If the game is ongoing, the evaluation is determined by the 'evaluate' function applied to the player and the game.
  - If the game is won by the specified player ('Win p'), the function returns 'maxBound'.
  - If the game is a draw ('Draw'), the function returns 0.

 Examples:
  evaluateState X (Ongoing game)  -- Returns the evaluation of the ongoing game for player X.
  evaluateState O (Win O)         -- Returns 'maxBound' as player O has won the game.
  evaluateState X Draw            -- Returns 0 as the game is a draw.
-}
evaluateState :: Player -> GameState BigGame -> Int
evaluateState player (Ongoing game) = evaluate player game
evaluateState player (Win p) = if p == player then maxBound else minBound
evaluateState _ Draw = 0

{-
 | 'evaluate' helper function calculates the evaluation score for a player in a given 'BigGame'.

The function takes two arguments:
  1. 'player' - The player for whom the evaluation score is being calculated.
  2. 'game' - The 'BigGame' for which the evaluation score is determined.

The function returns an 'Int' value representing the evaluation score for the specified player in the given game.

The evaluation score is computed based on the following criteria:
  - For each subgame in the 'BigGame', the function calculates a 'simpleEvaluation' score and accumulates them.
  - The 'simpleEvaluation' function assigns scores to different states within a subgame, and these scores are summed up.
  - The 'simpleEvaluation' score for a subgame is multiplied by 9 and added to the sum of 'subgameScores'.
  - The 'evaluateDstate' function assigns scores to different states within a subgame based on the player's moves.
  - 'evaluateDstate' returns 0 for an empty state, 1 for a state with the player's move, -1 for the opponent's move,
     and 2 for a state with two of the player's moves (indicating potential win).
  - The 'evaluateDstate' scores are summed up to get the 'simpleEvaluation' score for a subgame.
  - The overall evaluation score is the sum of 'subgameScores'.

Examples:
  evaluate X (initialBigGame 3)  -- Returns the evaluation score for player X in the initial 'BigGame'.
  evaluate O someBigGame         -- Returns the evaluation score for player O in the specified 'BigGame'.
-}
evaluate :: Player -> BigGame -> Int
evaluate player game = 9 * simpleEvaluation game + sum subgameScores
  where
    subgames = map snd (available game)
    subgameScores = map simpleEvaluation subgames
    simpleEvaluation game = sum dstateScores
      where
        dstateScores = map evaluateDstate [h1, h2, h3, v1, v2, v3, s1, s2]
        DStates {h1, h2, h3, v1, v2, v3, s1, s2} = dstates game
        evaluateDstate Empty = 0
        evaluateDstate (One p) = if p == player then 1 else -1
        evaluateDstate (Two p) = if p == player then 2 else -2
        evaluateDstate Both = 0

{-
  Haskell Program and its Function Specifications:
  ================================================

  The program and its function specifications above are inspired by common Haskell conventions and practices.

  References:
  1. Haskell Official Documentation (Haskell.org): https://www.haskell.org/documentation/
  2. Haskell Programming from First Principles: http://haskellbook.com/
  3. Real World Haskell: http://book.realworldhaskell.org/
  4. Learn You a Haskell for Great Good!: http://learnyouahaskell.com/
  5. Hoogle (Haskell API Search): https://hoogle.haskell.org/

  Note: The specifications aim to be informative and align with Haskell conventions but may not always reflect specific library or project documentation as per industry standards.
-}

