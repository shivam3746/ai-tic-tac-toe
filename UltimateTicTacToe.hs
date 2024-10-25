-- DO NOT MODIFY THIS FILE

module UltimateTicTacToe(Player(X,O), Move(Move)) where

{- The two players: X and O.
 -}
data Player = X | O
  deriving (Eq,Show)

{- Game moves.
   Move b f represents a move on board b, field f.
   INVARIANT: 1 ≤ b ≤ 9 and 1 ≤ f ≤ 9.
 -}
data Move = Move Int Int
  deriving (Eq,Show)
