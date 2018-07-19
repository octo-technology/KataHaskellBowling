module Bowling where

data Throw = Miss
           | Regular Int
           | Spare
           | Strike
    deriving (Eq)

data Game = Game [Throw]

scoreThrow :: Throw -> Int
scoreThrow Miss = 0
scoreThrow (Regular x) = x
scoreThrow Spare = 10
scoreThrow Strike = 10

bonus :: Int -> Int
bonus = id

score :: Game -> Int
score = score' 0 

score' :: Int -> Game -> Int
score' 10 _ = 0
score' _ (Game []) = 0
score' frame (Game (Strike
            :x
            :y
            :zs)) = strikeScore + score' (frame + 1) (Game (x:y:zs))
            where
            strikeScore = scoreThrow Strike + bonus (scoreThrow x) + bonus (scoreThrow y)
score' frame (Game (Regular _
            :Spare 
            :next
            :throws)) = (scoreThrow Spare) 
                      + bonus (scoreThrow next) 
                      + score' (frame + 1) (Game (next:throws))

score' frame (Game (Regular x
            :Regular y 
            :throws)) = (scoreThrow (Regular x)) + (scoreThrow (Regular y)) + score' (frame + 1) (Game throws)

score' frame (Game (throw:throws)) = scoreThrow throw + score' frame (Game (throws))
