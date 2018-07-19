import Test.Hspec
import Bowling

main  = hspec $ do
    describe "base values for scoring rolls" $ do
        it "scores 0 if the player misses" $ do
            scoreThrow Miss `shouldBe` 0

        it "scores the number of knocked pins" $ do
            scoreThrow (Regular 9) `shouldBe` 9  
            scoreThrow (Regular 5) `shouldBe` 5  
    
        it "scores 10 if the player does a spare" $ do
            scoreThrow Spare `shouldBe` 10

        it "scores 10 if the player does a strike" $  do
            scoreThrow Strike `shouldBe` 10

    describe "score games" $ do
        describe "with several frames" $ do
            it "scores a four rolls" $ do
                score (Game [Regular 1
                            ,Regular 4
                            ,Regular 2
                            ,Regular 3])
                 `shouldBe` (1 + 4 + 2 + 3)
                score (Game [Regular 1
                            ,Regular 1
                            ,Regular 1
                            ,Regular 1])
                 `shouldBe` (1 + 1 + 1 + 1)
        describe "with spares" $ do
            it "scores a Spare and two rolls" $ do
                score (Game [Regular 3
                            ,Spare
                            ,Regular 5
                            ,Regular 3])
                 `shouldBe` (10 + 5 + 5 + 3)
            it "scores a Spare and three rolls" $ do
                score (Game [Regular 1
                            ,Spare
                            ,Regular 2
                            ,Regular 3
                            ,Regular 8])
                 `shouldBe` (10 + 2 + 2 + 3 + 8)
            it "scores two Spares and 1 rolls" $ do
                score (Game [Regular 1
                            ,Spare
                            ,Regular 2
                            ,Spare 
                            ,Regular 8])
                 `shouldBe` (10 + 2 + 10 + 8 + 8)
        describe "with strikes" $ do
            it "scores a strike and two rolls" $ do
                score (Game [Strike
                            ,Regular 1
                            ,Regular 2])
                 `shouldBe` (10 + 1 + 1 + 2 + 2)
            it "scores a strike and three rolls" $ do
                score (Game [Strike
                            ,Regular 1
                            ,Regular 2
                            ,Regular 3])
                 `shouldBe` (10 + 1 + 1 + 2 + 2 + 3)
        describe "perfect" $ do
            it "should score 300" $ do
                score (Game $ replicate 12 Strike) `shouldBe` 300
        describe "almost perfect (so close)" $ do
            it "should score 300" $ do
                score (Game $ (replicate 10 Strike) ++ [Regular 3, Regular 5]) `shouldBe` 281
            
