module PokerCardEvaluatorTest

open Xunit
open Cards
open PokerCardEvaluator

[<Fact>]
let DeckGetsShuffledWithSeed1() = 
       let expected = [ValueCard(5, Club); ValueCard(8, Heart); ValueCard(9, Club); ValueCard(6, Diamond);ValueCard(6, Heart);King(Heart)]
       Assert.Equal (expected, ShuffleCards (new System.Random(1)) Deck |> Seq.take 6 |> Seq.toList)

[<Fact>]
let DeckGetsShuffledWithSeed3() = 
       let expected = [Ace(Diamond); ValueCard(5, Club); ValueCard(10, Spade); ValueCard(8, Diamond); King(Diamond); Jack(Diamond)]
       Assert.Equal (expected, ShuffleCards (new System.Random(3)) Deck |> Seq.take 6 |> Seq.toList)