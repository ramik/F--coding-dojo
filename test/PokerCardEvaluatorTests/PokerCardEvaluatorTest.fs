module PokerCardEvaluatorTest

open Xunit
open Cards
open PokerCardEvaluator

[<Fact>]
let DeckGetsShuffledWithSeed1() = 
       let expected = [ValueCard(5, Club); ValueCard(8, Heart); ValueCard(9, Club); ValueCard(6, Diamond);ValueCard(6, Heart);King(Heart)]
       Assert.Equal (expected, ShuffleCards (new System.Random(1)) (new Deck()).Deck |> Seq.take 6 |> Seq.toList)

[<Fact>]
let DeckGetsShuffledWithSeed3() = 
       let expected = [Ace(Diamond); ValueCard(5, Club); ValueCard(10, Spade); ValueCard(8, Diamond); King(Diamond); Jack(Diamond)]
       Assert.Equal (expected, ShuffleCards (new System.Random(3)) (new Deck()).Deck |> Seq.take 6 |> Seq.toList)

[<Fact>]
let FindsPairFromHand() = 
   Assert.Equal (Pair({value = 14; shark = Some(11)}), EvaluateHand [Ace(Diamond); ValueCard(5, Club); Ace(Spade); ValueCard(8, Diamond); Jack(Diamond)])
   Assert.Equal (Pair({value = 11; shark = Some(14)}), EvaluateHand [Jack(Diamond); ValueCard(5, Club); Ace(Spade); ValueCard(8, Diamond); Jack(Club)])

[<Fact>]
let FindsThreeOfKindFromHand() = 
   Assert.Equal (ThreeOfKind({value = 14; shark = Some(11)}), EvaluateHand [Ace(Diamond); Ace(Club); Ace(Spade); ValueCard(8, Diamond); Jack(Diamond)])
   Assert.Equal (ThreeOfKind({value = 11; shark = Some(8)}), EvaluateHand [Jack(Diamond); ValueCard(5, Club); Jack(Spade); ValueCard(8, Diamond); Jack(Club)])
   Assert.Equal (ThreeOfKind({value = 3; shark = Some(13)}), EvaluateHand [ValueCard(10, Spade); ValueCard(3, Spade); ValueCard(3, Club); King(Club); ValueCard(3, Diamond)])

[<Fact>]
let FindsFourOfKindFromHand() = 
   Assert.Equal (FourOfKind({value = 14; shark = Some(8)}), EvaluateHand [Ace(Diamond); Ace(Club); Ace(Spade); ValueCard(8, Diamond); Ace(Heart)])
   Assert.Equal (FourOfKind({value = 11; shark = Some(5)}), EvaluateHand [Jack(Diamond); ValueCard(5, Club); Jack(Spade); Jack(Heart); Jack(Club)])

[<Fact>]
let FindsTwoPairsFromHand() = 
   Assert.Equal (TwoPairs(Pair({ value = 14; shark = None}), Pair({value=8; shark = None }), Some(13)),
                 EvaluateHand [Ace(Diamond); Ace(Club); ValueCard(8, Spade); ValueCard(8, Diamond); King(Heart)])
   Assert.Equal (TwoPairs(Pair({ value = 13; shark = None}), Pair({value=3; shark = None }), Some(8)),
                 EvaluateHand [ValueCard(3, Diamond); King(Club); ValueCard(3, Spade); ValueCard(8, Diamond); King(Heart)])

[<Fact>]
let FindsFullHousesFromHand() = 
   Assert.Equal (FullHouse(ThreeOfKind({ value = 14; shark = None}), Pair({value=8; shark = None })),
                 EvaluateHand [Ace(Diamond); Ace(Club); Ace(Spade); ValueCard(8, Diamond); ValueCard(8, Heart)])
   Assert.Equal (FullHouse(ThreeOfKind({ value = 13; shark = None}), Pair({value=3; shark = None })),
                 EvaluateHand [ValueCard(3, Diamond); King(Club); ValueCard(3, Spade); King(Diamond); King(Heart)])

[<Fact>]
let FindsFlushFromHand() = 
   Assert.Equal (Flush(Some(8)),
                 EvaluateHand [ValueCard(5, Diamond); ValueCard(4, Diamond); ValueCard(7, Diamond); ValueCard(2, Diamond); ValueCard(8, Diamond)])
   Assert.Equal (Flush(Some(12)),
                 EvaluateHand [ValueCard(9, Diamond); ValueCard(4, Diamond); Queen(Diamond); ValueCard(2, Diamond); ValueCard(8, Diamond)])


[<Fact>]
let FindStraightFromHand() = 
   Assert.Equal (Straight(Some(12)),
                 EvaluateHand [Jack(Heart); ValueCard(10, Diamond); Queen(Diamond); ValueCard(8, Spade); ValueCard(9, Diamond)])
   Assert.Equal (Straight(Some(5)),
                 EvaluateHand [ValueCard(5, Diamond); ValueCard(4, Diamond); Ace(Diamond); ValueCard(2, Diamond); ValueCard(3, Spade)])
 
[<Fact>]
let FindStraightFlushFromHand() = 
   Assert.Equal (StraightFlush(Some(12)),
                 EvaluateHand [Jack(Spade); ValueCard(10, Spade); Queen(Spade); ValueCard(8, Spade); ValueCard(9, Spade)])
   Assert.Equal (StraightFlush(Some(5)),
                 EvaluateHand [ValueCard(5, Diamond); ValueCard(4, Diamond); Ace(Diamond); ValueCard(2, Diamond); ValueCard(3, Diamond)])

[<Fact>]
let FindOnlySharkFromHand() = 
   Assert.Equal (OnlyShark(Some(12)),
                 EvaluateHand [Jack(Spade); ValueCard(3, Club); Queen(Spade); ValueCard(8, Spade); ValueCard(9, Spade)])
   Assert.Equal (OnlyShark(Some(8)),
                 EvaluateHand [ValueCard(5, Diamond); ValueCard(4, Club); ValueCard(6, Diamond); ValueCard(8, Diamond); ValueCard(3, Diamond)])