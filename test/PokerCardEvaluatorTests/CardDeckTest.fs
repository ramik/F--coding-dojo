module CardDeckTest

open Xunit
open Cards

let fullSuiteOf = function | x -> [Ace(x);King(x);Queen(x);Jack(x);ValueCard(2, x); 
                                   ValueCard(3, x);ValueCard(4, x);ValueCard(5, x);ValueCard(6, x);
                                   ValueCard(7, x);ValueCard(8, x);ValueCard(9, x);ValueCard(10, x)]

[<Fact>]
let DeckContainsAllCards() = Assert.Equal (52, (Seq.length Deck))

[<Fact>]
let DeckContainsAllHearts() = Assert.Equal (fullSuiteOf Heart, Deck |> Seq.take 13 |> Seq.toList)
          
[<Fact>]
let DeckContainsAllDiamonds() = Assert.Equal (fullSuiteOf Diamond, Deck |> Seq.skip 13 |> Seq.take 13 |> Seq.toList)     

[<Fact>]
let DeckContainsAllClubs() = Assert.Equal (fullSuiteOf Club, Deck |> Seq.skip 26 |> Seq.take 13 |> Seq.toList)     

[<Fact>]
let DeckContainsAllSpapes() = Assert.Equal (fullSuiteOf Spade, Deck |> Seq.skip 39 |> Seq.take 13 |> Seq.toList)     