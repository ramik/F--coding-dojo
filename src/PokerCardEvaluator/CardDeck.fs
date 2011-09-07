module PokerCardEvaluator

type Suit = Heart | Diamond | Club | Spade
type Card = | Ace of Suit | King of Suit | Queen of Suit | Jack of Suit
            | ValueCard of int * Suit

let GetFullCardsForSuit suit = seq { yield Ace(suit); yield King(suit); yield Queen(suit); yield Jack(suit)
                                     for value in [2..10] do yield ValueCard(value, suit); }

let GetDeck = Seq.map (fun c -> GetFullCardsForSuit c) >> Seq.concat
let Deck = GetDeck [Heart;Diamond;Club;Spade]