module Cards
open System

type Suit = Heart | Diamond | Club | Spade
            override this.ToString() = match this with | Heart -> "Heart" | Diamond -> "Diamond" 
                                                       | Club -> "Club" | Spade -> "Spade"
type Card =
    | Ace of Suit | King of Suit | Queen of Suit | Jack of Suit
    | ValueCard of int * Suit
    override this.ToString() = 
                        let Format a b = String.Format ("{0} of {1}", a, b)
                        match this with | ValueCard(a, b) -> Format a b
                                        | Ace(x) -> Format "Ace" x
                                        | King(x) -> Format "King" x
                                        | Queen(x) -> Format "Queen" x
                                        | Jack(x) -> Format "Jack" x

let GetFullCardsForSuit suit = seq { yield Ace(suit); yield King(suit); yield Queen(suit); yield Jack(suit)
                                     for value in [2..10] do yield ValueCard(value, suit); }

let private GetDeck = Seq.map (fun c -> GetFullCardsForSuit c) >> Seq.concat
let Deck = GetDeck [Heart;Diamond;Club;Spade]