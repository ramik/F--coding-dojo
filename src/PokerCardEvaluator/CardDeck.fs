module Cards
open System

type Suit = Heart | Diamond | Club | Spade
            override this.ToString() = match this with | Heart -> "Heart" | Diamond -> "Diamond" 
                                                       | Club -> "Club" | Spade -> "Spade"
type Card =
    | Ace of Suit | King of Suit | Queen of Suit | Jack of Suit
    | ValueCard of int * Suit
    member this.FaceValue = 
        match this with | Ace(_) -> 14
                        | King(_) -> 13
                        | Queen(_) -> 12
                        | Jack(_) -> 11
                        | ValueCard(x, _) -> x
    member this.Suit = 
        match this with | Ace(x) -> x
                        | King(x) -> x
                        | Queen(x) -> x
                        | Jack(x) -> x
                        | ValueCard(_, x) -> x
    override this.ToString() = 
                        let Format a b = String.Format ("{0} of {1}", a, b)
                        match this with | ValueCard(a, b) -> Format a b
                                        | Ace(x) -> Format "Ace" x
                                        | King(x) -> Format "King" x
                                        | Queen(x) -> Format "Queen" x
                                        | Jack(x) -> Format "Jack" x

let GetFullCardsForSuit suit = seq { yield Ace(suit); yield King(suit); yield Queen(suit); yield Jack(suit)
                                     yield! [2..10] |> Seq.map (fun c -> ValueCard(c, suit)); } |> Seq.toList

let private GetDeck = List.map (fun c -> GetFullCardsForSuit c) >> List.concat

let ShuffleCards (random : Random) = List.map (fun c -> (random.Next(), c)) >>
                                     List.sortBy (fun (a, _) -> a) >>
                                     List.map (fun (_, b) -> b)

let GetShuffledDeck deck = ShuffleCards (new System.Random()) deck
let GetHand deck = deck |> GetShuffledDeck |> Seq.take 5

type Deck() = 
  let mutable deck = GetDeck [Heart;Diamond;Club;Spade] 
  member this.DrawHand = deck <- GetShuffledDeck deck
                         let hand = Seq.take 5 deck
                         deck <- Seq.skip 5 deck |> Seq.toList
                         Seq.toList hand
  member this.Deck = deck 