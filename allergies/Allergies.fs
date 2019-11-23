module Allergies

open System

type Allergen =
    | Eggs = 1
    | Peanuts = 2
    | Shellfish = 4
    | Strawberries = 8
    | Tomatoes = 16
    | Chocolate = 32
    | Pollen = 64
    | Cats = 128

let allergicTo (codedAllergies: int) (allergen: Allergen): bool = codedAllergies &&& int allergen <> 0

let list (codedAllergies: int): Allergen list =
    Enum.GetValues(typeof<Allergen>)
    |> Seq.cast<Allergen>
    |> Seq.filter (allergicTo codedAllergies)
    |> Seq.toList
