module SizeRelations exposing (..)


type SizeRelation
    = MainSpacing
    | Clock
    | HandWidth
    | QuarterLineWidth
    | DateBlur
    | DateFont


size : Float -> SizeRelation -> Float
size baseSize sizeRelation =
    baseSize
        * (case sizeRelation of
            MainSpacing ->
                0.08

            Clock ->
                0.6

            HandWidth ->
                0.033

            QuarterLineWidth ->
                0.01

            DateBlur ->
                0.02

            DateFont ->
                0.07
          )
