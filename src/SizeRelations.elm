module SizeRelations exposing (..)


type SizeRelation
    = MainSpacing
    | Clock
    | MoonClock
    | MoonClockPadding
    | HandWidth
    | QuarterLineWidth
    | DateBlur
    | DateFont
    | DateCloudShift
    | SchedulePadding
    | ScheduleFontSize
    | ScheduleLineSpacing


size : Float -> SizeRelation -> Float
size baseSize sizeRelation =
    baseSize
        * (case sizeRelation of
            MainSpacing ->
                0.08

            Clock ->
                0.6

            MoonClock ->
                0.06

            MoonClockPadding ->
                0.05

            HandWidth ->
                0.033

            QuarterLineWidth ->
                0.01

            DateBlur ->
                0.02

            DateFont ->
                0.07

            DateCloudShift ->
                0.03

            SchedulePadding ->
                0.1

            ScheduleFontSize ->
                0.05

            ScheduleLineSpacing ->
                0.05
          )
