module SizeRelations exposing (..)


type SizeRelation
    = MainSpacing
    | Clock
    | MoonClock
    | MoonClockPadding
    | HandWidth
    | QuarterLineWidth
    | EventLineWidth
    | DateBlur
    | DateFont
    | DateCloudShift
    | SchedulePadding
    | ScheduleFontSize
    | ScheduleLineSpacing
    | EventInputPaddingY
    | ButtonPadding
    | InputFieldPaddingY
    | InputFieldUnderline
    | AddTimeListSpacing
    | AddTimeListFont
    | AddTimeListPadding
    | RoundedBorder


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

            EventLineWidth ->
                0.027

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

            EventInputPaddingY ->
                0.015

            ButtonPadding ->
                0.03

            InputFieldPaddingY ->
                0.005

            InputFieldUnderline ->
                0.01

            AddTimeListSpacing ->
                0.01

            AddTimeListFont ->
                0.045

            AddTimeListPadding ->
                0.015

            RoundedBorder ->
                0.03
          )
