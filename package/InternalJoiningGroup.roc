## GENERATED from Unicode 18.0.0. Run `python3 scripts/unicode_data.py generate`. ##
## layout: 2176 U8 page ids + 6 x 512 U8 values; logical payload 5248 bytes. ##

import InternalLooseAlias

InternalJoiningGroup :: [].{
    Value : [No_Joining_Group, African_Feh, African_Noon, African_Qaf, Ain, Alaph, Alef, Beh, Beth, Burushaski_Yeh_Barree, Crown_Ain, Crown_Beh, Crown_Feh, Crown_Hah, Crown_Heh, Crown_Kaf, Crown_Meem, Crown_Sad, Crown_Seen, Crown_Tah, Dal, Dalath_Rish, E, Farsi_Yeh, Fe, Feh, Final_Semkath, Gaf, Gamal, Hah, Hanifi_Rohingya_Kinna_Ya, Hanifi_Rohingya_Pa, He, Heh, Heh_Goal, Heth, Kaf, Kaph, Kashmiri_Yeh, Khaph, Knotted_Heh, Lam, Lamadh, Malayalam_Bha, Malayalam_Ja, Malayalam_Lla, Malayalam_Llla, Malayalam_Nga, Malayalam_Nna, Malayalam_Nnna, Malayalam_Nya, Malayalam_Ra, Malayalam_Ssa, Malayalam_Tta, Manichaean_Aleph, Manichaean_Ayin, Manichaean_Beth, Manichaean_Daleth, Manichaean_Dhamedh, Manichaean_Five, Manichaean_Gimel, Manichaean_Heth, Manichaean_Hundred, Manichaean_Kaph, Manichaean_Lamedh, Manichaean_Mem, Manichaean_Nun, Manichaean_One, Manichaean_Pe, Manichaean_Qoph, Manichaean_Resh, Manichaean_Sadhe, Manichaean_Samekh, Manichaean_Taw, Manichaean_Ten, Manichaean_Teth, Manichaean_Thamedh, Manichaean_Twenty, Manichaean_Waw, Manichaean_Yodh, Manichaean_Zayin, Meem, Mim, Noon, Nun, Nya, Pe, Qaf, Qaph, Reh, Reversed_Pe, Rohingya_Yeh, Sad, Sadhe, Seen, Semkath, Shin, Straight_Waw, Swash_Kaf, Syriac_Waw, Tah, Taw, Teh_Marbuta, Teh_Marbuta_Goal, Teth, Thin_Noon, Thin_Yeh, Vertical_Tail, Waw, Yeh, Yeh_Barree, Yeh_With_Tail, Yudh, Yudh_He, Zain, Zhain]
    PropertyName : { short : Str, long : Str }

    property_name : PropertyName
    property_name = { short: "jg", long: "Joining_Group" }

    lookup : U32 -> Value
    lookup = |scalar| from_u8(lookup_u8(scalar))

    lookup_u8 : U32 -> U8
    lookup_u8 = |scalar| {
        if scalar < 128 {
            ascii_value(scalar)
        } else if scalar > 0x10FFFF {
            0
        } else {
            page_id = page_index.get(scalar.shr_wrap(9).to_u64()) ?? 0
            offset = page_id.to_u64() * 512 + scalar.bitwise_and(511).to_u64()
            pages.get(offset) ?? 0
        }
    }

    short : Value -> Str
    short = |value| {
        match value {
            No_Joining_Group => "No_Joining_Group"
            African_Feh => "African_Feh"
            African_Noon => "African_Noon"
            African_Qaf => "African_Qaf"
            Ain => "Ain"
            Alaph => "Alaph"
            Alef => "Alef"
            Beh => "Beh"
            Beth => "Beth"
            Burushaski_Yeh_Barree => "Burushaski_Yeh_Barree"
            Crown_Ain => "Crown_Ain"
            Crown_Beh => "Crown_Beh"
            Crown_Feh => "Crown_Feh"
            Crown_Hah => "Crown_Hah"
            Crown_Heh => "Crown_Heh"
            Crown_Kaf => "Crown_Kaf"
            Crown_Meem => "Crown_Meem"
            Crown_Sad => "Crown_Sad"
            Crown_Seen => "Crown_Seen"
            Crown_Tah => "Crown_Tah"
            Dal => "Dal"
            Dalath_Rish => "Dalath_Rish"
            E => "E"
            Farsi_Yeh => "Farsi_Yeh"
            Fe => "Fe"
            Feh => "Feh"
            Final_Semkath => "Final_Semkath"
            Gaf => "Gaf"
            Gamal => "Gamal"
            Hah => "Hah"
            Hanifi_Rohingya_Kinna_Ya => "Hanifi_Rohingya_Kinna_Ya"
            Hanifi_Rohingya_Pa => "Hanifi_Rohingya_Pa"
            He => "He"
            Heh => "Heh"
            Heh_Goal => "Heh_Goal"
            Heth => "Heth"
            Kaf => "Kaf"
            Kaph => "Kaph"
            Kashmiri_Yeh => "Kashmiri_Yeh"
            Khaph => "Khaph"
            Knotted_Heh => "Knotted_Heh"
            Lam => "Lam"
            Lamadh => "Lamadh"
            Malayalam_Bha => "Malayalam_Bha"
            Malayalam_Ja => "Malayalam_Ja"
            Malayalam_Lla => "Malayalam_Lla"
            Malayalam_Llla => "Malayalam_Llla"
            Malayalam_Nga => "Malayalam_Nga"
            Malayalam_Nna => "Malayalam_Nna"
            Malayalam_Nnna => "Malayalam_Nnna"
            Malayalam_Nya => "Malayalam_Nya"
            Malayalam_Ra => "Malayalam_Ra"
            Malayalam_Ssa => "Malayalam_Ssa"
            Malayalam_Tta => "Malayalam_Tta"
            Manichaean_Aleph => "Manichaean_Aleph"
            Manichaean_Ayin => "Manichaean_Ayin"
            Manichaean_Beth => "Manichaean_Beth"
            Manichaean_Daleth => "Manichaean_Daleth"
            Manichaean_Dhamedh => "Manichaean_Dhamedh"
            Manichaean_Five => "Manichaean_Five"
            Manichaean_Gimel => "Manichaean_Gimel"
            Manichaean_Heth => "Manichaean_Heth"
            Manichaean_Hundred => "Manichaean_Hundred"
            Manichaean_Kaph => "Manichaean_Kaph"
            Manichaean_Lamedh => "Manichaean_Lamedh"
            Manichaean_Mem => "Manichaean_Mem"
            Manichaean_Nun => "Manichaean_Nun"
            Manichaean_One => "Manichaean_One"
            Manichaean_Pe => "Manichaean_Pe"
            Manichaean_Qoph => "Manichaean_Qoph"
            Manichaean_Resh => "Manichaean_Resh"
            Manichaean_Sadhe => "Manichaean_Sadhe"
            Manichaean_Samekh => "Manichaean_Samekh"
            Manichaean_Taw => "Manichaean_Taw"
            Manichaean_Ten => "Manichaean_Ten"
            Manichaean_Teth => "Manichaean_Teth"
            Manichaean_Thamedh => "Manichaean_Thamedh"
            Manichaean_Twenty => "Manichaean_Twenty"
            Manichaean_Waw => "Manichaean_Waw"
            Manichaean_Yodh => "Manichaean_Yodh"
            Manichaean_Zayin => "Manichaean_Zayin"
            Meem => "Meem"
            Mim => "Mim"
            Noon => "Noon"
            Nun => "Nun"
            Nya => "Nya"
            Pe => "Pe"
            Qaf => "Qaf"
            Qaph => "Qaph"
            Reh => "Reh"
            Reversed_Pe => "Reversed_Pe"
            Rohingya_Yeh => "Rohingya_Yeh"
            Sad => "Sad"
            Sadhe => "Sadhe"
            Seen => "Seen"
            Semkath => "Semkath"
            Shin => "Shin"
            Straight_Waw => "Straight_Waw"
            Swash_Kaf => "Swash_Kaf"
            Syriac_Waw => "Syriac_Waw"
            Tah => "Tah"
            Taw => "Taw"
            Teh_Marbuta => "Teh_Marbuta"
            Teh_Marbuta_Goal => "Teh_Marbuta_Goal"
            Teth => "Teth"
            Thin_Noon => "Thin_Noon"
            Thin_Yeh => "Thin_Yeh"
            Vertical_Tail => "Vertical_Tail"
            Waw => "Waw"
            Yeh => "Yeh"
            Yeh_Barree => "Yeh_Barree"
            Yeh_With_Tail => "Yeh_With_Tail"
            Yudh => "Yudh"
            Yudh_He => "Yudh_He"
            Zain => "Zain"
            Zhain => "Zhain"
        }
    }

    long : Value -> Str
    long = |value| {
        match value {
            No_Joining_Group => "No_Joining_Group"
            African_Feh => "African_Feh"
            African_Noon => "African_Noon"
            African_Qaf => "African_Qaf"
            Ain => "Ain"
            Alaph => "Alaph"
            Alef => "Alef"
            Beh => "Beh"
            Beth => "Beth"
            Burushaski_Yeh_Barree => "Burushaski_Yeh_Barree"
            Crown_Ain => "Crown_Ain"
            Crown_Beh => "Crown_Beh"
            Crown_Feh => "Crown_Feh"
            Crown_Hah => "Crown_Hah"
            Crown_Heh => "Crown_Heh"
            Crown_Kaf => "Crown_Kaf"
            Crown_Meem => "Crown_Meem"
            Crown_Sad => "Crown_Sad"
            Crown_Seen => "Crown_Seen"
            Crown_Tah => "Crown_Tah"
            Dal => "Dal"
            Dalath_Rish => "Dalath_Rish"
            E => "E"
            Farsi_Yeh => "Farsi_Yeh"
            Fe => "Fe"
            Feh => "Feh"
            Final_Semkath => "Final_Semkath"
            Gaf => "Gaf"
            Gamal => "Gamal"
            Hah => "Hah"
            Hanifi_Rohingya_Kinna_Ya => "Hanifi_Rohingya_Kinna_Ya"
            Hanifi_Rohingya_Pa => "Hanifi_Rohingya_Pa"
            He => "He"
            Heh => "Heh"
            Heh_Goal => "Heh_Goal"
            Heth => "Heth"
            Kaf => "Kaf"
            Kaph => "Kaph"
            Kashmiri_Yeh => "Kashmiri_Yeh"
            Khaph => "Khaph"
            Knotted_Heh => "Knotted_Heh"
            Lam => "Lam"
            Lamadh => "Lamadh"
            Malayalam_Bha => "Malayalam_Bha"
            Malayalam_Ja => "Malayalam_Ja"
            Malayalam_Lla => "Malayalam_Lla"
            Malayalam_Llla => "Malayalam_Llla"
            Malayalam_Nga => "Malayalam_Nga"
            Malayalam_Nna => "Malayalam_Nna"
            Malayalam_Nnna => "Malayalam_Nnna"
            Malayalam_Nya => "Malayalam_Nya"
            Malayalam_Ra => "Malayalam_Ra"
            Malayalam_Ssa => "Malayalam_Ssa"
            Malayalam_Tta => "Malayalam_Tta"
            Manichaean_Aleph => "Manichaean_Aleph"
            Manichaean_Ayin => "Manichaean_Ayin"
            Manichaean_Beth => "Manichaean_Beth"
            Manichaean_Daleth => "Manichaean_Daleth"
            Manichaean_Dhamedh => "Manichaean_Dhamedh"
            Manichaean_Five => "Manichaean_Five"
            Manichaean_Gimel => "Manichaean_Gimel"
            Manichaean_Heth => "Manichaean_Heth"
            Manichaean_Hundred => "Manichaean_Hundred"
            Manichaean_Kaph => "Manichaean_Kaph"
            Manichaean_Lamedh => "Manichaean_Lamedh"
            Manichaean_Mem => "Manichaean_Mem"
            Manichaean_Nun => "Manichaean_Nun"
            Manichaean_One => "Manichaean_One"
            Manichaean_Pe => "Manichaean_Pe"
            Manichaean_Qoph => "Manichaean_Qoph"
            Manichaean_Resh => "Manichaean_Resh"
            Manichaean_Sadhe => "Manichaean_Sadhe"
            Manichaean_Samekh => "Manichaean_Samekh"
            Manichaean_Taw => "Manichaean_Taw"
            Manichaean_Ten => "Manichaean_Ten"
            Manichaean_Teth => "Manichaean_Teth"
            Manichaean_Thamedh => "Manichaean_Thamedh"
            Manichaean_Twenty => "Manichaean_Twenty"
            Manichaean_Waw => "Manichaean_Waw"
            Manichaean_Yodh => "Manichaean_Yodh"
            Manichaean_Zayin => "Manichaean_Zayin"
            Meem => "Meem"
            Mim => "Mim"
            Noon => "Noon"
            Nun => "Nun"
            Nya => "Nya"
            Pe => "Pe"
            Qaf => "Qaf"
            Qaph => "Qaph"
            Reh => "Reh"
            Reversed_Pe => "Reversed_Pe"
            Rohingya_Yeh => "Rohingya_Yeh"
            Sad => "Sad"
            Sadhe => "Sadhe"
            Seen => "Seen"
            Semkath => "Semkath"
            Shin => "Shin"
            Straight_Waw => "Straight_Waw"
            Swash_Kaf => "Swash_Kaf"
            Syriac_Waw => "Syriac_Waw"
            Tah => "Tah"
            Taw => "Taw"
            Teh_Marbuta => "Teh_Marbuta"
            Teh_Marbuta_Goal => "Teh_Marbuta_Goal"
            Teth => "Teth"
            Thin_Noon => "Thin_Noon"
            Thin_Yeh => "Thin_Yeh"
            Vertical_Tail => "Vertical_Tail"
            Waw => "Waw"
            Yeh => "Yeh"
            Yeh_Barree => "Yeh_Barree"
            Yeh_With_Tail => "Yeh_With_Tail"
            Yudh => "Yudh"
            Yudh_He => "Yudh_He"
            Zain => "Zain"
            Zhain => "Zhain"
        }
    }

    alias_count : Value -> U8
    alias_count = |value| {
        match value {
            No_Joining_Group => 1
            African_Feh => 1
            African_Noon => 1
            African_Qaf => 1
            Ain => 1
            Alaph => 1
            Alef => 1
            Beh => 1
            Beth => 1
            Burushaski_Yeh_Barree => 1
            Crown_Ain => 1
            Crown_Beh => 1
            Crown_Feh => 1
            Crown_Hah => 1
            Crown_Heh => 1
            Crown_Kaf => 1
            Crown_Meem => 1
            Crown_Sad => 1
            Crown_Seen => 1
            Crown_Tah => 1
            Dal => 1
            Dalath_Rish => 1
            E => 1
            Farsi_Yeh => 1
            Fe => 1
            Feh => 1
            Final_Semkath => 1
            Gaf => 1
            Gamal => 1
            Hah => 1
            Hanifi_Rohingya_Kinna_Ya => 1
            Hanifi_Rohingya_Pa => 1
            He => 1
            Heh => 1
            Heh_Goal => 1
            Heth => 1
            Kaf => 1
            Kaph => 1
            Kashmiri_Yeh => 1
            Khaph => 1
            Knotted_Heh => 1
            Lam => 1
            Lamadh => 1
            Malayalam_Bha => 1
            Malayalam_Ja => 1
            Malayalam_Lla => 1
            Malayalam_Llla => 1
            Malayalam_Nga => 1
            Malayalam_Nna => 1
            Malayalam_Nnna => 1
            Malayalam_Nya => 1
            Malayalam_Ra => 1
            Malayalam_Ssa => 1
            Malayalam_Tta => 1
            Manichaean_Aleph => 1
            Manichaean_Ayin => 1
            Manichaean_Beth => 1
            Manichaean_Daleth => 1
            Manichaean_Dhamedh => 1
            Manichaean_Five => 1
            Manichaean_Gimel => 1
            Manichaean_Heth => 1
            Manichaean_Hundred => 1
            Manichaean_Kaph => 1
            Manichaean_Lamedh => 1
            Manichaean_Mem => 1
            Manichaean_Nun => 1
            Manichaean_One => 1
            Manichaean_Pe => 1
            Manichaean_Qoph => 1
            Manichaean_Resh => 1
            Manichaean_Sadhe => 1
            Manichaean_Samekh => 1
            Manichaean_Taw => 1
            Manichaean_Ten => 1
            Manichaean_Teth => 1
            Manichaean_Thamedh => 1
            Manichaean_Twenty => 1
            Manichaean_Waw => 1
            Manichaean_Yodh => 1
            Manichaean_Zayin => 1
            Meem => 1
            Mim => 1
            Noon => 1
            Nun => 1
            Nya => 1
            Pe => 1
            Qaf => 1
            Qaph => 1
            Reh => 1
            Reversed_Pe => 1
            Rohingya_Yeh => 1
            Sad => 1
            Sadhe => 1
            Seen => 1
            Semkath => 1
            Shin => 1
            Straight_Waw => 1
            Swash_Kaf => 1
            Syriac_Waw => 1
            Tah => 1
            Taw => 1
            Teh_Marbuta => 1
            Teh_Marbuta_Goal => 2
            Teth => 1
            Thin_Noon => 1
            Thin_Yeh => 1
            Vertical_Tail => 1
            Waw => 1
            Yeh => 1
            Yeh_Barree => 1
            Yeh_With_Tail => 1
            Yudh => 1
            Yudh_He => 1
            Zain => 1
            Zhain => 1
        }
    }

    alias_at : Value, U8 -> [Some(Str), None]
    alias_at = |value, index| {
        match (value, index) {
            (No_Joining_Group, 0) => Some("No_Joining_Group")
            (African_Feh, 0) => Some("African_Feh")
            (African_Noon, 0) => Some("African_Noon")
            (African_Qaf, 0) => Some("African_Qaf")
            (Ain, 0) => Some("Ain")
            (Alaph, 0) => Some("Alaph")
            (Alef, 0) => Some("Alef")
            (Beh, 0) => Some("Beh")
            (Beth, 0) => Some("Beth")
            (Burushaski_Yeh_Barree, 0) => Some("Burushaski_Yeh_Barree")
            (Crown_Ain, 0) => Some("Crown_Ain")
            (Crown_Beh, 0) => Some("Crown_Beh")
            (Crown_Feh, 0) => Some("Crown_Feh")
            (Crown_Hah, 0) => Some("Crown_Hah")
            (Crown_Heh, 0) => Some("Crown_Heh")
            (Crown_Kaf, 0) => Some("Crown_Kaf")
            (Crown_Meem, 0) => Some("Crown_Meem")
            (Crown_Sad, 0) => Some("Crown_Sad")
            (Crown_Seen, 0) => Some("Crown_Seen")
            (Crown_Tah, 0) => Some("Crown_Tah")
            (Dal, 0) => Some("Dal")
            (Dalath_Rish, 0) => Some("Dalath_Rish")
            (E, 0) => Some("E")
            (Farsi_Yeh, 0) => Some("Farsi_Yeh")
            (Fe, 0) => Some("Fe")
            (Feh, 0) => Some("Feh")
            (Final_Semkath, 0) => Some("Final_Semkath")
            (Gaf, 0) => Some("Gaf")
            (Gamal, 0) => Some("Gamal")
            (Hah, 0) => Some("Hah")
            (Hanifi_Rohingya_Kinna_Ya, 0) => Some("Hanifi_Rohingya_Kinna_Ya")
            (Hanifi_Rohingya_Pa, 0) => Some("Hanifi_Rohingya_Pa")
            (He, 0) => Some("He")
            (Heh, 0) => Some("Heh")
            (Heh_Goal, 0) => Some("Heh_Goal")
            (Heth, 0) => Some("Heth")
            (Kaf, 0) => Some("Kaf")
            (Kaph, 0) => Some("Kaph")
            (Kashmiri_Yeh, 0) => Some("Kashmiri_Yeh")
            (Khaph, 0) => Some("Khaph")
            (Knotted_Heh, 0) => Some("Knotted_Heh")
            (Lam, 0) => Some("Lam")
            (Lamadh, 0) => Some("Lamadh")
            (Malayalam_Bha, 0) => Some("Malayalam_Bha")
            (Malayalam_Ja, 0) => Some("Malayalam_Ja")
            (Malayalam_Lla, 0) => Some("Malayalam_Lla")
            (Malayalam_Llla, 0) => Some("Malayalam_Llla")
            (Malayalam_Nga, 0) => Some("Malayalam_Nga")
            (Malayalam_Nna, 0) => Some("Malayalam_Nna")
            (Malayalam_Nnna, 0) => Some("Malayalam_Nnna")
            (Malayalam_Nya, 0) => Some("Malayalam_Nya")
            (Malayalam_Ra, 0) => Some("Malayalam_Ra")
            (Malayalam_Ssa, 0) => Some("Malayalam_Ssa")
            (Malayalam_Tta, 0) => Some("Malayalam_Tta")
            (Manichaean_Aleph, 0) => Some("Manichaean_Aleph")
            (Manichaean_Ayin, 0) => Some("Manichaean_Ayin")
            (Manichaean_Beth, 0) => Some("Manichaean_Beth")
            (Manichaean_Daleth, 0) => Some("Manichaean_Daleth")
            (Manichaean_Dhamedh, 0) => Some("Manichaean_Dhamedh")
            (Manichaean_Five, 0) => Some("Manichaean_Five")
            (Manichaean_Gimel, 0) => Some("Manichaean_Gimel")
            (Manichaean_Heth, 0) => Some("Manichaean_Heth")
            (Manichaean_Hundred, 0) => Some("Manichaean_Hundred")
            (Manichaean_Kaph, 0) => Some("Manichaean_Kaph")
            (Manichaean_Lamedh, 0) => Some("Manichaean_Lamedh")
            (Manichaean_Mem, 0) => Some("Manichaean_Mem")
            (Manichaean_Nun, 0) => Some("Manichaean_Nun")
            (Manichaean_One, 0) => Some("Manichaean_One")
            (Manichaean_Pe, 0) => Some("Manichaean_Pe")
            (Manichaean_Qoph, 0) => Some("Manichaean_Qoph")
            (Manichaean_Resh, 0) => Some("Manichaean_Resh")
            (Manichaean_Sadhe, 0) => Some("Manichaean_Sadhe")
            (Manichaean_Samekh, 0) => Some("Manichaean_Samekh")
            (Manichaean_Taw, 0) => Some("Manichaean_Taw")
            (Manichaean_Ten, 0) => Some("Manichaean_Ten")
            (Manichaean_Teth, 0) => Some("Manichaean_Teth")
            (Manichaean_Thamedh, 0) => Some("Manichaean_Thamedh")
            (Manichaean_Twenty, 0) => Some("Manichaean_Twenty")
            (Manichaean_Waw, 0) => Some("Manichaean_Waw")
            (Manichaean_Yodh, 0) => Some("Manichaean_Yodh")
            (Manichaean_Zayin, 0) => Some("Manichaean_Zayin")
            (Meem, 0) => Some("Meem")
            (Mim, 0) => Some("Mim")
            (Noon, 0) => Some("Noon")
            (Nun, 0) => Some("Nun")
            (Nya, 0) => Some("Nya")
            (Pe, 0) => Some("Pe")
            (Qaf, 0) => Some("Qaf")
            (Qaph, 0) => Some("Qaph")
            (Reh, 0) => Some("Reh")
            (Reversed_Pe, 0) => Some("Reversed_Pe")
            (Rohingya_Yeh, 0) => Some("Rohingya_Yeh")
            (Sad, 0) => Some("Sad")
            (Sadhe, 0) => Some("Sadhe")
            (Seen, 0) => Some("Seen")
            (Semkath, 0) => Some("Semkath")
            (Shin, 0) => Some("Shin")
            (Straight_Waw, 0) => Some("Straight_Waw")
            (Swash_Kaf, 0) => Some("Swash_Kaf")
            (Syriac_Waw, 0) => Some("Syriac_Waw")
            (Tah, 0) => Some("Tah")
            (Taw, 0) => Some("Taw")
            (Teh_Marbuta, 0) => Some("Teh_Marbuta")
            (Teh_Marbuta_Goal, 0) => Some("Teh_Marbuta_Goal")
            (Teh_Marbuta_Goal, 1) => Some("Hamza_On_Heh_Goal")
            (Teth, 0) => Some("Teth")
            (Thin_Noon, 0) => Some("Thin_Noon")
            (Thin_Yeh, 0) => Some("Thin_Yeh")
            (Vertical_Tail, 0) => Some("Vertical_Tail")
            (Waw, 0) => Some("Waw")
            (Yeh, 0) => Some("Yeh")
            (Yeh_Barree, 0) => Some("Yeh_Barree")
            (Yeh_With_Tail, 0) => Some("Yeh_With_Tail")
            (Yudh, 0) => Some("Yudh")
            (Yudh_He, 0) => Some("Yudh_He")
            (Zain, 0) => Some("Zain")
            (Zhain, 0) => Some("Zhain")
            _ => None
        }
    }

    parse : Str -> [Some(Value), None]
    parse = |name|         if InternalLooseAlias.matches(name, "No_Joining_Group") Some(No_Joining_Group) else         if InternalLooseAlias.matches(name, "African_Feh") Some(African_Feh) else         if InternalLooseAlias.matches(name, "African_Noon") Some(African_Noon) else         if InternalLooseAlias.matches(name, "African_Qaf") Some(African_Qaf) else         if InternalLooseAlias.matches(name, "Ain") Some(Ain) else         if InternalLooseAlias.matches(name, "Alaph") Some(Alaph) else         if InternalLooseAlias.matches(name, "Alef") Some(Alef) else         if InternalLooseAlias.matches(name, "Beh") Some(Beh) else         if InternalLooseAlias.matches(name, "Beth") Some(Beth) else         if InternalLooseAlias.matches(name, "Burushaski_Yeh_Barree") Some(Burushaski_Yeh_Barree) else         if InternalLooseAlias.matches(name, "Crown_Ain") Some(Crown_Ain) else         if InternalLooseAlias.matches(name, "Crown_Beh") Some(Crown_Beh) else         if InternalLooseAlias.matches(name, "Crown_Feh") Some(Crown_Feh) else         if InternalLooseAlias.matches(name, "Crown_Hah") Some(Crown_Hah) else         if InternalLooseAlias.matches(name, "Crown_Heh") Some(Crown_Heh) else         if InternalLooseAlias.matches(name, "Crown_Kaf") Some(Crown_Kaf) else         if InternalLooseAlias.matches(name, "Crown_Meem") Some(Crown_Meem) else         if InternalLooseAlias.matches(name, "Crown_Sad") Some(Crown_Sad) else         if InternalLooseAlias.matches(name, "Crown_Seen") Some(Crown_Seen) else         if InternalLooseAlias.matches(name, "Crown_Tah") Some(Crown_Tah) else         if InternalLooseAlias.matches(name, "Dal") Some(Dal) else         if InternalLooseAlias.matches(name, "Dalath_Rish") Some(Dalath_Rish) else         if InternalLooseAlias.matches(name, "E") Some(E) else         if InternalLooseAlias.matches(name, "Farsi_Yeh") Some(Farsi_Yeh) else         if InternalLooseAlias.matches(name, "Fe") Some(Fe) else         if InternalLooseAlias.matches(name, "Feh") Some(Feh) else         if InternalLooseAlias.matches(name, "Final_Semkath") Some(Final_Semkath) else         if InternalLooseAlias.matches(name, "Gaf") Some(Gaf) else         if InternalLooseAlias.matches(name, "Gamal") Some(Gamal) else         if InternalLooseAlias.matches(name, "Hah") Some(Hah) else         if InternalLooseAlias.matches(name, "Hanifi_Rohingya_Kinna_Ya") Some(Hanifi_Rohingya_Kinna_Ya) else         if InternalLooseAlias.matches(name, "Hanifi_Rohingya_Pa") Some(Hanifi_Rohingya_Pa) else         if InternalLooseAlias.matches(name, "He") Some(He) else         if InternalLooseAlias.matches(name, "Heh") Some(Heh) else         if InternalLooseAlias.matches(name, "Heh_Goal") Some(Heh_Goal) else         if InternalLooseAlias.matches(name, "Heth") Some(Heth) else         if InternalLooseAlias.matches(name, "Kaf") Some(Kaf) else         if InternalLooseAlias.matches(name, "Kaph") Some(Kaph) else         if InternalLooseAlias.matches(name, "Kashmiri_Yeh") Some(Kashmiri_Yeh) else         if InternalLooseAlias.matches(name, "Khaph") Some(Khaph) else         if InternalLooseAlias.matches(name, "Knotted_Heh") Some(Knotted_Heh) else         if InternalLooseAlias.matches(name, "Lam") Some(Lam) else         if InternalLooseAlias.matches(name, "Lamadh") Some(Lamadh) else         if InternalLooseAlias.matches(name, "Malayalam_Bha") Some(Malayalam_Bha) else         if InternalLooseAlias.matches(name, "Malayalam_Ja") Some(Malayalam_Ja) else         if InternalLooseAlias.matches(name, "Malayalam_Lla") Some(Malayalam_Lla) else         if InternalLooseAlias.matches(name, "Malayalam_Llla") Some(Malayalam_Llla) else         if InternalLooseAlias.matches(name, "Malayalam_Nga") Some(Malayalam_Nga) else         if InternalLooseAlias.matches(name, "Malayalam_Nna") Some(Malayalam_Nna) else         if InternalLooseAlias.matches(name, "Malayalam_Nnna") Some(Malayalam_Nnna) else         if InternalLooseAlias.matches(name, "Malayalam_Nya") Some(Malayalam_Nya) else         if InternalLooseAlias.matches(name, "Malayalam_Ra") Some(Malayalam_Ra) else         if InternalLooseAlias.matches(name, "Malayalam_Ssa") Some(Malayalam_Ssa) else         if InternalLooseAlias.matches(name, "Malayalam_Tta") Some(Malayalam_Tta) else         if InternalLooseAlias.matches(name, "Manichaean_Aleph") Some(Manichaean_Aleph) else         if InternalLooseAlias.matches(name, "Manichaean_Ayin") Some(Manichaean_Ayin) else         if InternalLooseAlias.matches(name, "Manichaean_Beth") Some(Manichaean_Beth) else         if InternalLooseAlias.matches(name, "Manichaean_Daleth") Some(Manichaean_Daleth) else         if InternalLooseAlias.matches(name, "Manichaean_Dhamedh") Some(Manichaean_Dhamedh) else         if InternalLooseAlias.matches(name, "Manichaean_Five") Some(Manichaean_Five) else         if InternalLooseAlias.matches(name, "Manichaean_Gimel") Some(Manichaean_Gimel) else         if InternalLooseAlias.matches(name, "Manichaean_Heth") Some(Manichaean_Heth) else         if InternalLooseAlias.matches(name, "Manichaean_Hundred") Some(Manichaean_Hundred) else         if InternalLooseAlias.matches(name, "Manichaean_Kaph") Some(Manichaean_Kaph) else         if InternalLooseAlias.matches(name, "Manichaean_Lamedh") Some(Manichaean_Lamedh) else         if InternalLooseAlias.matches(name, "Manichaean_Mem") Some(Manichaean_Mem) else         if InternalLooseAlias.matches(name, "Manichaean_Nun") Some(Manichaean_Nun) else         if InternalLooseAlias.matches(name, "Manichaean_One") Some(Manichaean_One) else         if InternalLooseAlias.matches(name, "Manichaean_Pe") Some(Manichaean_Pe) else         if InternalLooseAlias.matches(name, "Manichaean_Qoph") Some(Manichaean_Qoph) else         if InternalLooseAlias.matches(name, "Manichaean_Resh") Some(Manichaean_Resh) else         if InternalLooseAlias.matches(name, "Manichaean_Sadhe") Some(Manichaean_Sadhe) else         if InternalLooseAlias.matches(name, "Manichaean_Samekh") Some(Manichaean_Samekh) else         if InternalLooseAlias.matches(name, "Manichaean_Taw") Some(Manichaean_Taw) else         if InternalLooseAlias.matches(name, "Manichaean_Ten") Some(Manichaean_Ten) else         if InternalLooseAlias.matches(name, "Manichaean_Teth") Some(Manichaean_Teth) else         if InternalLooseAlias.matches(name, "Manichaean_Thamedh") Some(Manichaean_Thamedh) else         if InternalLooseAlias.matches(name, "Manichaean_Twenty") Some(Manichaean_Twenty) else         if InternalLooseAlias.matches(name, "Manichaean_Waw") Some(Manichaean_Waw) else         if InternalLooseAlias.matches(name, "Manichaean_Yodh") Some(Manichaean_Yodh) else         if InternalLooseAlias.matches(name, "Manichaean_Zayin") Some(Manichaean_Zayin) else         if InternalLooseAlias.matches(name, "Meem") Some(Meem) else         if InternalLooseAlias.matches(name, "Mim") Some(Mim) else         if InternalLooseAlias.matches(name, "Noon") Some(Noon) else         if InternalLooseAlias.matches(name, "Nun") Some(Nun) else         if InternalLooseAlias.matches(name, "Nya") Some(Nya) else         if InternalLooseAlias.matches(name, "Pe") Some(Pe) else         if InternalLooseAlias.matches(name, "Qaf") Some(Qaf) else         if InternalLooseAlias.matches(name, "Qaph") Some(Qaph) else         if InternalLooseAlias.matches(name, "Reh") Some(Reh) else         if InternalLooseAlias.matches(name, "Reversed_Pe") Some(Reversed_Pe) else         if InternalLooseAlias.matches(name, "Rohingya_Yeh") Some(Rohingya_Yeh) else         if InternalLooseAlias.matches(name, "Sad") Some(Sad) else         if InternalLooseAlias.matches(name, "Sadhe") Some(Sadhe) else         if InternalLooseAlias.matches(name, "Seen") Some(Seen) else         if InternalLooseAlias.matches(name, "Semkath") Some(Semkath) else         if InternalLooseAlias.matches(name, "Shin") Some(Shin) else         if InternalLooseAlias.matches(name, "Straight_Waw") Some(Straight_Waw) else         if InternalLooseAlias.matches(name, "Swash_Kaf") Some(Swash_Kaf) else         if InternalLooseAlias.matches(name, "Syriac_Waw") Some(Syriac_Waw) else         if InternalLooseAlias.matches(name, "Tah") Some(Tah) else         if InternalLooseAlias.matches(name, "Taw") Some(Taw) else         if InternalLooseAlias.matches(name, "Teh_Marbuta") Some(Teh_Marbuta) else         if InternalLooseAlias.matches(name, "Teh_Marbuta_Goal") or InternalLooseAlias.matches(name, "Hamza_On_Heh_Goal") Some(Teh_Marbuta_Goal) else         if InternalLooseAlias.matches(name, "Teth") Some(Teth) else         if InternalLooseAlias.matches(name, "Thin_Noon") Some(Thin_Noon) else         if InternalLooseAlias.matches(name, "Thin_Yeh") Some(Thin_Yeh) else         if InternalLooseAlias.matches(name, "Vertical_Tail") Some(Vertical_Tail) else         if InternalLooseAlias.matches(name, "Waw") Some(Waw) else         if InternalLooseAlias.matches(name, "Yeh") Some(Yeh) else         if InternalLooseAlias.matches(name, "Yeh_Barree") Some(Yeh_Barree) else         if InternalLooseAlias.matches(name, "Yeh_With_Tail") Some(Yeh_With_Tail) else         if InternalLooseAlias.matches(name, "Yudh") Some(Yudh) else         if InternalLooseAlias.matches(name, "Yudh_He") Some(Yudh_He) else         if InternalLooseAlias.matches(name, "Zain") Some(Zain) else         if InternalLooseAlias.matches(name, "Zhain") Some(Zhain) else None
}

from_u8 : U8 -> InternalJoiningGroup.Value
from_u8 = |value| {
    match value {
        0 => No_Joining_Group
        1 => African_Feh
        2 => African_Noon
        3 => African_Qaf
        4 => Ain
        5 => Alaph
        6 => Alef
        7 => Beh
        8 => Beth
        9 => Burushaski_Yeh_Barree
        10 => Crown_Ain
        11 => Crown_Beh
        12 => Crown_Feh
        13 => Crown_Hah
        14 => Crown_Heh
        15 => Crown_Kaf
        16 => Crown_Meem
        17 => Crown_Sad
        18 => Crown_Seen
        19 => Crown_Tah
        20 => Dal
        21 => Dalath_Rish
        22 => E
        23 => Farsi_Yeh
        24 => Fe
        25 => Feh
        26 => Final_Semkath
        27 => Gaf
        28 => Gamal
        29 => Hah
        30 => Hanifi_Rohingya_Kinna_Ya
        31 => Hanifi_Rohingya_Pa
        32 => He
        33 => Heh
        34 => Heh_Goal
        35 => Heth
        36 => Kaf
        37 => Kaph
        38 => Kashmiri_Yeh
        39 => Khaph
        40 => Knotted_Heh
        41 => Lam
        42 => Lamadh
        43 => Malayalam_Bha
        44 => Malayalam_Ja
        45 => Malayalam_Lla
        46 => Malayalam_Llla
        47 => Malayalam_Nga
        48 => Malayalam_Nna
        49 => Malayalam_Nnna
        50 => Malayalam_Nya
        51 => Malayalam_Ra
        52 => Malayalam_Ssa
        53 => Malayalam_Tta
        54 => Manichaean_Aleph
        55 => Manichaean_Ayin
        56 => Manichaean_Beth
        57 => Manichaean_Daleth
        58 => Manichaean_Dhamedh
        59 => Manichaean_Five
        60 => Manichaean_Gimel
        61 => Manichaean_Heth
        62 => Manichaean_Hundred
        63 => Manichaean_Kaph
        64 => Manichaean_Lamedh
        65 => Manichaean_Mem
        66 => Manichaean_Nun
        67 => Manichaean_One
        68 => Manichaean_Pe
        69 => Manichaean_Qoph
        70 => Manichaean_Resh
        71 => Manichaean_Sadhe
        72 => Manichaean_Samekh
        73 => Manichaean_Taw
        74 => Manichaean_Ten
        75 => Manichaean_Teth
        76 => Manichaean_Thamedh
        77 => Manichaean_Twenty
        78 => Manichaean_Waw
        79 => Manichaean_Yodh
        80 => Manichaean_Zayin
        81 => Meem
        82 => Mim
        83 => Noon
        84 => Nun
        85 => Nya
        86 => Pe
        87 => Qaf
        88 => Qaph
        89 => Reh
        90 => Reversed_Pe
        91 => Rohingya_Yeh
        92 => Sad
        93 => Sadhe
        94 => Seen
        95 => Semkath
        96 => Shin
        97 => Straight_Waw
        98 => Swash_Kaf
        99 => Syriac_Waw
        100 => Tah
        101 => Taw
        102 => Teh_Marbuta
        103 => Teh_Marbuta_Goal
        104 => Teth
        105 => Thin_Noon
        106 => Thin_Yeh
        107 => Vertical_Tail
        108 => Waw
        109 => Yeh
        110 => Yeh_Barree
        111 => Yeh_With_Tail
        112 => Yudh
        113 => Yudh_He
        114 => Zain
        115 => Zhain
        _ => No_Joining_Group
    }
}

to_u8 : InternalJoiningGroup.Value -> U8
to_u8 = |value| {
    match value {
            No_Joining_Group => 0
            African_Feh => 1
            African_Noon => 2
            African_Qaf => 3
            Ain => 4
            Alaph => 5
            Alef => 6
            Beh => 7
            Beth => 8
            Burushaski_Yeh_Barree => 9
            Crown_Ain => 10
            Crown_Beh => 11
            Crown_Feh => 12
            Crown_Hah => 13
            Crown_Heh => 14
            Crown_Kaf => 15
            Crown_Meem => 16
            Crown_Sad => 17
            Crown_Seen => 18
            Crown_Tah => 19
            Dal => 20
            Dalath_Rish => 21
            E => 22
            Farsi_Yeh => 23
            Fe => 24
            Feh => 25
            Final_Semkath => 26
            Gaf => 27
            Gamal => 28
            Hah => 29
            Hanifi_Rohingya_Kinna_Ya => 30
            Hanifi_Rohingya_Pa => 31
            He => 32
            Heh => 33
            Heh_Goal => 34
            Heth => 35
            Kaf => 36
            Kaph => 37
            Kashmiri_Yeh => 38
            Khaph => 39
            Knotted_Heh => 40
            Lam => 41
            Lamadh => 42
            Malayalam_Bha => 43
            Malayalam_Ja => 44
            Malayalam_Lla => 45
            Malayalam_Llla => 46
            Malayalam_Nga => 47
            Malayalam_Nna => 48
            Malayalam_Nnna => 49
            Malayalam_Nya => 50
            Malayalam_Ra => 51
            Malayalam_Ssa => 52
            Malayalam_Tta => 53
            Manichaean_Aleph => 54
            Manichaean_Ayin => 55
            Manichaean_Beth => 56
            Manichaean_Daleth => 57
            Manichaean_Dhamedh => 58
            Manichaean_Five => 59
            Manichaean_Gimel => 60
            Manichaean_Heth => 61
            Manichaean_Hundred => 62
            Manichaean_Kaph => 63
            Manichaean_Lamedh => 64
            Manichaean_Mem => 65
            Manichaean_Nun => 66
            Manichaean_One => 67
            Manichaean_Pe => 68
            Manichaean_Qoph => 69
            Manichaean_Resh => 70
            Manichaean_Sadhe => 71
            Manichaean_Samekh => 72
            Manichaean_Taw => 73
            Manichaean_Ten => 74
            Manichaean_Teth => 75
            Manichaean_Thamedh => 76
            Manichaean_Twenty => 77
            Manichaean_Waw => 78
            Manichaean_Yodh => 79
            Manichaean_Zayin => 80
            Meem => 81
            Mim => 82
            Noon => 83
            Nun => 84
            Nya => 85
            Pe => 86
            Qaf => 87
            Qaph => 88
            Reh => 89
            Reversed_Pe => 90
            Rohingya_Yeh => 91
            Sad => 92
            Sadhe => 93
            Seen => 94
            Semkath => 95
            Shin => 96
            Straight_Waw => 97
            Swash_Kaf => 98
            Syriac_Waw => 99
            Tah => 100
            Taw => 101
            Teh_Marbuta => 102
            Teh_Marbuta_Goal => 103
            Teth => 104
            Thin_Noon => 105
            Thin_Yeh => 106
            Vertical_Tail => 107
            Waw => 108
            Yeh => 109
            Yeh_Barree => 110
            Yeh_With_Tail => 111
            Yudh => 112
            Yudh_He => 113
            Zain => 114
            Zhain => 115
    }
}

ascii_value : U32 -> U8
ascii_value = |_u32| 0

page_index : List(U8)
page_index = [
    0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 3, 4, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
]

pages : List(U8)
pages = [
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    38, 0, 6, 6, 108, 6, 109, 6, 7, 102, 7, 7, 29, 29, 29, 20, 20, 89, 89, 94, 94, 92, 92, 100, 100, 4, 4, 27, 27, 23, 23, 23,
    0, 25, 87, 36, 41, 81, 83, 33, 108, 109, 109, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 87, 0, 6, 6, 6, 0, 6, 108, 108, 109, 7, 7, 7, 7, 7, 7, 7,
    7, 29, 29, 29, 29, 29, 29, 29, 20, 20, 20, 20, 20, 20, 20, 20, 20, 89, 89, 89, 89, 89, 89, 89, 89, 89, 94, 94, 94, 92, 92, 100,
    4, 25, 25, 25, 25, 25, 25, 87, 87, 27, 98, 27, 36, 36, 36, 27, 27, 27, 27, 27, 27, 41, 41, 41, 41, 83, 83, 83, 83, 85, 40, 29,
    102, 34, 34, 103, 108, 108, 108, 108, 108, 108, 108, 108, 23, 111, 23, 108, 109, 109, 110, 110, 0, 102, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 20, 89, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 94, 92, 4, 0, 0, 40,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 8, 28, 28, 21, 21, 32, 99, 114, 35, 104, 104, 112, 113, 37,
    42, 82, 84, 95, 26, 22, 86, 90, 93, 88, 21, 96, 101, 8, 28, 21, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 115, 39, 24, 7, 7, 7, 7, 7, 7, 7, 29, 29, 20, 20, 89, 94, 4, 4, 4,
    25, 25, 27, 27, 27, 81, 81, 83, 83, 83, 41, 89, 89, 94, 29, 29, 94, 89, 29, 6, 6, 23, 23, 109, 108, 108, 9, 9, 29, 94, 94, 36,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    47, 44, 50, 53, 48, 49, 43, 51, 45, 46, 52, 0, 0, 0, 0, 0, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 0, 0, 0, 106, 0, 0, 83, 29, 100, 100, 27, 107, 83, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    7, 7, 29, 100, 25, 87, 41, 81, 109, 109, 89, 108, 91, 0, 20, 92, 27, 97, 89, 4, 36, 87, 7, 7, 7, 89, 109, 1, 3, 2, 7, 7,
    7, 29, 27, 4, 3, 29, 29, 41, 27, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    54, 56, 56, 60, 60, 57, 0, 78, 0, 80, 80, 0, 0, 61, 75, 79, 63, 63, 63, 64, 58, 76, 65, 66, 72, 55, 55, 68, 68, 71, 69, 69,
    69, 70, 0, 0, 73, 0, 0, 0, 0, 0, 0, 67, 59, 74, 77, 62, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 31, 0, 0, 0, 0, 0, 0, 31, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 30, 0, 0, 31, 0, 30, 0,
    30, 0, 0, 30, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 20, 100, 36, 0, 105, 109, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11, 11, 11, 11, 13, 13, 13,
    18, 18, 17, 17, 19, 19, 10, 10, 12, 12, 15, 16, 11, 14, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
]
