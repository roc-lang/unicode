## GENERATED from Unicode 17.0.0. Run `python3 scripts/unicode_data.py generate`. ##
## layout: 2176 U8 page ids + 6 x 512 U8 values; logical payload 5248 bytes. ##

import InternalLooseAlias

InternalJoiningGroup :: [].{
    Value : [No_Joining_Group, African_Feh, African_Noon, African_Qaf, Ain, Alaph, Alef, Beh, Beth, Burushaski_Yeh_Barree, Dal, Dalath_Rish, E, Farsi_Yeh, Fe, Feh, Final_Semkath, Gaf, Gamal, Hah, Hanifi_Rohingya_Kinna_Ya, Hanifi_Rohingya_Pa, He, Heh, Heh_Goal, Heth, Kaf, Kaph, Kashmiri_Yeh, Khaph, Knotted_Heh, Lam, Lamadh, Malayalam_Bha, Malayalam_Ja, Malayalam_Lla, Malayalam_Llla, Malayalam_Nga, Malayalam_Nna, Malayalam_Nnna, Malayalam_Nya, Malayalam_Ra, Malayalam_Ssa, Malayalam_Tta, Manichaean_Aleph, Manichaean_Ayin, Manichaean_Beth, Manichaean_Daleth, Manichaean_Dhamedh, Manichaean_Five, Manichaean_Gimel, Manichaean_Heth, Manichaean_Hundred, Manichaean_Kaph, Manichaean_Lamedh, Manichaean_Mem, Manichaean_Nun, Manichaean_One, Manichaean_Pe, Manichaean_Qoph, Manichaean_Resh, Manichaean_Sadhe, Manichaean_Samekh, Manichaean_Taw, Manichaean_Ten, Manichaean_Teth, Manichaean_Thamedh, Manichaean_Twenty, Manichaean_Waw, Manichaean_Yodh, Manichaean_Zayin, Meem, Mim, Noon, Nun, Nya, Pe, Qaf, Qaph, Reh, Reversed_Pe, Rohingya_Yeh, Sad, Sadhe, Seen, Semkath, Shin, Straight_Waw, Swash_Kaf, Syriac_Waw, Tah, Taw, Teh_Marbuta, Teh_Marbuta_Goal, Teth, Thin_Noon, Thin_Yeh, Vertical_Tail, Waw, Yeh, Yeh_Barree, Yeh_With_Tail, Yudh, Yudh_He, Zain, Zhain]
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
    parse = |name|         if InternalLooseAlias.matches(name, "No_Joining_Group") Some(No_Joining_Group) else         if InternalLooseAlias.matches(name, "African_Feh") Some(African_Feh) else         if InternalLooseAlias.matches(name, "African_Noon") Some(African_Noon) else         if InternalLooseAlias.matches(name, "African_Qaf") Some(African_Qaf) else         if InternalLooseAlias.matches(name, "Ain") Some(Ain) else         if InternalLooseAlias.matches(name, "Alaph") Some(Alaph) else         if InternalLooseAlias.matches(name, "Alef") Some(Alef) else         if InternalLooseAlias.matches(name, "Beh") Some(Beh) else         if InternalLooseAlias.matches(name, "Beth") Some(Beth) else         if InternalLooseAlias.matches(name, "Burushaski_Yeh_Barree") Some(Burushaski_Yeh_Barree) else         if InternalLooseAlias.matches(name, "Dal") Some(Dal) else         if InternalLooseAlias.matches(name, "Dalath_Rish") Some(Dalath_Rish) else         if InternalLooseAlias.matches(name, "E") Some(E) else         if InternalLooseAlias.matches(name, "Farsi_Yeh") Some(Farsi_Yeh) else         if InternalLooseAlias.matches(name, "Fe") Some(Fe) else         if InternalLooseAlias.matches(name, "Feh") Some(Feh) else         if InternalLooseAlias.matches(name, "Final_Semkath") Some(Final_Semkath) else         if InternalLooseAlias.matches(name, "Gaf") Some(Gaf) else         if InternalLooseAlias.matches(name, "Gamal") Some(Gamal) else         if InternalLooseAlias.matches(name, "Hah") Some(Hah) else         if InternalLooseAlias.matches(name, "Hanifi_Rohingya_Kinna_Ya") Some(Hanifi_Rohingya_Kinna_Ya) else         if InternalLooseAlias.matches(name, "Hanifi_Rohingya_Pa") Some(Hanifi_Rohingya_Pa) else         if InternalLooseAlias.matches(name, "He") Some(He) else         if InternalLooseAlias.matches(name, "Heh") Some(Heh) else         if InternalLooseAlias.matches(name, "Heh_Goal") Some(Heh_Goal) else         if InternalLooseAlias.matches(name, "Heth") Some(Heth) else         if InternalLooseAlias.matches(name, "Kaf") Some(Kaf) else         if InternalLooseAlias.matches(name, "Kaph") Some(Kaph) else         if InternalLooseAlias.matches(name, "Kashmiri_Yeh") Some(Kashmiri_Yeh) else         if InternalLooseAlias.matches(name, "Khaph") Some(Khaph) else         if InternalLooseAlias.matches(name, "Knotted_Heh") Some(Knotted_Heh) else         if InternalLooseAlias.matches(name, "Lam") Some(Lam) else         if InternalLooseAlias.matches(name, "Lamadh") Some(Lamadh) else         if InternalLooseAlias.matches(name, "Malayalam_Bha") Some(Malayalam_Bha) else         if InternalLooseAlias.matches(name, "Malayalam_Ja") Some(Malayalam_Ja) else         if InternalLooseAlias.matches(name, "Malayalam_Lla") Some(Malayalam_Lla) else         if InternalLooseAlias.matches(name, "Malayalam_Llla") Some(Malayalam_Llla) else         if InternalLooseAlias.matches(name, "Malayalam_Nga") Some(Malayalam_Nga) else         if InternalLooseAlias.matches(name, "Malayalam_Nna") Some(Malayalam_Nna) else         if InternalLooseAlias.matches(name, "Malayalam_Nnna") Some(Malayalam_Nnna) else         if InternalLooseAlias.matches(name, "Malayalam_Nya") Some(Malayalam_Nya) else         if InternalLooseAlias.matches(name, "Malayalam_Ra") Some(Malayalam_Ra) else         if InternalLooseAlias.matches(name, "Malayalam_Ssa") Some(Malayalam_Ssa) else         if InternalLooseAlias.matches(name, "Malayalam_Tta") Some(Malayalam_Tta) else         if InternalLooseAlias.matches(name, "Manichaean_Aleph") Some(Manichaean_Aleph) else         if InternalLooseAlias.matches(name, "Manichaean_Ayin") Some(Manichaean_Ayin) else         if InternalLooseAlias.matches(name, "Manichaean_Beth") Some(Manichaean_Beth) else         if InternalLooseAlias.matches(name, "Manichaean_Daleth") Some(Manichaean_Daleth) else         if InternalLooseAlias.matches(name, "Manichaean_Dhamedh") Some(Manichaean_Dhamedh) else         if InternalLooseAlias.matches(name, "Manichaean_Five") Some(Manichaean_Five) else         if InternalLooseAlias.matches(name, "Manichaean_Gimel") Some(Manichaean_Gimel) else         if InternalLooseAlias.matches(name, "Manichaean_Heth") Some(Manichaean_Heth) else         if InternalLooseAlias.matches(name, "Manichaean_Hundred") Some(Manichaean_Hundred) else         if InternalLooseAlias.matches(name, "Manichaean_Kaph") Some(Manichaean_Kaph) else         if InternalLooseAlias.matches(name, "Manichaean_Lamedh") Some(Manichaean_Lamedh) else         if InternalLooseAlias.matches(name, "Manichaean_Mem") Some(Manichaean_Mem) else         if InternalLooseAlias.matches(name, "Manichaean_Nun") Some(Manichaean_Nun) else         if InternalLooseAlias.matches(name, "Manichaean_One") Some(Manichaean_One) else         if InternalLooseAlias.matches(name, "Manichaean_Pe") Some(Manichaean_Pe) else         if InternalLooseAlias.matches(name, "Manichaean_Qoph") Some(Manichaean_Qoph) else         if InternalLooseAlias.matches(name, "Manichaean_Resh") Some(Manichaean_Resh) else         if InternalLooseAlias.matches(name, "Manichaean_Sadhe") Some(Manichaean_Sadhe) else         if InternalLooseAlias.matches(name, "Manichaean_Samekh") Some(Manichaean_Samekh) else         if InternalLooseAlias.matches(name, "Manichaean_Taw") Some(Manichaean_Taw) else         if InternalLooseAlias.matches(name, "Manichaean_Ten") Some(Manichaean_Ten) else         if InternalLooseAlias.matches(name, "Manichaean_Teth") Some(Manichaean_Teth) else         if InternalLooseAlias.matches(name, "Manichaean_Thamedh") Some(Manichaean_Thamedh) else         if InternalLooseAlias.matches(name, "Manichaean_Twenty") Some(Manichaean_Twenty) else         if InternalLooseAlias.matches(name, "Manichaean_Waw") Some(Manichaean_Waw) else         if InternalLooseAlias.matches(name, "Manichaean_Yodh") Some(Manichaean_Yodh) else         if InternalLooseAlias.matches(name, "Manichaean_Zayin") Some(Manichaean_Zayin) else         if InternalLooseAlias.matches(name, "Meem") Some(Meem) else         if InternalLooseAlias.matches(name, "Mim") Some(Mim) else         if InternalLooseAlias.matches(name, "Noon") Some(Noon) else         if InternalLooseAlias.matches(name, "Nun") Some(Nun) else         if InternalLooseAlias.matches(name, "Nya") Some(Nya) else         if InternalLooseAlias.matches(name, "Pe") Some(Pe) else         if InternalLooseAlias.matches(name, "Qaf") Some(Qaf) else         if InternalLooseAlias.matches(name, "Qaph") Some(Qaph) else         if InternalLooseAlias.matches(name, "Reh") Some(Reh) else         if InternalLooseAlias.matches(name, "Reversed_Pe") Some(Reversed_Pe) else         if InternalLooseAlias.matches(name, "Rohingya_Yeh") Some(Rohingya_Yeh) else         if InternalLooseAlias.matches(name, "Sad") Some(Sad) else         if InternalLooseAlias.matches(name, "Sadhe") Some(Sadhe) else         if InternalLooseAlias.matches(name, "Seen") Some(Seen) else         if InternalLooseAlias.matches(name, "Semkath") Some(Semkath) else         if InternalLooseAlias.matches(name, "Shin") Some(Shin) else         if InternalLooseAlias.matches(name, "Straight_Waw") Some(Straight_Waw) else         if InternalLooseAlias.matches(name, "Swash_Kaf") Some(Swash_Kaf) else         if InternalLooseAlias.matches(name, "Syriac_Waw") Some(Syriac_Waw) else         if InternalLooseAlias.matches(name, "Tah") Some(Tah) else         if InternalLooseAlias.matches(name, "Taw") Some(Taw) else         if InternalLooseAlias.matches(name, "Teh_Marbuta") Some(Teh_Marbuta) else         if InternalLooseAlias.matches(name, "Teh_Marbuta_Goal") or InternalLooseAlias.matches(name, "Hamza_On_Heh_Goal") Some(Teh_Marbuta_Goal) else         if InternalLooseAlias.matches(name, "Teth") Some(Teth) else         if InternalLooseAlias.matches(name, "Thin_Noon") Some(Thin_Noon) else         if InternalLooseAlias.matches(name, "Thin_Yeh") Some(Thin_Yeh) else         if InternalLooseAlias.matches(name, "Vertical_Tail") Some(Vertical_Tail) else         if InternalLooseAlias.matches(name, "Waw") Some(Waw) else         if InternalLooseAlias.matches(name, "Yeh") Some(Yeh) else         if InternalLooseAlias.matches(name, "Yeh_Barree") Some(Yeh_Barree) else         if InternalLooseAlias.matches(name, "Yeh_With_Tail") Some(Yeh_With_Tail) else         if InternalLooseAlias.matches(name, "Yudh") Some(Yudh) else         if InternalLooseAlias.matches(name, "Yudh_He") Some(Yudh_He) else         if InternalLooseAlias.matches(name, "Zain") Some(Zain) else         if InternalLooseAlias.matches(name, "Zhain") Some(Zhain) else None
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
        10 => Dal
        11 => Dalath_Rish
        12 => E
        13 => Farsi_Yeh
        14 => Fe
        15 => Feh
        16 => Final_Semkath
        17 => Gaf
        18 => Gamal
        19 => Hah
        20 => Hanifi_Rohingya_Kinna_Ya
        21 => Hanifi_Rohingya_Pa
        22 => He
        23 => Heh
        24 => Heh_Goal
        25 => Heth
        26 => Kaf
        27 => Kaph
        28 => Kashmiri_Yeh
        29 => Khaph
        30 => Knotted_Heh
        31 => Lam
        32 => Lamadh
        33 => Malayalam_Bha
        34 => Malayalam_Ja
        35 => Malayalam_Lla
        36 => Malayalam_Llla
        37 => Malayalam_Nga
        38 => Malayalam_Nna
        39 => Malayalam_Nnna
        40 => Malayalam_Nya
        41 => Malayalam_Ra
        42 => Malayalam_Ssa
        43 => Malayalam_Tta
        44 => Manichaean_Aleph
        45 => Manichaean_Ayin
        46 => Manichaean_Beth
        47 => Manichaean_Daleth
        48 => Manichaean_Dhamedh
        49 => Manichaean_Five
        50 => Manichaean_Gimel
        51 => Manichaean_Heth
        52 => Manichaean_Hundred
        53 => Manichaean_Kaph
        54 => Manichaean_Lamedh
        55 => Manichaean_Mem
        56 => Manichaean_Nun
        57 => Manichaean_One
        58 => Manichaean_Pe
        59 => Manichaean_Qoph
        60 => Manichaean_Resh
        61 => Manichaean_Sadhe
        62 => Manichaean_Samekh
        63 => Manichaean_Taw
        64 => Manichaean_Ten
        65 => Manichaean_Teth
        66 => Manichaean_Thamedh
        67 => Manichaean_Twenty
        68 => Manichaean_Waw
        69 => Manichaean_Yodh
        70 => Manichaean_Zayin
        71 => Meem
        72 => Mim
        73 => Noon
        74 => Nun
        75 => Nya
        76 => Pe
        77 => Qaf
        78 => Qaph
        79 => Reh
        80 => Reversed_Pe
        81 => Rohingya_Yeh
        82 => Sad
        83 => Sadhe
        84 => Seen
        85 => Semkath
        86 => Shin
        87 => Straight_Waw
        88 => Swash_Kaf
        89 => Syriac_Waw
        90 => Tah
        91 => Taw
        92 => Teh_Marbuta
        93 => Teh_Marbuta_Goal
        94 => Teth
        95 => Thin_Noon
        96 => Thin_Yeh
        97 => Vertical_Tail
        98 => Waw
        99 => Yeh
        100 => Yeh_Barree
        101 => Yeh_With_Tail
        102 => Yudh
        103 => Yudh_He
        104 => Zain
        105 => Zhain
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
            Dal => 10
            Dalath_Rish => 11
            E => 12
            Farsi_Yeh => 13
            Fe => 14
            Feh => 15
            Final_Semkath => 16
            Gaf => 17
            Gamal => 18
            Hah => 19
            Hanifi_Rohingya_Kinna_Ya => 20
            Hanifi_Rohingya_Pa => 21
            He => 22
            Heh => 23
            Heh_Goal => 24
            Heth => 25
            Kaf => 26
            Kaph => 27
            Kashmiri_Yeh => 28
            Khaph => 29
            Knotted_Heh => 30
            Lam => 31
            Lamadh => 32
            Malayalam_Bha => 33
            Malayalam_Ja => 34
            Malayalam_Lla => 35
            Malayalam_Llla => 36
            Malayalam_Nga => 37
            Malayalam_Nna => 38
            Malayalam_Nnna => 39
            Malayalam_Nya => 40
            Malayalam_Ra => 41
            Malayalam_Ssa => 42
            Malayalam_Tta => 43
            Manichaean_Aleph => 44
            Manichaean_Ayin => 45
            Manichaean_Beth => 46
            Manichaean_Daleth => 47
            Manichaean_Dhamedh => 48
            Manichaean_Five => 49
            Manichaean_Gimel => 50
            Manichaean_Heth => 51
            Manichaean_Hundred => 52
            Manichaean_Kaph => 53
            Manichaean_Lamedh => 54
            Manichaean_Mem => 55
            Manichaean_Nun => 56
            Manichaean_One => 57
            Manichaean_Pe => 58
            Manichaean_Qoph => 59
            Manichaean_Resh => 60
            Manichaean_Sadhe => 61
            Manichaean_Samekh => 62
            Manichaean_Taw => 63
            Manichaean_Ten => 64
            Manichaean_Teth => 65
            Manichaean_Thamedh => 66
            Manichaean_Twenty => 67
            Manichaean_Waw => 68
            Manichaean_Yodh => 69
            Manichaean_Zayin => 70
            Meem => 71
            Mim => 72
            Noon => 73
            Nun => 74
            Nya => 75
            Pe => 76
            Qaf => 77
            Qaph => 78
            Reh => 79
            Reversed_Pe => 80
            Rohingya_Yeh => 81
            Sad => 82
            Sadhe => 83
            Seen => 84
            Semkath => 85
            Shin => 86
            Straight_Waw => 87
            Swash_Kaf => 88
            Syriac_Waw => 89
            Tah => 90
            Taw => 91
            Teh_Marbuta => 92
            Teh_Marbuta_Goal => 93
            Teth => 94
            Thin_Noon => 95
            Thin_Yeh => 96
            Vertical_Tail => 97
            Waw => 98
            Yeh => 99
            Yeh_Barree => 100
            Yeh_With_Tail => 101
            Yudh => 102
            Yudh_He => 103
            Zain => 104
            Zhain => 105
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
    28, 0, 6, 6, 98, 6, 99, 6, 7, 92, 7, 7, 19, 19, 19, 10, 10, 79, 79, 84, 84, 82, 82, 90, 90, 4, 4, 17, 17, 13, 13, 13,
    0, 15, 77, 26, 31, 71, 73, 23, 98, 99, 99, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7, 77, 0, 6, 6, 6, 0, 6, 98, 98, 99, 7, 7, 7, 7, 7, 7, 7,
    7, 19, 19, 19, 19, 19, 19, 19, 10, 10, 10, 10, 10, 10, 10, 10, 10, 79, 79, 79, 79, 79, 79, 79, 79, 79, 84, 84, 84, 82, 82, 90,
    4, 15, 15, 15, 15, 15, 15, 77, 77, 17, 88, 17, 26, 26, 26, 17, 17, 17, 17, 17, 17, 31, 31, 31, 31, 73, 73, 73, 73, 75, 30, 19,
    92, 24, 24, 93, 98, 98, 98, 98, 98, 98, 98, 98, 13, 101, 13, 98, 99, 99, 100, 100, 0, 92, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 79, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 84, 82, 4, 0, 0, 30,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 8, 18, 18, 11, 11, 22, 89, 104, 25, 94, 94, 102, 103, 27,
    32, 72, 74, 85, 16, 12, 76, 80, 83, 78, 11, 86, 91, 8, 18, 11, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 105, 29, 14, 7, 7, 7, 7, 7, 7, 7, 19, 19, 10, 10, 79, 84, 4, 4, 4,
    15, 15, 17, 17, 17, 71, 71, 73, 73, 73, 31, 79, 79, 84, 19, 19, 84, 79, 19, 6, 6, 13, 13, 99, 98, 98, 9, 9, 19, 84, 84, 26,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    37, 34, 40, 43, 38, 39, 33, 41, 35, 36, 42, 0, 0, 0, 0, 0, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 0, 0, 0, 96, 0, 0, 73, 19, 90, 90, 17, 97, 73, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    7, 7, 19, 90, 15, 77, 31, 71, 99, 99, 79, 98, 81, 0, 10, 82, 17, 87, 79, 4, 26, 77, 7, 7, 7, 79, 99, 1, 3, 2, 7, 7,
    7, 19, 17, 4, 3, 19, 19, 31, 17, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
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
    44, 46, 46, 50, 50, 47, 0, 68, 0, 70, 70, 0, 0, 51, 65, 69, 53, 53, 53, 54, 48, 66, 55, 56, 62, 45, 45, 58, 58, 61, 59, 59,
    59, 60, 0, 0, 63, 0, 0, 0, 0, 0, 0, 57, 49, 64, 67, 52, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
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
    0, 0, 21, 0, 0, 0, 0, 0, 0, 21, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 20, 0, 0, 21, 0, 20, 0,
    20, 0, 0, 20, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
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
    0, 0, 10, 90, 26, 0, 95, 99, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
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
