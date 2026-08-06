## GENERATED from Unicode 17.0.0 Scripts.txt and PropertyValueAliases.txt under UAX #24 revision 39. ##
## Run `python3 scripts/unicode_data.py generate`. Named tags/aliases are stable; U8 values are private. ##
## Private IDs are generator-asserted canonical short-alias order solely to implement the public order contract. ##
## default: Zzzz; 176 identities; layout: 8704 U8 page ids + 255 x 128 U8 values; logical payload 41344 bytes. ##

import InternalUtf8

InternalScriptData :: [].{
    Script : [Adlm, Aghb, Ahom, Arab, Armi, Armn, Avst, Bali, Bamu, Bass, Batk, Beng, Berf, Bhks, Bopo, Brah, Brai, Bugi, Buhd, Cakm, Cans, Cari, Cham, Cher, Chrs, Copt, Cpmn, Cprt, Cyrl, Deva, Diak, Dogr, Dsrt, Dupl, Egyp, Elba, Elym, Ethi, Gara, Geor, Glag, Gong, Gonm, Goth, Gran, Grek, Gujr, Gukh, Guru, Hang, Hani, Hano, Hatr, Hebr, Hira, Hluw, Hmng, Hmnp, Hrkt, Hung, Ital, Java, Kali, Kana, Kawi, Khar, Khmr, Khoj, Kits, Knda, Krai, Kthi, Lana, Laoo, Latn, Lepc, Limb, Lina, Linb, Lisu, Lyci, Lydi, Mahj, Maka, Mand, Mani, Marc, Medf, Mend, Merc, Mero, Mlym, Modi, Mong, Mroo, Mtei, Mult, Mymr, Nagm, Nand, Narb, Nbat, Newa, Nkoo, Nshu, Ogam, Olck, Onao, Orkh, Orya, Osge, Osma, Ougr, Palm, Pauc, Perm, Phag, Phli, Phlp, Phnx, Plrd, Prti, Rjng, Rohg, Runr, Samr, Sarb, Saur, Sgnw, Shaw, Shrd, Sidd, Sidt, Sind, Sinh, Sogd, Sogo, Sora, Soyo, Sund, Sunu, Sylo, Syrc, Tagb, Takr, Tale, Talu, Taml, Tang, Tavt, Tayo, Telu, Tfng, Tglg, Thaa, Thai, Tibt, Tirh, Tnsa, Todr, Tols, Toto, Tutg, Ugar, Vaii, Vith, Wara, Wcho, Xpeo, Xsux, Yezi, Yiii, Zanb, Zinh, Zyyy, Zzzz]

    lookup : U32 -> Script
    lookup = |scalar| from_private_id(lookup_private(scalar))

    lookup_private : U32 -> U8
    lookup_private = |scalar| {
        if scalar < 128 {
            ascii_private_id(scalar)
        } else if scalar > 0x10FFFF {
            175
        } else {
            page_id = page_index.get(scalar.shr_wrap(7).to_u64()) ?? 0
            offset = page_id.to_u64() * 128 + scalar.bitwise_and(127).to_u64()
            pages.get(offset) ?? 175
        }
    }

    common_private_id : U8
    common_private_id = 174

    inherited_private_id : U8
    inherited_private_id = 173

    unknown_private_id : U8
    unknown_private_id = 175

    private_id : Script -> U8
    private_id = |script| {
        match script {
        Adlm => 0
        Aghb => 1
        Ahom => 2
        Arab => 3
        Armi => 4
        Armn => 5
        Avst => 6
        Bali => 7
        Bamu => 8
        Bass => 9
        Batk => 10
        Beng => 11
        Berf => 12
        Bhks => 13
        Bopo => 14
        Brah => 15
        Brai => 16
        Bugi => 17
        Buhd => 18
        Cakm => 19
        Cans => 20
        Cari => 21
        Cham => 22
        Cher => 23
        Chrs => 24
        Copt => 25
        Cpmn => 26
        Cprt => 27
        Cyrl => 28
        Deva => 29
        Diak => 30
        Dogr => 31
        Dsrt => 32
        Dupl => 33
        Egyp => 34
        Elba => 35
        Elym => 36
        Ethi => 37
        Gara => 38
        Geor => 39
        Glag => 40
        Gong => 41
        Gonm => 42
        Goth => 43
        Gran => 44
        Grek => 45
        Gujr => 46
        Gukh => 47
        Guru => 48
        Hang => 49
        Hani => 50
        Hano => 51
        Hatr => 52
        Hebr => 53
        Hira => 54
        Hluw => 55
        Hmng => 56
        Hmnp => 57
        Hrkt => 58
        Hung => 59
        Ital => 60
        Java => 61
        Kali => 62
        Kana => 63
        Kawi => 64
        Khar => 65
        Khmr => 66
        Khoj => 67
        Kits => 68
        Knda => 69
        Krai => 70
        Kthi => 71
        Lana => 72
        Laoo => 73
        Latn => 74
        Lepc => 75
        Limb => 76
        Lina => 77
        Linb => 78
        Lisu => 79
        Lyci => 80
        Lydi => 81
        Mahj => 82
        Maka => 83
        Mand => 84
        Mani => 85
        Marc => 86
        Medf => 87
        Mend => 88
        Merc => 89
        Mero => 90
        Mlym => 91
        Modi => 92
        Mong => 93
        Mroo => 94
        Mtei => 95
        Mult => 96
        Mymr => 97
        Nagm => 98
        Nand => 99
        Narb => 100
        Nbat => 101
        Newa => 102
        Nkoo => 103
        Nshu => 104
        Ogam => 105
        Olck => 106
        Onao => 107
        Orkh => 108
        Orya => 109
        Osge => 110
        Osma => 111
        Ougr => 112
        Palm => 113
        Pauc => 114
        Perm => 115
        Phag => 116
        Phli => 117
        Phlp => 118
        Phnx => 119
        Plrd => 120
        Prti => 121
        Rjng => 122
        Rohg => 123
        Runr => 124
        Samr => 125
        Sarb => 126
        Saur => 127
        Sgnw => 128
        Shaw => 129
        Shrd => 130
        Sidd => 131
        Sidt => 132
        Sind => 133
        Sinh => 134
        Sogd => 135
        Sogo => 136
        Sora => 137
        Soyo => 138
        Sund => 139
        Sunu => 140
        Sylo => 141
        Syrc => 142
        Tagb => 143
        Takr => 144
        Tale => 145
        Talu => 146
        Taml => 147
        Tang => 148
        Tavt => 149
        Tayo => 150
        Telu => 151
        Tfng => 152
        Tglg => 153
        Thaa => 154
        Thai => 155
        Tibt => 156
        Tirh => 157
        Tnsa => 158
        Todr => 159
        Tols => 160
        Toto => 161
        Tutg => 162
        Ugar => 163
        Vaii => 164
        Vith => 165
        Wara => 166
        Wcho => 167
        Xpeo => 168
        Xsux => 169
        Yezi => 170
        Yiii => 171
        Zanb => 172
        Zinh => 173
        Zyyy => 174
        Zzzz => 175
        }
    }

    from_private_id : U8 -> Script
    from_private_id = |value| {
        match value {
        0 => Adlm
        1 => Aghb
        2 => Ahom
        3 => Arab
        4 => Armi
        5 => Armn
        6 => Avst
        7 => Bali
        8 => Bamu
        9 => Bass
        10 => Batk
        11 => Beng
        12 => Berf
        13 => Bhks
        14 => Bopo
        15 => Brah
        16 => Brai
        17 => Bugi
        18 => Buhd
        19 => Cakm
        20 => Cans
        21 => Cari
        22 => Cham
        23 => Cher
        24 => Chrs
        25 => Copt
        26 => Cpmn
        27 => Cprt
        28 => Cyrl
        29 => Deva
        30 => Diak
        31 => Dogr
        32 => Dsrt
        33 => Dupl
        34 => Egyp
        35 => Elba
        36 => Elym
        37 => Ethi
        38 => Gara
        39 => Geor
        40 => Glag
        41 => Gong
        42 => Gonm
        43 => Goth
        44 => Gran
        45 => Grek
        46 => Gujr
        47 => Gukh
        48 => Guru
        49 => Hang
        50 => Hani
        51 => Hano
        52 => Hatr
        53 => Hebr
        54 => Hira
        55 => Hluw
        56 => Hmng
        57 => Hmnp
        58 => Hrkt
        59 => Hung
        60 => Ital
        61 => Java
        62 => Kali
        63 => Kana
        64 => Kawi
        65 => Khar
        66 => Khmr
        67 => Khoj
        68 => Kits
        69 => Knda
        70 => Krai
        71 => Kthi
        72 => Lana
        73 => Laoo
        74 => Latn
        75 => Lepc
        76 => Limb
        77 => Lina
        78 => Linb
        79 => Lisu
        80 => Lyci
        81 => Lydi
        82 => Mahj
        83 => Maka
        84 => Mand
        85 => Mani
        86 => Marc
        87 => Medf
        88 => Mend
        89 => Merc
        90 => Mero
        91 => Mlym
        92 => Modi
        93 => Mong
        94 => Mroo
        95 => Mtei
        96 => Mult
        97 => Mymr
        98 => Nagm
        99 => Nand
        100 => Narb
        101 => Nbat
        102 => Newa
        103 => Nkoo
        104 => Nshu
        105 => Ogam
        106 => Olck
        107 => Onao
        108 => Orkh
        109 => Orya
        110 => Osge
        111 => Osma
        112 => Ougr
        113 => Palm
        114 => Pauc
        115 => Perm
        116 => Phag
        117 => Phli
        118 => Phlp
        119 => Phnx
        120 => Plrd
        121 => Prti
        122 => Rjng
        123 => Rohg
        124 => Runr
        125 => Samr
        126 => Sarb
        127 => Saur
        128 => Sgnw
        129 => Shaw
        130 => Shrd
        131 => Sidd
        132 => Sidt
        133 => Sind
        134 => Sinh
        135 => Sogd
        136 => Sogo
        137 => Sora
        138 => Soyo
        139 => Sund
        140 => Sunu
        141 => Sylo
        142 => Syrc
        143 => Tagb
        144 => Takr
        145 => Tale
        146 => Talu
        147 => Taml
        148 => Tang
        149 => Tavt
        150 => Tayo
        151 => Telu
        152 => Tfng
        153 => Tglg
        154 => Thaa
        155 => Thai
        156 => Tibt
        157 => Tirh
        158 => Tnsa
        159 => Todr
        160 => Tols
        161 => Toto
        162 => Tutg
        163 => Ugar
        164 => Vaii
        165 => Vith
        166 => Wara
        167 => Wcho
        168 => Xpeo
        169 => Xsux
        170 => Yezi
        171 => Yiii
        172 => Zanb
        173 => Zinh
        174 => Zyyy
        175 => Zzzz
        _ => Zzzz
        }
    }

    short_alias : Script -> Str
    short_alias = |script| {
        match script {
        Adlm => "Adlm"
        Aghb => "Aghb"
        Ahom => "Ahom"
        Arab => "Arab"
        Armi => "Armi"
        Armn => "Armn"
        Avst => "Avst"
        Bali => "Bali"
        Bamu => "Bamu"
        Bass => "Bass"
        Batk => "Batk"
        Beng => "Beng"
        Berf => "Berf"
        Bhks => "Bhks"
        Bopo => "Bopo"
        Brah => "Brah"
        Brai => "Brai"
        Bugi => "Bugi"
        Buhd => "Buhd"
        Cakm => "Cakm"
        Cans => "Cans"
        Cari => "Cari"
        Cham => "Cham"
        Cher => "Cher"
        Chrs => "Chrs"
        Copt => "Copt"
        Cpmn => "Cpmn"
        Cprt => "Cprt"
        Cyrl => "Cyrl"
        Deva => "Deva"
        Diak => "Diak"
        Dogr => "Dogr"
        Dsrt => "Dsrt"
        Dupl => "Dupl"
        Egyp => "Egyp"
        Elba => "Elba"
        Elym => "Elym"
        Ethi => "Ethi"
        Gara => "Gara"
        Geor => "Geor"
        Glag => "Glag"
        Gong => "Gong"
        Gonm => "Gonm"
        Goth => "Goth"
        Gran => "Gran"
        Grek => "Grek"
        Gujr => "Gujr"
        Gukh => "Gukh"
        Guru => "Guru"
        Hang => "Hang"
        Hani => "Hani"
        Hano => "Hano"
        Hatr => "Hatr"
        Hebr => "Hebr"
        Hira => "Hira"
        Hluw => "Hluw"
        Hmng => "Hmng"
        Hmnp => "Hmnp"
        Hrkt => "Hrkt"
        Hung => "Hung"
        Ital => "Ital"
        Java => "Java"
        Kali => "Kali"
        Kana => "Kana"
        Kawi => "Kawi"
        Khar => "Khar"
        Khmr => "Khmr"
        Khoj => "Khoj"
        Kits => "Kits"
        Knda => "Knda"
        Krai => "Krai"
        Kthi => "Kthi"
        Lana => "Lana"
        Laoo => "Laoo"
        Latn => "Latn"
        Lepc => "Lepc"
        Limb => "Limb"
        Lina => "Lina"
        Linb => "Linb"
        Lisu => "Lisu"
        Lyci => "Lyci"
        Lydi => "Lydi"
        Mahj => "Mahj"
        Maka => "Maka"
        Mand => "Mand"
        Mani => "Mani"
        Marc => "Marc"
        Medf => "Medf"
        Mend => "Mend"
        Merc => "Merc"
        Mero => "Mero"
        Mlym => "Mlym"
        Modi => "Modi"
        Mong => "Mong"
        Mroo => "Mroo"
        Mtei => "Mtei"
        Mult => "Mult"
        Mymr => "Mymr"
        Nagm => "Nagm"
        Nand => "Nand"
        Narb => "Narb"
        Nbat => "Nbat"
        Newa => "Newa"
        Nkoo => "Nkoo"
        Nshu => "Nshu"
        Ogam => "Ogam"
        Olck => "Olck"
        Onao => "Onao"
        Orkh => "Orkh"
        Orya => "Orya"
        Osge => "Osge"
        Osma => "Osma"
        Ougr => "Ougr"
        Palm => "Palm"
        Pauc => "Pauc"
        Perm => "Perm"
        Phag => "Phag"
        Phli => "Phli"
        Phlp => "Phlp"
        Phnx => "Phnx"
        Plrd => "Plrd"
        Prti => "Prti"
        Rjng => "Rjng"
        Rohg => "Rohg"
        Runr => "Runr"
        Samr => "Samr"
        Sarb => "Sarb"
        Saur => "Saur"
        Sgnw => "Sgnw"
        Shaw => "Shaw"
        Shrd => "Shrd"
        Sidd => "Sidd"
        Sidt => "Sidt"
        Sind => "Sind"
        Sinh => "Sinh"
        Sogd => "Sogd"
        Sogo => "Sogo"
        Sora => "Sora"
        Soyo => "Soyo"
        Sund => "Sund"
        Sunu => "Sunu"
        Sylo => "Sylo"
        Syrc => "Syrc"
        Tagb => "Tagb"
        Takr => "Takr"
        Tale => "Tale"
        Talu => "Talu"
        Taml => "Taml"
        Tang => "Tang"
        Tavt => "Tavt"
        Tayo => "Tayo"
        Telu => "Telu"
        Tfng => "Tfng"
        Tglg => "Tglg"
        Thaa => "Thaa"
        Thai => "Thai"
        Tibt => "Tibt"
        Tirh => "Tirh"
        Tnsa => "Tnsa"
        Todr => "Todr"
        Tols => "Tols"
        Toto => "Toto"
        Tutg => "Tutg"
        Ugar => "Ugar"
        Vaii => "Vaii"
        Vith => "Vith"
        Wara => "Wara"
        Wcho => "Wcho"
        Xpeo => "Xpeo"
        Xsux => "Xsux"
        Yezi => "Yezi"
        Yiii => "Yiii"
        Zanb => "Zanb"
        Zinh => "Zinh"
        Zyyy => "Zyyy"
        Zzzz => "Zzzz"
        }
    }

    long_alias : Script -> Str
    long_alias = |script| {
        match script {
        Adlm => "Adlam"
        Aghb => "Caucasian_Albanian"
        Ahom => "Ahom"
        Arab => "Arabic"
        Armi => "Imperial_Aramaic"
        Armn => "Armenian"
        Avst => "Avestan"
        Bali => "Balinese"
        Bamu => "Bamum"
        Bass => "Bassa_Vah"
        Batk => "Batak"
        Beng => "Bengali"
        Berf => "Beria_Erfe"
        Bhks => "Bhaiksuki"
        Bopo => "Bopomofo"
        Brah => "Brahmi"
        Brai => "Braille"
        Bugi => "Buginese"
        Buhd => "Buhid"
        Cakm => "Chakma"
        Cans => "Canadian_Aboriginal"
        Cari => "Carian"
        Cham => "Cham"
        Cher => "Cherokee"
        Chrs => "Chorasmian"
        Copt => "Coptic"
        Cpmn => "Cypro_Minoan"
        Cprt => "Cypriot"
        Cyrl => "Cyrillic"
        Deva => "Devanagari"
        Diak => "Dives_Akuru"
        Dogr => "Dogra"
        Dsrt => "Deseret"
        Dupl => "Duployan"
        Egyp => "Egyptian_Hieroglyphs"
        Elba => "Elbasan"
        Elym => "Elymaic"
        Ethi => "Ethiopic"
        Gara => "Garay"
        Geor => "Georgian"
        Glag => "Glagolitic"
        Gong => "Gunjala_Gondi"
        Gonm => "Masaram_Gondi"
        Goth => "Gothic"
        Gran => "Grantha"
        Grek => "Greek"
        Gujr => "Gujarati"
        Gukh => "Gurung_Khema"
        Guru => "Gurmukhi"
        Hang => "Hangul"
        Hani => "Han"
        Hano => "Hanunoo"
        Hatr => "Hatran"
        Hebr => "Hebrew"
        Hira => "Hiragana"
        Hluw => "Anatolian_Hieroglyphs"
        Hmng => "Pahawh_Hmong"
        Hmnp => "Nyiakeng_Puachue_Hmong"
        Hrkt => "Katakana_Or_Hiragana"
        Hung => "Old_Hungarian"
        Ital => "Old_Italic"
        Java => "Javanese"
        Kali => "Kayah_Li"
        Kana => "Katakana"
        Kawi => "Kawi"
        Khar => "Kharoshthi"
        Khmr => "Khmer"
        Khoj => "Khojki"
        Kits => "Khitan_Small_Script"
        Knda => "Kannada"
        Krai => "Kirat_Rai"
        Kthi => "Kaithi"
        Lana => "Tai_Tham"
        Laoo => "Lao"
        Latn => "Latin"
        Lepc => "Lepcha"
        Limb => "Limbu"
        Lina => "Linear_A"
        Linb => "Linear_B"
        Lisu => "Lisu"
        Lyci => "Lycian"
        Lydi => "Lydian"
        Mahj => "Mahajani"
        Maka => "Makasar"
        Mand => "Mandaic"
        Mani => "Manichaean"
        Marc => "Marchen"
        Medf => "Medefaidrin"
        Mend => "Mende_Kikakui"
        Merc => "Meroitic_Cursive"
        Mero => "Meroitic_Hieroglyphs"
        Mlym => "Malayalam"
        Modi => "Modi"
        Mong => "Mongolian"
        Mroo => "Mro"
        Mtei => "Meetei_Mayek"
        Mult => "Multani"
        Mymr => "Myanmar"
        Nagm => "Nag_Mundari"
        Nand => "Nandinagari"
        Narb => "Old_North_Arabian"
        Nbat => "Nabataean"
        Newa => "Newa"
        Nkoo => "Nko"
        Nshu => "Nushu"
        Ogam => "Ogham"
        Olck => "Ol_Chiki"
        Onao => "Ol_Onal"
        Orkh => "Old_Turkic"
        Orya => "Oriya"
        Osge => "Osage"
        Osma => "Osmanya"
        Ougr => "Old_Uyghur"
        Palm => "Palmyrene"
        Pauc => "Pau_Cin_Hau"
        Perm => "Old_Permic"
        Phag => "Phags_Pa"
        Phli => "Inscriptional_Pahlavi"
        Phlp => "Psalter_Pahlavi"
        Phnx => "Phoenician"
        Plrd => "Miao"
        Prti => "Inscriptional_Parthian"
        Rjng => "Rejang"
        Rohg => "Hanifi_Rohingya"
        Runr => "Runic"
        Samr => "Samaritan"
        Sarb => "Old_South_Arabian"
        Saur => "Saurashtra"
        Sgnw => "SignWriting"
        Shaw => "Shavian"
        Shrd => "Sharada"
        Sidd => "Siddham"
        Sidt => "Sidetic"
        Sind => "Khudawadi"
        Sinh => "Sinhala"
        Sogd => "Sogdian"
        Sogo => "Old_Sogdian"
        Sora => "Sora_Sompeng"
        Soyo => "Soyombo"
        Sund => "Sundanese"
        Sunu => "Sunuwar"
        Sylo => "Syloti_Nagri"
        Syrc => "Syriac"
        Tagb => "Tagbanwa"
        Takr => "Takri"
        Tale => "Tai_Le"
        Talu => "New_Tai_Lue"
        Taml => "Tamil"
        Tang => "Tangut"
        Tavt => "Tai_Viet"
        Tayo => "Tai_Yo"
        Telu => "Telugu"
        Tfng => "Tifinagh"
        Tglg => "Tagalog"
        Thaa => "Thaana"
        Thai => "Thai"
        Tibt => "Tibetan"
        Tirh => "Tirhuta"
        Tnsa => "Tangsa"
        Todr => "Todhri"
        Tols => "Tolong_Siki"
        Toto => "Toto"
        Tutg => "Tulu_Tigalari"
        Ugar => "Ugaritic"
        Vaii => "Vai"
        Vith => "Vithkuqi"
        Wara => "Warang_Citi"
        Wcho => "Wancho"
        Xpeo => "Old_Persian"
        Xsux => "Cuneiform"
        Yezi => "Yezidi"
        Yiii => "Yi"
        Zanb => "Zanabazar_Square"
        Zinh => "Inherited"
        Zyyy => "Common"
        Zzzz => "Unknown"
        }
    }

    alias_count : Script -> U8
    alias_count = |script| {
        match script {
        Adlm => 2
        Aghb => 2
        Ahom => 1
        Arab => 2
        Armi => 2
        Armn => 2
        Avst => 2
        Bali => 2
        Bamu => 2
        Bass => 2
        Batk => 2
        Beng => 2
        Berf => 2
        Bhks => 2
        Bopo => 2
        Brah => 2
        Brai => 2
        Bugi => 2
        Buhd => 2
        Cakm => 2
        Cans => 2
        Cari => 2
        Cham => 1
        Cher => 2
        Chrs => 2
        Copt => 3
        Cpmn => 2
        Cprt => 2
        Cyrl => 2
        Deva => 2
        Diak => 2
        Dogr => 2
        Dsrt => 2
        Dupl => 2
        Egyp => 2
        Elba => 2
        Elym => 2
        Ethi => 2
        Gara => 2
        Geor => 2
        Glag => 2
        Gong => 2
        Gonm => 2
        Goth => 2
        Gran => 2
        Grek => 2
        Gujr => 2
        Gukh => 2
        Guru => 2
        Hang => 2
        Hani => 2
        Hano => 2
        Hatr => 2
        Hebr => 2
        Hira => 2
        Hluw => 2
        Hmng => 2
        Hmnp => 2
        Hrkt => 2
        Hung => 2
        Ital => 2
        Java => 2
        Kali => 2
        Kana => 2
        Kawi => 1
        Khar => 2
        Khmr => 2
        Khoj => 2
        Kits => 2
        Knda => 2
        Krai => 2
        Kthi => 2
        Lana => 2
        Laoo => 2
        Latn => 2
        Lepc => 2
        Limb => 2
        Lina => 2
        Linb => 2
        Lisu => 1
        Lyci => 2
        Lydi => 2
        Mahj => 2
        Maka => 2
        Mand => 2
        Mani => 2
        Marc => 2
        Medf => 2
        Mend => 2
        Merc => 2
        Mero => 2
        Mlym => 2
        Modi => 1
        Mong => 2
        Mroo => 2
        Mtei => 2
        Mult => 2
        Mymr => 2
        Nagm => 2
        Nand => 2
        Narb => 2
        Nbat => 2
        Newa => 1
        Nkoo => 2
        Nshu => 2
        Ogam => 2
        Olck => 2
        Onao => 2
        Orkh => 2
        Orya => 2
        Osge => 2
        Osma => 2
        Ougr => 2
        Palm => 2
        Pauc => 2
        Perm => 2
        Phag => 2
        Phli => 2
        Phlp => 2
        Phnx => 2
        Plrd => 2
        Prti => 2
        Rjng => 2
        Rohg => 2
        Runr => 2
        Samr => 2
        Sarb => 2
        Saur => 2
        Sgnw => 2
        Shaw => 2
        Shrd => 2
        Sidd => 2
        Sidt => 2
        Sind => 2
        Sinh => 2
        Sogd => 2
        Sogo => 2
        Sora => 2
        Soyo => 2
        Sund => 2
        Sunu => 2
        Sylo => 2
        Syrc => 2
        Tagb => 2
        Takr => 2
        Tale => 2
        Talu => 2
        Taml => 2
        Tang => 2
        Tavt => 2
        Tayo => 2
        Telu => 2
        Tfng => 2
        Tglg => 2
        Thaa => 2
        Thai => 1
        Tibt => 2
        Tirh => 2
        Tnsa => 2
        Todr => 2
        Tols => 2
        Toto => 1
        Tutg => 2
        Ugar => 2
        Vaii => 2
        Vith => 2
        Wara => 2
        Wcho => 2
        Xpeo => 2
        Xsux => 2
        Yezi => 2
        Yiii => 2
        Zanb => 2
        Zinh => 3
        Zyyy => 2
        Zzzz => 2
        }
    }

    alias_at : Script, U8 -> [Some(Str), None]
    alias_at = |script, index| {
        match (script, index) {
        (Adlm, 0) => Some("Adlm")
        (Adlm, 1) => Some("Adlam")
        (Aghb, 0) => Some("Aghb")
        (Aghb, 1) => Some("Caucasian_Albanian")
        (Ahom, 0) => Some("Ahom")
        (Arab, 0) => Some("Arab")
        (Arab, 1) => Some("Arabic")
        (Armi, 0) => Some("Armi")
        (Armi, 1) => Some("Imperial_Aramaic")
        (Armn, 0) => Some("Armn")
        (Armn, 1) => Some("Armenian")
        (Avst, 0) => Some("Avst")
        (Avst, 1) => Some("Avestan")
        (Bali, 0) => Some("Bali")
        (Bali, 1) => Some("Balinese")
        (Bamu, 0) => Some("Bamu")
        (Bamu, 1) => Some("Bamum")
        (Bass, 0) => Some("Bass")
        (Bass, 1) => Some("Bassa_Vah")
        (Batk, 0) => Some("Batk")
        (Batk, 1) => Some("Batak")
        (Beng, 0) => Some("Beng")
        (Beng, 1) => Some("Bengali")
        (Berf, 0) => Some("Berf")
        (Berf, 1) => Some("Beria_Erfe")
        (Bhks, 0) => Some("Bhks")
        (Bhks, 1) => Some("Bhaiksuki")
        (Bopo, 0) => Some("Bopo")
        (Bopo, 1) => Some("Bopomofo")
        (Brah, 0) => Some("Brah")
        (Brah, 1) => Some("Brahmi")
        (Brai, 0) => Some("Brai")
        (Brai, 1) => Some("Braille")
        (Bugi, 0) => Some("Bugi")
        (Bugi, 1) => Some("Buginese")
        (Buhd, 0) => Some("Buhd")
        (Buhd, 1) => Some("Buhid")
        (Cakm, 0) => Some("Cakm")
        (Cakm, 1) => Some("Chakma")
        (Cans, 0) => Some("Cans")
        (Cans, 1) => Some("Canadian_Aboriginal")
        (Cari, 0) => Some("Cari")
        (Cari, 1) => Some("Carian")
        (Cham, 0) => Some("Cham")
        (Cher, 0) => Some("Cher")
        (Cher, 1) => Some("Cherokee")
        (Chrs, 0) => Some("Chrs")
        (Chrs, 1) => Some("Chorasmian")
        (Copt, 0) => Some("Copt")
        (Copt, 1) => Some("Coptic")
        (Copt, 2) => Some("Qaac")
        (Cpmn, 0) => Some("Cpmn")
        (Cpmn, 1) => Some("Cypro_Minoan")
        (Cprt, 0) => Some("Cprt")
        (Cprt, 1) => Some("Cypriot")
        (Cyrl, 0) => Some("Cyrl")
        (Cyrl, 1) => Some("Cyrillic")
        (Deva, 0) => Some("Deva")
        (Deva, 1) => Some("Devanagari")
        (Diak, 0) => Some("Diak")
        (Diak, 1) => Some("Dives_Akuru")
        (Dogr, 0) => Some("Dogr")
        (Dogr, 1) => Some("Dogra")
        (Dsrt, 0) => Some("Dsrt")
        (Dsrt, 1) => Some("Deseret")
        (Dupl, 0) => Some("Dupl")
        (Dupl, 1) => Some("Duployan")
        (Egyp, 0) => Some("Egyp")
        (Egyp, 1) => Some("Egyptian_Hieroglyphs")
        (Elba, 0) => Some("Elba")
        (Elba, 1) => Some("Elbasan")
        (Elym, 0) => Some("Elym")
        (Elym, 1) => Some("Elymaic")
        (Ethi, 0) => Some("Ethi")
        (Ethi, 1) => Some("Ethiopic")
        (Gara, 0) => Some("Gara")
        (Gara, 1) => Some("Garay")
        (Geor, 0) => Some("Geor")
        (Geor, 1) => Some("Georgian")
        (Glag, 0) => Some("Glag")
        (Glag, 1) => Some("Glagolitic")
        (Gong, 0) => Some("Gong")
        (Gong, 1) => Some("Gunjala_Gondi")
        (Gonm, 0) => Some("Gonm")
        (Gonm, 1) => Some("Masaram_Gondi")
        (Goth, 0) => Some("Goth")
        (Goth, 1) => Some("Gothic")
        (Gran, 0) => Some("Gran")
        (Gran, 1) => Some("Grantha")
        (Grek, 0) => Some("Grek")
        (Grek, 1) => Some("Greek")
        (Gujr, 0) => Some("Gujr")
        (Gujr, 1) => Some("Gujarati")
        (Gukh, 0) => Some("Gukh")
        (Gukh, 1) => Some("Gurung_Khema")
        (Guru, 0) => Some("Guru")
        (Guru, 1) => Some("Gurmukhi")
        (Hang, 0) => Some("Hang")
        (Hang, 1) => Some("Hangul")
        (Hani, 0) => Some("Hani")
        (Hani, 1) => Some("Han")
        (Hano, 0) => Some("Hano")
        (Hano, 1) => Some("Hanunoo")
        (Hatr, 0) => Some("Hatr")
        (Hatr, 1) => Some("Hatran")
        (Hebr, 0) => Some("Hebr")
        (Hebr, 1) => Some("Hebrew")
        (Hira, 0) => Some("Hira")
        (Hira, 1) => Some("Hiragana")
        (Hluw, 0) => Some("Hluw")
        (Hluw, 1) => Some("Anatolian_Hieroglyphs")
        (Hmng, 0) => Some("Hmng")
        (Hmng, 1) => Some("Pahawh_Hmong")
        (Hmnp, 0) => Some("Hmnp")
        (Hmnp, 1) => Some("Nyiakeng_Puachue_Hmong")
        (Hrkt, 0) => Some("Hrkt")
        (Hrkt, 1) => Some("Katakana_Or_Hiragana")
        (Hung, 0) => Some("Hung")
        (Hung, 1) => Some("Old_Hungarian")
        (Ital, 0) => Some("Ital")
        (Ital, 1) => Some("Old_Italic")
        (Java, 0) => Some("Java")
        (Java, 1) => Some("Javanese")
        (Kali, 0) => Some("Kali")
        (Kali, 1) => Some("Kayah_Li")
        (Kana, 0) => Some("Kana")
        (Kana, 1) => Some("Katakana")
        (Kawi, 0) => Some("Kawi")
        (Khar, 0) => Some("Khar")
        (Khar, 1) => Some("Kharoshthi")
        (Khmr, 0) => Some("Khmr")
        (Khmr, 1) => Some("Khmer")
        (Khoj, 0) => Some("Khoj")
        (Khoj, 1) => Some("Khojki")
        (Kits, 0) => Some("Kits")
        (Kits, 1) => Some("Khitan_Small_Script")
        (Knda, 0) => Some("Knda")
        (Knda, 1) => Some("Kannada")
        (Krai, 0) => Some("Krai")
        (Krai, 1) => Some("Kirat_Rai")
        (Kthi, 0) => Some("Kthi")
        (Kthi, 1) => Some("Kaithi")
        (Lana, 0) => Some("Lana")
        (Lana, 1) => Some("Tai_Tham")
        (Laoo, 0) => Some("Laoo")
        (Laoo, 1) => Some("Lao")
        (Latn, 0) => Some("Latn")
        (Latn, 1) => Some("Latin")
        (Lepc, 0) => Some("Lepc")
        (Lepc, 1) => Some("Lepcha")
        (Limb, 0) => Some("Limb")
        (Limb, 1) => Some("Limbu")
        (Lina, 0) => Some("Lina")
        (Lina, 1) => Some("Linear_A")
        (Linb, 0) => Some("Linb")
        (Linb, 1) => Some("Linear_B")
        (Lisu, 0) => Some("Lisu")
        (Lyci, 0) => Some("Lyci")
        (Lyci, 1) => Some("Lycian")
        (Lydi, 0) => Some("Lydi")
        (Lydi, 1) => Some("Lydian")
        (Mahj, 0) => Some("Mahj")
        (Mahj, 1) => Some("Mahajani")
        (Maka, 0) => Some("Maka")
        (Maka, 1) => Some("Makasar")
        (Mand, 0) => Some("Mand")
        (Mand, 1) => Some("Mandaic")
        (Mani, 0) => Some("Mani")
        (Mani, 1) => Some("Manichaean")
        (Marc, 0) => Some("Marc")
        (Marc, 1) => Some("Marchen")
        (Medf, 0) => Some("Medf")
        (Medf, 1) => Some("Medefaidrin")
        (Mend, 0) => Some("Mend")
        (Mend, 1) => Some("Mende_Kikakui")
        (Merc, 0) => Some("Merc")
        (Merc, 1) => Some("Meroitic_Cursive")
        (Mero, 0) => Some("Mero")
        (Mero, 1) => Some("Meroitic_Hieroglyphs")
        (Mlym, 0) => Some("Mlym")
        (Mlym, 1) => Some("Malayalam")
        (Modi, 0) => Some("Modi")
        (Mong, 0) => Some("Mong")
        (Mong, 1) => Some("Mongolian")
        (Mroo, 0) => Some("Mroo")
        (Mroo, 1) => Some("Mro")
        (Mtei, 0) => Some("Mtei")
        (Mtei, 1) => Some("Meetei_Mayek")
        (Mult, 0) => Some("Mult")
        (Mult, 1) => Some("Multani")
        (Mymr, 0) => Some("Mymr")
        (Mymr, 1) => Some("Myanmar")
        (Nagm, 0) => Some("Nagm")
        (Nagm, 1) => Some("Nag_Mundari")
        (Nand, 0) => Some("Nand")
        (Nand, 1) => Some("Nandinagari")
        (Narb, 0) => Some("Narb")
        (Narb, 1) => Some("Old_North_Arabian")
        (Nbat, 0) => Some("Nbat")
        (Nbat, 1) => Some("Nabataean")
        (Newa, 0) => Some("Newa")
        (Nkoo, 0) => Some("Nkoo")
        (Nkoo, 1) => Some("Nko")
        (Nshu, 0) => Some("Nshu")
        (Nshu, 1) => Some("Nushu")
        (Ogam, 0) => Some("Ogam")
        (Ogam, 1) => Some("Ogham")
        (Olck, 0) => Some("Olck")
        (Olck, 1) => Some("Ol_Chiki")
        (Onao, 0) => Some("Onao")
        (Onao, 1) => Some("Ol_Onal")
        (Orkh, 0) => Some("Orkh")
        (Orkh, 1) => Some("Old_Turkic")
        (Orya, 0) => Some("Orya")
        (Orya, 1) => Some("Oriya")
        (Osge, 0) => Some("Osge")
        (Osge, 1) => Some("Osage")
        (Osma, 0) => Some("Osma")
        (Osma, 1) => Some("Osmanya")
        (Ougr, 0) => Some("Ougr")
        (Ougr, 1) => Some("Old_Uyghur")
        (Palm, 0) => Some("Palm")
        (Palm, 1) => Some("Palmyrene")
        (Pauc, 0) => Some("Pauc")
        (Pauc, 1) => Some("Pau_Cin_Hau")
        (Perm, 0) => Some("Perm")
        (Perm, 1) => Some("Old_Permic")
        (Phag, 0) => Some("Phag")
        (Phag, 1) => Some("Phags_Pa")
        (Phli, 0) => Some("Phli")
        (Phli, 1) => Some("Inscriptional_Pahlavi")
        (Phlp, 0) => Some("Phlp")
        (Phlp, 1) => Some("Psalter_Pahlavi")
        (Phnx, 0) => Some("Phnx")
        (Phnx, 1) => Some("Phoenician")
        (Plrd, 0) => Some("Plrd")
        (Plrd, 1) => Some("Miao")
        (Prti, 0) => Some("Prti")
        (Prti, 1) => Some("Inscriptional_Parthian")
        (Rjng, 0) => Some("Rjng")
        (Rjng, 1) => Some("Rejang")
        (Rohg, 0) => Some("Rohg")
        (Rohg, 1) => Some("Hanifi_Rohingya")
        (Runr, 0) => Some("Runr")
        (Runr, 1) => Some("Runic")
        (Samr, 0) => Some("Samr")
        (Samr, 1) => Some("Samaritan")
        (Sarb, 0) => Some("Sarb")
        (Sarb, 1) => Some("Old_South_Arabian")
        (Saur, 0) => Some("Saur")
        (Saur, 1) => Some("Saurashtra")
        (Sgnw, 0) => Some("Sgnw")
        (Sgnw, 1) => Some("SignWriting")
        (Shaw, 0) => Some("Shaw")
        (Shaw, 1) => Some("Shavian")
        (Shrd, 0) => Some("Shrd")
        (Shrd, 1) => Some("Sharada")
        (Sidd, 0) => Some("Sidd")
        (Sidd, 1) => Some("Siddham")
        (Sidt, 0) => Some("Sidt")
        (Sidt, 1) => Some("Sidetic")
        (Sind, 0) => Some("Sind")
        (Sind, 1) => Some("Khudawadi")
        (Sinh, 0) => Some("Sinh")
        (Sinh, 1) => Some("Sinhala")
        (Sogd, 0) => Some("Sogd")
        (Sogd, 1) => Some("Sogdian")
        (Sogo, 0) => Some("Sogo")
        (Sogo, 1) => Some("Old_Sogdian")
        (Sora, 0) => Some("Sora")
        (Sora, 1) => Some("Sora_Sompeng")
        (Soyo, 0) => Some("Soyo")
        (Soyo, 1) => Some("Soyombo")
        (Sund, 0) => Some("Sund")
        (Sund, 1) => Some("Sundanese")
        (Sunu, 0) => Some("Sunu")
        (Sunu, 1) => Some("Sunuwar")
        (Sylo, 0) => Some("Sylo")
        (Sylo, 1) => Some("Syloti_Nagri")
        (Syrc, 0) => Some("Syrc")
        (Syrc, 1) => Some("Syriac")
        (Tagb, 0) => Some("Tagb")
        (Tagb, 1) => Some("Tagbanwa")
        (Takr, 0) => Some("Takr")
        (Takr, 1) => Some("Takri")
        (Tale, 0) => Some("Tale")
        (Tale, 1) => Some("Tai_Le")
        (Talu, 0) => Some("Talu")
        (Talu, 1) => Some("New_Tai_Lue")
        (Taml, 0) => Some("Taml")
        (Taml, 1) => Some("Tamil")
        (Tang, 0) => Some("Tang")
        (Tang, 1) => Some("Tangut")
        (Tavt, 0) => Some("Tavt")
        (Tavt, 1) => Some("Tai_Viet")
        (Tayo, 0) => Some("Tayo")
        (Tayo, 1) => Some("Tai_Yo")
        (Telu, 0) => Some("Telu")
        (Telu, 1) => Some("Telugu")
        (Tfng, 0) => Some("Tfng")
        (Tfng, 1) => Some("Tifinagh")
        (Tglg, 0) => Some("Tglg")
        (Tglg, 1) => Some("Tagalog")
        (Thaa, 0) => Some("Thaa")
        (Thaa, 1) => Some("Thaana")
        (Thai, 0) => Some("Thai")
        (Tibt, 0) => Some("Tibt")
        (Tibt, 1) => Some("Tibetan")
        (Tirh, 0) => Some("Tirh")
        (Tirh, 1) => Some("Tirhuta")
        (Tnsa, 0) => Some("Tnsa")
        (Tnsa, 1) => Some("Tangsa")
        (Todr, 0) => Some("Todr")
        (Todr, 1) => Some("Todhri")
        (Tols, 0) => Some("Tols")
        (Tols, 1) => Some("Tolong_Siki")
        (Toto, 0) => Some("Toto")
        (Tutg, 0) => Some("Tutg")
        (Tutg, 1) => Some("Tulu_Tigalari")
        (Ugar, 0) => Some("Ugar")
        (Ugar, 1) => Some("Ugaritic")
        (Vaii, 0) => Some("Vaii")
        (Vaii, 1) => Some("Vai")
        (Vith, 0) => Some("Vith")
        (Vith, 1) => Some("Vithkuqi")
        (Wara, 0) => Some("Wara")
        (Wara, 1) => Some("Warang_Citi")
        (Wcho, 0) => Some("Wcho")
        (Wcho, 1) => Some("Wancho")
        (Xpeo, 0) => Some("Xpeo")
        (Xpeo, 1) => Some("Old_Persian")
        (Xsux, 0) => Some("Xsux")
        (Xsux, 1) => Some("Cuneiform")
        (Yezi, 0) => Some("Yezi")
        (Yezi, 1) => Some("Yezidi")
        (Yiii, 0) => Some("Yiii")
        (Yiii, 1) => Some("Yi")
        (Zanb, 0) => Some("Zanb")
        (Zanb, 1) => Some("Zanabazar_Square")
        (Zinh, 0) => Some("Zinh")
        (Zinh, 1) => Some("Inherited")
        (Zinh, 2) => Some("Qaai")
        (Zyyy, 0) => Some("Zyyy")
        (Zyyy, 1) => Some("Common")
        (Zzzz, 0) => Some("Zzzz")
        (Zzzz, 1) => Some("Unknown")
        _ => None
        }
    }

    from_alias : Str -> [Some(Script), None]
    from_alias = |value| {
        match loose_hash(value) {
            4144741 => if loose_eq(value, 0x61686E6963756170, 0x0000000000000075, 0x0000000000000000, 9) Some(Pauc) else None
            46304872 => if loose_eq(value, 0x636974696F72656D, 0x0065766973727563, 0x0000000000000000, 15) Some(Merc) else None
            54176342 => if loose_eq(value, 0x616E616761726968, 0x0000000000000000, 0x0000000000000000, 8) Some(Hira) else None
            61028153 => if loose_eq(value, 0x0000006F79696174, 0x0000000000000000, 0x0000000000000000, 5) Some(Tayo) else None
            85395043 => if loose_eq(value, 0x0000000062676174, 0x0000000000000000, 0x0000000000000000, 4) Some(Tagb) else None
            108325841 => if loose_eq(value, 0x0000000077756C68, 0x0000000000000000, 0x0000000000000000, 4) Some(Hluw) else None
            115558953 => if loose_eq(value, 0x00000000626D696C, 0x0000000000000000, 0x0000000000000000, 4) Some(Limb) else None
            121335101 => if loose_eq(value, 0x65656B6F72656863, 0x0000000000000000, 0x0000000000000000, 8) Some(Cher) else None
            136295781 => if loose_eq(value, 0x000000006176616A, 0x0000000000000000, 0x0000000000000000, 4) Some(Java) else None
            155690797 => if loose_eq(value, 0x000000006F676F73, 0x0000000000000000, 0x0000000000000000, 4) Some(Sogo) else None
            162182025 => if loose_eq(value, 0x006E617361626C65, 0x0000000000000000, 0x0000000000000000, 7) Some(Elba) else None
            170710161 => if loose_eq(value, 0x00000000756C6174, 0x0000000000000000, 0x0000000000000000, 4) Some(Talu) else None
            177827169 => if loose_eq(value, 0x61646E756D67616E, 0x0000000000006972, 0x0000000000000000, 10) Some(Nagm) else None
            187825022 => if loose_eq(value, 0x6971756B68746976, 0x0000000000000000, 0x0000000000000000, 8) Some(Vith) else None
            200585399 => if loose_eq(value, 0x6F726966696E6168, 0x00006179676E6968, 0x0000000000000000, 14) Some(Rohg) else None
            201021166 => if loose_eq(value, 0x000000007573696C, 0x0000000000000000, 0x0000000000000000, 4) Some(Lisu) else None
            254745351 => if loose_eq(value, 0x000000006C6D6174, 0x0000000000000000, 0x0000000000000000, 4) Some(Taml) else None
            269689434 => if loose_eq(value, 0x6973726570646C6F, 0x0000000000006E61, 0x0000000000000000, 10) Some(Xpeo) else None
            279476947 => if loose_eq(value, 0x00000000006F6B6E, 0x0000000000000000, 0x0000000000000000, 3) Some(Nkoo) else None
            282220607 => if loose_eq(value, 0x00000000686E6973, 0x0000000000000000, 0x0000000000000000, 4) Some(Sinh) else None
            300524632 => if loose_eq(value, 0x6F666F6D6F706F62, 0x0000000000000000, 0x0000000000000000, 8) Some(Bopo) else None
            333123547 => if loose_eq(value, 0x00000000616E696C, 0x0000000000000000, 0x0000000000000000, 4) Some(Lina) else None
            334951179 => if loose_eq(value, 0x706D6F7361726F73, 0x0000000000676E65, 0x0000000000000000, 11) Some(Sora) else None
            336757657 => if loose_eq(value, 0x0074656976696174, 0x0000000000000000, 0x0000000000000000, 7) Some(Tavt) else None
            349331083 => if loose_eq(value, 0x00000000646E6973, 0x0000000000000000, 0x0000000000000000, 4) Some(Sind) else None
            349901166 => if loose_eq(value, 0x00000000626E696C, 0x0000000000000000, 0x0000000000000000, 4) Some(Linb) else None
            388753082 => if loose_eq(value, 0x0000000061726F73, 0x0000000000000000, 0x0000000000000000, 4) Some(Sora) else None
            389541541 => if loose_eq(value, 0x00000000736B6862, 0x0000000000000000, 0x0000000000000000, 4) Some(Bhks) else None
            392338966 => if loose_eq(value, 0x000000006D6E6F67, 0x0000000000000000, 0x0000000000000000, 4) Some(Gonm) else None
            401753765 => if loose_eq(value, 0x00006E616963796C, 0x0000000000000000, 0x0000000000000000, 6) Some(Lyci) else None
            405302637 => if loose_eq(value, 0x00000000676E6174, 0x0000000000000000, 0x0000000000000000, 4) Some(Tang) else None
            409191545 => if loose_eq(value, 0x6964676F73646C6F, 0x0000000000006E61, 0x0000000000000000, 10) Some(Sogo) else None
            414867441 => if loose_eq(value, 0x0000000064646973, 0x0000000000000000, 0x0000000000000000, 4) Some(Sidd) else None
            422767528 => if loose_eq(value, 0x00000000726E7572, 0x0000000000000000, 0x0000000000000000, 4) Some(Runr) else None
            426609079 => if loose_eq(value, 0x000000006370656C, 0x0000000000000000, 0x0000000000000000, 4) Some(Lepc) else None
            432601459 => if loose_eq(value, 0x00617261656E696C, 0x0000000000000000, 0x0000000000000000, 7) Some(Lina) else None
            439152065 => if loose_eq(value, 0x00000000656C6174, 0x0000000000000000, 0x0000000000000000, 4) Some(Tale) else None
            449379078 => if loose_eq(value, 0x00627261656E696C, 0x0000000000000000, 0x0000000000000000, 7) Some(Linb) else None
            464033790 => if loose_eq(value, 0x00006E616964796C, 0x0000000000000000, 0x0000000000000000, 6) Some(Lydi) else None
            493004680 => if loose_eq(value, 0x00000000676E6F67, 0x0000000000000000, 0x0000000000000000, 4) Some(Gong) else None
            494144966 => if loose_eq(value, 0x69746172616A7567, 0x0000000000000000, 0x0000000000000000, 8) Some(Gujr) else None
            539833567 => if loose_eq(value, 0x00000000646E7573, 0x0000000000000000, 0x0000000000000000, 4) Some(Sund) else None
            554258946 => if loose_eq(value, 0x0063697465646973, 0x0000000000000000, 0x0000000000000000, 7) Some(Sidt) else None
            558270595 => if loose_eq(value, 0x707265746C617370, 0x00006976616C6861, 0x0000000000000000, 14) Some(Phlp) else None
            575169303 => if loose_eq(value, 0x61676E7568646C6F, 0x000000006E616972, 0x0000000000000000, 12) Some(Hung) else None
            589089039 => if loose_eq(value, 0x00006E6169726163, 0x0000000000000000, 0x0000000000000000, 6) Some(Cari) else None
            599604969 => if loose_eq(value, 0x646961666564656D, 0x00000000006E6972, 0x0000000000000000, 11) Some(Medf) else None
            614738253 => if loose_eq(value, 0x00006964697A6579, 0x0000000000000000, 0x0000000000000000, 6) Some(Yezi) else None
            633481180 => if loose_eq(value, 0x00696B6968636C6F, 0x0000000000000000, 0x0000000000000000, 7) Some(Olck) else None
            643387971 => if loose_eq(value, 0x0061796E616D736F, 0x0000000000000000, 0x0000000000000000, 7) Some(Osma) else None
            650713443 => if loose_eq(value, 0x00726173616B616D, 0x0000000000000000, 0x0000000000000000, 7) Some(Maka) else None
            683309345 => if loose_eq(value, 0x0000000074646973, 0x0000000000000000, 0x0000000000000000, 4) Some(Sidt) else None
            683475358 => if loose_eq(value, 0x0000000069696176, 0x0000000000000000, 0x0000000000000000, 4) Some(Vaii) else None
            716440097 => if loose_eq(value, 0x00007567756C6574, 0x0000000000000000, 0x0000000000000000, 6) Some(Telu) else None
            727371646 => if loose_eq(value, 0x6672656169726562, 0x0000000000000065, 0x0000000000000000, 9) Some(Berf) else None
            733759557 => if loose_eq(value, 0x686B676E75727567, 0x0000000000616D65, 0x0000000000000000, 11) Some(Gukh) else None
            743784431 => if loose_eq(value, 0x006E616976616873, 0x0000000000000000, 0x0000000000000000, 7) Some(Shaw) else None
            761769329 => if loose_eq(value, 0x0074657265736564, 0x0000000000000000, 0x0000000000000000, 7) Some(Dsrt) else None
            772402863 => if loose_eq(value, 0x0000000067686F72, 0x0000000000000000, 0x0000000000000000, 4) Some(Rohg) else None
            778070020 => if loose_eq(value, 0x000061686370656C, 0x0000000000000000, 0x0000000000000000, 6) Some(Lepc) else None
            785306538 => if loose_eq(value, 0x00000000696C616B, 0x0000000000000000, 0x0000000000000000, 4) Some(Kali) else None
            791497852 => if loose_eq(value, 0x00000000756E7573, 0x0000000000000000, 0x0000000000000000, 4) Some(Sunu) else None
            812410495 => if loose_eq(value, 0x61696C6F676E6F6D, 0x000000000000006E, 0x0000000000000000, 9) Some(Mong) else None
            814708684 => if loose_eq(value, 0x00000000676E6674, 0x0000000000000000, 0x0000000000000000, 4) Some(Tfng) else None
            816922767 => if loose_eq(value, 0x0000000069687465, 0x0000000000000000, 0x0000000000000000, 4) Some(Ethi) else None
            831458987 => if loose_eq(value, 0x00000000696C6162, 0x0000000000000000, 0x0000000000000000, 4) Some(Bali) else None
            836913313 => if loose_eq(value, 0x0000000072676F64, 0x0000000000000000, 0x0000000000000000, 4) Some(Dogr) else None
            852711204 => if loose_eq(value, 0x00000000616E616B, 0x0000000000000000, 0x0000000000000000, 4) Some(Kana) else None
            868957177 => if loose_eq(value, 0x000000006B746162, 0x0000000000000000, 0x0000000000000000, 4) Some(Batk) else None
            869142228 => if loose_eq(value, 0x00000075626D696C, 0x0000000000000000, 0x0000000000000000, 5) Some(Limb) else None
            890490207 => if loose_eq(value, 0x0000007961726167, 0x0000000000000000, 0x0000000000000000, 5) Some(Gara) else None
            895028278 => if loose_eq(value, 0x0000000072646F74, 0x0000000000000000, 0x0000000000000000, 4) Some(Todr) else None
            895377948 => if loose_eq(value, 0x00676F6C61676174, 0x0000000000000000, 0x0000000000000000, 7) Some(Tglg) else None
            912430666 => if loose_eq(value, 0x0000000072626568, 0x0000000000000000, 0x0000000000000000, 4) Some(Hebr) else None
            913216764 => if loose_eq(value, 0x0000000061726167, 0x0000000000000000, 0x0000000000000000, 4) Some(Gara) else None
            913709578 => if loose_eq(value, 0x0000676E616A6572, 0x0000000000000000, 0x0000000000000000, 6) Some(Rjng) else None
            1017124202 => if loose_eq(value, 0x00000000686B7567, 0x0000000000000000, 0x0000000000000000, 4) Some(Gukh) else None
            1020549158 => if loose_eq(value, 0x0000000074727364, 0x0000000000000000, 0x0000000000000000, 4) Some(Dsrt) else None
            1037991868 => if loose_eq(value, 0x0000000075727567, 0x0000000000000000, 0x0000000000000000, 4) Some(Guru) else None
            1056163635 => if loose_eq(value, 0x6874726F6E646C6F, 0x006E616962617261, 0x0000000000000000, 15) Some(Narb) else None
            1070033645 => if loose_eq(value, 0x000000006F657078, 0x0000000000000000, 0x0000000000000000, 4) Some(Xpeo) else None
            1070375155 => if loose_eq(value, 0x0000636968746F67, 0x0000000000000000, 0x0000000000000000, 6) Some(Goth) else None
            1082761344 => if loose_eq(value, 0x74696C6F67616C67, 0x0000000000006369, 0x0000000000000000, 10) Some(Glag) else None
            1100047986 => if loose_eq(value, 0x00000000756D6162, 0x0000000000000000, 0x0000000000000000, 4) Some(Bamu) else None
            1111130598 => if loose_eq(value, 0x6E696D6F72707963, 0x00000000006E616F, 0x0000000000000000, 11) Some(Cpmn) else None
            1130201130 => if loose_eq(value, 0x00000072656D686B, 0x0000000000000000, 0x0000000000000000, 5) Some(Khmr) else None
            1181479088 => if loose_eq(value, 0x7468736F7261686B, 0x0000000000006968, 0x0000000000000000, 10) Some(Khar) else None
            1181527729 => if loose_eq(value, 0x00000000736C6F74, 0x0000000000000000, 0x0000000000000000, 4) Some(Tols) else None
            1184173537 => if loose_eq(value, 0x000000006977616B, 0x0000000000000000, 0x0000000000000000, 4) Some(Kawi) else None
            1194886720 => if loose_eq(value, 0x0000006172676F64, 0x0000000000000000, 0x0000000000000000, 5) Some(Dogr) else None
            1228996495 => if loose_eq(value, 0x000000006C727963, 0x0000000000000000, 0x0000000000000000, 4) Some(Cyrl) else None
            1252734524 => if loose_eq(value, 0x616C6179616C616D, 0x000000000000006D, 0x0000000000000000, 9) Some(Mlym) else None
            1255613789 => if loose_eq(value, 0x6874756F73646C6F, 0x006E616962617261, 0x0000000000000000, 15) Some(Sarb) else None
            1255784980 => if loose_eq(value, 0x00000000626E617A, 0x0000000000000000, 0x0000000000000000, 4) Some(Zanb) else None
            1257381273 => if loose_eq(value, 0x0061707367616870, 0x0000000000000000, 0x0000000000000000, 7) Some(Phag) else None
            1259539206 => if loose_eq(value, 0x6573656E696C6162, 0x0000000000000000, 0x0000000000000000, 8) Some(Bali) else None
            1276859404 => if loose_eq(value, 0x0000006C696D6174, 0x0000000000000000, 0x0000000000000000, 5) Some(Taml) else None
            1285713201 => if loose_eq(value, 0x00000000726A7567, 0x0000000000000000, 0x0000000000000000, 4) Some(Gujr) else None
            1297810548 => if loose_eq(value, 0x00006E6F6D6D6F63, 0x0000000000000000, 0x0000000000000000, 6) Some(Zyyy) else None
            1309392770 => if loose_eq(value, 0x00000069726B6174, 0x0000000000000000, 0x0000000000000000, 5) Some(Takr) else None
            1332165440 => if loose_eq(value, 0x696D726570646C6F, 0x0000000000000063, 0x0000000000000000, 9) Some(Perm) else None
            1333797141 => if loose_eq(value, 0x726F6669656E7563, 0x000000000000006D, 0x0000000000000000, 9) Some(Xsux) else None
            1347882587 => if loose_eq(value, 0x0000000061646E6B, 0x0000000000000000, 0x0000000000000000, 4) Some(Knda) else None
            1355240930 => if loose_eq(value, 0x0000616E61616874, 0x0000000000000000, 0x0000000000000000, 6) Some(Thaa) else None
            1379502483 => if loose_eq(value, 0x000000006A6F686B, 0x0000000000000000, 0x0000000000000000, 4) Some(Khoj) else None
            1384139085 => if loose_eq(value, 0x000000006F746F74, 0x0000000000000000, 0x0000000000000000, 4) Some(Toto) else None
            1401959279 => if loose_eq(value, 0x7470697263736E69, 0x6861706C616E6F69, 0x000000006976616C, 20) Some(Phli) else None
            1405351331 => if loose_eq(value, 0x000000006D6C6461, 0x0000000000000000, 0x0000000000000000, 4) Some(Adlm) else None
            1406973818 => if loose_eq(value, 0x0000000073736162, 0x0000000000000000, 0x0000000000000000, 4) Some(Bass) else None
            1410503101 => if loose_eq(value, 0x00000000616D736F, 0x0000000000000000, 0x0000000000000000, 4) Some(Osma) else None
            1443261824 => if loose_eq(value, 0x6D736E617469686B, 0x70697263736C6C61, 0x0000000000000074, 17) Some(Kits) else None
            1457767117 => if loose_eq(value, 0x0000006D756D6162, 0x0000000000000000, 0x0000000000000000, 5) Some(Bamu) else None
            1465942435 => if loose_eq(value, 0x00000000006F616C, 0x0000000000000000, 0x0000000000000000, 3) Some(Laoo) else None
            1479232228 => if loose_eq(value, 0x0000000068726974, 0x0000000000000000, 0x0000000000000000, 4) Some(Tirh) else None
            1479591684 => if loose_eq(value, 0x0000000064687562, 0x0000000000000000, 0x0000000000000000, 4) Some(Buhd) else None
            1487538459 => if loose_eq(value, 0x0000000061726968, 0x0000000000000000, 0x0000000000000000, 4) Some(Hira) else None
            1491883044 => if loose_eq(value, 0x000000006F6F616C, 0x0000000000000000, 0x0000000000000000, 4) Some(Laoo) else None
            1506267728 => if loose_eq(value, 0x000000006177656E, 0x0000000000000000, 0x0000000000000000, 4) Some(Newa) else None
            1514120793 => if loose_eq(value, 0x00000000726D686B, 0x0000000000000000, 0x0000000000000000, 4) Some(Khmr) else None
            1521369461 => if loose_eq(value, 0x0000006469687562, 0x0000000000000000, 0x0000000000000000, 5) Some(Buhd) else None
            1543046767 => if loose_eq(value, 0x000000006567736F, 0x0000000000000000, 0x0000000000000000, 4) Some(Osge) else None
            1545663074 => if loose_eq(value, 0x6973676E6F6C6F74, 0x000000000000696B, 0x0000000000000000, 10) Some(Tols) else None
            1549005728 => if loose_eq(value, 0x0000000074626974, 0x0000000000000000, 0x0000000000000000, 4) Some(Tibt) else None
            1554663930 => if loose_eq(value, 0x00000000676E6A72, 0x0000000000000000, 0x0000000000000000, 4) Some(Rjng) else None
            1578791434 => if loose_eq(value, 0x617A6162616E617A, 0x0065726175717372, 0x0000000000000000, 15) Some(Zanb) else None
            1585476329 => if loose_eq(value, 0x6E61796F6C707564, 0x0000000000000000, 0x0000000000000000, 8) Some(Dupl) else None
            1627127737 => if loose_eq(value, 0x000000006D61676F, 0x0000000000000000, 0x0000000000000000, 4) Some(Ogam) else None
            1627632668 => if loose_eq(value, 0x6963676E61726177, 0x0000000000006974, 0x0000000000000000, 10) Some(Wara) else None
            1632270350 => if loose_eq(value, 0x000000006965746D, 0x0000000000000000, 0x0000000000000000, 4) Some(Mtei) else None
            1647650197 => if loose_eq(value, 0x000000007261686B, 0x0000000000000000, 0x0000000000000000, 4) Some(Khar) else None
            1662960664 => if loose_eq(value, 0x0072616D6E61796D, 0x0000000000000000, 0x0000000000000000, 7) Some(Mymr) else None
            1683697517 => if loose_eq(value, 0x000000006968746B, 0x0000000000000000, 0x0000000000000000, 4) Some(Kthi) else None
            1683996967 => if loose_eq(value, 0x0000000000006979, 0x0000000000000000, 0x0000000000000000, 2) Some(Yiii) else None
            1685029195 => if loose_eq(value, 0x000000006F61696D, 0x0000000000000000, 0x0000000000000000, 4) Some(Plrd) else None
            1687280138 => if loose_eq(value, 0x6B75736B69616862, 0x0000000000000069, 0x0000000000000000, 9) Some(Bhks) else None
            1692682322 => if loose_eq(value, 0x61696C6F74616E61, 0x6C676F726569686E, 0x0000000073687079, 20) Some(Hluw) else None
            1698554050 => if loose_eq(value, 0x000000006B616964, 0x0000000000000000, 0x0000000000000000, 4) Some(Diak) else None
            1703858441 => if loose_eq(value, 0x0000636962617261, 0x0000000000000000, 0x0000000000000000, 6) Some(Arab) else None
            1716276825 => if loose_eq(value, 0x00696C61676E6562, 0x0000000000000000, 0x0000000000000000, 7) Some(Beng) else None
            1756251884 => if loose_eq(value, 0x000000006B636C6F, 0x0000000000000000, 0x0000000000000000, 4) Some(Olck) else None
            1784323999 => if loose_eq(value, 0x0000000067747574, 0x0000000000000000, 0x0000000000000000, 4) Some(Tutg) else None
            1784330750 => if loose_eq(value, 0x0000000067616C67, 0x0000000000000000, 0x0000000000000000, 4) Some(Glag) else None
            1788969220 => if loose_eq(value, 0x000000006F6F726D, 0x0000000000000000, 0x0000000000000000, 4) Some(Mroo) else None
            1793733091 => if loose_eq(value, 0x00000000616E616C, 0x0000000000000000, 0x0000000000000000, 4) Some(Lana) else None
            1795320016 => if loose_eq(value, 0x00000000726F6567, 0x0000000000000000, 0x0000000000000000, 4) Some(Geor) else None
            1796259334 => if loose_eq(value, 0x00000000696C6870, 0x0000000000000000, 0x0000000000000000, 4) Some(Phli) else None
            1812579512 => if loose_eq(value, 0x0000000074727063, 0x0000000000000000, 0x0000000000000000, 4) Some(Cprt) else None
            1813013215 => if loose_eq(value, 0x006D616874696174, 0x0000000000000000, 0x0000000000000000, 7) Some(Lana) else None
            1835925744 => if loose_eq(value, 0x000000007267756F, 0x0000000000000000, 0x0000000000000000, 4) Some(Ougr) else None
            1838148016 => if loose_eq(value, 0x0000636169727973, 0x0000000000000000, 0x0000000000000000, 6) Some(Syrc) else None
            1848662113 => if loose_eq(value, 0x0000006E6974616C, 0x0000000000000000, 0x0000000000000000, 5) Some(Latn) else None
            1852369084 => if loose_eq(value, 0x0000000068617262, 0x0000000000000000, 0x0000000000000000, 4) Some(Brah) else None
            1869146703 => if loose_eq(value, 0x0000000069617262, 0x0000000000000000, 0x0000000000000000, 4) Some(Brai) else None
            1872097689 => if loose_eq(value, 0x006D616864646973, 0x0000000000000000, 0x0000000000000000, 7) Some(Sidd) else None
            1910898809 => if loose_eq(value, 0x00616C61686E6973, 0x0000000000000000, 0x0000000000000000, 7) Some(Sinh) else None
            1936813044 => if loose_eq(value, 0x6C61697265706D69, 0x006369616D617261, 0x0000000000000000, 15) Some(Armi) else None
            1938387210 => if loose_eq(value, 0x0000000063756170, 0x0000000000000000, 0x0000000000000000, 4) Some(Pauc) else None
            1945940254 => if loose_eq(value, 0x000000756873756E, 0x0000000000000000, 0x0000000000000000, 5) Some(Nshu) else None
            1963196881 => if loose_eq(value, 0x0000000067616870, 0x0000000000000000, 0x0000000000000000, 4) Some(Phag) else None
            1965892372 => if loose_eq(value, 0x0000000069677562, 0x0000000000000000, 0x0000000000000000, 4) Some(Bugi) else None
            1973421709 => if loose_eq(value, 0x00000000676E7568, 0x0000000000000000, 0x0000000000000000, 4) Some(Hung) else None
            2010856400 => if loose_eq(value, 0x000000006E74616C, 0x0000000000000000, 0x0000000000000000, 4) Some(Latn) else None
            2012380749 => if loose_eq(value, 0x000000006E6D7063, 0x0000000000000000, 0x0000000000000000, 4) Some(Cpmn) else None
            2014662571 => if loose_eq(value, 0x00000000786E6870, 0x0000000000000000, 0x0000000000000000, 4) Some(Phnx) else None
            2044408069 => if loose_eq(value, 0x00000000676E6D68, 0x0000000000000000, 0x0000000000000000, 4) Some(Hmng) else None
            2047952907 => if loose_eq(value, 0x676E656B6169796E, 0x6865756863617570, 0x00000000676E6F6D, 20) Some(Hmnp) else None
            2069298552 => if loose_eq(value, 0x00006C75676E6168, 0x0000000000000000, 0x0000000000000000, 6) Some(Hang) else None
            2113560556 => if loose_eq(value, 0x0000000061726177, 0x0000000000000000, 0x0000000000000000, 4) Some(Wara) else None
            2119320795 => if loose_eq(value, 0x69686B756D727567, 0x0000000000000000, 0x0000000000000000, 8) Some(Guru) else None
            2143561314 => if loose_eq(value, 0x6E61697470796765, 0x796C676F72656968, 0x0000000000736870, 19) Some(Egyp) else None
            2147666272 => if loose_eq(value, 0x000000006F72656D, 0x0000000000000000, 0x0000000000000000, 4) Some(Mero) else None
            2154334766 => if loose_eq(value, 0x000000656761736F, 0x0000000000000000, 0x0000000000000000, 5) Some(Osge) else None
            2195406640 => if loose_eq(value, 0x00000000706E6D68, 0x0000000000000000, 0x0000000000000000, 4) Some(Hmnp) else None
            2215699809 => if loose_eq(value, 0x00000000706C6870, 0x0000000000000000, 0x0000000000000000, 4) Some(Phlp) else None
            2228091033 => if loose_eq(value, 0x646177616475686B, 0x0000000000000069, 0x0000000000000000, 9) Some(Sind) else None
            2248541657 => if loose_eq(value, 0x0000000062617261, 0x0000000000000000, 0x0000000000000000, 4) Some(Arab) else None
            2256405423 => if loose_eq(value, 0x0000006D6168676F, 0x0000000000000000, 0x0000000000000000, 5) Some(Ogam) else None
            2258483678 => if loose_eq(value, 0x0000696D68617262, 0x0000000000000000, 0x0000000000000000, 6) Some(Brah) else None
            2269752869 => if loose_eq(value, 0x00746F6972707963, 0x0000000000000000, 0x0000000000000000, 7) Some(Cprt) else None
            2284120156 => if loose_eq(value, 0x00000000776E6773, 0x0000000000000000, 0x0000000000000000, 4) Some(Sgnw) else None
            2300597625 => if loose_eq(value, 0x616E616B6174616B, 0x616761726968726F, 0x000000000000616E, 18) Some(Hrkt) else None
            2303507949 => if loose_eq(value, 0x000000006D6C6170, 0x0000000000000000, 0x0000000000000000, 4) Some(Palm) else None
            2327976948 => if loose_eq(value, 0x00007475676E6174, 0x0000000000000000, 0x0000000000000000, 6) Some(Tang) else None
            2328876209 => if loose_eq(value, 0x6169736163756163, 0x61696E61626C616E, 0x000000000000006E, 17) Some(Aghb) else None
            2330248605 => if loose_eq(value, 0x00000000646E656D, 0x0000000000000000, 0x0000000000000000, 4) Some(Mend) else None
            2334613016 => if loose_eq(value, 0x0000000069726163, 0x0000000000000000, 0x0000000000000000, 4) Some(Cari) else None
            2340694564 => if loose_eq(value, 0x000000006F616E6F, 0x0000000000000000, 0x0000000000000000, 4) Some(Onao) else None
            2348997700 => if loose_eq(value, 0x000000006372656D, 0x0000000000000000, 0x0000000000000000, 4) Some(Merc) else None
            2358739092 => if loose_eq(value, 0x61776E6162676174, 0x0000000000000000, 0x0000000000000000, 8) Some(Tagb) else None
            2361377338 => if loose_eq(value, 0x696C617469646C6F, 0x0000000000000063, 0x0000000000000000, 9) Some(Ital) else None
            2373100727 => if loose_eq(value, 0x00000000676E6562, 0x0000000000000000, 0x0000000000000000, 4) Some(Beng) else None
            2373974075 => if loose_eq(value, 0x6963696E656F6870, 0x0000000000006E61, 0x0000000000000000, 10) Some(Phnx) else None
            2381765004 => if loose_eq(value, 0x000000006272616E, 0x0000000000000000, 0x0000000000000000, 4) Some(Narb) else None
            2416481554 => if loose_eq(value, 0x00006C616E6F6C6F, 0x0000000000000000, 0x0000000000000000, 6) Some(Onao) else None
            2420302585 => if loose_eq(value, 0x0000000072656863, 0x0000000000000000, 0x0000000000000000, 4) Some(Cher) else None
            2433436669 => if loose_eq(value, 0x67616E69646E616E, 0x0000000000697261, 0x0000000000000000, 11) Some(Nand) else None
            2434906768 => if loose_eq(value, 0x63696C6C69727963, 0x0000000000000000, 0x0000000000000000, 8) Some(Cyrl) else None
            2435183299 => if loose_eq(value, 0x0000000000696176, 0x0000000000000000, 0x0000000000000000, 3) Some(Vaii) else None
            2437286623 => if loose_eq(value, 0x00006F68636E6177, 0x0000000000000000, 0x0000000000000000, 6) Some(Wcho) else None
            2451800716 => if loose_eq(value, 0x0000000070796765, 0x0000000000000000, 0x0000000000000000, 4) Some(Egyp) else None
            2464847089 => if loose_eq(value, 0x000000006D6B6163, 0x0000000000000000, 0x0000000000000000, 4) Some(Cakm) else None
            2473941097 => if loose_eq(value, 0x006F626D6F796F73, 0x0000000000000000, 0x0000000000000000, 7) Some(Soyo) else None
            2515896206 => if loose_eq(value, 0x00000063696E7572, 0x0000000000000000, 0x0000000000000000, 5) Some(Runr) else None
            2520447072 => if loose_eq(value, 0x0000000064726873, 0x0000000000000000, 0x0000000000000000, 4) Some(Shrd) else None
            2532982981 => if loose_eq(value, 0x61746972616D6173, 0x000000000000006E, 0x0000000000000000, 9) Some(Samr) else None
            2540452869 => if loose_eq(value, 0x0000000073726863, 0x0000000000000000, 0x0000000000000000, 4) Some(Chrs) else None
            2551050114 => if loose_eq(value, 0x6E61696E656D7261, 0x0000000000000000, 0x0000000000000000, 8) Some(Armn) else None
            2557386105 => if loose_eq(value, 0x6573656E69677562, 0x0000000000000000, 0x0000000000000000, 8) Some(Bugi) else None
            2563324286 => if loose_eq(value, 0x00656C6C69617262, 0x0000000000000000, 0x0000000000000000, 7) Some(Brai) else None
            2608177081 => if loose_eq(value, 0x006E776F6E6B6E75, 0x0000000000000000, 0x0000000000000000, 7) Some(Zzzz) else None
            2614182209 => if loose_eq(value, 0x00006173676E6174, 0x0000000000000000, 0x0000000000000000, 6) Some(Tnsa) else None
            2625353392 => if loose_eq(value, 0x0000000066726562, 0x0000000000000000, 0x0000000000000000, 4) Some(Berf) else None
            2626721964 => if loose_eq(value, 0x61676974756C7574, 0x000000006972616C, 0x0000000000000000, 12) Some(Tutg) else None
            2630387858 => if loose_eq(value, 0x000000007979797A, 0x0000000000000000, 0x0000000000000000, 4) Some(Zyyy) else None
            2633358754 => if loose_eq(value, 0x00000000736E6163, 0x0000000000000000, 0x0000000000000000, 4) Some(Cans) else None
            2687346717 => if loose_eq(value, 0x000000006C617469, 0x0000000000000000, 0x0000000000000000, 4) Some(Ital) else None
            2692042822 => if loose_eq(value, 0x67616C616A6E7567, 0x0000000069646E6F, 0x0000000000000000, 12) Some(Gong) else None
            2702205278 => if loose_eq(value, 0x696D7361726F6863, 0x0000000000006E61, 0x0000000000000000, 10) Some(Chrs) else None
            2707030620 => if loose_eq(value, 0x000000006D6F6861, 0x0000000000000000, 0x0000000000000000, 4) Some(Ahom) else None
            2717401106 => if loose_eq(value, 0x756B617365766964, 0x0000000000007572, 0x0000000000000000, 10) Some(Diak) else None
            2722702483 => if loose_eq(value, 0x0000696B6A6F686B, 0x0000000000000000, 0x0000000000000000, 6) Some(Khoj) else None
            2731715163 => if loose_eq(value, 0x0000000062686761, 0x0000000000000000, 0x0000000000000000, 4) Some(Aghb) else None
            2745543540 => if loose_eq(value, 0x746972776E676973, 0x0000000000676E69, 0x0000000000000000, 11) Some(Sgnw) else None
            2756609597 => if loose_eq(value, 0x7470697263736E69, 0x7261706C616E6F69, 0x0000006E61696874, 21) Some(Prti) else None
            2778370699 => if loose_eq(value, 0x000000006E617267, 0x0000000000000000, 0x0000000000000000, 4) Some(Gran) else None
            2801327783 => if loose_eq(value, 0x000069687469616B, 0x0000000000000000, 0x0000000000000000, 6) Some(Kthi) else None
            2828346456 => if loose_eq(value, 0x00000000746B7268, 0x0000000000000000, 0x0000000000000000, 4) Some(Hrkt) else None
            2832591437 => if loose_eq(value, 0x000000006664656D, 0x0000000000000000, 0x0000000000000000, 4) Some(Medf) else None
            2837135895 => if loose_eq(value, 0x0000000061736E74, 0x0000000000000000, 0x0000000000000000, 4) Some(Tnsa) else None
            2840331440 => if loose_eq(value, 0x000000006D616863, 0x0000000000000000, 0x0000000000000000, 4) Some(Cham) else None
            2848868094 => if loose_eq(value, 0x000000006D796C65, 0x0000000000000000, 0x0000000000000000, 4) Some(Elym) else None
            2858308472 => if loose_eq(value, 0x00000000726D796D, 0x0000000000000000, 0x0000000000000000, 4) Some(Mymr) else None
            2866367806 => if loose_eq(value, 0x000000006F686377, 0x0000000000000000, 0x0000000000000000, 4) Some(Wcho) else None
            2878998622 => if loose_eq(value, 0x0000006D616C6461, 0x0000000000000000, 0x0000000000000000, 5) Some(Adlm) else None
            2885651355 => if loose_eq(value, 0x616E616B6174616B, 0x0000000000000000, 0x0000000000000000, 8) Some(Kana) else None
            2897391516 => if loose_eq(value, 0x7568677975646C6F, 0x0000000000000072, 0x0000000000000000, 9) Some(Ougr) else None
            2926219974 => if loose_eq(value, 0x000000006961726B, 0x0000000000000000, 0x0000000000000000, 4) Some(Krai) else None
            2939512545 => if loose_eq(value, 0x6574697265686E69, 0x0000000000000064, 0x0000000000000000, 9) Some(Zinh) else None
            2968531819 => if loose_eq(value, 0x000000006D726570, 0x0000000000000000, 0x0000000000000000, 4) Some(Perm) else None
            2972380168 => if loose_eq(value, 0x000000006D67616E, 0x0000000000000000, 0x0000000000000000, 4) Some(Nagm) else None
            2985859074 => if loose_eq(value, 0x0000000072756173, 0x0000000000000000, 0x0000000000000000, 4) Some(Saur) else None
            2985969608 => if loose_eq(value, 0x6573656E6176616A, 0x0000000000000000, 0x0000000000000000, 8) Some(Java) else None
            2986065345 => if loose_eq(value, 0x000000006E6D7261, 0x0000000000000000, 0x0000000000000000, 4) Some(Armn) else None
            2991256162 => if loose_eq(value, 0x616561746162616E, 0x000000000000006E, 0x0000000000000000, 9) Some(Nbat) else None
            3002842964 => if loose_eq(value, 0x00000000696D7261, 0x0000000000000000, 0x0000000000000000, 4) Some(Armi) else None
            3030113015 => if loose_eq(value, 0x00006E6172746168, 0x0000000000000000, 0x0000000000000000, 6) Some(Hatr) else None
            3035854053 => if loose_eq(value, 0x6867616E69666974, 0x0000000000000000, 0x0000000000000000, 8) Some(Tfng) else None
            3043353695 => if loose_eq(value, 0x6B696B65646E656D, 0x0000000069756B61, 0x0000000000000000, 12) Some(Mend) else None
            3074594991 => if loose_eq(value, 0x00000000746C756D, 0x0000000000000000, 0x0000000000000000, 4) Some(Mult) else None
            3083808838 => if loose_eq(value, 0x616E69746F6C7973, 0x0000000000697267, 0x0000000000000000, 11) Some(Sylo) else None
            3094424342 => if loose_eq(value, 0x006E617465626974, 0x0000000000000000, 0x0000000000000000, 7) Some(Tibt) else None
            3096451437 => if loose_eq(value, 0x000000007568736E, 0x0000000000000000, 0x0000000000000000, 4) Some(Nshu) else None
            3104444647 => if loose_eq(value, 0x0000000061766564, 0x0000000000000000, 0x0000000000000000, 4) Some(Deva) else None
            3110768046 => if loose_eq(value, 0x0000000077616873, 0x0000000000000000, 0x0000000000000000, 4) Some(Shaw) else None
            3143658460 => if loose_eq(value, 0x6861766173736162, 0x0000000000000000, 0x0000000000000000, 8) Some(Bass) else None
            3153488169 => if loose_eq(value, 0x0000000062726173, 0x0000000000000000, 0x0000000000000000, 4) Some(Sarb) else None
            3158066810 => if loose_eq(value, 0x00000000646E616E, 0x0000000000000000, 0x0000000000000000, 4) Some(Nand) else None
            3165670261 => if loose_eq(value, 0x0000697268646F74, 0x0000000000000000, 0x0000000000000000, 6) Some(Todr) else None
            3166609459 => if loose_eq(value, 0x006E65686372616D, 0x0000000000000000, 0x0000000000000000, 7) Some(Marc) else None
            3170973593 => if loose_eq(value, 0x0000000069696979, 0x0000000000000000, 0x0000000000000000, 4) Some(Yiii) else None
            3180624617 => if loose_eq(value, 0x0000000061626C65, 0x0000000000000000, 0x0000000000000000, 4) Some(Elba) else None
            3186817087 => if loose_eq(value, 0x006164616E6E616B, 0x0000000000000000, 0x0000000000000000, 7) Some(Knda) else None
            3189477354 => if loose_eq(value, 0x00000000686E697A, 0x0000000000000000, 0x0000000000000000, 4) Some(Zinh) else None
            3198565416 => if loose_eq(value, 0x6369706F69687465, 0x0000000000000000, 0x0000000000000000, 8) Some(Ethi) else None
            3206016157 => if loose_eq(value, 0x00000000756C6574, 0x0000000000000000, 0x0000000000000000, 4) Some(Telu) else None
            3222104702 => if loose_eq(value, 0x000000006C707564, 0x0000000000000000, 0x0000000000000000, 4) Some(Dupl) else None
            3230674864 => if loose_eq(value, 0x000000006B657267, 0x0000000000000000, 0x0000000000000000, 4) Some(Grek) else None
            3250045846 => if loose_eq(value, 0x00000000697A6579, 0x0000000000000000, 0x0000000000000000, 4) Some(Yezi) else None
            3255580906 => if loose_eq(value, 0x00000000726D6173, 0x0000000000000000, 0x0000000000000000, 4) Some(Samr) else None
            3260662848 => if loose_eq(value, 0x0000000069747270, 0x0000000000000000, 0x0000000000000000, 4) Some(Prti) else None
            3275935431 => if loose_eq(value, 0x73656E61646E7573, 0x0000000000000065, 0x0000000000000000, 9) Some(Sund) else None
            3282996293 => if loose_eq(value, 0x00000000676C6774, 0x0000000000000000, 0x0000000000000000, 4) Some(Tglg) else None
            3314303423 => if loose_eq(value, 0x000000006F706F62, 0x0000000000000000, 0x0000000000000000, 4) Some(Bopo) else None
            3315685778 => if loose_eq(value, 0x000000006963796C, 0x0000000000000000, 0x0000000000000000, 4) Some(Lyci) else None
            3335312164 => if loose_eq(value, 0x6E6572796D6C6170, 0x0000000000000065, 0x0000000000000000, 9) Some(Palm) else None
            3350868953 => if loose_eq(value, 0x00000000676E6168, 0x0000000000000000, 0x0000000000000000, 4) Some(Hang) else None
            3355254882 => if loose_eq(value, 0x00696C686179616B, 0x0000000000000000, 0x0000000000000000, 7) Some(Kali) else None
            3365071641 => if loose_eq(value, 0x000000006A68616D, 0x0000000000000000, 0x0000000000000000, 4) Some(Mahj) else None
            3427637558 => if loose_eq(value, 0x000000006D796C6D, 0x0000000000000000, 0x0000000000000000, 4) Some(Mlym) else None
            3439006748 => if loose_eq(value, 0x0061747568726974, 0x0000000000000000, 0x0000000000000000, 7) Some(Tirh) else None
            3455831052 => if loose_eq(value, 0x000000007374696B, 0x0000000000000000, 0x0000000000000000, 4) Some(Kits) else None
            3480940856 => if loose_eq(value, 0x000000006372616D, 0x0000000000000000, 0x0000000000000000, 4) Some(Marc) else None
            3482932952 => if loose_eq(value, 0x696E616A6168616D, 0x0000000000000000, 0x0000000000000000, 8) Some(Mahj) else None
            3484129583 => if loose_eq(value, 0x0000000064726C70, 0x0000000000000000, 0x0000000000000000, 4) Some(Plrd) else None
            3485089905 => if loose_eq(value, 0x000000006F6E6168, 0x0000000000000000, 0x0000000000000000, 4) Some(Hano) else None
            3516364402 => if loose_eq(value, 0x00000000696E616D, 0x0000000000000000, 0x0000000000000000, 4) Some(Mani) else None
            3518645143 => if loose_eq(value, 0x00000000696E6168, 0x0000000000000000, 0x0000000000000000, 4) Some(Hani) else None
            3536996880 => if loose_eq(value, 0x0000000072746168, 0x0000000000000000, 0x0000000000000000, 4) Some(Hatr) else None
            3567391075 => if loose_eq(value, 0x0000000063616171, 0x0000000000000000, 0x0000000000000000, 4) Some(Copt) else None
            3595867884 => if loose_eq(value, 0x006E616964676F73, 0x0000000000000000, 0x0000000000000000, 7) Some(Sogd) else None
            3600252497 => if loose_eq(value, 0x00000000646E616D, 0x0000000000000000, 0x0000000000000000, 4) Some(Mand) else None
            3613497042 => if loose_eq(value, 0x000000006179726F, 0x0000000000000000, 0x0000000000000000, 4) Some(Orya) else None
            3625469859 => if loose_eq(value, 0x0000000061616874, 0x0000000000000000, 0x0000000000000000, 4) Some(Thaa) else None
            3625832999 => if loose_eq(value, 0x6167616E61766564, 0x0000000000006972, 0x0000000000000000, 10) Some(Deva) else None
            3652313051 => if loose_eq(value, 0x6369746972616775, 0x0000000000000000, 0x0000000000000000, 8) Some(Ugar) else None
            3656884855 => if loose_eq(value, 0x0000636974706F63, 0x0000000000000000, 0x0000000000000000, 6) Some(Copt) else None
            3660148506 => if loose_eq(value, 0x006168746E617267, 0x0000000000000000, 0x0000000000000000, 7) Some(Gran) else None
            3663911155 => if loose_eq(value, 0x0000000074706F63, 0x0000000000000000, 0x0000000000000000, 4) Some(Copt) else None
            3668004980 => if loose_eq(value, 0x0000616D6B616863, 0x0000000000000000, 0x0000000000000000, 6) Some(Cakm) else None
            3693510853 => if loose_eq(value, 0x616D69657465656D, 0x00000000006B6579, 0x0000000000000000, 11) Some(Mtei) else None
            3698474787 => if loose_eq(value, 0x7468736172756173, 0x0000000000006172, 0x0000000000000000, 10) Some(Saur) else None
            3715629196 => if loose_eq(value, 0x65616863696E616D, 0x0000000000006E61, 0x0000000000000000, 10) Some(Mani) else None
            3716960355 => if loose_eq(value, 0x00000000616B616D, 0x0000000000000000, 0x0000000000000000, 4) Some(Maka) else None
            3720577637 => if loose_eq(value, 0x006F6F6E756E6168, 0x0000000000000000, 0x0000000000000000, 7) Some(Hano) else None
            3735167265 => if loose_eq(value, 0x0000000069616171, 0x0000000000000000, 0x0000000000000000, 4) Some(Zinh) else None
            3755629569 => if loose_eq(value, 0x006E617473657661, 0x0000000000000000, 0x0000000000000000, 7) Some(Avst) else None
            3759690811 => if loose_eq(value, 0x0000000069616874, 0x0000000000000000, 0x0000000000000000, 4) Some(Thai) else None
            3781792677 => if loose_eq(value, 0x000000007A7A7A7A, 0x0000000000000000, 0x0000000000000000, 4) Some(Zzzz) else None
            3783597566 => if loose_eq(value, 0x0000000063727973, 0x0000000000000000, 0x0000000000000000, 4) Some(Syrc) else None
            3809528351 => if loose_eq(value, 0x00696E61746C756D, 0x0000000000000000, 0x0000000000000000, 7) Some(Mult) else None
            3831418129 => if loose_eq(value, 0x000000617969726F, 0x0000000000000000, 0x0000000000000000, 5) Some(Orya) else None
            3838432252 => if loose_eq(value, 0x0000000068746976, 0x0000000000000000, 0x0000000000000000, 4) Some(Vith) else None
            3852025133 => if loose_eq(value, 0x000000006964796C, 0x0000000000000000, 0x0000000000000000, 4) Some(Lydi) else None
            3858870726 => if loose_eq(value, 0x00726177756E7573, 0x0000000000000000, 0x0000000000000000, 7) Some(Sunu) else None
            3900583171 => if loose_eq(value, 0x00000000006F726D, 0x0000000000000000, 0x0000000000000000, 3) Some(Mroo) else None
            3904833540 => if loose_eq(value, 0x00000000006E6168, 0x0000000000000000, 0x0000000000000000, 3) Some(Hani) else None
            3912040265 => if loose_eq(value, 0x0000000078757378, 0x0000000000000000, 0x0000000000000000, 4) Some(Xsux) else None
            3913219298 => if loose_eq(value, 0x696B727574646C6F, 0x0000000000000063, 0x0000000000000000, 9) Some(Orkh) else None
            3913480095 => if loose_eq(value, 0x000000006F796F73, 0x0000000000000000, 0x0000000000000000, 4) Some(Soyo) else None
            3917848435 => if loose_eq(value, 0x6E616967726F6567, 0x0000000000000000, 0x0000000000000000, 8) Some(Geor) else None
            3922329705 => if loose_eq(value, 0x0000000074737661, 0x0000000000000000, 0x0000000000000000, 4) Some(Avst) else None
            3934708178 => if loose_eq(value, 0x00000000676E6F6D, 0x0000000000000000, 0x0000000000000000, 4) Some(Mong) else None
            3983667256 => if loose_eq(value, 0x696172746172696B, 0x0000000000000000, 0x0000000000000000, 8) Some(Krai) else None
            4027050692 => if loose_eq(value, 0x000000007461626E, 0x0000000000000000, 0x0000000000000000, 4) Some(Nbat) else None
            4032857862 => if loose_eq(value, 0x000000006F796174, 0x0000000000000000, 0x0000000000000000, 4) Some(Tayo) else None
            4033799774 => if loose_eq(value, 0x0000000069646F6D, 0x0000000000000000, 0x0000000000000000, 4) Some(Modi) else None
            4067744746 => if loose_eq(value, 0x0000000072616775, 0x0000000000000000, 0x0000000000000000, 4) Some(Ugar) else None
            4074175261 => if loose_eq(value, 0x636974696F72656D, 0x796C676F72656968, 0x0000000000736870, 19) Some(Mero) else None
            4093256147 => if loose_eq(value, 0x0061646172616873, 0x0000000000000000, 0x0000000000000000, 7) Some(Shrd) else None
            4114167284 => if loose_eq(value, 0x000000006F6F6B6E, 0x0000000000000000, 0x0000000000000000, 4) Some(Nkoo) else None
            4114238752 => if loose_eq(value, 0x6E616964616E6163, 0x6E696769726F6261, 0x0000000000006C61, 18) Some(Cans) else None
            4114333196 => if loose_eq(value, 0x0000000074766174, 0x0000000000000000, 0x0000000000000000, 4) Some(Tavt) else None
            4171437215 => if loose_eq(value, 0x0000000068746F67, 0x0000000000000000, 0x0000000000000000, 4) Some(Goth) else None
            4176278753 => if loose_eq(value, 0x756C69617477656E, 0x0000000000000065, 0x0000000000000000, 9) Some(Talu) else None
            4210925312 => if loose_eq(value, 0x0000006B61746162, 0x0000000000000000, 0x0000000000000000, 5) Some(Batk) else None
            4221770180 => if loose_eq(value, 0x676D61726173616D, 0x0000000069646E6F, 0x0000000000000000, 12) Some(Gonm) else None
            4232003279 => if loose_eq(value, 0x00000000686B726F, 0x0000000000000000, 0x0000000000000000, 4) Some(Orkh) else None
            4232346077 => if loose_eq(value, 0x006369616D796C65, 0x0000000000000000, 0x0000000000000000, 7) Some(Elym) else None
            4248009695 => if loose_eq(value, 0x00000000726B6174, 0x0000000000000000, 0x0000000000000000, 4) Some(Takr) else None
            4250046449 => if loose_eq(value, 0x6D68687761686170, 0x0000000000676E6F, 0x0000000000000000, 11) Some(Hmng) else None
            4250880574 => if loose_eq(value, 0x0000776572626568, 0x0000000000000000, 0x0000000000000000, 6) Some(Hebr) else None
            4252385160 => if loose_eq(value, 0x000000006F6C7973, 0x0000000000000000, 0x0000000000000000, 4) Some(Sylo) else None
            4257448306 => if loose_eq(value, 0x000000656C696174, 0x0000000000000000, 0x0000000000000000, 5) Some(Tale) else None
            4263372803 => if loose_eq(value, 0x0000006B65657267, 0x0000000000000000, 0x0000000000000000, 5) Some(Grek) else None
            4266104284 => if loose_eq(value, 0x0000000064676F73, 0x0000000000000000, 0x0000000000000000, 4) Some(Sogd) else None
            4274069832 => if loose_eq(value, 0x00636961646E616D, 0x0000000000000000, 0x0000000000000000, 7) Some(Mand) else None
            _ => None
        }
    }
}

ascii_private_id : U32 -> U8
ascii_private_id = |u32| if (u32 >= 65 and u32 <= 90) or (u32 >= 97 and u32 <= 122) 74 else if (u32 >= 0 and u32 <= 64) or (u32 >= 91 and u32 <= 96) or (u32 >= 123 and u32 <= 127) 174 else 175

LoosePrefix : [PrefixStart, PrefixInitialI, PrefixInitialIs, PrefixDone]
LooseCompare : { matches : Bool, index : U64, prefix : LoosePrefix }

loose_hash : Str -> U32
loose_hash = |value| {
    folded = InternalUtf8.fold_scalars(
        value,
        { hash: 2166136261.U32, prefix: PrefixStart },
        |state, scalar, _byte_start, _byte_end, _scalar_index| {
            if is_loose_ignored(scalar) {
                state
            } else {
                normalized = ascii_lower(scalar)
                match state.prefix {
                    PrefixStart => if normalized == 0x69 {
                        { ..state, prefix: PrefixInitialI }
                    } else {
                        { hash: hash_scalar(state.hash, normalized), prefix: PrefixDone }
                    }
                    PrefixInitialI => if normalized == 0x73 {
                        { ..state, prefix: PrefixInitialIs }
                    } else {
                        { hash: hash_scalar(hash_scalar(state.hash, 0x69), normalized), prefix: PrefixDone }
                    }
                    PrefixInitialIs => { hash: hash_scalar(state.hash, normalized), prefix: PrefixDone }
                    PrefixDone => { ..state, hash: hash_scalar(state.hash, normalized) }
                }
            }
        },
    )
    match folded.prefix {
        PrefixInitialI => hash_scalar(folded.hash, 0x69)
        PrefixInitialIs => hash_scalar(hash_scalar(folded.hash, 0x69), 0x73)
        _ => folded.hash
    }
}

loose_eq : Str, U64, U64, U64, U8 -> Bool
loose_eq = |value, word0, word1, word2, target_length| {
    folded = InternalUtf8.fold_scalars(
        value,
        { matches: Bool.True, index: 0.U64, prefix: PrefixStart },
        |state, scalar, _byte_start, _byte_end, _scalar_index| {
            if is_loose_ignored(scalar) {
                state
            } else {
                normalized = ascii_lower(scalar)
                match state.prefix {
                    PrefixStart => if normalized == 0x69 {
                        { ..state, prefix: PrefixInitialI }
                    } else {
                        compare_target({ ..state, prefix: PrefixDone }, normalized, word0, word1, word2, target_length)
                    }
                    PrefixInitialI => if normalized == 0x73 {
                        { ..state, prefix: PrefixInitialIs }
                    } else {
                        after_i = compare_target({ ..state, prefix: PrefixDone }, 0x69, word0, word1, word2, target_length)
                        compare_target(after_i, normalized, word0, word1, word2, target_length)
                    }
                    PrefixInitialIs => compare_target({ ..state, prefix: PrefixDone }, normalized, word0, word1, word2, target_length)
                    PrefixDone => compare_target(state, normalized, word0, word1, word2, target_length)
                }
            }
        },
    )
    complete = match folded.prefix {
        PrefixInitialI => compare_target({ ..folded, prefix: PrefixDone }, 0x69, word0, word1, word2, target_length)
        PrefixInitialIs => {
            after_i = compare_target({ ..folded, prefix: PrefixDone }, 0x69, word0, word1, word2, target_length)
            compare_target(after_i, 0x73, word0, word1, word2, target_length)
        }
        _ => folded
    }
    complete.matches and complete.index == target_length.to_u64()
}

compare_target : LooseCompare, U32, U64, U64, U64, U8 -> LooseCompare
compare_target = |state, scalar, word0, word1, word2, target_length| {
    word = match state.index / 8 {
        0 => word0
        1 => word1
        _ => word2
    }
    expected = word.shr_wrap(((state.index % 8) * 8).to_u8_wrap()).bitwise_and(0xFF)
    matches = state.matches
        and state.index < target_length.to_u64()
        and scalar < 0x80
        and expected == scalar.to_u64()
    { ..state, matches, index: state.index + 1 }
}

hash_scalar = |hash, scalar| {
    if scalar < 0x80 {
        hash_byte(hash, scalar)
    } else if scalar < 0x800 {
        first = scalar.shr_wrap(6).bitwise_or(0xC0)
        second = scalar.bitwise_and(0x3F).bitwise_or(0x80)
        hash_byte(hash_byte(hash, first), second)
    } else if scalar < 0x10000 {
        first = scalar.shr_wrap(12).bitwise_or(0xE0)
        second = scalar.shr_wrap(6).bitwise_and(0x3F).bitwise_or(0x80)
        third = scalar.bitwise_and(0x3F).bitwise_or(0x80)
        hash_byte(hash_byte(hash_byte(hash, first), second), third)
    } else {
        first = scalar.shr_wrap(18).bitwise_or(0xF0)
        second = scalar.shr_wrap(12).bitwise_and(0x3F).bitwise_or(0x80)
        third = scalar.shr_wrap(6).bitwise_and(0x3F).bitwise_or(0x80)
        fourth = scalar.bitwise_and(0x3F).bitwise_or(0x80)
        hash_byte(hash_byte(hash_byte(hash_byte(hash, first), second), third), fourth)
    }
}

hash_byte = |hash, byte| hash.bitwise_xor(byte).times_wrap(16777619)

is_loose_ignored = |scalar| {
    scalar == 0x2D or scalar == 0x5F
        or (0x0009 <= scalar and scalar <= 0x000D)
        or scalar == 0x0020
        or scalar == 0x0085
        or scalar == 0x00A0
        or scalar == 0x1680
        or (0x2000 <= scalar and scalar <= 0x200A)
        or (0x2028 <= scalar and scalar <= 0x2029)
        or scalar == 0x202F
        or scalar == 0x205F
        or scalar == 0x3000
}

ascii_lower = |scalar| if 0x41 <= scalar and scalar <= 0x5A { scalar + 0x20 } else { scalar }

page_index : List(U8)
page_index = [
    0, 1, 2, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
    30, 31, 32, 32, 33, 34, 35, 36, 37, 37, 37, 37, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 2, 2, 53, 54,
    55, 56, 57, 58, 59, 59, 59, 59, 60, 59, 59, 59, 59, 59, 59, 59, 61, 61, 59, 59, 59, 59, 62, 59, 63, 64, 65, 66, 67, 68, 69, 70,
    71, 72, 73, 74, 75, 76, 77, 59, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 78, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    79, 79, 79, 79, 79, 79, 79, 79, 79, 80, 81, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 32, 32, 32, 32, 32, 32, 32, 32,
    32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
    32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
    32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 94, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 69, 69, 96, 97, 98, 99, 99, 99, 100, 101, 102, 103, 104, 105,
    106, 107, 108, 109, 95, 110, 111, 112, 113, 114, 115, 116, 117, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135,
    136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 95, 146, 147, 148, 149, 95, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 95, 162, 163, 164,
    165, 165, 165, 165, 165, 165, 165, 166, 167, 165, 168, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 169,
    170, 170, 170, 170, 170, 170, 170, 170, 171, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170,
    170, 170, 170, 170, 170, 170, 170, 172, 173, 173, 173, 173, 174, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 175, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 176, 176, 176, 176, 177, 178, 179, 180, 95, 95, 181, 95, 182, 183, 184, 185,
    186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186,
    186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 186, 187, 187, 187, 188, 189, 190, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 191,
    192, 193, 194, 195, 195, 196, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 197, 198, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 59, 199, 59, 59, 59, 200, 201, 202,
    59, 203, 204, 205, 206, 207, 208, 95, 209, 210, 211, 59, 59, 212, 59, 213, 214, 214, 214, 214, 214, 215, 95, 95, 95, 95, 95, 95, 95, 95, 216, 95,
    217, 218, 219, 95, 95, 220, 95, 95, 95, 221, 95, 222, 95, 223, 95, 224, 225, 226, 227, 95, 95, 95, 95, 95, 228, 229, 230, 95, 231, 232, 95, 95,
    233, 234, 59, 235, 236, 95, 59, 59, 59, 59, 59, 59, 59, 237, 59, 238, 239, 240, 59, 59, 241, 242, 59, 243, 95, 95, 95, 95, 95, 95, 95, 95,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 244, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 245, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 246, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 247, 69, 69, 69, 69, 248, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 69, 69, 69, 69, 249, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 250, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 251, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    252, 95, 253, 254, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
]

pages : List(U8)
pages = [
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 174, 174, 174, 174, 174,
    174, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 74, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 74, 174, 174, 174, 174, 174,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 174, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 174, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    74, 74, 74, 74, 74, 174, 174, 174, 174, 174, 14, 14, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 45, 45, 45, 45, 174, 45, 45, 45, 175, 175, 45, 45, 45, 45, 174, 45,
    175, 175, 175, 175, 45, 174, 45, 174, 45, 45, 45, 175, 45, 175, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
    45, 45, 175, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
    45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
    45, 45, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 173, 173, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 175, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 175, 175, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
    5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 175, 175, 5, 5, 5, 175, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53,
    53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53,
    53, 53, 53, 53, 53, 53, 53, 53, 175, 175, 175, 175, 175, 175, 175, 175, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53,
    53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 175, 175, 175, 175, 53, 53, 53, 53, 53, 53, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    3, 3, 3, 3, 3, 174, 3, 3, 3, 3, 3, 3, 174, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 174, 3, 3, 3, 174,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    174, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 173, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 174, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 175, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
    142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142,
    142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 175, 175, 142, 142, 142, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154,
    154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 154, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103,
    103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 103, 175, 175, 103, 103, 103,
    125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125,
    125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 175, 175, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 125, 175,
    84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 84, 175, 175, 84, 175,
    142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 142, 175, 175, 175, 175, 175, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 175, 175, 175, 175, 175, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 174, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
    29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
    29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 173, 173, 173, 173, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
    29, 29, 29, 29, 174, 174, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
    11, 11, 11, 11, 175, 11, 11, 11, 11, 11, 11, 11, 11, 175, 175, 11, 11, 175, 175, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11,
    11, 11, 11, 11, 11, 11, 11, 11, 11, 175, 11, 11, 11, 11, 11, 11, 11, 175, 11, 175, 175, 175, 11, 11, 11, 11, 175, 175, 11, 11, 11, 11,
    11, 11, 11, 11, 11, 175, 175, 11, 11, 175, 175, 11, 11, 11, 11, 175, 175, 175, 175, 175, 175, 175, 175, 11, 175, 175, 175, 175, 11, 11, 175, 11,
    11, 11, 11, 11, 175, 175, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 11, 175,
    175, 48, 48, 48, 175, 48, 48, 48, 48, 48, 48, 175, 175, 175, 175, 48, 48, 175, 175, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48,
    48, 48, 48, 48, 48, 48, 48, 48, 48, 175, 48, 48, 48, 48, 48, 48, 48, 175, 48, 48, 175, 48, 48, 175, 48, 48, 175, 175, 48, 175, 48, 48,
    48, 48, 48, 175, 175, 175, 175, 48, 48, 175, 175, 48, 48, 48, 175, 175, 175, 48, 175, 175, 175, 175, 175, 175, 175, 48, 48, 48, 48, 175, 48, 175,
    175, 175, 175, 175, 175, 175, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 48, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 46, 46, 46, 175, 46, 46, 46, 46, 46, 46, 46, 46, 46, 175, 46, 46, 46, 175, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46,
    46, 46, 46, 46, 46, 46, 46, 46, 46, 175, 46, 46, 46, 46, 46, 46, 46, 175, 46, 46, 175, 46, 46, 46, 46, 46, 175, 175, 46, 46, 46, 46,
    46, 46, 46, 46, 46, 46, 175, 46, 46, 46, 175, 46, 46, 46, 175, 175, 46, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    46, 46, 46, 46, 175, 175, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 46, 175, 175, 175, 175, 175, 175, 175, 46, 46, 46, 46, 46, 46, 46,
    175, 109, 109, 109, 175, 109, 109, 109, 109, 109, 109, 109, 109, 175, 175, 109, 109, 175, 175, 109, 109, 109, 109, 109, 109, 109, 109, 109, 109, 109, 109, 109,
    109, 109, 109, 109, 109, 109, 109, 109, 109, 175, 109, 109, 109, 109, 109, 109, 109, 175, 109, 109, 175, 109, 109, 109, 109, 109, 175, 175, 109, 109, 109, 109,
    109, 109, 109, 109, 109, 175, 175, 109, 109, 175, 175, 109, 109, 109, 175, 175, 175, 175, 175, 175, 175, 109, 109, 109, 175, 175, 175, 175, 109, 109, 175, 109,
    109, 109, 109, 109, 175, 175, 109, 109, 109, 109, 109, 109, 109, 109, 109, 109, 109, 109, 109, 109, 109, 109, 109, 109, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 147, 147, 175, 147, 147, 147, 147, 147, 147, 175, 175, 175, 147, 147, 147, 175, 147, 147, 147, 147, 175, 175, 175, 147, 147, 175, 147, 175, 147, 147,
    175, 175, 175, 147, 147, 175, 175, 175, 147, 147, 147, 175, 175, 175, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 175, 175, 175, 175, 147, 147,
    147, 147, 147, 175, 175, 175, 147, 147, 147, 175, 147, 147, 147, 147, 175, 175, 147, 175, 175, 175, 175, 175, 175, 147, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 175, 175, 175, 175, 175,
    151, 151, 151, 151, 151, 151, 151, 151, 151, 151, 151, 151, 151, 175, 151, 151, 151, 175, 151, 151, 151, 151, 151, 151, 151, 151, 151, 151, 151, 151, 151, 151,
    151, 151, 151, 151, 151, 151, 151, 151, 151, 175, 151, 151, 151, 151, 151, 151, 151, 151, 151, 151, 151, 151, 151, 151, 151, 151, 175, 175, 151, 151, 151, 151,
    151, 151, 151, 151, 151, 175, 151, 151, 151, 175, 151, 151, 151, 151, 175, 175, 175, 175, 175, 175, 175, 151, 151, 175, 151, 151, 151, 175, 151, 151, 175, 175,
    151, 151, 151, 151, 175, 175, 151, 151, 151, 151, 151, 151, 151, 151, 151, 151, 175, 175, 175, 175, 175, 175, 175, 151, 151, 151, 151, 151, 151, 151, 151, 151,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 175, 69, 69, 69, 175, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 69, 69, 69, 69, 175, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 175, 69, 69, 69, 69, 69, 175, 175, 69, 69, 69, 69,
    69, 69, 69, 69, 69, 175, 69, 69, 69, 175, 69, 69, 69, 69, 175, 175, 175, 175, 175, 175, 175, 69, 69, 175, 175, 175, 175, 175, 69, 69, 69, 175,
    69, 69, 69, 69, 175, 175, 69, 69, 69, 69, 69, 69, 69, 69, 69, 69, 175, 69, 69, 69, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 175, 91, 91, 91, 175, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
    91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
    91, 91, 91, 91, 91, 175, 91, 91, 91, 175, 91, 91, 91, 91, 91, 91, 175, 175, 175, 175, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
    91, 91, 91, 91, 175, 175, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91, 91,
    175, 134, 134, 134, 175, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 175, 175, 175, 134, 134, 134, 134, 134, 134,
    134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 175, 134, 134, 134, 134, 134, 134, 134, 134, 134, 175, 134, 175, 175,
    134, 134, 134, 134, 134, 134, 134, 175, 175, 175, 134, 175, 175, 175, 175, 134, 134, 134, 134, 134, 134, 175, 134, 175, 134, 134, 134, 134, 134, 134, 134, 134,
    175, 175, 175, 175, 175, 175, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 175, 175, 134, 134, 134, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155,
    155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 175, 175, 175, 175, 174,
    155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 73, 73, 175, 73, 175, 73, 73, 73, 73, 73, 175, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73,
    73, 73, 73, 73, 175, 73, 175, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 175, 175,
    73, 73, 73, 73, 73, 175, 73, 175, 73, 73, 73, 73, 73, 73, 73, 175, 73, 73, 73, 73, 73, 73, 73, 73, 73, 73, 175, 175, 73, 73, 73, 73,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156,
    156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156,
    156, 156, 156, 156, 156, 156, 156, 156, 175, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156,
    156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 175, 175, 175, 175, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156,
    156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 175, 156, 156, 156, 156, 156, 156, 156,
    156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 175, 156, 156,
    156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 156, 175, 156, 156, 156, 156, 156, 156, 156, 174, 174, 174, 174, 156, 156, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
    97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
    97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
    97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
    97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
    39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39,
    39, 39, 39, 39, 39, 39, 175, 39, 175, 175, 175, 175, 175, 39, 175, 175, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39,
    39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 174, 39, 39, 39, 39,
    49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
    49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
    49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
    49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
    37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,
    37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,
    37, 37, 37, 37, 37, 37, 37, 37, 37, 175, 37, 37, 37, 37, 175, 175, 37, 37, 37, 37, 37, 37, 37, 175, 37, 175, 37, 37, 37, 37, 175, 175,
    37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,
    37, 37, 37, 37, 37, 37, 37, 37, 37, 175, 37, 37, 37, 37, 175, 175, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,
    37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 175, 37, 37, 37, 37, 175, 175, 37, 37, 37, 37, 37, 37, 37, 175,
    37, 175, 37, 37, 37, 37, 175, 175, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 175, 37, 37, 37, 37, 37, 37, 37, 37,
    37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,
    37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 175, 37, 37, 37, 37, 175, 175, 37, 37, 37, 37, 37, 37, 37, 37,
    37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37,
    37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 175, 175, 37, 37, 37,
    37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 175, 175, 175,
    37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 175, 175, 175, 175, 175, 175,
    23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23,
    23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23,
    23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 175, 175, 23, 23, 23, 23, 23, 23, 175, 175,
    20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
    20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
    20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
    20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
    105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 105, 175, 175, 175,
    124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124,
    124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124,
    124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 174, 174, 174, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 124, 175, 175, 175, 175, 175, 175, 175,
    153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 153, 175, 175, 175, 175, 175, 175, 175, 175, 175, 153,
    51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 51, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 143, 175, 143, 143, 143, 175, 143, 143, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
    66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
    66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 175, 175,
    66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 175, 175, 175, 175, 175, 175, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 175, 175, 175, 175, 175, 175,
    93, 93, 174, 174, 93, 174, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 175, 175, 175, 175, 175, 175,
    93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93,
    93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93,
    93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 175, 175, 175, 175, 175, 175, 175,
    93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93,
    93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 175, 175, 175, 175, 175, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
    20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
    20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 175,
    76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 175, 175, 175, 175, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 175, 175, 175, 175,
    76, 175, 175, 175, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 76, 145, 145, 145, 145, 145, 145, 145, 145, 145, 145, 145, 145, 145, 145, 145, 145,
    145, 145, 145, 145, 145, 145, 145, 145, 145, 145, 145, 145, 145, 145, 175, 175, 145, 145, 145, 145, 145, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146,
    146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 175, 175, 175, 175, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146,
    146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 175, 175, 175, 175, 175, 175, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 146, 175, 175, 175, 146, 146,
    66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66, 66,
    17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 17, 175, 175, 17, 17,
    72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72,
    72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 175,
    72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 175, 175, 72,
    72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 175, 175, 175, 175, 175, 175, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 175, 175, 175, 175, 175, 175,
    72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 72, 175, 175, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 175, 175,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 175, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
    139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
    139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139, 139,
    10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
    10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 175, 175, 175, 175, 175, 175, 175, 175, 10, 10, 10, 10,
    75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75,
    75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 175, 175, 175, 75, 75, 75, 75, 75,
    75, 75, 75, 75, 75, 75, 75, 75, 75, 75, 175, 175, 175, 75, 75, 75, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106,
    106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106, 106,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 175, 175, 175, 175, 175, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39,
    39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 175, 175, 39, 39, 39,
    139, 139, 139, 139, 139, 139, 139, 139, 175, 175, 175, 175, 175, 175, 175, 175, 173, 173, 173, 174, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173,
    173, 174, 173, 173, 173, 173, 173, 173, 173, 174, 174, 174, 174, 173, 174, 174, 174, 174, 174, 174, 173, 174, 174, 174, 173, 173, 174, 175, 175, 175, 175, 175,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 45, 45, 45, 45, 45, 28, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 45, 45, 45,
    45, 45, 74, 74, 74, 74, 45, 45, 45, 45, 45, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 28, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 45,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173,
    45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 175, 175, 45, 45, 45, 45, 45, 45, 175, 175,
    45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
    45, 45, 45, 45, 45, 45, 175, 175, 45, 45, 45, 45, 45, 45, 175, 175, 45, 45, 45, 45, 45, 45, 45, 45, 175, 45, 175, 45, 175, 45, 175, 45,
    45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 175, 175,
    45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
    45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 175, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
    45, 45, 45, 45, 45, 175, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 175, 175, 45, 45, 45, 45, 45, 45, 175, 45, 45, 45,
    45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 175, 175, 45, 45, 45, 175, 45, 45, 45, 45, 45, 45, 45, 45, 45, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 173, 173, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 74, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 74,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 45, 174, 174, 174, 74, 74, 174, 174, 174, 174, 174, 174, 74, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 74, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 174, 174, 174, 175, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
    40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
    40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
    25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
    25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25,
    25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 25, 175, 175, 175, 175, 175, 25, 25, 25, 25, 25, 25, 25,
    39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39, 39,
    39, 39, 39, 39, 39, 39, 175, 39, 175, 175, 175, 175, 175, 39, 175, 175, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152,
    152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152, 152,
    152, 152, 152, 152, 152, 152, 152, 152, 175, 175, 175, 175, 175, 175, 175, 152, 152, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 152,
    37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    37, 37, 37, 37, 37, 37, 37, 175, 37, 37, 37, 37, 37, 37, 37, 175, 37, 37, 37, 37, 37, 37, 37, 175, 37, 37, 37, 37, 37, 37, 37, 175,
    37, 37, 37, 37, 37, 37, 37, 175, 37, 37, 37, 37, 37, 37, 37, 175, 37, 37, 37, 37, 37, 37, 37, 175, 37, 37, 37, 37, 37, 37, 37, 175,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 175, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 50, 174, 50, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 50, 50, 50, 50, 50, 50, 50, 50, 50, 173, 173, 173, 173, 49, 49, 174, 174, 174, 174, 174, 174, 174, 174, 50, 50, 50, 50, 174, 174, 174, 174,
    175, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
    54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
    54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 175, 175, 173, 173, 174, 174, 54, 54, 54,
    174, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63,
    63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63,
    63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 174, 174, 63, 63, 63,
    175, 175, 175, 175, 175, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
    14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 175, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
    49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
    49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
    49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14, 14,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 174, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63,
    49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63,
    63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 174,
    63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63,
    63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63,
    63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171,
    171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171,
    171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171,
    171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171,
    171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 175, 175, 175, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171,
    171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171, 171,
    171, 171, 171, 171, 171, 171, 171, 175, 175, 175, 175, 175, 175, 175, 175, 175, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79,
    79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79, 79,
    164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164,
    164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164,
    164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164,
    164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164,
    164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164,
    164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 164, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 174, 174, 174, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141,
    141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 141, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175,
    116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116,
    116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 175, 175, 175, 175, 175, 175, 175, 175,
    127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127,
    127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127,
    127, 127, 127, 127, 127, 127, 175, 175, 175, 175, 175, 175, 175, 175, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 127, 175, 175, 175, 175, 175, 175,
    29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29,
    62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62,
    62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 62, 174, 62, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122,
    122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 122, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 122,
    49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 175, 175, 175,
    61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
    61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61,
    61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 175, 174, 61, 61, 61, 61, 61, 61, 61, 61, 61, 61, 175, 175, 175, 175, 61, 61,
    97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 175,
    22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
    22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 175, 175, 22, 22, 22, 22, 22, 22, 22, 22, 22, 22, 175, 175, 22, 22, 22, 22,
    97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
    149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
    149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149, 149,
    149, 149, 149, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 149, 149, 149, 149, 149,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 37, 37, 37, 37, 37, 37, 175, 175, 37, 37, 37, 37, 37, 37, 175, 175, 37, 37, 37, 37, 37, 37, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    37, 37, 37, 37, 37, 37, 37, 175, 37, 37, 37, 37, 37, 37, 37, 175, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 174, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 45, 74, 74, 74, 74, 174, 174, 175, 175, 175, 175, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23,
    23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23,
    23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23, 23,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95,
    95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 175, 175, 95, 95, 95, 95, 95, 95, 95, 95, 95, 95, 175, 175, 175, 175, 175, 175,
    49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
    49, 49, 49, 49, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
    49, 49, 49, 49, 49, 49, 49, 175, 175, 175, 175, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49,
    49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 175, 175, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    74, 74, 74, 74, 74, 74, 74, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 5, 5, 5, 5, 5, 175, 175, 175, 175, 175, 53, 53, 53,
    53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 175, 53, 53, 53, 53, 53, 175, 53, 175,
    53, 53, 175, 53, 53, 175, 53, 53, 53, 53, 53, 53, 53, 53, 53, 53, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 174, 174,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 28, 28, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 175, 174, 174, 174, 174, 175, 175, 175, 175, 3, 3, 3, 3, 3, 175, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 175, 175, 174,
    175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 174, 174, 174, 174, 174,
    174, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 174, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63,
    63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 63, 174, 174,
    49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 49, 175,
    175, 175, 49, 49, 49, 49, 49, 49, 175, 175, 49, 49, 49, 49, 49, 49, 175, 175, 49, 49, 49, 49, 49, 49, 175, 175, 49, 49, 49, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 175, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 174, 174, 174, 174, 174, 175, 175,
    78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 175, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78,
    78, 78, 78, 78, 78, 78, 78, 175, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 175, 78, 78, 175, 78,
    78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 175, 175, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78,
    78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78,
    78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78,
    78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 78, 175, 175, 175, 175, 175,
    174, 174, 174, 175, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
    45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
    45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175,
    45, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 173, 175, 175,
    80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 80, 175, 175, 175,
    21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21,
    21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 21, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    173, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175,
    60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60, 60,
    60, 60, 60, 60, 175, 175, 175, 175, 175, 175, 175, 175, 175, 60, 60, 60, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43,
    43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 43, 175, 175, 175, 175, 175, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115,
    115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 115, 175, 175, 175, 175, 175,
    163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 163, 175, 163,
    168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168,
    168, 168, 168, 168, 175, 175, 175, 175, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 168, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
    32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32,
    32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 32, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129,
    129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129, 129,
    111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 175, 175,
    111, 111, 111, 111, 111, 111, 111, 111, 111, 111, 175, 175, 175, 175, 175, 175, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110,
    110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 175, 175, 175, 175, 110, 110, 110, 110, 110, 110, 110, 110,
    110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 110, 175, 175, 175, 175,
    35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35,
    35, 35, 35, 35, 35, 35, 35, 35, 175, 175, 175, 175, 175, 175, 175, 175, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 1, 165, 165, 165, 165, 165, 165, 165, 165, 165, 165, 165, 175, 165, 165, 165, 165,
    165, 165, 165, 165, 165, 165, 165, 165, 165, 165, 165, 175, 165, 165, 165, 165, 165, 165, 165, 175, 165, 165, 175, 165, 165, 165, 165, 165, 165, 165, 165, 165,
    165, 165, 175, 165, 165, 165, 165, 165, 165, 165, 165, 165, 165, 165, 165, 165, 165, 165, 175, 165, 165, 165, 165, 165, 165, 165, 175, 165, 165, 175, 175, 175,
    159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159,
    159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 159, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77,
    77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77,
    77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77,
    77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77,
    77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77,
    77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 77, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    77, 77, 77, 77, 77, 77, 77, 77, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    74, 74, 74, 74, 74, 74, 175, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 175, 74, 74, 74, 74, 74, 74, 74, 74, 74, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    27, 27, 27, 27, 27, 27, 175, 175, 27, 175, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27,
    27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 27, 175, 27, 27, 175, 175, 175, 27, 175, 175, 27,
    4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 175, 4, 4, 4, 4, 4, 4, 4, 4, 4,
    113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113, 113,
    101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 101, 175,
    175, 175, 175, 175, 175, 175, 175, 101, 101, 101, 101, 101, 101, 101, 101, 101, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 52, 175, 52, 52, 175, 175, 175, 175, 175, 52, 52, 52, 52, 52,
    119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 119, 175, 175, 175, 119,
    81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 81, 175, 175, 175, 175, 175, 81,
    132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 132, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90, 90,
    89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 175, 175, 175, 175, 89, 89, 89, 89,
    89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 175, 175, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89,
    89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89,
    65, 65, 65, 65, 175, 65, 65, 175, 175, 175, 175, 175, 65, 65, 65, 65, 65, 65, 65, 65, 175, 65, 65, 65, 175, 65, 65, 65, 65, 65, 65, 65,
    65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 65, 175, 175, 65, 65, 65, 175, 175, 175, 175, 65,
    65, 65, 65, 65, 65, 65, 65, 65, 65, 175, 175, 175, 175, 175, 175, 175, 65, 65, 65, 65, 65, 65, 65, 65, 65, 175, 175, 175, 175, 175, 175, 175,
    126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
    100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85,
    85, 85, 85, 85, 85, 85, 85, 175, 175, 175, 175, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 85, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
    6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 175, 175, 175, 6, 6, 6, 6, 6, 6, 6,
    121, 121, 121, 121, 121, 121, 121, 121, 121, 121, 121, 121, 121, 121, 121, 121, 121, 121, 121, 121, 121, 121, 175, 175, 121, 121, 121, 121, 121, 121, 121, 121,
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 175, 175, 175, 175, 175, 117, 117, 117, 117, 117, 117, 117, 117,
    118, 118, 118, 118, 118, 118, 118, 118, 118, 118, 118, 118, 118, 118, 118, 118, 118, 118, 175, 175, 175, 175, 175, 175, 175, 118, 118, 118, 118, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 118, 118, 118, 118, 118, 118, 118, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108,
    108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108, 108,
    108, 108, 108, 108, 108, 108, 108, 108, 108, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59,
    59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59,
    59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 59, 175, 175, 175, 175, 175, 175, 175, 59, 59, 59, 59, 59, 59,
    123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123,
    123, 123, 123, 123, 123, 123, 123, 123, 175, 175, 175, 175, 175, 175, 175, 175, 123, 123, 123, 123, 123, 123, 123, 123, 123, 123, 175, 175, 175, 175, 175, 175,
    38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
    38, 38, 38, 38, 38, 38, 175, 175, 175, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38, 38,
    38, 38, 38, 38, 38, 38, 175, 175, 175, 175, 175, 175, 175, 175, 38, 38, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 175,
    170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170,
    170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 175, 170, 170, 170, 175, 175, 170, 170, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 3, 3, 3, 3, 3, 3, 175, 175, 175, 175, 175, 175, 175, 175, 3, 3, 3, 3, 3, 3, 3, 3, 3, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 3, 3, 3, 3, 3, 3,
    136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136, 136,
    136, 136, 136, 136, 136, 136, 136, 136, 175, 175, 175, 175, 175, 175, 175, 175, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135,
    135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 135, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 112,
    112, 112, 112, 112, 112, 112, 112, 112, 112, 112, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24,
    24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 24, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 36, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 175, 175, 175, 175, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
    15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 15, 175, 175, 175, 175, 175, 175, 175, 175, 175, 15,
    71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
    71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71, 71,
    71, 71, 71, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 71, 175, 175, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137,
    137, 137, 137, 137, 137, 137, 137, 137, 137, 175, 175, 175, 175, 175, 175, 175, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 175, 175, 175, 175, 175, 175,
    19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19,
    19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19, 175, 19, 19, 19, 19, 19, 19, 19, 19, 19, 19,
    19, 19, 19, 19, 19, 19, 19, 19, 175, 175, 175, 175, 175, 175, 175, 175, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82,
    82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 82, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130,
    130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130,
    130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130, 130,
    175, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 134, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 175, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67,
    67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67, 67,
    67, 67, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    96, 96, 96, 96, 96, 96, 96, 175, 96, 175, 96, 96, 96, 96, 175, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 175, 96,
    96, 96, 96, 96, 96, 96, 96, 96, 96, 96, 175, 175, 175, 175, 175, 175, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133,
    133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133,
    133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 175, 175, 175, 175, 175, 133, 133, 133, 133, 133, 133, 133, 133, 133, 133, 175, 175, 175, 175, 175, 175,
    44, 44, 44, 44, 175, 44, 44, 44, 44, 44, 44, 44, 44, 175, 175, 44, 44, 175, 175, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44, 44,
    44, 44, 44, 44, 44, 44, 44, 44, 44, 175, 44, 44, 44, 44, 44, 44, 44, 175, 44, 44, 175, 44, 44, 44, 44, 44, 175, 173, 44, 44, 44, 44,
    44, 44, 44, 44, 44, 175, 175, 44, 44, 175, 175, 44, 44, 44, 175, 175, 44, 175, 175, 175, 175, 175, 175, 44, 175, 175, 175, 175, 175, 44, 44, 44,
    44, 44, 44, 44, 175, 175, 44, 44, 44, 44, 44, 44, 44, 175, 175, 175, 44, 44, 44, 44, 44, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 175, 162, 175, 175, 162, 175, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162,
    162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 175, 162, 162, 162, 162, 162, 162, 162, 162, 162,
    162, 175, 162, 175, 175, 162, 175, 162, 162, 162, 162, 175, 162, 162, 162, 162, 162, 162, 162, 162, 162, 162, 175, 162, 162, 175, 175, 175, 175, 175, 175, 175,
    175, 162, 162, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102,
    102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102,
    102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 102, 175, 102, 102, 102,
    102, 102, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157,
    157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157,
    157, 157, 157, 157, 157, 157, 157, 157, 175, 175, 175, 175, 175, 175, 175, 175, 157, 157, 157, 157, 157, 157, 157, 157, 157, 157, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131,
    131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 175, 175, 131, 131, 131, 131, 131, 131, 131, 131,
    131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 131, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92,
    92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92,
    92, 92, 92, 92, 92, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 92, 92, 92, 92, 92, 92, 92, 92, 92, 92, 175, 175, 175, 175, 175, 175,
    93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 93, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144,
    144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 175, 175, 175, 175, 175, 175,
    144, 144, 144, 144, 144, 144, 144, 144, 144, 144, 175, 175, 175, 175, 175, 175, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97, 97,
    97, 97, 97, 97, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 175, 175, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 175, 175, 175, 175, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
    2, 2, 2, 2, 2, 2, 2, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31,
    31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166,
    166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166,
    166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 166, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 166,
    30, 30, 30, 30, 30, 30, 30, 175, 175, 30, 175, 175, 30, 30, 30, 30, 30, 30, 30, 30, 175, 30, 30, 175, 30, 30, 30, 30, 30, 30, 30, 30,
    30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 175, 30, 30, 175, 175, 30, 30, 30, 30, 30,
    30, 30, 30, 30, 30, 30, 30, 175, 175, 175, 175, 175, 175, 175, 175, 175, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    99, 99, 99, 99, 99, 99, 99, 99, 175, 175, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 175, 175, 99, 99, 99, 99, 99, 99,
    99, 99, 99, 99, 99, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172,
    172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172,
    172, 172, 172, 172, 172, 172, 172, 172, 175, 175, 175, 175, 175, 175, 175, 175, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138,
    138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138,
    138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138, 138,
    138, 138, 138, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
    114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114,
    114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 114, 175, 175, 175, 175, 175, 175, 175,
    29, 29, 29, 29, 29, 29, 29, 29, 29, 29, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    130, 130, 130, 130, 130, 130, 130, 130, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140,
    140, 140, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 140, 140, 140, 140, 140, 140, 140, 140, 140, 140, 175, 175, 175, 175, 175, 175,
    13, 13, 13, 13, 13, 13, 13, 13, 13, 175, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 175, 13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
    13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13, 175, 175, 175, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
    86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 175, 175, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86,
    86, 86, 86, 86, 86, 86, 86, 86, 175, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 86, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    42, 42, 42, 42, 42, 42, 42, 175, 42, 42, 175, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42,
    42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 175, 175, 175, 42, 175, 42, 42, 175, 42,
    42, 42, 42, 42, 42, 42, 42, 42, 175, 175, 175, 175, 175, 175, 175, 175, 42, 42, 42, 42, 42, 42, 42, 42, 42, 42, 175, 175, 175, 175, 175, 175,
    41, 41, 41, 41, 41, 41, 175, 41, 41, 175, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41,
    41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 175, 41, 41, 175, 41, 41, 41, 41, 41, 41, 175, 175, 175, 175, 175, 175, 175,
    41, 41, 41, 41, 41, 41, 41, 41, 41, 41, 175, 175, 175, 175, 175, 175, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160,
    160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 175, 175, 175, 175,
    160, 160, 160, 160, 160, 160, 160, 160, 160, 160, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 83, 175, 175, 175, 175, 175, 175, 175,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 175, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 175, 175, 175, 64, 64,
    64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 79, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147,
    147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 147, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 147,
    169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169,
    169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169,
    169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169,
    169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169,
    169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169,
    169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169,
    169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169,
    169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 175, 169, 169, 169, 169, 169, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169,
    169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169, 169,
    169, 169, 169, 169, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
    26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
    26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26,
    26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 26, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
    34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
    34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
    34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
    34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
    34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
    34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
    34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
    34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
    34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34,
    34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 34, 175, 175, 175, 175, 175,
    55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55,
    55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55,
    55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55,
    55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55,
    55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55,
    55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55, 55,
    55, 55, 55, 55, 55, 55, 55, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47,
    47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 47, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
    8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 175, 175, 175, 175, 175, 175, 175,
    94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 175,
    94, 94, 94, 94, 94, 94, 94, 94, 94, 94, 175, 175, 175, 175, 94, 94, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158,
    158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158,
    158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 175,
    158, 158, 158, 158, 158, 158, 158, 158, 158, 158, 175, 175, 175, 175, 175, 175, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
    9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 175, 175, 9, 9, 9, 9, 9, 9, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
    56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56,
    56, 56, 56, 56, 56, 56, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 175, 56, 56, 56, 56, 56,
    56, 56, 175, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 175, 175, 175, 175, 175, 56, 56, 56,
    56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 56, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70,
    70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 70, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
    87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87,
    87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 175, 175, 175, 175, 175,
    12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 175, 175, 12, 12, 12, 12, 12,
    12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120,
    120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120,
    120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 175, 175, 175, 175, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120,
    120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120,
    120, 120, 120, 120, 120, 120, 120, 120, 175, 175, 175, 175, 175, 175, 175, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120, 120,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    148, 104, 50, 50, 68, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 50, 50, 50, 50, 50, 50, 50, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148,
    148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148,
    148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148,
    148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148,
    68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68,
    68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68,
    68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68,
    68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68,
    68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68,
    68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68,
    68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 68, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 68,
    148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148,
    148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148,
    148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148,
    148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 148, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 63, 63, 63, 63, 175, 63, 63, 63, 63, 63, 63, 63, 175, 63, 63, 175,
    63, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
    54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
    54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
    54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
    54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
    54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
    54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
    54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
    54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54, 54,
    63, 63, 63, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 54, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 54, 54, 54, 175, 175, 63, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 63, 63, 63, 63, 175, 175, 175, 175, 175, 175, 175, 175, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104,
    104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104,
    104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104,
    104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104,
    104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104,
    104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104,
    104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104,
    104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104,
    104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 104, 175, 175, 175, 175,
    33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33,
    33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33,
    33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33,
    33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 175, 175, 175, 175, 175, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 175, 175, 175,
    33, 33, 33, 33, 33, 33, 33, 33, 33, 175, 175, 175, 175, 175, 175, 175, 33, 33, 33, 33, 33, 33, 33, 33, 33, 33, 175, 175, 33, 33, 33, 33,
    174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 175, 175, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173,
    173, 173, 173, 173, 173, 173, 173, 175, 175, 175, 175, 175, 175, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 173, 173, 173, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 173, 173, 173, 173, 173,
    173, 173, 173, 174, 174, 173, 173, 173, 173, 173, 173, 173, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 173, 173, 173, 173, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
    45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45, 45,
    45, 45, 45, 45, 45, 45, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 174, 174,
    175, 175, 174, 175, 175, 174, 174, 175, 175, 174, 174, 174, 174, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 174, 175, 174, 174, 174,
    174, 174, 174, 174, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 175, 174, 174, 174, 174, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 175, 174, 174, 174, 174, 174, 174, 174, 175, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 174, 174, 174, 174, 175,
    174, 174, 174, 174, 174, 175, 174, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
    128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
    128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
    128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128,
    128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 128, 128, 128, 128, 128,
    175, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 128, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 74, 175,
    175, 175, 175, 175, 175, 74, 74, 74, 74, 74, 74, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    40, 40, 40, 40, 40, 40, 40, 175, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 40, 175, 175, 40, 40, 40, 40, 40,
    40, 40, 175, 40, 40, 175, 40, 40, 40, 40, 40, 175, 175, 175, 175, 175, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28,
    28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 28, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57,
    57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 175, 175, 175, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 175, 175,
    57, 57, 57, 57, 57, 57, 57, 57, 57, 57, 175, 175, 175, 175, 57, 57, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161,
    161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 161, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167,
    167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 167, 175, 175, 175, 175, 175, 167,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98,
    98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 98, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107,
    107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 107, 175, 175, 175, 175, 107,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 175,
    150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 150, 175, 175, 175, 175, 175, 175, 175, 175, 150, 150,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    37, 37, 37, 37, 37, 37, 37, 175, 37, 37, 37, 37, 175, 37, 37, 175, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 37, 175,
    88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
    88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
    88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
    88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
    88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
    88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88,
    88, 88, 88, 88, 88, 175, 175, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 88, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 175, 175, 175, 175, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 175, 175, 175, 175, 0, 0,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    3, 3, 3, 3, 175, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
    175, 3, 3, 175, 3, 175, 175, 3, 175, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 175, 3, 3, 3, 3, 175, 3, 175, 3, 175, 175, 175, 175,
    175, 175, 3, 175, 175, 175, 175, 3, 175, 3, 175, 3, 175, 3, 3, 3, 175, 3, 3, 175, 3, 175, 175, 3, 175, 3, 175, 3, 175, 3, 175, 3,
    175, 3, 3, 175, 3, 175, 175, 3, 3, 3, 3, 175, 3, 3, 3, 3, 3, 3, 3, 175, 3, 3, 3, 3, 175, 3, 3, 3, 3, 175, 3, 175,
    3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 175, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 175, 175, 175, 175,
    175, 3, 3, 3, 175, 3, 3, 3, 3, 3, 175, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 3, 3, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    54, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175,
    174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 175, 174, 175, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 175, 175,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 175, 175, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 175, 175, 175, 175, 175, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50,
    50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 175, 175, 175, 175, 175, 175,
    175, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 174,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173,
    173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 173, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,
]
