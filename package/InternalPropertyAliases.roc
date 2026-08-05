## GENERATED from Unicode 17.0.0 PropertyAliases and PropertyValueAliases. Run `python3 scripts/unicode_data.py generate`. ##
## These canonical names are metadata for stable identities, never storage ordinals. ##
## metadata: 30 scalar category identities; 58 exact CCC identities. ##

import InternalGeneralCategory
import InternalLooseAlias

InternalPropertyAliases :: [].{
    PropertyName : { short : Str, long : Str }

    general_category_property : PropertyName
    general_category_property = { short: "gc", long: "General_Category" }

    canonical_combining_class_property : PropertyName
    canonical_combining_class_property = { short: "ccc", long: "Canonical_Combining_Class" }

    general_category_short : InternalGeneralCategory.GeneralCategory -> Str
    general_category_short = |category| {
        match category {
        Cc => "Cc"
        Cf => "Cf"
        Cn => "Cn"
        Co => "Co"
        Cs => "Cs"
        Ll => "Ll"
        Lm => "Lm"
        Lo => "Lo"
        Lt => "Lt"
        Lu => "Lu"
        Mc => "Mc"
        Me => "Me"
        Mn => "Mn"
        Nd => "Nd"
        Nl => "Nl"
        No => "No"
        Pc => "Pc"
        Pd => "Pd"
        Pe => "Pe"
        Pf => "Pf"
        Pi => "Pi"
        Po => "Po"
        Ps => "Ps"
        Sc => "Sc"
        Sk => "Sk"
        Sm => "Sm"
        So => "So"
        Zl => "Zl"
        Zp => "Zp"
        Zs => "Zs"
        }
    }

    general_category_long : InternalGeneralCategory.GeneralCategory -> Str
    general_category_long = |category| {
        match category {
        Cc => "Control"
        Cf => "Format"
        Cn => "Unassigned"
        Co => "Private_Use"
        Cs => "Surrogate"
        Ll => "Lowercase_Letter"
        Lm => "Modifier_Letter"
        Lo => "Other_Letter"
        Lt => "Titlecase_Letter"
        Lu => "Uppercase_Letter"
        Mc => "Spacing_Mark"
        Me => "Enclosing_Mark"
        Mn => "Nonspacing_Mark"
        Nd => "Decimal_Number"
        Nl => "Letter_Number"
        No => "Other_Number"
        Pc => "Connector_Punctuation"
        Pd => "Dash_Punctuation"
        Pe => "Close_Punctuation"
        Pf => "Final_Punctuation"
        Pi => "Initial_Punctuation"
        Po => "Other_Punctuation"
        Ps => "Open_Punctuation"
        Sc => "Currency_Symbol"
        Sk => "Modifier_Symbol"
        Sm => "Math_Symbol"
        So => "Other_Symbol"
        Zl => "Line_Separator"
        Zp => "Paragraph_Separator"
        Zs => "Space_Separator"
        }
    }

    general_category_alias_count : InternalGeneralCategory.GeneralCategory -> U8
    general_category_alias_count = |category| {
        match category {
        Cc => 3
        Cf => 2
        Cn => 2
        Co => 2
        Cs => 2
        Ll => 2
        Lm => 2
        Lo => 2
        Lt => 2
        Lu => 2
        Mc => 2
        Me => 2
        Mn => 2
        Nd => 3
        Nl => 2
        No => 2
        Pc => 2
        Pd => 2
        Pe => 2
        Pf => 2
        Pi => 2
        Po => 2
        Ps => 2
        Sc => 2
        Sk => 2
        Sm => 2
        So => 2
        Zl => 2
        Zp => 2
        Zs => 2
        }
    }

    general_category_alias_at : InternalGeneralCategory.GeneralCategory, U8 -> [Some(Str), None]
    general_category_alias_at = |category, index| {
        match (category, index) {
        (Cc, 0) => Some("Cc")
        (Cc, 1) => Some("Control")
        (Cc, 2) => Some("cntrl")
        (Cf, 0) => Some("Cf")
        (Cf, 1) => Some("Format")
        (Cn, 0) => Some("Cn")
        (Cn, 1) => Some("Unassigned")
        (Co, 0) => Some("Co")
        (Co, 1) => Some("Private_Use")
        (Cs, 0) => Some("Cs")
        (Cs, 1) => Some("Surrogate")
        (Ll, 0) => Some("Ll")
        (Ll, 1) => Some("Lowercase_Letter")
        (Lm, 0) => Some("Lm")
        (Lm, 1) => Some("Modifier_Letter")
        (Lo, 0) => Some("Lo")
        (Lo, 1) => Some("Other_Letter")
        (Lt, 0) => Some("Lt")
        (Lt, 1) => Some("Titlecase_Letter")
        (Lu, 0) => Some("Lu")
        (Lu, 1) => Some("Uppercase_Letter")
        (Mc, 0) => Some("Mc")
        (Mc, 1) => Some("Spacing_Mark")
        (Me, 0) => Some("Me")
        (Me, 1) => Some("Enclosing_Mark")
        (Mn, 0) => Some("Mn")
        (Mn, 1) => Some("Nonspacing_Mark")
        (Nd, 0) => Some("Nd")
        (Nd, 1) => Some("Decimal_Number")
        (Nd, 2) => Some("digit")
        (Nl, 0) => Some("Nl")
        (Nl, 1) => Some("Letter_Number")
        (No, 0) => Some("No")
        (No, 1) => Some("Other_Number")
        (Pc, 0) => Some("Pc")
        (Pc, 1) => Some("Connector_Punctuation")
        (Pd, 0) => Some("Pd")
        (Pd, 1) => Some("Dash_Punctuation")
        (Pe, 0) => Some("Pe")
        (Pe, 1) => Some("Close_Punctuation")
        (Pf, 0) => Some("Pf")
        (Pf, 1) => Some("Final_Punctuation")
        (Pi, 0) => Some("Pi")
        (Pi, 1) => Some("Initial_Punctuation")
        (Po, 0) => Some("Po")
        (Po, 1) => Some("Other_Punctuation")
        (Ps, 0) => Some("Ps")
        (Ps, 1) => Some("Open_Punctuation")
        (Sc, 0) => Some("Sc")
        (Sc, 1) => Some("Currency_Symbol")
        (Sk, 0) => Some("Sk")
        (Sk, 1) => Some("Modifier_Symbol")
        (Sm, 0) => Some("Sm")
        (Sm, 1) => Some("Math_Symbol")
        (So, 0) => Some("So")
        (So, 1) => Some("Other_Symbol")
        (Zl, 0) => Some("Zl")
        (Zl, 1) => Some("Line_Separator")
        (Zp, 0) => Some("Zp")
        (Zp, 1) => Some("Paragraph_Separator")
        (Zs, 0) => Some("Zs")
        (Zs, 1) => Some("Space_Separator")
            _ => None
        }
    }

    general_category_parse : Str -> [Some(InternalGeneralCategory.GeneralCategory), None]
    general_category_parse = |name|         if InternalLooseAlias.matches(name, "Cc") or InternalLooseAlias.matches(name, "Control") or InternalLooseAlias.matches(name, "cntrl") Some(Cc) else         if InternalLooseAlias.matches(name, "Cf") or InternalLooseAlias.matches(name, "Format") Some(Cf) else         if InternalLooseAlias.matches(name, "Cn") or InternalLooseAlias.matches(name, "Unassigned") Some(Cn) else         if InternalLooseAlias.matches(name, "Co") or InternalLooseAlias.matches(name, "Private_Use") Some(Co) else         if InternalLooseAlias.matches(name, "Cs") or InternalLooseAlias.matches(name, "Surrogate") Some(Cs) else         if InternalLooseAlias.matches(name, "Ll") or InternalLooseAlias.matches(name, "Lowercase_Letter") Some(Ll) else         if InternalLooseAlias.matches(name, "Lm") or InternalLooseAlias.matches(name, "Modifier_Letter") Some(Lm) else         if InternalLooseAlias.matches(name, "Lo") or InternalLooseAlias.matches(name, "Other_Letter") Some(Lo) else         if InternalLooseAlias.matches(name, "Lt") or InternalLooseAlias.matches(name, "Titlecase_Letter") Some(Lt) else         if InternalLooseAlias.matches(name, "Lu") or InternalLooseAlias.matches(name, "Uppercase_Letter") Some(Lu) else         if InternalLooseAlias.matches(name, "Mc") or InternalLooseAlias.matches(name, "Spacing_Mark") Some(Mc) else         if InternalLooseAlias.matches(name, "Me") or InternalLooseAlias.matches(name, "Enclosing_Mark") Some(Me) else         if InternalLooseAlias.matches(name, "Mn") or InternalLooseAlias.matches(name, "Nonspacing_Mark") Some(Mn) else         if InternalLooseAlias.matches(name, "Nd") or InternalLooseAlias.matches(name, "Decimal_Number") or InternalLooseAlias.matches(name, "digit") Some(Nd) else         if InternalLooseAlias.matches(name, "Nl") or InternalLooseAlias.matches(name, "Letter_Number") Some(Nl) else         if InternalLooseAlias.matches(name, "No") or InternalLooseAlias.matches(name, "Other_Number") Some(No) else         if InternalLooseAlias.matches(name, "Pc") or InternalLooseAlias.matches(name, "Connector_Punctuation") Some(Pc) else         if InternalLooseAlias.matches(name, "Pd") or InternalLooseAlias.matches(name, "Dash_Punctuation") Some(Pd) else         if InternalLooseAlias.matches(name, "Pe") or InternalLooseAlias.matches(name, "Close_Punctuation") Some(Pe) else         if InternalLooseAlias.matches(name, "Pf") or InternalLooseAlias.matches(name, "Final_Punctuation") Some(Pf) else         if InternalLooseAlias.matches(name, "Pi") or InternalLooseAlias.matches(name, "Initial_Punctuation") Some(Pi) else         if InternalLooseAlias.matches(name, "Po") or InternalLooseAlias.matches(name, "Other_Punctuation") Some(Po) else         if InternalLooseAlias.matches(name, "Ps") or InternalLooseAlias.matches(name, "Open_Punctuation") Some(Ps) else         if InternalLooseAlias.matches(name, "Sc") or InternalLooseAlias.matches(name, "Currency_Symbol") Some(Sc) else         if InternalLooseAlias.matches(name, "Sk") or InternalLooseAlias.matches(name, "Modifier_Symbol") Some(Sk) else         if InternalLooseAlias.matches(name, "Sm") or InternalLooseAlias.matches(name, "Math_Symbol") Some(Sm) else         if InternalLooseAlias.matches(name, "So") or InternalLooseAlias.matches(name, "Other_Symbol") Some(So) else         if InternalLooseAlias.matches(name, "Zl") or InternalLooseAlias.matches(name, "Line_Separator") Some(Zl) else         if InternalLooseAlias.matches(name, "Zp") or InternalLooseAlias.matches(name, "Paragraph_Separator") Some(Zp) else         if InternalLooseAlias.matches(name, "Zs") or InternalLooseAlias.matches(name, "Space_Separator") Some(Zs) else None

    canonical_combining_class_short : U8 -> [Some(Str), None]
    canonical_combining_class_short = |value| {
        match value {
        0 => Some("NR")
        1 => Some("OV")
        6 => Some("HANR")
        7 => Some("NK")
        8 => Some("KV")
        9 => Some("VR")
        10 => Some("CCC10")
        11 => Some("CCC11")
        12 => Some("CCC12")
        13 => Some("CCC13")
        14 => Some("CCC14")
        15 => Some("CCC15")
        16 => Some("CCC16")
        17 => Some("CCC17")
        18 => Some("CCC18")
        19 => Some("CCC19")
        20 => Some("CCC20")
        21 => Some("CCC21")
        22 => Some("CCC22")
        23 => Some("CCC23")
        24 => Some("CCC24")
        25 => Some("CCC25")
        26 => Some("CCC26")
        27 => Some("CCC27")
        28 => Some("CCC28")
        29 => Some("CCC29")
        30 => Some("CCC30")
        31 => Some("CCC31")
        32 => Some("CCC32")
        33 => Some("CCC33")
        34 => Some("CCC34")
        35 => Some("CCC35")
        36 => Some("CCC36")
        84 => Some("CCC84")
        91 => Some("CCC91")
        103 => Some("CCC103")
        107 => Some("CCC107")
        118 => Some("CCC118")
        122 => Some("CCC122")
        129 => Some("CCC129")
        130 => Some("CCC130")
        132 => Some("CCC132")
        133 => Some("CCC133")
        200 => Some("ATBL")
        202 => Some("ATB")
        214 => Some("ATA")
        216 => Some("ATAR")
        218 => Some("BL")
        220 => Some("B")
        222 => Some("BR")
        224 => Some("L")
        226 => Some("R")
        228 => Some("AL")
        230 => Some("A")
        232 => Some("AR")
        233 => Some("DB")
        234 => Some("DA")
        240 => Some("IS")
            _ => None
        }
    }

    canonical_combining_class_long : U8 -> [Some(Str), None]
    canonical_combining_class_long = |value| {
        match value {
        0 => Some("Not_Reordered")
        1 => Some("Overlay")
        6 => Some("Han_Reading")
        7 => Some("Nukta")
        8 => Some("Kana_Voicing")
        9 => Some("Virama")
        10 => Some("CCC10")
        11 => Some("CCC11")
        12 => Some("CCC12")
        13 => Some("CCC13")
        14 => Some("CCC14")
        15 => Some("CCC15")
        16 => Some("CCC16")
        17 => Some("CCC17")
        18 => Some("CCC18")
        19 => Some("CCC19")
        20 => Some("CCC20")
        21 => Some("CCC21")
        22 => Some("CCC22")
        23 => Some("CCC23")
        24 => Some("CCC24")
        25 => Some("CCC25")
        26 => Some("CCC26")
        27 => Some("CCC27")
        28 => Some("CCC28")
        29 => Some("CCC29")
        30 => Some("CCC30")
        31 => Some("CCC31")
        32 => Some("CCC32")
        33 => Some("CCC33")
        34 => Some("CCC34")
        35 => Some("CCC35")
        36 => Some("CCC36")
        84 => Some("CCC84")
        91 => Some("CCC91")
        103 => Some("CCC103")
        107 => Some("CCC107")
        118 => Some("CCC118")
        122 => Some("CCC122")
        129 => Some("CCC129")
        130 => Some("CCC130")
        132 => Some("CCC132")
        133 => Some("CCC133")
        200 => Some("Attached_Below_Left")
        202 => Some("Attached_Below")
        214 => Some("Attached_Above")
        216 => Some("Attached_Above_Right")
        218 => Some("Below_Left")
        220 => Some("Below")
        222 => Some("Below_Right")
        224 => Some("Left")
        226 => Some("Right")
        228 => Some("Above_Left")
        230 => Some("Above")
        232 => Some("Above_Right")
        233 => Some("Double_Below")
        234 => Some("Double_Above")
        240 => Some("Iota_Subscript")
            _ => None
        }
    }

    canonical_combining_class_alias_count : U8 -> U8
    canonical_combining_class_alias_count = |value| {
        match value {
        0 => 3
        1 => 3
        6 => 3
        7 => 3
        8 => 3
        9 => 3
        10 => 2
        11 => 2
        12 => 2
        13 => 2
        14 => 2
        15 => 2
        16 => 2
        17 => 2
        18 => 2
        19 => 2
        20 => 2
        21 => 2
        22 => 2
        23 => 2
        24 => 2
        25 => 2
        26 => 2
        27 => 2
        28 => 2
        29 => 2
        30 => 2
        31 => 2
        32 => 2
        33 => 2
        34 => 2
        35 => 2
        36 => 2
        84 => 2
        91 => 2
        103 => 2
        107 => 2
        118 => 2
        122 => 2
        129 => 2
        130 => 2
        132 => 2
        133 => 2
        200 => 3
        202 => 3
        214 => 3
        216 => 3
        218 => 3
        220 => 3
        222 => 3
        224 => 3
        226 => 3
        228 => 3
        230 => 3
        232 => 3
        233 => 3
        234 => 3
        240 => 3
            _ => 0
        }
    }

    canonical_combining_class_alias_at : U8, U8 -> [Some(Str), None]
    canonical_combining_class_alias_at = |value, index| {
        match (value, index) {
        (0, 0) => Some("0")
        (0, 1) => Some("NR")
        (0, 2) => Some("Not_Reordered")
        (1, 0) => Some("1")
        (1, 1) => Some("OV")
        (1, 2) => Some("Overlay")
        (6, 0) => Some("6")
        (6, 1) => Some("HANR")
        (6, 2) => Some("Han_Reading")
        (7, 0) => Some("7")
        (7, 1) => Some("NK")
        (7, 2) => Some("Nukta")
        (8, 0) => Some("8")
        (8, 1) => Some("KV")
        (8, 2) => Some("Kana_Voicing")
        (9, 0) => Some("9")
        (9, 1) => Some("VR")
        (9, 2) => Some("Virama")
        (10, 0) => Some("10")
        (10, 1) => Some("CCC10")
        (11, 0) => Some("11")
        (11, 1) => Some("CCC11")
        (12, 0) => Some("12")
        (12, 1) => Some("CCC12")
        (13, 0) => Some("13")
        (13, 1) => Some("CCC13")
        (14, 0) => Some("14")
        (14, 1) => Some("CCC14")
        (15, 0) => Some("15")
        (15, 1) => Some("CCC15")
        (16, 0) => Some("16")
        (16, 1) => Some("CCC16")
        (17, 0) => Some("17")
        (17, 1) => Some("CCC17")
        (18, 0) => Some("18")
        (18, 1) => Some("CCC18")
        (19, 0) => Some("19")
        (19, 1) => Some("CCC19")
        (20, 0) => Some("20")
        (20, 1) => Some("CCC20")
        (21, 0) => Some("21")
        (21, 1) => Some("CCC21")
        (22, 0) => Some("22")
        (22, 1) => Some("CCC22")
        (23, 0) => Some("23")
        (23, 1) => Some("CCC23")
        (24, 0) => Some("24")
        (24, 1) => Some("CCC24")
        (25, 0) => Some("25")
        (25, 1) => Some("CCC25")
        (26, 0) => Some("26")
        (26, 1) => Some("CCC26")
        (27, 0) => Some("27")
        (27, 1) => Some("CCC27")
        (28, 0) => Some("28")
        (28, 1) => Some("CCC28")
        (29, 0) => Some("29")
        (29, 1) => Some("CCC29")
        (30, 0) => Some("30")
        (30, 1) => Some("CCC30")
        (31, 0) => Some("31")
        (31, 1) => Some("CCC31")
        (32, 0) => Some("32")
        (32, 1) => Some("CCC32")
        (33, 0) => Some("33")
        (33, 1) => Some("CCC33")
        (34, 0) => Some("34")
        (34, 1) => Some("CCC34")
        (35, 0) => Some("35")
        (35, 1) => Some("CCC35")
        (36, 0) => Some("36")
        (36, 1) => Some("CCC36")
        (84, 0) => Some("84")
        (84, 1) => Some("CCC84")
        (91, 0) => Some("91")
        (91, 1) => Some("CCC91")
        (103, 0) => Some("103")
        (103, 1) => Some("CCC103")
        (107, 0) => Some("107")
        (107, 1) => Some("CCC107")
        (118, 0) => Some("118")
        (118, 1) => Some("CCC118")
        (122, 0) => Some("122")
        (122, 1) => Some("CCC122")
        (129, 0) => Some("129")
        (129, 1) => Some("CCC129")
        (130, 0) => Some("130")
        (130, 1) => Some("CCC130")
        (132, 0) => Some("132")
        (132, 1) => Some("CCC132")
        (133, 0) => Some("133")
        (133, 1) => Some("CCC133")
        (200, 0) => Some("200")
        (200, 1) => Some("ATBL")
        (200, 2) => Some("Attached_Below_Left")
        (202, 0) => Some("202")
        (202, 1) => Some("ATB")
        (202, 2) => Some("Attached_Below")
        (214, 0) => Some("214")
        (214, 1) => Some("ATA")
        (214, 2) => Some("Attached_Above")
        (216, 0) => Some("216")
        (216, 1) => Some("ATAR")
        (216, 2) => Some("Attached_Above_Right")
        (218, 0) => Some("218")
        (218, 1) => Some("BL")
        (218, 2) => Some("Below_Left")
        (220, 0) => Some("220")
        (220, 1) => Some("B")
        (220, 2) => Some("Below")
        (222, 0) => Some("222")
        (222, 1) => Some("BR")
        (222, 2) => Some("Below_Right")
        (224, 0) => Some("224")
        (224, 1) => Some("L")
        (224, 2) => Some("Left")
        (226, 0) => Some("226")
        (226, 1) => Some("R")
        (226, 2) => Some("Right")
        (228, 0) => Some("228")
        (228, 1) => Some("AL")
        (228, 2) => Some("Above_Left")
        (230, 0) => Some("230")
        (230, 1) => Some("A")
        (230, 2) => Some("Above")
        (232, 0) => Some("232")
        (232, 1) => Some("AR")
        (232, 2) => Some("Above_Right")
        (233, 0) => Some("233")
        (233, 1) => Some("DB")
        (233, 2) => Some("Double_Below")
        (234, 0) => Some("234")
        (234, 1) => Some("DA")
        (234, 2) => Some("Double_Above")
        (240, 0) => Some("240")
        (240, 1) => Some("IS")
        (240, 2) => Some("Iota_Subscript")
            _ => None
        }
    }

    canonical_combining_class_parse : Str -> [Some(U8), None]
    canonical_combining_class_parse = |name|         if InternalLooseAlias.matches(name, "0") or InternalLooseAlias.matches(name, "NR") or InternalLooseAlias.matches(name, "Not_Reordered") Some(0) else         if InternalLooseAlias.matches(name, "1") or InternalLooseAlias.matches(name, "OV") or InternalLooseAlias.matches(name, "Overlay") Some(1) else         if InternalLooseAlias.matches(name, "6") or InternalLooseAlias.matches(name, "HANR") or InternalLooseAlias.matches(name, "Han_Reading") Some(6) else         if InternalLooseAlias.matches(name, "7") or InternalLooseAlias.matches(name, "NK") or InternalLooseAlias.matches(name, "Nukta") Some(7) else         if InternalLooseAlias.matches(name, "8") or InternalLooseAlias.matches(name, "KV") or InternalLooseAlias.matches(name, "Kana_Voicing") Some(8) else         if InternalLooseAlias.matches(name, "9") or InternalLooseAlias.matches(name, "VR") or InternalLooseAlias.matches(name, "Virama") Some(9) else         if InternalLooseAlias.matches(name, "10") or InternalLooseAlias.matches(name, "CCC10") Some(10) else         if InternalLooseAlias.matches(name, "11") or InternalLooseAlias.matches(name, "CCC11") Some(11) else         if InternalLooseAlias.matches(name, "12") or InternalLooseAlias.matches(name, "CCC12") Some(12) else         if InternalLooseAlias.matches(name, "13") or InternalLooseAlias.matches(name, "CCC13") Some(13) else         if InternalLooseAlias.matches(name, "14") or InternalLooseAlias.matches(name, "CCC14") Some(14) else         if InternalLooseAlias.matches(name, "15") or InternalLooseAlias.matches(name, "CCC15") Some(15) else         if InternalLooseAlias.matches(name, "16") or InternalLooseAlias.matches(name, "CCC16") Some(16) else         if InternalLooseAlias.matches(name, "17") or InternalLooseAlias.matches(name, "CCC17") Some(17) else         if InternalLooseAlias.matches(name, "18") or InternalLooseAlias.matches(name, "CCC18") Some(18) else         if InternalLooseAlias.matches(name, "19") or InternalLooseAlias.matches(name, "CCC19") Some(19) else         if InternalLooseAlias.matches(name, "20") or InternalLooseAlias.matches(name, "CCC20") Some(20) else         if InternalLooseAlias.matches(name, "21") or InternalLooseAlias.matches(name, "CCC21") Some(21) else         if InternalLooseAlias.matches(name, "22") or InternalLooseAlias.matches(name, "CCC22") Some(22) else         if InternalLooseAlias.matches(name, "23") or InternalLooseAlias.matches(name, "CCC23") Some(23) else         if InternalLooseAlias.matches(name, "24") or InternalLooseAlias.matches(name, "CCC24") Some(24) else         if InternalLooseAlias.matches(name, "25") or InternalLooseAlias.matches(name, "CCC25") Some(25) else         if InternalLooseAlias.matches(name, "26") or InternalLooseAlias.matches(name, "CCC26") Some(26) else         if InternalLooseAlias.matches(name, "27") or InternalLooseAlias.matches(name, "CCC27") Some(27) else         if InternalLooseAlias.matches(name, "28") or InternalLooseAlias.matches(name, "CCC28") Some(28) else         if InternalLooseAlias.matches(name, "29") or InternalLooseAlias.matches(name, "CCC29") Some(29) else         if InternalLooseAlias.matches(name, "30") or InternalLooseAlias.matches(name, "CCC30") Some(30) else         if InternalLooseAlias.matches(name, "31") or InternalLooseAlias.matches(name, "CCC31") Some(31) else         if InternalLooseAlias.matches(name, "32") or InternalLooseAlias.matches(name, "CCC32") Some(32) else         if InternalLooseAlias.matches(name, "33") or InternalLooseAlias.matches(name, "CCC33") Some(33) else         if InternalLooseAlias.matches(name, "34") or InternalLooseAlias.matches(name, "CCC34") Some(34) else         if InternalLooseAlias.matches(name, "35") or InternalLooseAlias.matches(name, "CCC35") Some(35) else         if InternalLooseAlias.matches(name, "36") or InternalLooseAlias.matches(name, "CCC36") Some(36) else         if InternalLooseAlias.matches(name, "84") or InternalLooseAlias.matches(name, "CCC84") Some(84) else         if InternalLooseAlias.matches(name, "91") or InternalLooseAlias.matches(name, "CCC91") Some(91) else         if InternalLooseAlias.matches(name, "103") or InternalLooseAlias.matches(name, "CCC103") Some(103) else         if InternalLooseAlias.matches(name, "107") or InternalLooseAlias.matches(name, "CCC107") Some(107) else         if InternalLooseAlias.matches(name, "118") or InternalLooseAlias.matches(name, "CCC118") Some(118) else         if InternalLooseAlias.matches(name, "122") or InternalLooseAlias.matches(name, "CCC122") Some(122) else         if InternalLooseAlias.matches(name, "129") or InternalLooseAlias.matches(name, "CCC129") Some(129) else         if InternalLooseAlias.matches(name, "130") or InternalLooseAlias.matches(name, "CCC130") Some(130) else         if InternalLooseAlias.matches(name, "132") or InternalLooseAlias.matches(name, "CCC132") Some(132) else         if InternalLooseAlias.matches(name, "133") or InternalLooseAlias.matches(name, "CCC133") Some(133) else         if InternalLooseAlias.matches(name, "200") or InternalLooseAlias.matches(name, "ATBL") or InternalLooseAlias.matches(name, "Attached_Below_Left") Some(200) else         if InternalLooseAlias.matches(name, "202") or InternalLooseAlias.matches(name, "ATB") or InternalLooseAlias.matches(name, "Attached_Below") Some(202) else         if InternalLooseAlias.matches(name, "214") or InternalLooseAlias.matches(name, "ATA") or InternalLooseAlias.matches(name, "Attached_Above") Some(214) else         if InternalLooseAlias.matches(name, "216") or InternalLooseAlias.matches(name, "ATAR") or InternalLooseAlias.matches(name, "Attached_Above_Right") Some(216) else         if InternalLooseAlias.matches(name, "218") or InternalLooseAlias.matches(name, "BL") or InternalLooseAlias.matches(name, "Below_Left") Some(218) else         if InternalLooseAlias.matches(name, "220") or InternalLooseAlias.matches(name, "B") or InternalLooseAlias.matches(name, "Below") Some(220) else         if InternalLooseAlias.matches(name, "222") or InternalLooseAlias.matches(name, "BR") or InternalLooseAlias.matches(name, "Below_Right") Some(222) else         if InternalLooseAlias.matches(name, "224") or InternalLooseAlias.matches(name, "L") or InternalLooseAlias.matches(name, "Left") Some(224) else         if InternalLooseAlias.matches(name, "226") or InternalLooseAlias.matches(name, "R") or InternalLooseAlias.matches(name, "Right") Some(226) else         if InternalLooseAlias.matches(name, "228") or InternalLooseAlias.matches(name, "AL") or InternalLooseAlias.matches(name, "Above_Left") Some(228) else         if InternalLooseAlias.matches(name, "230") or InternalLooseAlias.matches(name, "A") or InternalLooseAlias.matches(name, "Above") Some(230) else         if InternalLooseAlias.matches(name, "232") or InternalLooseAlias.matches(name, "AR") or InternalLooseAlias.matches(name, "Above_Right") Some(232) else         if InternalLooseAlias.matches(name, "233") or InternalLooseAlias.matches(name, "DB") or InternalLooseAlias.matches(name, "Double_Below") Some(233) else         if InternalLooseAlias.matches(name, "234") or InternalLooseAlias.matches(name, "DA") or InternalLooseAlias.matches(name, "Double_Above") Some(234) else         if InternalLooseAlias.matches(name, "240") or InternalLooseAlias.matches(name, "IS") or InternalLooseAlias.matches(name, "Iota_Subscript") Some(240) else None
}
