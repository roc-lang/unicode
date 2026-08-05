## GENERATED from Unicode 17.0.0 PropertyAliases and PropertyValueAliases. Run `python3 scripts/unicode_data.py generate`. ##
## These canonical names are metadata for stable identities, never storage ordinals. ##
## metadata: 30 scalar category identities; 58 exact CCC identities. ##

import InternalGeneralCategory

InternalPropertyAliases :: [].{
    PropertyName : { short : Str, long : Str }

    general_category_property : PropertyName
    general_category_property = { short: "gc", long: "General_Category" }

    canonical_combining_class_property : PropertyName
    canonical_combining_class_property = { short: "ccc", long: "Canonical_Combining_Class" }

    general_category_aliases : InternalGeneralCategory.GeneralCategory -> List(Str)
    general_category_aliases = |category| {
        match category {
        Cc => ["Cc", "Control", "cntrl"]
        Cf => ["Cf", "Format"]
        Cn => ["Cn", "Unassigned"]
        Co => ["Co", "Private_Use"]
        Cs => ["Cs", "Surrogate"]
        Ll => ["Ll", "Lowercase_Letter"]
        Lm => ["Lm", "Modifier_Letter"]
        Lo => ["Lo", "Other_Letter"]
        Lt => ["Lt", "Titlecase_Letter"]
        Lu => ["Lu", "Uppercase_Letter"]
        Mc => ["Mc", "Spacing_Mark"]
        Me => ["Me", "Enclosing_Mark"]
        Mn => ["Mn", "Nonspacing_Mark"]
        Nd => ["Nd", "Decimal_Number", "digit"]
        Nl => ["Nl", "Letter_Number"]
        No => ["No", "Other_Number"]
        Pc => ["Pc", "Connector_Punctuation"]
        Pd => ["Pd", "Dash_Punctuation"]
        Pe => ["Pe", "Close_Punctuation"]
        Pf => ["Pf", "Final_Punctuation"]
        Pi => ["Pi", "Initial_Punctuation"]
        Po => ["Po", "Other_Punctuation"]
        Ps => ["Ps", "Open_Punctuation"]
        Sc => ["Sc", "Currency_Symbol"]
        Sk => ["Sk", "Modifier_Symbol"]
        Sm => ["Sm", "Math_Symbol"]
        So => ["So", "Other_Symbol"]
        Zl => ["Zl", "Line_Separator"]
        Zp => ["Zp", "Paragraph_Separator"]
        Zs => ["Zs", "Space_Separator"]
        }
    }

    canonical_combining_class_aliases : U8 -> List(Str)
    canonical_combining_class_aliases = |value| {
        match value {
        0 => ["0", "NR", "Not_Reordered"]
        1 => ["1", "OV", "Overlay"]
        6 => ["6", "HANR", "Han_Reading"]
        7 => ["7", "NK", "Nukta"]
        8 => ["8", "KV", "Kana_Voicing"]
        9 => ["9", "VR", "Virama"]
        10 => ["10", "CCC10"]
        11 => ["11", "CCC11"]
        12 => ["12", "CCC12"]
        13 => ["13", "CCC13"]
        14 => ["14", "CCC14"]
        15 => ["15", "CCC15"]
        16 => ["16", "CCC16"]
        17 => ["17", "CCC17"]
        18 => ["18", "CCC18"]
        19 => ["19", "CCC19"]
        20 => ["20", "CCC20"]
        21 => ["21", "CCC21"]
        22 => ["22", "CCC22"]
        23 => ["23", "CCC23"]
        24 => ["24", "CCC24"]
        25 => ["25", "CCC25"]
        26 => ["26", "CCC26"]
        27 => ["27", "CCC27"]
        28 => ["28", "CCC28"]
        29 => ["29", "CCC29"]
        30 => ["30", "CCC30"]
        31 => ["31", "CCC31"]
        32 => ["32", "CCC32"]
        33 => ["33", "CCC33"]
        34 => ["34", "CCC34"]
        35 => ["35", "CCC35"]
        36 => ["36", "CCC36"]
        84 => ["84", "CCC84"]
        91 => ["91", "CCC91"]
        103 => ["103", "CCC103"]
        107 => ["107", "CCC107"]
        118 => ["118", "CCC118"]
        122 => ["122", "CCC122"]
        129 => ["129", "CCC129"]
        130 => ["130", "CCC130"]
        132 => ["132", "CCC132"]
        133 => ["133", "CCC133"]
        200 => ["200", "ATBL", "Attached_Below_Left"]
        202 => ["202", "ATB", "Attached_Below"]
        214 => ["214", "ATA", "Attached_Above"]
        216 => ["216", "ATAR", "Attached_Above_Right"]
        218 => ["218", "BL", "Below_Left"]
        220 => ["220", "B", "Below"]
        222 => ["222", "BR", "Below_Right"]
        224 => ["224", "L", "Left"]
        226 => ["226", "R", "Right"]
        228 => ["228", "AL", "Above_Left"]
        230 => ["230", "A", "Above"]
        232 => ["232", "AR", "Above_Right"]
        233 => ["233", "DB", "Double_Below"]
        234 => ["234", "DA", "Double_Above"]
        240 => ["240", "IS", "Iota_Subscript"]
            _ => []
        }
    }
}
