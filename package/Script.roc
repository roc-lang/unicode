## GENERATED public Unicode 17.0.0 Script/Script_Extensions API from PropertyValueAliases.txt under UAX #24 revision 39. ##
## Run `python3 scripts/unicode_data.py generate`; representation IDs and bit ordering are private. ##
## ScriptSet traversal and comparison use canonical short-alias lexicographic order. ##

import InternalScriptData
import InternalScriptExtensionsData
import Scalar

## Normative Unicode Script and Script_Extensions scalar properties.
##
## Script is not a block, language, direction, font, or security classification.
## Common, Inherited, and Unknown are real property values. Script_Extensions
## is always nonempty: absent override data means the singleton Script(cp).
Script :: [].{
    Value := [Adlm, Aghb, Ahom, Arab, Armi, Armn, Avst, Bali, Bamu, Bass, Batk, Beng, Berf, Bhks, Bopo, Brah, Brai, Bugi, Buhd, Cakm, Cans, Cari, Cham, Cher, Chrs, Copt, Cpmn, Cprt, Cyrl, Deva, Diak, Dogr, Dsrt, Dupl, Egyp, Elba, Elym, Ethi, Gara, Geor, Glag, Gong, Gonm, Goth, Gran, Grek, Gujr, Gukh, Guru, Hang, Hani, Hano, Hatr, Hebr, Hira, Hluw, Hmng, Hmnp, Hrkt, Hung, Ital, Java, Kali, Kana, Kawi, Khar, Khmr, Khoj, Kits, Knda, Krai, Kthi, Lana, Laoo, Latn, Lepc, Limb, Lina, Linb, Lisu, Lyci, Lydi, Mahj, Maka, Mand, Mani, Marc, Medf, Mend, Merc, Mero, Mlym, Modi, Mong, Mroo, Mtei, Mult, Mymr, Nagm, Nand, Narb, Nbat, Newa, Nkoo, Nshu, Ogam, Olck, Onao, Orkh, Orya, Osge, Osma, Ougr, Palm, Pauc, Perm, Phag, Phli, Phlp, Phnx, Plrd, Prti, Rjng, Rohg, Runr, Samr, Sarb, Saur, Sgnw, Shaw, Shrd, Sidd, Sidt, Sind, Sinh, Sogd, Sogo, Sora, Soyo, Sund, Sunu, Sylo, Syrc, Tagb, Takr, Tale, Talu, Taml, Tang, Tavt, Tayo, Telu, Tfng, Tglg, Thaa, Thai, Tibt, Tirh, Tnsa, Todr, Tols, Toto, Tutg, Ugar, Vaii, Vith, Wara, Wcho, Xpeo, Xsux, Yezi, Yiii, Zanb, Zinh, Zyyy, Zzzz].{ is_eq : _ }

    ScriptSet := { word0 : U64, word1 : U64, word2 : U64, length : U8 }

    of_scalar : Scalar -> Value
    of_scalar = |scalar| InternalScriptData.lookup(Scalar.to_u32(scalar))

    extensions_of_scalar : Scalar -> ScriptSet
    extensions_of_scalar = |scalar| {
        code_point = Scalar.to_u32(scalar)
        override_id = InternalScriptExtensionsData.lookup_override(code_point)
        if override_id == 0 { singleton(InternalScriptData.lookup(code_point)) } else {
            bits = InternalScriptExtensionsData.set_bits(override_id)
            { word0: bits.word0, word1: bits.word1, word2: bits.word2, length: bits.length }
        }
    }

    from_alias : Str -> Try(Value, [UnrecognizedScriptAlias])
    from_alias = |alias| match InternalScriptData.from_alias(alias) {
        Some(script) => Ok(script)
        None => Err(UnrecognizedScriptAlias)
    }

    short_alias : Value -> Str
    short_alias = |script| InternalScriptData.short_alias(script)

    long_alias : Value -> Str
    long_alias = |script| InternalScriptData.long_alias(script)

    alias_count : Value -> U8
    alias_count = |script| InternalScriptData.alias_count(script)

    alias_at : Value, U8 -> [Some(Str), None]
    alias_at = |script, index| InternalScriptData.alias_at(script, index)

    is_common : Value -> Bool
    is_common = |script| script == Zyyy

    is_inherited : Value -> Bool
    is_inherited = |script| script == Zinh

    is_unknown : Value -> Bool
    is_unknown = |script| script == Zzzz

    is_explicit : Value -> Bool
    is_explicit = |script| script != Zyyy and script != Zinh and script != Zzzz

    singleton : Value -> ScriptSet
    singleton = |script| {
        private_id = InternalScriptData.private_id(script)
        word = private_id / 64
        bit = 1.U64.shl_wrap(private_id % 64)
        match word {
            0 => { word0: bit, word1: 0, word2: 0, length: 1 }
            1 => { word0: 0, word1: bit, word2: 0, length: 1 }
            _ => { word0: 0, word1: 0, word2: bit, length: 1 }
        }
    }

    contains : ScriptSet, Value -> Bool
    contains = |set, script| {
        private_id = InternalScriptData.private_id(script)
        contains_private(set, private_id)
    }

    len : ScriptSet -> U64
    len = |set| set.length.to_u64()

    intersection : ScriptSet, ScriptSet -> [Some(ScriptSet), None]
    intersection = |left, right| {
        word0 = left.word0.bitwise_and(right.word0)
        word1 = left.word1.bitwise_and(right.word1)
        word2 = left.word2.bitwise_and(right.word2)
        length = U64.count_one_bits(word0) + U64.count_one_bits(word1) + U64.count_one_bits(word2)
        if length == 0 { None } else {
            Some({ word0, word1, word2, length })
        }
    }

    explicit_members : ScriptSet -> [Some(ScriptSet), None]
    explicit_members = |set| {
        without_common = remove(set, Zyyy)
        without_inherited = remove(without_common, Zinh)
        without_unknown = remove(without_inherited, Zzzz)
        if without_unknown.length == 0 { None } else { Some(without_unknown) }
    }

    is_eq_set : ScriptSet, ScriptSet -> Bool
    is_eq_set = |left, right| left.word0 == right.word0 and left.word1 == right.word1 and left.word2 == right.word2

    ## Lexicographic comparison in stable canonical short-alias order.
    compare : ScriptSet, ScriptSet -> [Before, Equal, After]
    compare = |left, right| {
        var private_id = 0.U8
        var left_seen = 0.U8
        var right_seen = 0.U8
        while private_id < 176 {
            left_has = contains_private(left, private_id)
            right_has = contains_private(right, private_id)
            if left_has and right_has {
                left_seen = left_seen + 1
                right_seen = right_seen + 1
            } else if left_has {
                return if right_seen == right.length { After } else { Before }
            } else if right_has {
                return if left_seen == left.length { Before } else { After }
            }
            private_id = private_id + 1
        }
        Equal
    }

    ## Member by stable canonical short-alias order.
    at : ScriptSet, U8 -> [Some(Value), None]
    at = |set, wanted| {
        if wanted >= set.length { return None }
        var private_id = 0.U8
        var seen = 0.U8
        while private_id < 176 {
            if contains_private(set, private_id) {
                if seen == wanted { return Some(InternalScriptData.from_private_id(private_id)) }
                seen = seen + 1
            }
            private_id = private_id + 1
        }
        None
    }

    ## Visit members in stable canonical short-alias order without allocating.
    walk : ScriptSet, state, (state, Value -> state) -> state
    walk = |set, initial, visit| {
        var state = initial
        var private_id = 0.U8
        while private_id < 176 {
            if contains_private(set, private_id) {
                state = visit(state, InternalScriptData.from_private_id(private_id))
            }
            private_id = private_id + 1
        }
        state
    }

    ## Materialize members in stable canonical short-alias order.
    to_list : ScriptSet -> List(Value)
    to_list = |set| walk(set, [], |scripts, script| scripts.append(script))
}

remove = |set, script| {
    private_id = InternalScriptData.private_id(script)
    bit = 1.U64.shl_wrap(private_id % 64)
    mask = bit.bitwise_not()
    if !contains_private(set, private_id) { set } else {
        match private_id / 64 {
            0 => { word0: set.word0.bitwise_and(mask), word1: set.word1, word2: set.word2, length: set.length - 1 }
            1 => { word0: set.word0, word1: set.word1.bitwise_and(mask), word2: set.word2, length: set.length - 1 }
            _ => { word0: set.word0, word1: set.word1, word2: set.word2.bitwise_and(mask), length: set.length - 1 }
        }
    }
}

contains_private = |set, private_id| {
    bit = 1.U64.shl_wrap(private_id % 64)
    word = match private_id / 64 {
        0 => set.word0
        1 => set.word1
        _ => set.word2
    }
    word.bitwise_and(bit) != 0
}
