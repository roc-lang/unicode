## GENERATED public Unicode 17.0.0 Script/Script_Extensions API from PropertyValueAliases.txt under UAX #24 revision 39. ##
## Run `python3 scripts/unicode_data.py generate`; representation IDs and bit ordering are private. ##

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
        primary = InternalScriptData.lookup(code_point)
        override_id = InternalScriptExtensionsData.lookup_override(code_point)
        if override_id == 0 { singleton(primary) } else {
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
        bit = 1.U64.shl_wrap(private_id % 64)
        word = match private_id / 64 {
            0 => set.word0
            1 => set.word1
            _ => set.word2
        }
        word.bitwise_and(bit) != 0
    }

    len : ScriptSet -> U64
    len = |set| set.length.to_u64()

    intersection : ScriptSet, ScriptSet -> [Some(ScriptSet), None]
    intersection = |left, right| {
        word0 = left.word0.bitwise_and(right.word0)
        word1 = left.word1.bitwise_and(right.word1)
        word2 = left.word2.bitwise_and(right.word2)
        length = bit_count(word0) + bit_count(word1) + bit_count(word2)
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

    compare : ScriptSet, ScriptSet -> [Before, Equal, After]
    compare = |left, right| {
        common = if left.length < right.length { left.length } else { right.length }
        var index = 0.U8
        while index < common {
            left_script = match at(left, index) { Some(value) => value None => Zzzz }
            right_script = match at(right, index) { Some(value) => value None => Zzzz }
            left_id = InternalScriptData.private_id(left_script)
            right_id = InternalScriptData.private_id(right_script)
            if left_id < right_id { return Before }
            if left_id > right_id { return After }
            index = index + 1
        }
        if left.length < right.length { Before } else if left.length > right.length { After } else { Equal }
    }

    at : ScriptSet, U8 -> [Some(Value), None]
    at = |set, wanted| {
        if wanted >= set.length { return None }
        var private_id = 0.U8
        var seen = 0.U8
        while private_id < 176 {
            script = InternalScriptData.from_private_id(private_id)
            if contains(set, script) {
                if seen == wanted { return Some(script) }
                seen = seen + 1
            }
            private_id = private_id + 1
        }
        None
    }

    walk : ScriptSet, state, (state, Value -> state) -> state
    walk = |set, initial, visit| {
        var state = initial
        var index = 0.U8
        while index < set.length {
            script = match at(set, index) { Some(value) => value None => Zzzz }
            state = visit(state, script)
            index = index + 1
        }
        state
    }

    to_list : ScriptSet -> List(Value)
    to_list = |set| walk(set, [], |scripts, script| scripts.append(script))
}

remove = |set, script| {
    private_id = InternalScriptData.private_id(script)
    bit = 1.U64.shl_wrap(private_id % 64)
    mask = bit.bitwise_not()
    if !Script.contains(set, script) { set } else {
        match private_id / 64 {
            0 => { word0: set.word0.bitwise_and(mask), word1: set.word1, word2: set.word2, length: set.length - 1 }
            1 => { word0: set.word0, word1: set.word1.bitwise_and(mask), word2: set.word2, length: set.length - 1 }
            _ => { word0: set.word0, word1: set.word1, word2: set.word2.bitwise_and(mask), length: set.length - 1 }
        }
    }
}

bit_count = |initial| {
    var value = initial
    var count = 0.U8
    while value != 0 {
        count = count + value.bitwise_and(1).to_u8_wrap()
        value = value.shr_wrap(1)
    }
    count
}
