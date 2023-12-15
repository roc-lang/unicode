# Work in progress!

Someday this will be a useful collection of Unicode operations, but for right now it's extremely WIP.

## Text Segmentation `Str -> List Str`

### General Process
1. Convert to `List U8`
2. Process bytes into code points 
3. Process code points into respective [GraphemeBreakProperty](https://www.unicode.org/Public/UCD/latest/ucd/auxiliary/GraphemeBreakProperty.txt) e.g. U+000D -> CR, \u(11000) -> SpacingMark, \u(AC00) -> LV
4. Apply the [Graphmene Boundary](https://www.unicode.org/reports/tr29/#Grapheme_Cluster_Boundary_Rules) rules e.g. [utf8proc implementation](https://github.com/JuliaStrings/utf8proc/blob/1cb28a66ca79a0845e99433fd1056257456cef8b/utf8proc.c#L261) or [this blog](https://halt.software/optimizing-unicodes-grapheme-cluster-break-algorithm/) ** note I like the running state version with a table lookup, might need a `List.getUnchecked` to be efficient?? 
5. Run through code points to get break indexes, map using `List.subList`

## Other Ideas
- This [online unicode tool](https://util.unicode.org/UnicodeJsps/breaks.jsp) looks helpful for debugging
- Write a script to parse the `.txt` files into useful `.roc` files... e.g. the [Tests](https://www.unicode.org/reports/tr41/tr41-32.html#Tests29) could be auto-generated to test our implementation of boundaries. Note notation `÷` means Break, `×` means Don't Break
- Can apply similar process for Words and Sentence boundaries