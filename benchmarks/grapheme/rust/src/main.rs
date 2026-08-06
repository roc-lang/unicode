use std::hint::black_box;
use std::io::{self, Read};
use unicode_segmentation::UnicodeSegmentation;

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let repeats: u64 = input[..8].parse().expect("invalid repeat count");
    assert_eq!(input.as_bytes()[8], b'\n');
    let source = &input[9..];

    if repeats == 0 {
        let mut count = 0u64;
        let mut sum_ends = 0u64;
        let mut weighted_ends = 0u64;
        let mut end = 0u64;
        for grapheme in source.graphemes(true) {
            count += 1;
            end += grapheme.len() as u64;
            sum_ends += end;
            weighted_ends += count * end;
        }
        println!("{count} {sum_ends} {weighted_ends}");
        return;
    }

    let mut total = 0u64;
    for _ in 0..repeats {
        total = total.wrapping_add(black_box(source).graphemes(true).count() as u64);
    }
    println!("{}", black_box(total));
}
