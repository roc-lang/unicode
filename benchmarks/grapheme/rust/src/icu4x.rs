use icu_segmenter::GraphemeClusterSegmenter;
use std::hint::black_box;
use std::io::{self, Read};

fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).unwrap();
    let repeats: u64 = input[..8].parse().expect("invalid repeat count");
    assert_eq!(input.as_bytes()[8], b'\n');
    let source = &input[9..];
    let segmenter = GraphemeClusterSegmenter::new();

    if repeats == 0 {
        let mut count = 0u64;
        let mut sum_ends = 0u64;
        let mut weighted_ends = 0u64;
        for end in segmenter.segment_str(source).skip(1) {
            count += 1;
            let end = end as u64;
            sum_ends += end;
            weighted_ends += count * end;
        }
        println!("{count} {sum_ends} {weighted_ends}");
        return;
    }

    let mut total = 0u64;
    for _ in 0..repeats {
        let boundaries = black_box(&segmenter).segment_str(black_box(source)).count();
        total = total.wrapping_add(boundaries.saturating_sub(1) as u64);
    }
    println!("{}", black_box(total));
}
