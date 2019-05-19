extern crate time;
use time::PreciseTime;


pub fn timed<F: FnMut() -> bool>(f: &mut F) -> (u64, bool) {
    let start = PreciseTime::now();
    let result = f();
    let end = PreciseTime::now();
    (start.to(end).num_milliseconds() as u64, f())
}


#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn test_timed() {
        fn f() -> bool {
            true
        }

        assert_eq!(timed(&mut f).1, true);
        assert_eq!(timed(&mut || true).1, true);
    }
}
