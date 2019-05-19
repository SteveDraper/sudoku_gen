pub trait BitSet {
    fn max_card() -> u32;

    fn empty() -> Self;

    fn singleton(i: u32) -> Self;

    fn card(&self) -> u32;

    fn is_empty(&self) -> bool {
        self.card() == 0
    }

    fn insert(&mut self, value: u32) -> Self;

    fn remove(&mut self, value: u32) -> Self;

    fn has_member(&self, value: u32) -> bool;

    fn union(&self, other: &Self) -> Self;

    fn intersection(&self, other: &Self) -> Self;

    fn add(&mut self, other: &Self);

    fn intersect_with(&mut self, other: &Self);
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct  BitSet32(u32);

#[inline(always)]
fn encoding(bs: &BitSet32) -> u32 {
    let BitSet32(code) = bs;
    *code
}

impl BitSet for BitSet32 {
    fn max_card() -> u32 {
        32
    }

    fn empty() -> BitSet32 {
        BitSet32(0)
    }

    fn singleton(i: u32) -> BitSet32 {
        if i >= Self::max_card() {
            panic!(format!("Illegal index to BitSet32: {}", i))
        }
        BitSet32(1 << i)
    }

    #[inline(always)]
    fn card(&self) -> u32 {
        let mut result: u32 = 0;
        let mut val = encoding(self);
        while val > 0 {
            if val % 2 != 0 {
                result = result + 1;
            }
            val = val >> 1;
        }
        result
    }

    fn is_empty(&self) -> bool {
        encoding(self) == 0
    }

    #[inline(always)]
    fn insert(&mut self, value: u32) -> BitSet32 {
        let BitSet32(code) = self;
        *code = *code | (1 << value);
        *self
    }

    #[inline(always)]
    fn remove(&mut self, value: u32) -> BitSet32 {
        let BitSet32(code) = self;
        *code = *code & !(1 << value);
        *self
    }

    #[inline(always)]
    fn has_member(&self, value: u32) -> bool {
        if value > 31 {
            false
        }
        else {
            (encoding(self) & (1 << value)) != 0
        }
    }

    fn union(&self, other: &Self) -> BitSet32 {
        BitSet32(encoding(self) | encoding(other))
    }

    #[inline(always)]
    fn intersection(&self, other: &Self) -> BitSet32 {
        BitSet32(encoding(self) & encoding(other))
    }

    fn add(&mut self, other: &Self) {
        let BitSet32(code) = self;
        *code = *code | encoding(other);
    }

    fn intersect_with(&mut self, other: &Self) {
        let BitSet32(code) = self;
        *code = *code & encoding(other);
    }
}

pub struct BitSetIter32 {
    code: u32,
    cur: u32
}

impl Iterator for BitSetIter32 {
    type Item = u32;

    #[inline(always)]
    fn next(&mut self) -> Option<u32> {
        loop {
            if self.code == 0 {
                break;
            } else {
                let present = (self.code & 1) != 0;
                self.cur = self.cur + 1;
                self.code = self.code >> 1;
                if present {
                    return Some(self.cur - 1);
                }
            }
        }

        None
    }
}

impl BitSet32 {
    #[inline(always)]
    pub fn iter(& self) -> BitSetIter32 {
        BitSetIter32 {
            code: encoding(self),
            cur: 0
        }
    }
}

#[cfg(test)]
mod tests {
    // Note this useful idiom: importing names from outer (for mod tests) scope.
    use super::*;

    #[test]
    fn test_card32() {
        assert!(BitSet32::max_card() == 32);
    }

    #[test]
    fn test_empty() {
        assert!(BitSet32::empty().is_empty());
        assert_eq!(BitSet32::empty().card(), 0);
    }

    #[test]
    fn test_singleton() {
        let bs = BitSet32::singleton(0);
        let bs2 = BitSet32::singleton(5);
        assert_eq!(bs.card(), 1);
        assert_eq!(bs2.card(), 1);
    }

    #[test]
    fn test_insert() {
        let mut bs = BitSet32::empty();

        bs.insert(5);

        assert!(bs.has_member(5));
    }

    #[test]
    fn test_remove() {
        let mut bs = BitSet32::empty();

        bs.insert(5);
        bs.insert(6);
        bs.remove(5);

        assert!(!bs.has_member(5));
        assert!(bs.has_member(6));
    }

    #[test]
    fn test_union() {
        let bs = BitSet32::singleton(0);
        let bs2 = BitSet32::singleton(5);
        let union = bs.union(&bs2);
        assert_eq!(union.card(), 2);

        let union2 = union.union(&bs);
        assert_eq!(union2, union);
    }

    #[test]
    fn test_membership() {
        let bs = BitSet32::singleton(5);
        assert!(bs.has_member(5));
        assert!(!bs.has_member(4));
        assert!(!bs.has_member(32));
    }

    #[test]
    fn test_intersection() {
        let bs = BitSet32::singleton(0);
        let bs2 = BitSet32::singleton(5);
        let intersection = bs.intersection(&bs2);
        assert_eq!(intersection.card(), 0);
    }

    #[test]
    fn test_add() {
        let mut bs = BitSet32::singleton(0);
        let bs2 = BitSet32::singleton(5);
        let union = bs.union(&bs2);
        bs.add(&bs2);

        assert_eq!(bs, union);
    }

    #[test]
    fn test_intersect_with() {
        let mut bs = BitSet32::singleton(0);
        let bs2 = BitSet32::singleton(5);
        let intersection = bs.intersection(&bs2);
        bs.intersect_with(&bs2);

        assert_eq!(bs, intersection);
    }

    #[test]
    fn test_iterate() {
        let mut bs = BitSet32::empty();

        bs.insert(5);
        bs.insert(6);

        let mut count = 0;
        for x in bs.iter() {
            if count == 0 {
                assert_eq!(x, 5);
            }
            else if count == 1 {
                assert_eq!(x, 6);
            }
            else {
                assert!(false);
            }
            count = count + 1;
        }
        assert_eq!(count, 2);
    }

    #[should_panic]
    #[test]
    fn test_bad_singleton() {
        let _bs = BitSet32::singleton(32);
    }
}