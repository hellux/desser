use std::fmt;
use std::ops;

pub type BitPos = BitCount;
pub type BitSize = BitCount;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BitCount(u64);

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct BytePos(pub u64);
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct ByteSize(pub u64);

impl fmt::Display for BitCount {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:x}", self.0 / 8)?;
        if !self.byte_aligned() {
            write!(f, ":{}", self.bit_index())?;
        }
        Ok(())
    }
}

impl ops::Add<BitCount> for BitCount {
    type Output = BitCount;
    fn add(self, bits: BitCount) -> Self::Output {
        BitCount(self.0 + bits.0)
    }
}

impl ops::Sub<BitCount> for BitCount {
    type Output = BitCount;
    fn sub(self, bits: BitCount) -> Self::Output {
        BitCount(self.0 - bits.0)
    }
}

impl From<ByteSize> for BitCount {
    fn from(bytes: ByteSize) -> Self {
        BitCount(bytes.0 * 8)
    }
}

impl BitCount {
    pub fn new(magnitude: u64) -> Self {
        BitCount(magnitude)
    }

    pub fn origin() -> Self {
        BitCount(0)
    }

    pub fn byte_aligned(self) -> bool {
        self.0 % 8 == 0
    }

    pub fn bit_index(self) -> u64 {
        self.0 % 8
    }

    pub fn align(self, al: BitCount) -> Self {
        if self.0 % al.0 > 0 {
            BitCount(self.0 + al.0 - self.0 % al.0)
        } else {
            self
        }
    }
}

impl From<BitCount> for BytePos {
    fn from(bits: BitCount) -> Self {
        BytePos(bits.0 / 8)
    }
}

impl From<BitCount> for ByteSize {
    fn from(bits: BitCount) -> Self {
        ByteSize(bits.0 / 8 + if bits.0 % 8 > 0 { 1 } else { 0 })
    }
}

impl ops::Sub<BytePos> for BytePos {
    type Output = ByteSize;
    fn sub(self, bytes: BytePos) -> Self::Output {
        ByteSize(self.0 - bytes.0)
    }
}

impl ByteSize {
    pub fn size(self) -> usize {
        self.0 as usize
    }
}

impl BytePos {
    pub fn size(self) -> usize {
        self.0 as usize
    }
}
