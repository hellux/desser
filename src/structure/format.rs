use super::{Order, PrimType, Ptr, Val};
use std::num::Wrapping;

use std::io::{BufRead, Seek, SeekFrom};

pub fn eval<R: BufRead + Seek>(
    start: u64,
    pty: &PrimType,
    byte_order: Order,
    f: &mut R,
) -> Val {
    let bytes = read_bytes(start, pty.size(), byte_order, f);
    pty.eval(bytes.as_slice())
}

impl Ptr {
    pub fn eval<R: BufRead + Seek>(&self, f: &mut R) -> Val {
        let bytes =
            read_bytes(self.start, self.pty.size(), self.byte_order, f);
        self.pty.eval(bytes.as_slice())
    }
}

fn read_bytes<R: BufRead + Seek>(
    start: u64,
    size: u8,
    byte_order: Order,
    f: &mut R,
) -> Vec<u8> {
    let start_byte = start / 8;
    f.seek(SeekFrom::Start(start_byte)).unwrap();

    let size = size as u64;
    let end = start + size;
    let end_offset = end % 8;
    let end_byte = end / 8 + if end_offset > 0 { 1 } else { 0 };
    let size_bytes = (end_byte - start_byte) as usize;
    let mut buf = vec![0; size_bytes];
    f.read(buf.as_mut_slice()).unwrap();

    if end_offset > 0 {
        buf[size_bytes - 1] &= 0xff << (8 - end_offset);
    }

    // use big endian internally
    if byte_order == Order::LittleEndian {
        buf.reverse();
    }

    let start_offset = start % 8;
    if start_offset == 0 {
        buf
    } else {
        let size_aligned =
            (size / 8 + if size % 8 > 0 { 1 } else { 0 }) as usize;
        let buf_aligned = shl_vector_be(&buf, start_offset as usize);
        buf_aligned.into_iter().take(size_aligned).collect()
    }
}

// n < 8
fn shl_vector_be(v: &[u8], n: usize) -> Vec<u8> {
    let size = v.len();
    let mut shifted = vec![0; size];
    for i in 0..size {
        let left_bits = Wrapping(v[i]) << n;
        let right_bits = Wrapping(*v.get(i + 1).unwrap_or(&0)) >> (8 - n);
        shifted[i] = (left_bits | right_bits).0;
    }
    shifted
}

// n < 8
fn shr_vector_be(v: &[u8], n: usize) -> Vec<u8> {
    let size = v.len();
    let mut shifted = vec![0; size];
    for i in 0..size {
        let left_bits = if i > 0 {
            Wrapping(v[i - 1]) << (8 - n)
        } else {
            Wrapping(0)
        };
        let right_bits = Wrapping(v[i]) >> n;
        shifted[i] = (left_bits | right_bits).0;
    }
    shifted
}

impl PrimType {
    fn eval(&self, data: &[u8]) -> Val {
        match self {
            PrimType::Signed(n) => {
                let negative = (data[0] >> 7) == 1;
                let uint = PrimType::eval_uint(data, *n as usize);
                if negative {
                    -(!uint & (i128::MAX >> (128 - n))) as i64 - 1
                } else {
                    uint as i64
                }
            }
            PrimType::Unsigned(n) => {
                PrimType::eval_uint(data, *n as usize) as i64
            }
            PrimType::Float(_, _) => unimplemented!(),
            PrimType::BitVec(n) => {
                PrimType::eval_uint(data, *n as usize) as i64
            }
        }
    }

    fn eval_uint(mut data: &[u8], len: usize) -> i128 {
        let mut bytes = [0; 16];
        let n = data.len();
        // shift to lowest bits
        if len % 8 == 0 {
            for i in 0..n {
                bytes[i] = data[n - 1 - i];
            }
        } else {
            let right_aligned = shr_vector_be(data, 8 - (len % 8));
            for i in 0..n {
                bytes[i] = right_aligned[n - 1 - i];
            }
        }
        i128::from_le_bytes(bytes)
    }
}

#[cfg(test)]
mod test_format {
    use super::*;
    use std::io::Cursor;

    // big endian: 4278388720, -16578576
    // little endian: 4026991615, -267975681
    const D1: &[u8; 4] = &[0xff, 0x03, 0x07, 0xf0];
    const BE: Order = Order::BigEndian;
    const LE: Order = Order::LittleEndian;

    #[test]
    fn read_data() {
        let mut d1c = Cursor::new(D1);
        assert_eq!(read_bytes(8, 16, BE, &mut d1c), &[0x03, 0x07]);
        assert_eq!(read_bytes(2 * 8, 9, BE, &mut d1c), &[0x07, 0x80]);
        assert_eq!(read_bytes(4, 4, BE, &mut d1c), &[0xf0]);
        assert_eq!(read_bytes(20, 8, BE, &mut d1c), &[0x7f]);
        assert_eq!(read_bytes(0, 16, LE, &mut d1c), &[0x03, 0xff]);
        assert_eq!(read_bytes(4, 16, BE, &mut d1c), &[0xf0, 0x30]);
    }

    #[test]
    fn shift_vector() {
        assert_eq!(shl_vector_be(D1, 4), vec![0xf0, 0x30, 0x7f, 0x00]);
        assert_eq!(shl_vector_be(D1, 2), vec![0xfc, 0x0c, 0x1f, 0xc0]);
        assert_eq!(shl_vector_be(D1, 1), vec![0xfe, 0x06, 0x0f, 0xe0]);

        assert_eq!(shr_vector_be(D1, 4), vec![0x0f, 0xf0, 0x30, 0x7f]);
        assert_eq!(shr_vector_be(D1, 2), vec![0x3f, 0xc0, 0xc1, 0xfc]);
    }

    #[test]
    fn eval_uint() {
        assert_eq!(PrimType::eval_uint(D1, 32), 0xff0307f0);
        assert_eq!(PrimType::eval_uint(&[0xff, 0x30], 12), 0xff3);
    }

    #[test]
    fn eval() {
        assert_eq!(PrimType::Unsigned(32).eval(D1), 4278388720);
        assert_eq!(PrimType::Signed(32).eval(D1), -16578576);
    }
}
