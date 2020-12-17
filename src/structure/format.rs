use super::{Order, PrimType, Ptr, Val};
use std::num::Wrapping;

use std::io::{BufRead, Seek, SeekFrom};

pub fn eval_size<R: BufRead + Seek>(
    start: u64,
    pty: &PrimType,
    byte_order: Order,
    f: &mut R,
) -> Val {
    let bytes = read_bytes(start, pty.size(), byte_order, f);
    pty.eval_size(bytes.as_slice())
}

impl Ptr {
    pub fn eval_size<R: BufRead + Seek>(&self, f: &mut R) -> Val {
        let bytes =
            read_bytes(self.start, self.pty.size(), self.byte_order, f);
        self.pty.eval_size(bytes.as_slice())
    }
}

pub fn read_bytes<R: BufRead + Seek>(
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

    // use little endian internally
    if byte_order == Order::BigEndian {
        buf.reverse();
    }

    // shift value bits to rightmost side, e.g "[?xxx|xx??]" -> "[???x|xxxx]"
    let start_offset = start % 8;
    if start_offset == 0 {
        buf
    } else {
        let size_aligned =
            (size / 8 + if size % 8 > 0 { 1 } else { 0 }) as usize;
        let buf_aligned = le_shr(&buf, start_offset as usize);
        buf_aligned
            .into_iter()
            .skip(size_bytes - size_aligned)
            .collect()
    }
}

fn le_shr(v: &[u8], n: usize) -> Vec<u8> {
    let size = v.len();
    if size == 1 {
        v.iter().map(|c| c >> n).collect()
    } else if size <= 8 {
        let shifted: u64 = le8_to_uint(v) >> n;
        let shifted_bytes = u64::to_le_bytes(shifted);
        shifted_bytes.iter().map(|c| *c).take(v.len()).collect()
    } else {
        let shifted: u128 = le16_to_uint(v) >> n;
        let shifted_bytes = u128::to_le_bytes(shifted);
        shifted_bytes.iter().map(|c| *c).take(v.len()).collect()
    }
}

fn le16_to_uint(data: &[u8]) -> u128 {
    let n = data.len();
    let mut bytes = [0; 16];
    for i in 0..n {
        bytes[i] = data[i];
    }
    u128::from_le_bytes(bytes)
}

fn le8_to_uint(data: &[u8]) -> u64 {
    let n = data.len();
    let mut bytes = [0; 8];
    for i in 0..n {
        bytes[i] = data[i];
    }
    u64::from_le_bytes(bytes)
}

impl PrimType {
    fn eval_size(&self, data: &[u8]) -> Val {
        match self {
            PrimType::Signed(n) => {
                let negative = (data[0] >> 7) == 1; // TODO sometimes incorrect
                let uint = le8_to_uint(data) as i64;
                if negative {
                    -(!uint & (i64::MAX >> (64 - n))) as i64 - 1
                } else {
                    uint as i64
                }
            }
            PrimType::Unsigned(n) => le8_to_uint(data) as i64,
            PrimType::BitVec(n) => le8_to_uint(data) as i64,
            PrimType::Float(_, _) => panic!(),
        }
    }
}

impl PrimType {
    pub fn fmt(&self, data: &[u8]) -> String {
        match self {
            PrimType::Unsigned(_) => format!("{}", le16_to_uint(data)),
            PrimType::Signed(n) => todo!(),
            PrimType::Float(e, m) => {
                let mut uint = le16_to_uint(data);
                let mantissa = uint & (u128::MAX >> (128 - m));
                uint >>= m;
                let exponent = uint & (u128::MAX >> (128 - e));
                uint >>= e;
                let sign = uint & 1;

                let exponent = exponent as f64 / (2 >> m) as f64;
                let val = mantissa as f64
                    * 2.0_f64.powf(exponent)
                    * if sign == 1 { -1.0 } else { 1.0 };
                format!("{}", val)
            }
            PrimType::BitVec(n) => {
                let mut s = String::new();
                for i in 0..*n {
                    let bit = (data[i as usize / 8] >> (7 - (i % 8))) & 1;
                    s.push(if bit == 0 { '0' } else { '1' });
                }
                s
            }
        }
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
        assert_eq!(read_bytes(8, 16, BE, &mut d1c), &[0x07, 0x03]);
        assert_eq!(read_bytes(2 * 8, 9, BE, &mut d1c), &[0x80, 0x07]);
        assert_eq!(read_bytes(4, 4, BE, &mut d1c), &[0x0f]);
        assert_eq!(read_bytes(20, 12, BE, &mut d1c), &[0x7f, 0]);
        assert_eq!(read_bytes(0, 16, LE, &mut d1c), &[0xff, 0x03]);
    }

    #[test]
    fn shift_vector() {
        dbg!(Wrapping(0xab_u8) << 4 | Wrapping(0xcd_u8) >> 4);
        assert_eq!(le_shr(D1, 4), vec![0x3f, 0x70, 0x00, 0x0f]);
        assert_eq!(le_shr(D1, 2), vec![0xff, 0xc0, 0x01, 0x3c]);
    }

    #[test]
    fn eval_uint() {
        assert_eq!(le8_to_uint(D1), 0xf00703ff);
        assert_eq!(le8_to_uint(&[0x0f, 0x03]), 0x030f);
    }

    #[test]
    fn eval() {
        assert_eq!(PrimType::Unsigned(32).eval_size(D1), 0xf00703ff);
        assert_eq!(PrimType::Signed(32).eval_size(D1), -267975681);
    }
}
