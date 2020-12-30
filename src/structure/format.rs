use std::convert::TryInto;
use std::io;
use std::io::{BufRead, Seek, SeekFrom};

use super::{Order, PrimType, Ptr, Val};

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

    // use little endian internally
    if byte_order == Order::BigEndian {
        buf.reverse();
    }

    // shift value bits to rightmost side, e.g "[?xxx|xx??]" -> "[???x|xxxx]"
    if end_offset == 0 {
        buf
    } else {
        let size_aligned =
            (size / 8 + if size % 8 > 0 { 1 } else { 0 }) as usize;
        let buf_aligned = le_shr(&buf, 8 - end_offset as usize);
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
    let mut bytes = [0; 16];
    for i in 0..data.len() {
        bytes[i] = data[i];
    }
    u128::from_le_bytes(bytes)
}

fn le8_to_uint(data: &[u8]) -> u64 {
    let mut bytes = [0; 8];
    for i in 0..data.len() {
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
            PrimType::Unsigned(_) => le8_to_uint(data) as i64,
            PrimType::BitVec(_) => le8_to_uint(data) as i64,
            PrimType::Char => {
                u8::from_le_bytes(data.try_into().unwrap()) as Val
            }
            PrimType::U8 => u8::from_le_bytes(data.try_into().unwrap()) as Val,
            PrimType::S8 => i8::from_le_bytes(data.try_into().unwrap()) as Val,
            PrimType::U16 => {
                u16::from_le_bytes(data.try_into().unwrap()) as Val
            }
            PrimType::S16 => {
                i16::from_le_bytes(data.try_into().unwrap()) as Val
            }
            PrimType::U32 => {
                u32::from_le_bytes(data.try_into().unwrap()) as Val
            }
            PrimType::S32 => {
                i32::from_le_bytes(data.try_into().unwrap()) as Val
            }
            PrimType::U64 => {
                u64::from_le_bytes(data.try_into().unwrap()) as Val
            }
            PrimType::S64 => {
                i64::from_le_bytes(data.try_into().unwrap()) as Val
            }
            PrimType::U128 => {
                u128::from_le_bytes(data.try_into().unwrap()) as Val
            }
            PrimType::S128 => {
                i128::from_le_bytes(data.try_into().unwrap()) as Val
            }
            _ => panic!(),
        }
    }
}

impl PrimType {
    pub fn fmt<W: io::Write>(
        &self,
        out: &mut W,
        data: &[u8],
    ) -> io::Result<()> {
        match self {
            PrimType::Unsigned(_) => write!(out, "{}", le16_to_uint(data)),
            PrimType::Signed(_) => todo!(),
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
                write!(out, "{}", val)
            }
            PrimType::BitVec(n) => {
                write!(out, "{:0n$b}", le16_to_uint(data), n = *n as usize)
            }
            PrimType::Char => {
                write!(out, "{}", data[0] as char)
            }
            PrimType::U8
            | PrimType::S8
            | PrimType::U16
            | PrimType::S16
            | PrimType::U32
            | PrimType::S32
            | PrimType::U64
            | PrimType::S64
            | PrimType::U128
            | PrimType::S128 => write!(out, "{}", self.eval_size(data)),
            PrimType::F32 => {
                write!(out, "{}", f32::from_le_bytes(data.try_into().unwrap()))
            }
            PrimType::F64 => {
                write!(out, "{}", f64::from_le_bytes(data.try_into().unwrap()))
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
        assert_eq!(read_bytes(2 * 8, 9, BE, &mut d1c), &[0x0f, 0x00]);
        assert_eq!(read_bytes(8 + 4, 4, BE, &mut d1c), &[0x03]);
        assert_eq!(read_bytes(2 * 8 + 4, 12, BE, &mut d1c), &[0xf0, 0x07]);
        assert_eq!(read_bytes(0, 16, LE, &mut d1c), &[0xff, 0x03]);
    }

    #[test]
    fn shift_vector() {
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
