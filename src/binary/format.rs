use std::convert::TryInto;
use std::io;
use std::io::{Read, Seek, SeekFrom};

use super::bits::*;
use super::eval::{FloatVal, IntVal, Val};
use super::{Order, PrimType, Ptr};

impl Ptr {
    pub fn eval<R: Read + Seek>(&self, f: &mut R) -> Val {
        let bytes =
            read_bytes(self.start, self.pty.size(), self.byte_order, f);
        self.pty.eval(bytes.as_slice())
    }
}

pub fn read_bytes<R: Read + Seek>(
    start: BitPos,
    size: BitSize,
    byte_order: Order,
    f: &mut R,
) -> Vec<u8> {
    let start_byte: BytePos = start.into();
    f.seek(SeekFrom::Start(start_byte.0)).unwrap();

    let end = start + size;
    let end_byte: BytePos = end.into();
    let size_bytes = end_byte - start_byte;
    let mut buf = vec![0; size_bytes.size()];
    f.read_exact(buf.as_mut_slice()).unwrap();

    // use little endian internally
    if byte_order == Order::BigEndian {
        buf.reverse();
    }

    // shift value bits to rightmost side, e.g "[?xxx|xx??]" -> "[???x|xxxx]"
    if end.byte_aligned() {
        buf
    } else {
        let size_aligned: ByteSize = size.into();
        let buf_aligned = le_shr(&buf, 8 - end.bit_index() as usize);
        buf_aligned
            .into_iter()
            .skip(size_bytes.size() - size_aligned.size())
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
        shifted_bytes.iter().cloned().take(size).collect()
    } else {
        let shifts = size / 8 + if size % 8 > 0 { 1 } else { 0 };
        let mut buf = Vec::with_capacity(size);
        for _ in 0..shifts {
            let shifted: u64 = le8_to_uint(v) >> n;
            buf.extend(u64::to_le_bytes(shifted).iter());
        }
        buf.into_iter().take(size).collect()
    }
}

fn le8_to_uint(data: &[u8]) -> u64 {
    let mut bytes = [0; 8];
    bytes[..data.len()].clone_from_slice(&data[..]);
    u64::from_le_bytes(bytes)
}

impl PrimType {
    fn eval(&self, data: &[u8]) -> Val {
        match self {
            PrimType::BitVec(_) => Val::Integer(le8_to_uint(data) as IntVal),
            PrimType::Char => Val::Integer(u8::from_le_bytes(
                data.try_into().unwrap(),
            ) as IntVal),
            PrimType::U8 => Val::Integer(u8::from_le_bytes(
                data.try_into().unwrap(),
            ) as IntVal),
            PrimType::I8 => Val::Integer(i8::from_le_bytes(
                data.try_into().unwrap(),
            ) as IntVal),
            PrimType::U16 => Val::Integer(u16::from_le_bytes(
                data.try_into().unwrap(),
            ) as IntVal),
            PrimType::I16 => Val::Integer(i16::from_le_bytes(
                data.try_into().unwrap(),
            ) as IntVal),
            PrimType::U32 => Val::Integer(u32::from_le_bytes(
                data.try_into().unwrap(),
            ) as IntVal),
            PrimType::I32 => Val::Integer(i32::from_le_bytes(
                data.try_into().unwrap(),
            ) as IntVal),
            PrimType::U64 => Val::Integer(u64::from_le_bytes(
                data.try_into().unwrap(),
            ) as IntVal),
            PrimType::I64 => Val::Integer(i64::from_le_bytes(
                data.try_into().unwrap(),
            ) as IntVal),
            PrimType::F32 => Val::Float(f32::from_le_bytes(
                data.try_into().unwrap(),
            ) as FloatVal),
            PrimType::F64 => Val::Float(f64::from_le_bytes(
                data.try_into().unwrap(),
            ) as FloatVal),
        }
    }

    pub fn fmt<W: io::Write>(
        &self,
        out: &mut W,
        data: &[u8],
    ) -> io::Result<()> {
        match self {
            PrimType::BitVec(n) => {
                write!(out, "{:0n$b}", le8_to_uint(data), n = *n as usize)
            }
            PrimType::Char => write!(out, "{}", data[0] as char),
            PrimType::U8
            | PrimType::I8
            | PrimType::U16
            | PrimType::I16
            | PrimType::U32
            | PrimType::I32
            | PrimType::U64
            | PrimType::I64
            | PrimType::F32
            | PrimType::F64 => match self.eval(data) {
                Val::Integer(i) => write!(out, "{}", i),
                Val::Float(f) => write!(out, "{}", f),
                _ => unreachable!(),
            },
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