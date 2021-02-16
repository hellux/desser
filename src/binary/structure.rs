use super::scope::{NameArray, NameField, NameStruct};
use super::*;
use std::convert::{TryFrom, TryInto};

#[derive(Debug, Clone)]
pub enum StructField {
    Prim(Ptr),
    Array(Array),
    Struct(Struct),
}

#[derive(Debug, Clone)]
pub struct Array {
    pub start: BitPos,
    pub size: BitSize,
    pub elements: Vec<(usize, StructField)>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub start: BitPos,
    pub size: BitSize,
    pub fields: Vec<(Sym, StructField)>,
}

impl StructField {
    pub fn is_leaf(&self) -> bool {
        match self {
            StructField::Prim(_) => true,
            StructField::Array(Array { elements, .. }) => {
                if let Some(StructField::Prim(ptr)) =
                    elements.get(0).map(|e| &e.1)
                {
                    matches!(ptr.pty, PrimType::Char)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn start(&self) -> BitPos {
        match self {
            StructField::Prim(ptr) => ptr.start,
            StructField::Array(arr) => arr.start,
            StructField::Struct(st) => st.start,
        }
    }

    pub fn size(&self) -> BitSize {
        match self {
            StructField::Prim(Ptr { pty, .. }) => pty.size(),
            StructField::Array(arr) => arr.size,
            StructField::Struct(st) => st.size,
        }
    }
}

impl TryFrom<NameField> for StructField {
    type Error = ();
    fn try_from(nf: NameField) -> Result<Self, Self::Error> {
        match nf {
            NameField::Prim(ptr) => Ok(StructField::Prim(ptr)),
            NameField::Struct(nst) => nst.try_into(),
            NameField::Array(narr) => narr.try_into(),
            NameField::Null(_) => Err(()),
        }
    }
}

impl TryFrom<NameStruct> for StructField {
    type Error = ();
    fn try_from(nst: NameStruct) -> Result<Self, Self::Error> {
        let mut fields: Vec<(Sym, StructField)> = nst
            .fields
            .0
            .into_iter()
            .filter_map(|(sym, f)| match f.try_into() {
                Ok(t) => Some((sym, t)),
                _ => None,
            })
            .collect();

        match fields.len() {
            0 => Err(()),
            1 => Ok(fields.remove(0).1),
            _ => {
                fields.sort_by_key(|(_, f)| f.start());

                Ok(StructField::Struct(Struct {
                    start: nst.start,
                    size: nst.size,
                    fields,
                }))
            }
        }
    }
}

impl TryFrom<NameArray> for StructField {
    type Error = ();
    fn try_from(narr: NameArray) -> Result<Self, Self::Error> {
        let elements = narr
            .elements
            .into_iter()
            .enumerate()
            .filter_map(|(i, e)| match e.try_into() {
                Ok(oe) => Some((i, oe)),
                _ => None,
            })
            .collect();

        Ok(StructField::Array(Array {
            start: narr.start,
            size: narr.size,
            elements,
        }))
    }
}
