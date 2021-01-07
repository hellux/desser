use super::scope::{Name, NameArray, NameStruct};
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
    pub start: u64,
    pub size: u64,
    pub elements: Vec<StructField>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub start: u64,
    pub size: u64,
    pub fields: Vec<(Sym, StructField)>,
}

impl StructField {
    pub fn is_leaf(&self) -> bool {
        match self {
            StructField::Prim(_) => true,
            StructField::Array(Array { elements, .. }) => {
                if let Some(StructField::Prim(ptr)) = elements.get(0) {
                    matches!(ptr.pty, PrimType::Char)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn start(&self) -> u64 {
        match self {
            StructField::Prim(ptr) => ptr.start,
            StructField::Array(arr) => arr.start,
            StructField::Struct(st) => st.start,
        }
    }

    pub fn size(&self) -> u64 {
        match self {
            StructField::Prim(Ptr { pty, .. }) => pty.size() as u64,
            StructField::Array(arr) => arr.size,
            StructField::Struct(st) => st.size,
        }
    }
}

impl<'n> TryFrom<Name<'n>> for StructField {
    type Error = ();
    fn try_from(name: Name<'n>) -> Result<Self, Self::Error> {
        match name {
            Name::Struct(nst) => nst.try_into(),
            Name::Array(narr) => narr.try_into(),
            Name::Field(ptr) => Ok(StructField::Prim(ptr)),
            _ => Err(()),
        }
    }
}

impl<'n> TryFrom<NameStruct<'n>> for StructField {
    type Error = ();
    fn try_from(nst: NameStruct<'n>) -> Result<Self, Self::Error> {
        let mut fields: Vec<(Sym, StructField)> = nst
            .fields
            .into_iter()
            .filter_map(|(sym, f)| match (sym, f.try_into()) {
                (s, Ok(t)) => Some((s, t)),
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

impl<'n> TryFrom<NameArray<'n>> for StructField {
    type Error = ();
    fn try_from(narr: NameArray<'n>) -> Result<Self, Self::Error> {
        let elements = narr
            .elements
            .into_iter()
            .filter_map(|e| e.try_into().ok())
            .collect();

        Ok(StructField::Array(Array {
            start: narr.start,
            size: narr.size,
            elements,
        }))
    }
}