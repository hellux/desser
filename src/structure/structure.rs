use super::scope::{Name, NameArray, NameStruct};
use super::*;

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
                if let Some(StructField::Prim(ptr)) =
                    elements.get(0).map(|k| k)
                {
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

impl<'n> From<Name<'n>> for StructField {
    fn from(name: Name<'n>) -> Self {
        match name {
            Name::Struct(nst) => StructField::Struct(nst.into()),
            Name::Array(narr) => StructField::Array(narr.into()),
            Name::Field(ptr) => StructField::Prim(ptr),
            _ => panic!(),
        }
    }
}

impl<'n> From<NameStruct<'n>> for Struct {
    fn from(nst: NameStruct<'n>) -> Self {
        let mut fields: Vec<(Sym, StructField)> = nst
            .fields
            .into_iter()
            .map(|(sym, f)| (sym, f.into()))
            .collect();

        fields.sort_by_key(|(_, f)| f.start());

        Struct {
            start: nst.start,
            size: nst.size,
            fields,
        }
    }
}

impl<'n> From<NameArray<'n>> for Array {
    fn from(narr: NameArray<'n>) -> Self {
        let elements = narr.elements.into_iter().map(|e| e.into()).collect();

        Array {
            start: narr.start,
            size: narr.size,
            elements,
        }
    }
}
