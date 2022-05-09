#[derive(strum::EnumString, strum::AsRefStr, Debug, PartialEq, Eq, Copy, Clone, Hash)]
#[strum(serialize_all = "lowercase")]
pub enum PrimType {
    #[strum(disabled)]
    Unit,
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    Char, Bool,
    ISize, USize,
}