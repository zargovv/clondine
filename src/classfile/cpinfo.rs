use macros::Cpref;

#[derive(Cpref)]
#[repr(u8)]
#[allow(dead_code)]
pub(crate) enum CpInfo {
  Class {
    #[cpref]
    name_index: u16,
  } = 7,
  Fieldref {
    #[cpref]
    class_index: u16,
    #[cpref]
    name_and_type_index: u16,
  } = 9,
  Methodref {
    #[cpref]
    class_index: u16,
    #[cpref]
    name_and_type_index: u16,
  } = 10,
  InterfaceMethodref {
    #[cpref]
    class_index: u16,
    #[cpref]
    name_and_type_index: u16,
  } = 11,
  String {
    #[cpref]
    string_index: u16,
  } = 8,
  Integer {
    bytes: i32,
  } = 3,
  Float {
    bytes: f32,
  } = 4,
  Long {
    bytes: i64,
  } = 5,
  Double {
    bytes: f64,
  } = 6,
  NameAndType {
    #[cpref]
    name_index: u16,
    #[cpref]
    descriptor_index: u16,
  } = 12,
  Utf8 {
    bytes: String,
  } = 1,
  MethodHandle {
    reference_kind: u8,
    #[cpref]
    reference_index: u16,
  } = 15,
  MethodType {
    #[cpref]
    descriptor_index: u16,
  } = 16,
  InvokeDynamic {
    #[cpref]
    bootstrap_method_attr_index: u16,
    #[cpref]
    name_and_type_index: u16,
  } = 18,
}
