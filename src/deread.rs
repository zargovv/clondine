use std::io::{self, Read};

pub(crate) trait Deread: Sized {
  fn deread(r: impl Dereader) -> io::Result<Self>;
}

macro_rules! impl_deread {
  ($($ty:ty)*) => {
    $(impl Deread for $ty {
      fn deread(mut r: impl Dereader) -> ::std::io::Result<Self> {
        let mut buf = [0; ::std::mem::size_of::<$ty>()];
        r.read_exact(&mut buf)?;
        Ok(<$ty>::from_be_bytes(buf))
      }
    })*
  };
}

impl_deread!(u8 u16 u32 i32 i64 f32 f64);

pub(crate) trait Dereader: Read {
  fn deread<T: Deread>(&mut self) -> io::Result<T> {
    T::deread(self)
  }
}

impl<R: Read> Dereader for R {}

#[derive(Debug)]
#[allow(dead_code)]
pub(crate) struct ByteSeq(Vec<u8>);

impl Deread for ByteSeq {
  fn deread(mut r: impl Dereader) -> io::Result<Self> {
    let mut buf = vec![0; usize::try_from(r.deread::<u32>()?).map_err(io::Error::other)?];
    r.read_exact(&mut buf)?;
    Ok(Self(buf))
  }
}

impl<T: Deread> Deread for Vec<T> {
  fn deread(mut r: impl Dereader) -> io::Result<Self> {
    let mut buf = Vec::with_capacity(usize::from(r.deread::<u16>()?));
    for _ in 0..buf.capacity() {
      buf.push(r.deread()?);
    }
    Ok(buf)
  }
}

impl Deread for String {
  fn deread(mut r: impl Dereader) -> io::Result<Self> {
    let mut buf = vec![0; usize::from(r.deread::<u16>()?)];
    r.read_exact(&mut buf)?;
    Ok(unsafe { String::from_utf8_unchecked(buf) })
  }
}
