macro_rules! bitfield {
  ($(
    $(#[$meta:meta])?
    $ident:ident: $ty:ty {
      $(
        $(#[$field_meta:meta])?
        $field:ident = $lit:literal,
      )*
    }
  )*) => {
    $(
      $(#[$meta])?
      #[derive(Clone, Copy)]
      struct $ident($ty);

      impl<'a> $crate::classfile::CpDebug<'a> for $ident {
        type Output = &'a $ident;

        fn debug(&'a self, _pool: &ConstantPool) -> Self::Output {
          &self
        }
      }

      impl $crate::deread::Deread for $ident {
        fn deread(mut r: impl $crate::deread::Dereader) -> ::std::io::Result<Self> {
          r.deread().map(Self)
        }
      }

      impl ::std::ops::BitOr for $ident {
        type Output = Self;

        fn bitor(self, rhs: Self) -> Self::Output {
          Self(self.0 | rhs.0)
        }
      }

      impl ::std::ops::BitAnd for $ident {
        type Output = Self;

        fn bitand(self, rhs: Self) -> Self::Output {
          Self(self.0 & rhs.0)
        }
      }

      $(#[$meta])?
      impl $ident {
        $(
          $(#[$field_meta])?
          const $field: Self = Self($lit);
        )*
      }

      impl ::std::fmt::Debug for $ident {
        fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
          let mut consec = false;
          let bf = self.0;
          $(
            #[allow(unused_assignments)]
            if bf & $lit == $lit {
              if consec {
                f.write_str(" | ")?;
              }
              f.write_str(stringify!($field))?;
              consec = true;
            }
          )*
          Ok(())
        }
      }
    )*
  };
}

pub(crate) use bitfield;
