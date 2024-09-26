mod bitfield;
mod classfile;
mod deread;

use std::{env, fs::File, io::BufReader, path::Path};

use classfile::ClassFile;
use deread::Deread;

fn main() {
  let mut args_iter = env::args();
  let Some(exe) = args_iter.next() else {
    unreachable!();
  };
  let Some(mut class_filename) = args_iter.next() else {
    eprintln!("Usage: {exe} <class_filename>");
    return;
  };

  let mut class_filepath = Path::new(&class_filename);
  if !class_filepath.exists()
    && !class_filepath
      .extension()
      .map_or(false, |ext| ext.eq_ignore_ascii_case("class"))
  {
    class_filename.push_str(".class");
    class_filepath = Path::new(&class_filename);
  }

  let file = match File::open(class_filepath) {
    Ok(v) => v,
    Err(error) => {
      eprintln!("Failed to open file {class_filepath:?}: {error}");
      return;
    }
  };

  let classfile = match ClassFile::deread(&mut BufReader::new(file)) {
    Ok(v) => v,
    Err(error) => {
      eprintln!("Failed to read class file: {error}");
      return;
    }
  };

  println!("{classfile:#?}");
}
