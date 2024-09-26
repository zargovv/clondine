# Clonidine

> An unofficial parser for [Java class
> files](https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html) written in
> [Rust](https://rust-lang.org/).

Clonidine is a conceptual project that implements a number of noteworthy features, leveraging only
the standard library. These include:

- `TokenStream` parsing.
- `quote!` function-like macro.
- `derive` procedural macro featuring custom attributes.
- A simple and lightweight implementation with no external dependencies.

## Usage

### 1. Build the Clonidine executable

```sh
cargo b
```

### 2. Compile some Java source code into a class file

```sh
javac src/Main.java
```

### 3. Examine the insights of the class file

```sh
./target/clonidine src/Main.class
```
