# `console_async`

[![Build Status](https://github.com/azriel91/console_async/workflows/CI/badge.svg?branch=master)](https://github.com/azriel91/console_async/actions?query=workflow%3ACI)
[![Crates.io](https://img.shields.io/crates/d/console_async.svg)](https://crates.io/crates/console_async)
[![License](https://img.shields.io/github/license/azriel91/console_async)](https://github.com/azriel91/console_async/blob/master/LICENSE)
[![rustc 1.48.0](https://img.shields.io/badge/rust-1.48%2B-orange.svg)](https://img.shields.io/badge/rust-1.48%2B-orange.svg)
[![Documentation](https://docs.rs/console_async/badge.svg)](https://docs.rs/console_async)

`console_async` is a library for Rust that provides access to various terminal
features so you can build nicer looking command line interfaces.  It
comes with various tools and utilities for working with Terminals and
formatting text.

This is a fork of [`console`](https://github.com/console-rs/console), and updated to use async primitives from [`tokio`](https://github.com/tokio-rs/tokio). This is a migration of the original `console` crate to using `tokio::io` types, and made to compile. Therefore it may not necessarily fit proper async design.

Best paired with other libraries in the family:

* [dialoguer](https://docs.rs/dialoguer)
* [indicatif](https://docs.rs/indicatif)

## Terminal Access

The terminal is abstracted through the `console::Term` type.  It can
either directly provide access to the connected terminal or by buffering
up commands.  A buffered terminal will however not be completely buffered
on windows where cursor movements are currently directly passed through.

Example usage:

```rust
use std::thread;
use std::time::Duration;

use console_async::Term;

let term = Term::stdout();
term.write_line("Hello World!").await?;
tokio::time::sleep(Duration::from_millis(2000));
term.clear_line().await?;
```

## Colors and Styles

`console_async` automaticaly detects when to use colors based on the tty flag.  It also
provides higher level wrappers for styling text and other things that can be
displayed with the `style` function and utility types.

Example usage:

```rust
use console_async::style;

println!("This is {} neat", style("quite").cyan());
```

You can also store styles and apply them to text later:

```rust
use console_async::Style;

let cyan = Style::new().cyan();
println!("This is {} neat", cyan.apply_to("quite"));
```

## Working with ANSI Codes

The crate provides the function `strip_ansi_codes` to remove ANSI codes
from a string as well as `measure_text_width` to calculate the width of a
string as it would be displayed by the terminal.  Both of those together
are useful for more complex formatting.

## Unicode Width Support

By default this crate depends on the `unicode-width` crate to calculate
the width of terminal characters.  If you do not need this you can disable
the `unicode-width` feature which will cut down on dependencies.

License: MIT
