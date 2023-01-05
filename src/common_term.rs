use std::io;

use crate::term::Term;

pub async fn move_cursor_down(out: &Term, n: usize) -> io::Result<()> {
    if n > 0 {
        out.write_str(&format!("\x1b[{}B", n)).await
    } else {
        Ok(())
    }
}

pub async fn move_cursor_up(out: &Term, n: usize) -> io::Result<()> {
    if n > 0 {
        out.write_str(&format!("\x1b[{}A", n)).await
    } else {
        Ok(())
    }
}
pub async fn move_cursor_left(out: &Term, n: usize) -> io::Result<()> {
    if n > 0 {
        out.write_str(&format!("\x1b[{}D", n)).await
    } else {
        Ok(())
    }
}

pub async fn move_cursor_right(out: &Term, n: usize) -> io::Result<()> {
    if n > 0 {
        out.write_str(&format!("\x1b[{}C", n)).await
    } else {
        Ok(())
    }
}

#[inline]
pub async fn move_cursor_to(out: &Term, x: usize, y: usize) -> io::Result<()> {
    out.write_str(&format!("\x1B[{};{}H", y + 1, x + 1)).await
}

pub async fn clear_chars(out: &Term, n: usize) -> io::Result<()> {
    if n > 0 {
        out.write_str(&format!("\x1b[{}D\x1b[0K", n)).await
    } else {
        Ok(())
    }
}

#[inline]
pub async fn clear_line(out: &Term) -> io::Result<()> {
    out.write_str("\r\x1b[2K").await
}

#[inline]
pub async fn clear_screen(out: &Term) -> io::Result<()> {
    out.write_str("\r\x1b[2J\r\x1b[H").await
}

#[inline]
pub async fn clear_to_end_of_screen(out: &Term) -> io::Result<()> {
    out.write_str("\r\x1b[0J").await
}

#[inline]
pub async fn show_cursor(out: &Term) -> io::Result<()> {
    out.write_str("\x1b[?25h").await
}

#[inline]
pub async fn hide_cursor(out: &Term) -> io::Result<()> {
    out.write_str("\x1b[?25l").await
}
