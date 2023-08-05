use std::{
    fmt::{Debug, Display},
    io,
    pin::Pin,
    sync::{Arc, Mutex},
    task::{Context, Poll},
};

use tokio::io::{AsyncRead, AsyncWrite, AsyncWriteExt, ReadBuf};
#[cfg(not(target_family = "wasm"))]
use tokio::io::{Stderr, Stdout};

#[cfg(unix)]
use std::os::unix::io::{AsRawFd, RawFd};
#[cfg(windows)]
use std::os::windows::io::{AsRawHandle, RawHandle};

use crate::{kb::Key, utils::Style};

#[cfg(unix)]
trait TermWrite: AsyncWrite + Debug + AsRawFd + Unpin + Send {}
#[cfg(unix)]
impl<T: AsyncWrite + Debug + AsRawFd + Unpin + Send> TermWrite for T {}

#[cfg(unix)]
trait TermRead: AsyncRead + Debug + AsRawFd + Unpin + Send {}
#[cfg(unix)]
impl<T: AsyncRead + Debug + AsRawFd + Unpin + Send> TermRead for T {}

#[cfg(unix)]
#[derive(Debug, Clone)]
pub struct ReadWritePair {
    #[allow(unused)]
    read: Arc<Mutex<dyn TermRead>>,
    write: Arc<Mutex<dyn TermWrite>>,
    style: Style,
}

/// Where the term is writing.
#[derive(Debug, Clone)]
#[cfg(not(target_family = "wasm"))]
pub enum TermTarget {
    Stdout(Arc<Mutex<Stdout>>),
    Stderr(Arc<Mutex<Stderr>>),
    #[cfg(unix)]
    ReadWritePair(ReadWritePair),
}

/// Where the term is writing.
#[derive(Debug, Clone)]
#[cfg(target_family = "wasm")]
pub enum TermTarget {
    Stdout,
    Stderr,
}

#[derive(Debug)]
pub struct TermInner {
    target: TermTarget,
    buffer: Option<Mutex<Vec<u8>>>,
}

/// The family of the terminal.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TermFamily {
    /// Redirected to a file or file like thing.
    File,
    /// A standard unix terminal.
    UnixTerm,
    /// A cmd.exe like windows console.
    WindowsConsole,
    /// A dummy terminal (for instance on wasm)
    Dummy,
}

/// Gives access to the terminal features.
#[derive(Debug, Clone)]
pub struct TermFeatures<'a>(&'a Term);

impl<'a> TermFeatures<'a> {
    /// Check if this is a real user attended terminal (`isatty`)
    #[inline]
    pub fn is_attended(&self) -> bool {
        is_a_terminal(self.0)
    }

    /// Check if colors are supported by this terminal.
    ///
    /// This does not check if colors are enabled.  Currently all terminals
    /// are considered to support colors
    #[inline]
    pub fn colors_supported(&self) -> bool {
        is_a_color_terminal(self.0)
    }

    /// Check if this terminal is an msys terminal.
    ///
    /// This is sometimes useful to disable features that are known to not
    /// work on msys terminals or require special handling.
    #[inline]
    pub fn is_msys_tty(&self) -> bool {
        #[cfg(windows)]
        {
            msys_tty_on(self.0)
        }
        #[cfg(not(windows))]
        {
            false
        }
    }

    /// Check if this terminal wants emojis.
    #[inline]
    pub fn wants_emoji(&self) -> bool {
        self.is_attended() && wants_emoji()
    }

    /// Return the family of the terminal.
    #[inline]
    pub fn family(&self) -> TermFamily {
        if !self.is_attended() {
            return TermFamily::File;
        }
        #[cfg(windows)]
        {
            TermFamily::WindowsConsole
        }
        #[cfg(unix)]
        {
            TermFamily::UnixTerm
        }
        #[cfg(target_arch = "wasm32")]
        {
            TermFamily::Dummy
        }
    }
}

/// Abstraction around a terminal.
///
/// A terminal can be cloned.  If a buffer is used it's shared across all
/// clones which means it largely acts as a handle.
#[derive(Clone, Debug)]
pub struct Term {
    inner: Arc<TermInner>,
    pub(crate) is_msys_tty: bool,
    pub(crate) is_tty: bool,
}

impl Term {
    fn with_inner(inner: TermInner) -> Term {
        let mut term = Term {
            inner: Arc::new(inner),
            is_msys_tty: false,
            is_tty: false,
        };

        term.is_msys_tty = term.features().is_msys_tty();
        term.is_tty = term.features().is_attended();
        term
    }

    /// Return a new unbuffered terminal.
    #[inline]
    pub fn stdout() -> Term {
        Term::with_inner(TermInner {
            #[cfg(not(target_arch = "wasm32"))]
            target: TermTarget::Stdout(Arc::new(Mutex::new(tokio::io::stdout()))),
            #[cfg(target_arch = "wasm32")]
            target: TermTarget::Stdout,
            buffer: None,
        })
    }

    /// Return a new unbuffered terminal to stderr.
    #[inline]
    pub fn stderr() -> Term {
        Term::with_inner(TermInner {
            #[cfg(not(target_arch = "wasm32"))]
            target: TermTarget::Stderr(Arc::new(Mutex::new(tokio::io::stderr()))),
            #[cfg(target_arch = "wasm32")]
            target: TermTarget::Stderr,
            buffer: None,
        })
    }

    /// Return a new buffered terminal.
    pub fn buffered_stdout() -> Term {
        Term::with_inner(TermInner {
            #[cfg(not(target_arch = "wasm32"))]
            target: TermTarget::Stdout(Arc::new(Mutex::new(tokio::io::stdout()))),
            #[cfg(target_arch = "wasm32")]
            target: TermTarget::Stdout,
            buffer: Some(Mutex::new(vec![])),
        })
    }

    /// Return a new buffered terminal to stderr.
    pub fn buffered_stderr() -> Term {
        Term::with_inner(TermInner {
            #[cfg(not(target_arch = "wasm32"))]
            target: TermTarget::Stderr(Arc::new(Mutex::new(tokio::io::stderr()))),
            #[cfg(target_arch = "wasm32")]
            target: TermTarget::Stderr,
            buffer: Some(Mutex::new(vec![])),
        })
    }

    /// Return a terminal for the given Read/Write pair styled like stderr.
    #[cfg(unix)]
    pub fn read_write_pair<R, W>(read: R, write: W) -> Term
    where
        R: AsyncRead + Debug + AsRawFd + Unpin + Send + 'static,
        W: AsyncWrite + Debug + AsRawFd + Unpin + Send + 'static,
    {
        Self::read_write_pair_with_style(read, write, Style::new().for_stderr())
    }

    /// Return a terminal for the given Read/Write pair.
    #[cfg(unix)]
    pub fn read_write_pair_with_style<R, W>(read: R, write: W, style: Style) -> Term
    where
        R: AsyncRead + Debug + AsRawFd + Unpin + Send + 'static,
        W: AsyncWrite + Debug + AsRawFd + Unpin + Send + 'static,
    {
        Term::with_inner(TermInner {
            target: TermTarget::ReadWritePair(ReadWritePair {
                read: Arc::new(Mutex::new(read)),
                write: Arc::new(Mutex::new(write)),
                style,
            }),
            buffer: None,
        })
    }

    /// Return the style for this terminal.
    #[inline]
    pub fn style(&self) -> Style {
        #[cfg(not(target_family = "wasm"))]
        match self.inner.target {
            TermTarget::Stderr(..) => Style::new().for_stderr(),
            TermTarget::Stdout(..) => Style::new().for_stdout(),
            #[cfg(unix)]
            TermTarget::ReadWritePair(ReadWritePair { ref style, .. }) => style.clone(),
        }

        #[cfg(target_family = "wasm")]
        match self.inner.target {
            TermTarget::Stderr => Style::new().for_stderr(),
            TermTarget::Stdout => Style::new().for_stdout(),
        }
    }

    /// Return the target of this terminal.
    #[inline]
    pub fn target(&self) -> TermTarget {
        self.inner.target.clone()
    }

    #[doc(hidden)]
    pub async fn write_str(&self, s: &str) -> io::Result<()> {
        match self.inner.buffer {
            Some(ref buffer) => buffer.lock().unwrap().write_all(s.as_bytes()).await?,
            None => self.write_through(s.as_bytes()).await?,
        }
        self.flush().await
    }

    /// Write a string to the terminal and add a newline.
    pub async fn write_line(&self, s: &str) -> io::Result<()> {
        match self.inner.buffer {
            Some(ref mutex) => {
                let mut buffer = mutex.lock().unwrap();
                buffer.extend_from_slice(s.as_bytes());
                buffer.push(b'\n');
            }
            None => self.write_through(format!("{}\n\r", s).as_bytes()).await?,
        }
        self.flush().await
    }

    /// Read a single character from the terminal.
    ///
    /// This does not echo the character and blocks until a single character
    /// or complete key chord is entered.  If the terminal is not user attended
    /// the return value will be an error.
    pub fn read_char(&self) -> io::Result<char> {
        if !self.is_tty {
            return Err(io::Error::new(
                io::ErrorKind::NotConnected,
                "Not a terminal",
            ));
        }
        loop {
            match self.read_key()? {
                Key::Char(c) => {
                    return Ok(c);
                }
                Key::Enter => {
                    return Ok('\n');
                }
                _ => {}
            }
        }
    }

    /// Read a single key form the terminal.
    ///
    /// This does not echo anything.  If the terminal is not user attended
    /// the return value will always be the unknown key.
    pub fn read_key(&self) -> io::Result<Key> {
        if !self.is_tty {
            Ok(Key::Unknown)
        } else {
            read_single_key()
        }
    }

    /// Read one line of input.
    ///
    /// This does not include the trailing newline.  If the terminal is not
    /// user attended the return value will always be an empty string.
    pub async fn read_line(&self) -> io::Result<String> {
        if !self.is_tty {
            return Ok("".into());
        }
        let mut rv = String::new();
        io::stdin().read_line(&mut rv)?;
        let len = rv.trim_end_matches(&['\r', '\n'][..]).len();
        rv.truncate(len);
        Ok(rv)
    }

    /// Read one line of input with initial text.
    ///
    /// This does not include the trailing newline.  If the terminal is not
    /// user attended the return value will always be an empty string.
    pub async fn read_line_initial_text(&self, initial: &str) -> io::Result<String> {
        if !self.is_tty {
            return Ok("".into());
        }
        self.write_str(initial).await?;

        let mut chars: Vec<char> = initial.chars().collect();

        loop {
            match self.read_key()? {
                Key::Backspace => {
                    if chars.pop().is_some() {
                        self.clear_chars(1).await?;
                    }
                }
                Key::Char(chr) => {
                    chars.push(chr);
                    let mut bytes_char = [0; 4];
                    chr.encode_utf8(&mut bytes_char);
                    self.write_str(chr.encode_utf8(&mut bytes_char)).await?;
                }
                Key::Enter => {
                    self.write_line("").await?;
                    break;
                }
                _ => (),
            }
        }
        Ok(chars.iter().collect::<String>())
    }

    /// Read a line of input securely.
    ///
    /// This is similar to `read_line` but will not echo the output.  This
    /// also switches the terminal into a different mode where not all
    /// characters might be accepted.
    pub async fn read_secure_line(&self) -> io::Result<String> {
        if !self.is_tty {
            return Ok("".into());
        }
        match read_secure() {
            Ok(rv) => {
                self.write_line("").await?;
                Ok(rv)
            }
            Err(err) => Err(err),
        }
    }

    /// Flush internal buffers.
    ///
    /// This forces the contents of the internal buffer to be written to
    /// the terminal.  This is unnecessary for unbuffered terminals which
    /// will automatically flush.
    pub async fn flush(&self) -> io::Result<()> {
        if let Some(ref buffer) = self.inner.buffer {
            let mut buffer = buffer.lock().unwrap();
            if !buffer.is_empty() {
                self.write_through(&buffer[..]).await?;
                buffer.clear();
            }
        }

        #[cfg(not(target_family = "wasm"))]
        match &self.inner.target {
            TermTarget::Stdout(stdout) => stdout.lock().unwrap().flush().await?,
            TermTarget::Stderr(stderr) => stderr.lock().unwrap().flush().await?,
            #[cfg(unix)]
            TermTarget::ReadWritePair(ReadWritePair { ref write, .. }) => {
                let mut write = write.lock().unwrap();
                write.flush().await?
            }
        }

        Ok(())
    }

    /// Flush internal buffers.
    ///
    /// This forces the contents of the internal buffer to be written to
    /// the terminal.  This is unnecessary for unbuffered terminals which
    /// will automatically flush.
    pub fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        if let Some(ref buffer) = self.inner.buffer {
            let mut buffer = buffer.lock().unwrap();
            if !buffer.is_empty() {
                let _todo = self.poll_write_through(cx, &buffer[..]);
                buffer.clear();
            }
        }

        #[cfg(not(target_family = "wasm"))]
        match &self.inner.target {
            TermTarget::Stdout(stdout) => {
                let mut stdout = stdout.lock().unwrap();
                let stdout = Pin::new(&mut *stdout);
                stdout.poll_flush(cx)
            }
            TermTarget::Stderr(stderr) => {
                let mut stderr = stderr.lock().unwrap();
                let stderr = Pin::new(&mut *stderr);
                stderr.poll_flush(cx)
            }
            #[cfg(unix)]
            TermTarget::ReadWritePair(ReadWritePair { ref write, .. }) => {
                let mut write = write.lock().unwrap();
                let write = Pin::new(&mut *write);
                write.poll_flush(cx)
            }
        }

        #[cfg(target_family = "wasm")]
        Poll::Ready(Ok(()))
    }

    /// Polls for this to be shutdown.
    pub fn poll_shutdown(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        #[cfg(not(target_family = "wasm"))]
        {
            let mut poll = self.as_mut().poll_flush(cx);
            if poll.is_ready() {
                poll = match &self.inner.target {
                    TermTarget::Stdout(stdout) => {
                        let mut stdout = stdout.lock().unwrap();
                        let stdout = Pin::new(&mut *stdout);
                        stdout.poll_shutdown(cx)
                    }
                    TermTarget::Stderr(stderr) => {
                        let mut stderr = stderr.lock().unwrap();
                        let stderr = Pin::new(&mut *stderr);
                        stderr.poll_shutdown(cx)
                    }
                    #[cfg(unix)]
                    TermTarget::ReadWritePair(ReadWritePair { ref write, .. }) => {
                        let mut write = write.lock().unwrap();
                        let write = Pin::new(&mut *write);
                        write.poll_shutdown(cx)
                    }
                };
            }
            poll
        }

        #[cfg(target_family = "wasm")]
        Poll::Ready(Ok(()))
    }

    /// Check if the terminal is indeed a terminal.
    #[inline]
    pub fn is_term(&self) -> bool {
        self.is_tty
    }

    /// Check for common terminal features.
    #[inline]
    pub fn features(&self) -> TermFeatures<'_> {
        TermFeatures(self)
    }

    /// Return the terminal size in rows and columns or gets sensible defaults.
    #[inline]
    pub fn size(&self) -> (u16, u16) {
        self.size_checked().unwrap_or((24, DEFAULT_WIDTH))
    }

    /// Return the terminal size in rows and columns.
    ///
    /// If the size cannot be reliably determined `None` is returned.
    #[inline]
    pub fn size_checked(&self) -> Option<(u16, u16)> {
        terminal_size(self)
    }

    /// Move the cursor to row `x` and column `y`. Values are 0-based.
    #[inline]
    pub async fn move_cursor_to(&self, x: usize, y: usize) -> io::Result<()> {
        move_cursor_to(self, x, y).await?;
        self.flush().await
    }

    /// Move the cursor up by `n` lines, if possible.
    ///
    /// If there are less than `n` lines above the current cursor position,
    /// the cursor is moved to the top line of the terminal (i.e., as far up as
    /// possible).
    #[inline]
    pub async fn move_cursor_up(&self, n: usize) -> io::Result<()> {
        move_cursor_up(self, n).await?;
        self.flush().await
    }

    /// Move the cursor down by `n` lines, if possible.
    ///
    /// If there are less than `n` lines below the current cursor position,
    /// the cursor is moved to the bottom line of the terminal (i.e., as far
    /// down as possible).
    #[inline]
    pub async fn move_cursor_down(&self, n: usize) -> io::Result<()> {
        move_cursor_down(self, n).await?;
        self.flush().await
    }

    /// Move the cursor `n` characters to the left, if possible.
    ///
    /// If there are fewer than `n` characters to the left of the current cursor
    /// position, the cursor is moved to the beginning of the line (i.e., as
    /// far to the left as possible).
    #[inline]
    pub async fn move_cursor_left(&self, n: usize) -> io::Result<()> {
        move_cursor_left(self, n).await?;
        self.flush().await
    }

    /// Move the cursor `n` characters to the right.
    ///
    /// If there are fewer than `n` characters to the right of the current
    /// cursor position, the cursor is moved to the end of the current line
    /// (i.e., as far to the right as possible).
    #[inline]
    pub async fn move_cursor_right(&self, n: usize) -> io::Result<()> {
        move_cursor_right(self, n).await?;
        self.flush().await
    }

    /// Clear the current line.
    ///
    /// Position the cursor at the beginning of the current line.
    #[inline]
    pub async fn clear_line(&self) -> io::Result<()> {
        clear_line(self).await?;
        self.flush().await
    }

    /// Clear the last `n` lines before the current line.
    ///
    /// Position the cursor at the beginning of the first line that was cleared.
    pub async fn clear_last_lines(&self, n: usize) -> io::Result<()> {
        move_cursor_up(self, n).await?;
        for _ in 0..n {
            clear_line(self).await?;
            move_cursor_down(self, 1).await?;
        }
        move_cursor_up(self, n).await?;
        self.flush().await?;
        Ok(())
    }

    /// Clear the entire screen.
    ///
    /// Move the cursor to the upper left corner of the screen.
    #[inline]
    pub async fn clear_screen(&self) -> io::Result<()> {
        clear_screen(self).await?;
        self.flush().await
    }

    /// Clear everything from the current cursor position to the end of the
    /// screen. The cursor stays in its position.
    #[inline]
    pub async fn clear_to_end_of_screen(&self) -> io::Result<()> {
        clear_to_end_of_screen(self).await?;
        self.flush().await
    }

    /// Clear the last `n` characters of the current line.
    #[inline]
    pub async fn clear_chars(&self, n: usize) -> io::Result<()> {
        clear_chars(self, n).await?;
        self.flush().await
    }

    /// Set the terminal title.
    pub fn set_title<T: Display>(&self, title: T) {
        if !self.is_tty {
            return;
        }
        set_title(title);
    }

    /// Make the cursor visible again.
    #[inline]
    pub async fn show_cursor(&self) -> io::Result<()> {
        show_cursor(self).await?;
        self.flush().await
    }

    /// Hide the cursor.
    #[inline]
    pub async fn hide_cursor(&self) -> io::Result<()> {
        hide_cursor(self).await?;
        self.flush().await
    }

    // helpers

    fn poll_write(&self, cx: &mut Context<'_>, bytes: &[u8]) -> Poll<io::Result<usize>> {
        #[cfg(not(target_arch = "wasm32"))]
        match &self.inner.target {
            TermTarget::Stdout(stdout) => {
                let mut stdout = stdout.lock().unwrap();
                let stdout = Pin::new(&mut *stdout);
                stdout.poll_write(cx, bytes)
            }
            TermTarget::Stderr(stderr) => {
                let mut stderr = stderr.lock().unwrap();
                let stderr = Pin::new(&mut *stderr);
                stderr.poll_write(cx, bytes)
            }
            #[cfg(unix)]
            TermTarget::ReadWritePair(ReadWritePair { ref write, .. }) => {
                let mut write = write.lock().unwrap();
                let write = Pin::new(&mut *write);
                write.poll_write(cx, bytes)
            }
        }

        #[cfg(target_arch = "wasm32")]
        match &self.inner.target {
            TermTarget::Stdout => {
                // TODO: console.log
                Poll::Ready(Ok(bytes.len()))
            }
            TermTarget::Stderr => {
                // TODO: console.log
                Poll::Ready(Ok(bytes.len()))
            }
        }
    }

    #[cfg(all(windows, feature = "windows-console-colors"))]
    async fn write_through(&self, bytes: &[u8]) -> io::Result<()> {
        if self.is_msys_tty || !self.is_tty {
            self.write_through_common(bytes).await
        } else {
            match self.inner.target {
                TermTarget::Stdout(_stdout) => console_colors(self, Console::stdout()?, bytes),
                TermTarget::Stderr(_stderr) => console_colors(self, Console::stderr()?, bytes),
            }
        }
    }

    #[cfg(all(windows, feature = "windows-console-colors"))]
    fn poll_write_through(&self, cx: &mut Context<'_>, bytes: &[u8]) -> Poll<io::Result<usize>> {
        if self.is_msys_tty || !self.is_tty {
            self.poll_write_through_common(cx, bytes)
        } else {
            match self.inner.target {
                TermTarget::Stdout(_stdout) => console_colors(self, Console::stdout()?, bytes),
                TermTarget::Stderr(_stderr) => console_colors(self, Console::stderr()?, bytes),
            }
        }
    }

    #[cfg(not(all(windows, feature = "windows-console-colors")))]
    async fn write_through(&self, bytes: &[u8]) -> io::Result<()> {
        self.write_through_common(bytes).await
    }

    #[cfg(not(all(windows, feature = "windows-console-colors")))]
    fn poll_write_through(&self, cx: &mut Context<'_>, bytes: &[u8]) -> Poll<io::Result<usize>> {
        self.poll_write_through_common(cx, bytes)
    }

    #[cfg(all(windows, feature = "windows-console-colors"))]
    pub(crate) async fn write_through_common(&self, bytes: &[u8]) -> io::Result<()> {
        match self.inner.target {
            TermTarget::Stdout(stdout) => {
                let mut stdout = stdout.lock().unwrap();
                stdout.write_all(bytes).await?;
                stdout.flush().await?;
            }
            TermTarget::Stderr(stderr) => {
                let mut stderr = stderr.lock().unwrap();
                stderr.write_all(bytes).await?;
                stderr.flush().await?;
            }
            #[cfg(unix)]
            TermTarget::ReadWritePair(ReadWritePair { ref write, .. }) => {
                let mut write = write.lock().unwrap();
                write.write_all(bytes).await?;
                write.flush().await?;
            }
        }
        Ok(())
    }

    #[cfg(not(all(windows, feature = "windows-console-colors")))]
    pub(crate) async fn write_through_common(&self, bytes: &[u8]) -> io::Result<()> {
        #[cfg(not(target_arch = "wasm32"))]
        match &self.inner.target {
            TermTarget::Stdout(stdout) => {
                stdout.lock().unwrap().write_all(bytes).await?;
            }
            TermTarget::Stderr(stderr) => {
                stderr.lock().unwrap().write_all(bytes).await?;
            }
            #[cfg(unix)]
            TermTarget::ReadWritePair(ReadWritePair { ref write, .. }) => {
                let mut write = write.lock().unwrap();
                write.write_all(bytes).await?;
            }
        }

        #[cfg(target_arch = "wasm32")]
        match &self.inner.target {
            TermTarget::Stdout => {
                // TODO: console.log
            }
            TermTarget::Stderr => {
                // TODO: console.log
            }
        }

        Ok(())
    }

    pub(crate) fn poll_write_through_common(
        &self,
        cx: &mut Context<'_>,
        bytes: &[u8],
    ) -> Poll<io::Result<usize>> {
        #[cfg(not(target_arch = "wasm32"))]
        match &self.inner.target {
            TermTarget::Stdout(stdout) => {
                let mut stdout = stdout.lock().unwrap();
                let mut poll = {
                    let stdout = Pin::new(&mut *stdout);
                    stdout.poll_write(cx, bytes)
                };

                poll = if let Poll::Ready(Ok(n)) = poll {
                    let stdout = Pin::new(&mut *stdout);
                    stdout.poll_flush(cx).map_ok(|()| n)
                } else {
                    poll
                };

                poll
            }
            TermTarget::Stderr(stderr) => {
                let mut stderr = stderr.lock().unwrap();
                let mut poll = {
                    let stderr = Pin::new(&mut *stderr);
                    stderr.poll_write(cx, bytes)
                };

                poll = if let Poll::Ready(Ok(n)) = poll {
                    let stderr = Pin::new(&mut *stderr);
                    stderr.poll_flush(cx).map_ok(|()| n)
                } else {
                    poll
                };

                poll
            }
            #[cfg(unix)]
            TermTarget::ReadWritePair(ReadWritePair { ref write, .. }) => {
                let mut write = write.lock().unwrap();
                let mut poll = {
                    let write = Pin::new(&mut *write);
                    write.poll_write(cx, bytes)
                };

                poll = if let Poll::Ready(Ok(n)) = poll {
                    let write = Pin::new(&mut *write);
                    write.poll_flush(cx).map_ok(|()| n)
                } else {
                    poll
                };

                poll
            }
        }

        #[cfg(target_arch = "wasm32")]
        match &self.inner.target {
            TermTarget::Stdout => {
                // TODO: console.log
                Poll::Ready(Ok(bytes.len()))
            }
            TermTarget::Stderr => {
                // TODO: console.log
                Poll::Ready(Ok(bytes.len()))
            }
        }
    }
}

/// A fast way to check if the application has a user attended for stdout.
///
/// This means that stdout is connected to a terminal instead of a
/// file or redirected by other means. This is a shortcut for
/// checking the `is_attended` feature on the stdout terminal.
#[inline]
pub fn user_attended() -> bool {
    Term::stdout().features().is_attended()
}

/// A fast way to check if the application has a user attended for stderr.
///
/// This means that stderr is connected to a terminal instead of a
/// file or redirected by other means. This is a shortcut for
/// checking the `is_attended` feature on the stderr terminal.
#[inline]
pub fn user_attended_stderr() -> bool {
    Term::stderr().features().is_attended()
}

#[cfg(unix)]
impl AsRawFd for Term {
    fn as_raw_fd(&self) -> RawFd {
        match &self.inner.target {
            TermTarget::Stdout(_stdout) => libc::STDOUT_FILENO,
            TermTarget::Stderr(_stderr) => libc::STDERR_FILENO,
            TermTarget::ReadWritePair(ReadWritePair { ref write, .. }) => {
                write.lock().unwrap().as_raw_fd()
            }
        }
    }
}

#[cfg(windows)]
impl AsRawHandle for Term {
    fn as_raw_handle(&self) -> RawHandle {
        use windows_sys::Win32::System::Console::{
            GetStdHandle, STD_ERROR_HANDLE, STD_OUTPUT_HANDLE,
        };

        unsafe {
            GetStdHandle(match self.inner.target {
                TermTarget::Stdout(_stdout) => STD_OUTPUT_HANDLE,
                TermTarget::Stderr(_stderr) => STD_ERROR_HANDLE,
            }) as RawHandle
        }
    }
}

impl AsyncWrite for Term {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<io::Result<usize>> {
        match self.inner.buffer {
            Some(ref buffer) => Poll::Ready(
                std::io::Write::write_all(&mut *buffer.lock().unwrap(), buf).map(|()| buf.len()),
            ),
            None => Term::poll_write(&self, cx, buf),
        }
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), io::Error>> {
        Term::poll_flush(self, cx)
    }

    fn poll_shutdown(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Result<(), io::Error>> {
        Term::poll_shutdown(self, cx)
    }
}

impl AsyncRead for Term {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        #[cfg(not(target_arch = "wasm32"))]
        {
            let mut stdin = tokio::io::stdin();
            let stdin = Pin::new(&mut stdin);
            stdin.poll_read(cx, buf)
        }

        #[cfg(target_arch = "wasm32")]
        Poll::Ready(Ok(()))
    }
}

impl<'a> AsyncRead for &'a Term {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> Poll<io::Result<()>> {
        #[cfg(not(target_arch = "wasm32"))]
        {
            let mut stdin = tokio::io::stdin();
            let stdin = Pin::new(&mut stdin);
            stdin.poll_read(cx, buf)
        }

        #[cfg(target_arch = "wasm32")]
        Poll::Ready(Ok(()))
    }
}

#[cfg(unix)]
pub use crate::unix_term::*;
#[cfg(target_arch = "wasm32")]
pub use crate::wasm_term::*;
#[cfg(windows)]
pub use crate::windows_term::*;
