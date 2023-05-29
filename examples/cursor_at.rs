extern crate console;

use std::{io, time::Duration};

use console::{style, Term};

async fn write_chars() -> io::Result<()> {
    let term = Term::stdout();
    let (heigth, width) = term.size();
    for x in 0..width {
        for y in 0..heigth {
            term.move_cursor_to(x as usize, y as usize).await?;
            let text = if (x + y) % 2 == 0 {
                format!("{}", style(x % 10).black().on_red())
            } else {
                format!("{}", style(x % 10).red().on_black())
            };

            term.write_str(&text).await?;
            tokio::time::sleep(Duration::from_micros(600)).await;
        }
    }
    Ok(())
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    write_chars().await.unwrap();
}
