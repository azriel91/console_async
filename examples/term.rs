use std::{thread, time::Duration};

use console_async::{style, Term};

async fn do_stuff() -> std::io::Result<()> {
    let term = Term::stdout();
    term.set_title("Counting...");
    term.write_line("Going to do some counting now").await?;
    term.hide_cursor().await?;
    for x in 0..10 {
        if x != 0 {
            term.move_cursor_up(1).await?;
        }
        term.write_line(&format!("Counting {}/10", style(x + 1).cyan()))
            .await?;
        thread::sleep(Duration::from_millis(10));
    }
    term.show_cursor().await?;
    term.clear_last_lines(1).await?;
    term.write_line("Done counting!").await?;
    term.write_line("Hello World!").await?;

    term.write_str("To edit: ").await?;
    let res = term.read_line_initial_text("default").await?;
    term.write_line(format!("\n{res}", res = res).as_str())
        .await?;

    Ok(())
}

fn main() {
    let runtime = tokio::runtime::Builder::new_current_thread()
        .thread_name("main")
        .build()
        .unwrap();

    runtime.block_on(do_stuff()).unwrap();
}
