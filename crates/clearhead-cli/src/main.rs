use std::any::Any;
use std::panic;

fn main() {
    // Rust's print macros panic when a downstream consumer closes stdout early
    // (`clearhead read ... | head`). Treat that ordinary Unix pipeline event as
    // successful termination while preserving normal panic reporting.
    let default_hook = panic::take_hook();
    panic::set_hook(Box::new(move |info| {
        if !is_broken_pipe_panic(info.payload()) {
            default_hook(info);
        }
    }));

    match panic::catch_unwind(clearhead_cli::cli::run) {
        Ok(()) => {}
        Err(payload) if is_broken_pipe_panic(payload.as_ref()) => {}
        Err(payload) => panic::resume_unwind(payload),
    }
}

fn is_broken_pipe_panic(payload: &(dyn Any + Send)) -> bool {
    payload
        .downcast_ref::<String>()
        .map(String::as_str)
        .or_else(|| payload.downcast_ref::<&'static str>().copied())
        .is_some_and(|message| message.contains("failed printing to stdout: Broken pipe"))
}
