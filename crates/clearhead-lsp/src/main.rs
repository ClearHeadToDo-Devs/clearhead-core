use tracing_subscriber::EnvFilter;

#[tokio::main]
async fn main() {
    let _ = tracing_subscriber::fmt()
        .with_writer(std::io::stderr)
        .with_env_filter(EnvFilter::try_from_default_env().unwrap_or_else(|_| "warn".into()))
        .try_init();

    clearhead_lsp::serve_stdio().await;
}
