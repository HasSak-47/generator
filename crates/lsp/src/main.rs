use lsp_server::Message;

fn main() {
    println!("Hello, world!");
    let (con, io) = lsp_server::Connection::stdio();
    for message in &con.receiver {
        match message {
            Message::Request(r) => {}
            Message::Notification(n) => {}
            Message::Response(r) => {}
        }
    }

    io.join();
}
