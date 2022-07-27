use async_stream::stream;
use futures::{Stream, StreamExt};
use tokio_stream::StreamMap;

#[tokio::main]
async fn main() {
    let mut stream = StreamMap::from_iter(
        [create_stream(1), create_stream(2)]
            .into_iter()
            .enumerate()
            .map(|(index, stream)| (index, Box::pin(stream))),
    );

    while let Some(element) = stream.next().await {
        println!("{:?}", element);
    }
}

fn create_stream(number: usize) -> impl Stream<Item = usize> {
    stream! {
        loop {
            yield number;
        }
    }
}
