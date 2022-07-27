use async_stream::stream;
use futures::{Stream, StreamExt};
use std::{pin::Pin, time::Duration};
use tokio::time::sleep;
use tokio_stream::StreamMap;

#[tokio::main]
async fn main() {
    let mut stream = StreamMap::from_iter(
        [create_stream(1), create_stream(2), create_stuck_stream()]
            .into_iter()
            .enumerate(),
    );

    while let Some(element) = stream.next().await {
        println!("{:?}", element);
    }
}

fn create_stream(number: usize) -> Pin<Box<dyn Stream<Item = usize>>> {
    Box::pin(stream! {
        loop {
            yield number;
        }
    })
}

fn create_stuck_stream() -> Pin<Box<dyn Stream<Item = usize>>> {
    Box::pin(stream! {
        let mut sum = 0;

        while sum != usize::MAX {
            sum += 1;
            sleep(Duration::from_secs(1)).await;
        }

        yield 42;
    })
}
