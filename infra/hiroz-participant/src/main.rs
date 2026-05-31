use std::time::Duration;
use hiroz::{Builder, Result, context::ZContextBuilder};
use hiroz_msgs::std_msgs::String as RosString;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize hiroz context (connects to router on localhost:7447)
    let ctx = ZContextBuilder::default()
        .with_connect_endpoints(["tcp/127.0.0.1:7447"])
        .build()?;

    // Create a ROS 2 node
    let node = ctx.create_node("my_talker").build()?;

    // Create a publisher for the /chatter topic
    let pub_handle = node.create_pub::<RosString>("/chatter").build()?;

    // Publish messages every second
    let mut count = 0;
    loop {
        let msg = RosString {
            data: format!("Hello from hiroz #{}", count),
        };
        println!("Publishing: {}", msg.data);
        pub_handle.async_publish(&msg).await?;
        count += 1;
        tokio::time::sleep(Duration::from_secs(1)).await;
    }
}
