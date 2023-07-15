// Copyright (c) 2023 Artem Pianykh

use std::io;
use std::io::Write;

fn main() -> std::io::Result<()> {
    loop {
        let mut buffer = String::new();
        print!("Enter the size in megabytes to allocate > ");
        io::stdout().flush()?;

        match io::stdin().read_line(&mut buffer) {
            Ok(_) => match buffer.trim().parse::<usize>() {
                Ok(mbytes) => {
                    let bytes = mbytes * 1024 * 1024;
                    let _data = run_allocate(bytes);
                    println!("Press ENTER to release allocated data");
                    if io::stdin().read_line(&mut buffer).is_err() {
                        break;
                    }
                }
                Err(_) => {
                    println!("{} is not a number", buffer);
                }
            },
            Err(_) => break,
        }
    }

    println!("Finished");
    Ok(())
}

const SIZE_CLASSES: &[usize] = &[
    32,
    64,
    128,
    256,
    512,
    1024,
    4096,
    1024 * 1024,
    4 * 1024 * 1024,
];

fn run_allocate(num: usize) -> Vec<Box<[u8]>> {
    use rand::RngCore;

    let mut class_counts = vec![0 as usize; SIZE_CLASSES.len()];

    let mut remains: i64 = num as i64;
    let mut alloced = Vec::with_capacity(1024);

    while remains > 0 {
        let n_size_class = (rand::thread_rng().next_u64() % (SIZE_CLASSES.len() as u64)) as usize;
        let size_class = SIZE_CLASSES[n_size_class];

        let obj = vec![0 as u8; size_class].into_boxed_slice();

        class_counts[n_size_class] += size_class;
        remains -= size_class as i64;

        alloced.push(obj)
    }

    println!(
        "Allocated {} bytes, by size class: {:?}",
        num as i64 - remains,
        class_counts
    );

    alloced
}
