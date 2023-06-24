use std::io;
use std::io::Write;

trait Obj {}

#[derive(Default)]
#[allow(dead_code)]
struct Size32 {
    pub f1: usize,
    pub f2: usize,
    pub f3: usize,
    pub f4: usize,
}
impl Obj for Size32 {}

#[derive(Default)]
#[allow(dead_code)]
struct Size64 {
    pub f1: Size32,
    pub f2: Size32,
}
impl Obj for Size64 {}

#[derive(Default)]
#[allow(dead_code)]
struct Size256 {
    pub f1: Size64,
    pub f2: Size64,
    pub f3: Size64,
    pub f4: Size64,
}
impl Obj for Size256 {}

#[derive(Default)]
#[allow(dead_code)]
struct Size512 {
    pub f1: Size64,
    pub f2: Size64,
    pub f3: Size64,
    pub f4: Size64,
    pub f5: Size64,
    pub f6: Size64,
    pub f7: Size64,
    pub f8: Size64,
}
impl Obj for Size512 {}

#[derive(Default)]
#[allow(dead_code)]
struct Size1024 {
    pub f1: Size512,
    pub f2: Size512,
}

impl Obj for Size1024 {}

#[derive(Default)]
#[allow(dead_code)]
struct Size4096 {
    pub f1: Size512,
    pub f2: Size512,
    pub f3: Size512,
    pub f4: Size512,
    pub f5: Size512,
    pub f6: Size512,
    pub f7: Size512,
    pub f8: Size512,
}

impl Obj for Size4096 {}

#[derive(Copy, Clone)]
enum SizeClass {
    S32,
    S64,
    S256,
    S512,
    S1024,
    S4096,
}

impl SizeClass {
    fn into_size(self) -> usize {
        use SizeClass::*;

        match self {
            S32 => 32,
            S64 => 64,
            S256 => 256,
            S512 => 512,
            S1024 => 1024,
            S4096 => 4096,
        }
    }
}

fn main() -> std::io::Result<()> {
    loop {
        let mut buffer = String::new();
        print!("Enter size in bytes to allocate > ");
        io::stdout().flush()?;
        match io::stdin().read_line(&mut buffer) {
            Ok(_) => match buffer.trim().parse() {
                Ok(num) => {
                    let _data = run_allocate(num);
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

fn run_allocate(num: usize) -> Vec<Box<dyn Obj>> {
    use rand::RngCore;
    use SizeClass::*;

    let scall = vec![S32, S64, S256, S512, S1024, S4096];
    let mut sccnt = vec![0, 0, 0, 0, 0, 0];

    let mut remains: i64 = num as i64;
    let mut alloced = Vec::new();

    while remains > 0 {
        let scn = (rand::thread_rng().next_u64() % 6) as usize;
        let sc = scall[scn];
        let obj: Box<dyn Obj> = match sc {
            S32 => Box::<Size32>::default(),
            S64 => Box::<Size64>::default(),
            S256 => Box::<Size256>::default(),
            S512 => Box::<Size512>::default(),
            S1024 => Box::<Size1024>::default(),
            S4096 => Box::<Size4096>::default(),
        };
        sccnt[scn] += 1;
        remains -= sc.into_size() as i64;
        alloced.push(obj)
    }

    println!(
        "Allocated {} bytes, by size class: {:?}",
        num as i64 - remains,
        sccnt
    );

    alloced
}
