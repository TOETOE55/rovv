use rou_derive::*;
Row! { ..&str }

#[cfg(test)]
mod tests {
    use rou_derive::*;
    Row! { a: i32, b: T?, c: &'a Vec<T>*, ..&str }
}