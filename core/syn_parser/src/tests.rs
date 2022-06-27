use super::*;

#[test]
fn print() {
    let mut syn = Parser::new("0b101abc 0x 123");

    while let Some(expr) = syn.next() {
        println!("{:?}", expr);
    }
}