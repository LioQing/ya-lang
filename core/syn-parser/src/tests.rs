use super::*;

#[test]
fn print() {
    let mut syn = Parser::new("
    a + b-
    ");

    syn.assoc = |prec| {
        if [-1, 2, 3].contains(&prec) {
            Assoc::Right
        } else {
            Assoc::Left
        }
    };

    syn.rules = get_rules();

    syn.parse();

    println!("{:#?}", syn.stack);
}