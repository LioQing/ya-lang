use syn_parser as syn;

#[derive(Debug)]
pub struct Parser<'a> {
    pub syn: syn::Parser<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(code: &'a str) -> Parser<'a> {
        Parser {
            syn: syn::Parser::new(code),
        }
    }
}