use super::*;

#[test]
fn empty_func() {
    let syn_parser = syn::Parser::parse("let main = () {}");
    let sem_parser = Parser::parse(&syn_parser.items, Env {
        tys: HashMap::new(),
        vars: HashMap::new(),
        bin_ops: HashMap::new(),
    });

    assert_eq!(sem_parser.global_env.envs.len(), 1);
    assert_eq!(sem_parser.global_env.envs[0], Env {
        tys: HashMap::new(),
        vars: [
            ("main".to_owned(), Some(Type::Func(FuncType {
                params: vec![],
                ret_ty: Box::new(Type::Prim(PrimType::Unit)),
            })))
        ].into(),
        bin_ops: HashMap::new(),
    });
}