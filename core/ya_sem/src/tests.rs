use super::*;

#[test]
fn empty_func() {
    let syn_parser = ya_syn::Parser::parse("let main = () {}");
    let sem_parser = Parser::parse(&syn_parser.items, Env {
        tys: HashMap::new(),
        vars: HashMap::new(),
        bin_ops: HashMap::new(),
        un_ops: HashMap::new(),
    });

    assert_eq!(sem_parser.global_env, EnvStack {
        envs: vec![Env {
            tys: HashMap::new(),
            vars: [
                ("main".to_owned(), Some(Type::Func(FuncType {
                    params: vec![],
                    ret_ty: Box::new(Type::Prim(PrimType::Unit)),
                })))
            ].into(),
            bin_ops: HashMap::new(),
            un_ops: HashMap::new(),
        }],
        funcs: vec![Expr {
            ty: Type::Prim(PrimType::Unit),
            kind: ExprKind::Block(BlockExpr { stmts: vec![], expr: None }),
            errs: vec![],
            env: Some(Env { tys: HashMap::new(), vars: HashMap::new(), bin_ops: HashMap::new(), un_ops: HashMap::new() }),
        }],
    });
}