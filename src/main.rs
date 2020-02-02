use std::collections::HashMap;
use std::error::Error;
use std::io;
use std::mem;

mod ast;

#[derive(Hash, Eq, PartialEq, Debug)]
enum SueTagName {
    STRUCT(String),
    UNION(String),
    ENUM(String),
}

#[derive(Debug)]
enum Type {}

#[derive(Debug)]
struct QType {
    is_const: bool,
    is_volatile: bool,
    tp: Type,
}

#[derive(Debug)]
struct EnumType {}

#[derive(Debug)]
enum SueType {
    ENUM(Box<EnumType>),
}

#[derive(Debug)]
struct Scope {
    outer_scope: Box<Option<Scope>>,
    sue_tag_names_ns: HashMap<SueTagName, SueType>,
}

impl Scope {
    fn new() -> Scope {
        Scope {
            outer_scope: Box::new(None),
            sue_tag_names_ns: HashMap::new(),
        }
    }

    fn enter(self) -> Scope {
        Scope {
            outer_scope: Box::new(Some(self)),
            sue_tag_names_ns: HashMap::new(),
        }
    }

    fn leave(mut self) -> Scope {
        match *mem::replace(&mut self.outer_scope, Box::new(None)) {
            None => panic!("Cannot leave file scope {:?}", self),
            Some(s) => s
        }
    }

    fn is_file_scope(&self) -> bool {
        self.outer_scope.is_none()
    }

    fn get_file_scope(&self) -> &Scope {
        let mut s: &Scope = self;
        loop {
            match s.outer_scope.as_ref() {
                None => break s,
                Some(outer) => s = outer,
            }
        }
    }
}

impl Drop for Scope {
    fn drop(&mut self) {
        self.outer_scope = Box::new(None);
    }
}

struct Compiler<'a> {
    translation_unit: &'a ast::TranslationUnit,
    current_scope: Scope,
}

type L<'a, T> = (T, &'a ast::Loc);

impl Compiler<'_> {
    fn visit(tu: ast::TranslationUnit) {
        let mut cc =
            Compiler {
                translation_unit: &tu,
                current_scope: Scope::new(),
            };
        for ed in tu.eds.iter() {
            if ed.has_fd() {
                cc.visit_function_def(ed.get_fd())
            } else {
                cc.visit_declaration(ed.get_dl())
            }
        }
    }

    fn visit_function_def(&mut self, fd: &ast::FunctionDef) {
        // TODO
    }

    fn visit_declaration(&mut self, dl: &ast::Declaration) {
        let storage_class_specifiers: Vec<L<ast::StorageClassSpecifier>> =
            dl.get_dss().iter()
                .filter(|ds| ds.has_scs())
                .map(|ds| (ds.get_scs(), ds.get_loc()))
                .collect();
        let type_specifiers: Vec<L<&ast::TypeSpecifier>> =
            dl.get_dss().iter()
                .filter(|ds| ds.has_ts())
                .map(|ds| (ds.get_ts(), ds.get_loc()))
                .collect();
        let type_qualifiers: Vec<L<ast::TypeQualifier>> =
            dl.get_dss().iter()
                .filter(|ds| ds.has_tq())
                .map(|ds| (ds.get_tq(), ds.get_loc()))
                .collect();

        // 3.5.1: At most one storage-class specifier may be given in the
        // declaration specifiers in a declaration.
        if 1 < storage_class_specifiers.len() {
            panic!("{}: More than one storage class specifier found",
                   Compiler::format_loc(storage_class_specifiers[1].1));
        }

        let qualified_type: QType =
            Compiler::qualify_type(
                &type_qualifiers, Compiler::get_type(&type_specifiers));

//        use ast::StorageClassSpecifier as SCS;
//        if let &[(SCS::TYPEDEF, &loc)] = storage_class_specifiers {
//
//        }

        // TODO
    }

    fn get_type(tss: &Vec<L<&ast::TypeSpecifier>>) -> Type {
        unimplemented!() // TODO
    }

    fn qualify_type(tqs: &Vec<L<ast::TypeQualifier>>, tp: Type) -> QType {
        let mut qtype = QType { is_const: false, is_volatile: false, tp };
        for &(q, loc) in tqs {
            use ast::TypeQualifier as TQ;
            let is_const = qtype.is_const && q == TQ::CONST;
            let is_volatile = qtype.is_volatile && q == TQ::VOLATILE;

            // 3.5.3: The same type qualifier shall not appear more than once in
            // the same specifier list or qualifier list, either directly or via
            // one or more typedef s.
            if is_const == qtype.is_const && is_volatile == qtype.is_volatile {
                panic!("{}: Duplicate '{}' type qualifier",
                       Compiler::format_loc(loc),
                       format!("{:?}", q).to_lowercase());
            }

            qtype.is_const = is_const;
            qtype.is_volatile = is_volatile;
        }
        qtype
    }

    fn format_loc(loc: &ast::Loc) -> String {
        loc.get_levels().get(0)
            .map_or(
                String::from("<unknown location>"),
                |r| format!("{}:{}:{}", r.file_name, r.line_begin, r.col_begin))
    }
}

fn main() {
    let protobuf_result =
        ::protobuf::parse_from_reader::<ast::TranslationUnit>(&mut io::stdin());
    let translation_unit =
        match protobuf_result {
            Ok(tu) => tu,
            Err(e) => panic!(String::from(e.description())),
        };
    Compiler::visit(translation_unit);
}
