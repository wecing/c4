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
        println!("=====");
        println!("{:#?}", dl);
        // TODO
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
