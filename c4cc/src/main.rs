use protobuf::{Message, ProtobufEnum};
use std::borrow::Borrow;
use std::cmp::max;
use std::collections::{HashMap, HashSet};
use std::env;
use std::ffi::{CStr, CString};
use std::fs::File;
use std::io;
use std::mem;
use std::ptr;

mod ast;

// TODO: replace panic!(..., format_loc(...), ...) with Result<T, Err>

struct SemanticError {
    msg: String,
    loc: ast::Loc,
}

impl SemanticError {
    fn panic<T>(&self) -> T {
        panic!("{}: {}", Compiler::format_loc(&self.loc), self.msg)
    }
}

type R<T> = Result<T, SemanticError>;

macro_rules! c4_fail {
    ($loc:expr, $msg:expr) => {
        return Err(
            SemanticError {
                msg: format!("{}", $msg),
                loc: $loc.clone(),
            }
        )
    };
    ($loc:expr, $fmt:expr, $($arg:tt)*) => {
        return Err(
            SemanticError {
                msg: format!($fmt, $($arg)*),
                loc: $loc.clone(),
            }
        )
    };
}

#[derive(Debug, Clone)]
enum Type {
    Void,
    Char,
    UnsignedChar,
    Short,
    UnsignedShort,
    Int,
    UnsignedInt,
    Long,
    UnsignedLong,
    Float,
    Double,
    // LongDouble,
    Struct(Box<SuType>),
    Union(Box<SuType>),
    // Enum => Int
    Pointer(Box<QType>),
    Array(Box<QType>, Option<u32>),
    Function(Box<QType>, Option<FuncParams>),
}

#[derive(Debug, Clone)]
enum FuncParams {
    Typed(Vec<TypedFuncParam>, bool), // bool: is_varargs
    Names(Vec<String>),
}

#[derive(Debug, Clone)]
struct TypedFuncParam {
    is_register: bool,
    tp: QType,
    name: Option<String>,
}

#[derive(Debug, Clone)]
struct QType {
    is_const: bool,
    is_volatile: bool,
    tp: Type,
}

impl QType {
    fn from(tp: Type) -> QType {
        QType {
            is_const: false,
            is_volatile: false,
            tp,
        }
    }

    fn is_integral_type(&self) -> bool {
        match self.tp {
            Type::Char
            | Type::UnsignedChar
            | Type::Short
            | Type::UnsignedShort
            | Type::Int
            | Type::UnsignedInt
            | Type::Long
            | Type::UnsignedLong => true,
            _ => false,
        }
    }

    fn is_arithmetic_type(&self) -> bool {
        match self.tp {
            Type::Float | Type::Double => true,
            _ => self.is_integral_type(),
        }
    }

    fn is_scalar_type(&self) -> bool {
        match self.tp {
            Type::Pointer(_) => true,
            _ => self.is_arithmetic_type(),
        }
    }

    fn is_void(&self) -> bool {
        match self.tp {
            Type::Void => true,
            _ => false,
        }
    }

    fn is_pointer(&self) -> bool {
        match self.tp {
            Type::Pointer(_) => true,
            _ => false,
        }
    }

    fn is_array(&self) -> bool {
        match self.tp {
            Type::Array(_, _) => true,
            _ => false,
        }
    }

    fn is_function(&self) -> bool {
        match self.tp {
            Type::Function(_, _) => true,
            _ => false,
        }
    }

    fn char_ptr_tp() -> QType {
        QType::ptr_tp(QType::from(Type::Char))
    }

    fn ptr_tp(tp: QType) -> QType {
        QType::from(Type::Pointer(Box::new(tp)))
    }
}

#[derive(Debug, Clone)]
struct SuField {
    name: Option<String>,
    tp: QType,
    bit_field_size: Option<u8>,
}

#[derive(Debug, Clone)]
struct SuType {
    fields: Option<Vec<SuField>>,
    uuid: u32, // identical su types in different scopes are different types
}

#[derive(Debug)] // no copy, no clone
struct Scope {
    outer_scope: Box<Option<Scope>>,
    sue_tag_names_ns: HashMap<String, SueType>,
    ordinary_ids_ns: HashMap<String, OrdinaryIdRef>,
}

#[derive(Debug)]
enum SueType {
    Struct(Box<SuType>),
    Union(Box<SuType>),
    Enum(bool, u32), // is_defined, uuid
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Linkage {
    EXTERNAL,
    INTERNAL,
    NONE,
}

#[derive(Debug, Clone)]
enum OrdinaryIdRef {
    TypedefRef(Box<QType>),
    // 3.5.2.2: The identifiers in an enumerator list are declared as constants
    // that have type int and may appear wherever such are permitted.
    EnumRef(i32), // value

    // ObjFnRef(ir_id, tp, linkage, is_defined)
    // since it's a lvalue, the IR id refers to a pointer to `tp`.
    ObjFnRef(String, QType, Linkage, bool),
}

#[derive(Debug, Clone)]
enum ConstantOrIrValue {
    I8(i8),      // Char
    U8(u8),      // UnsignedChar
    I16(i16),    // Short
    U16(u16),    // UnsignedShort
    I32(i32),    // Int
    U32(u32),    // UnsignedInt
    I64(i64),    // Long
    U64(u64),    // UnsignedLong, Pointer(_)
    Float(f32),  // Float
    Double(f64), // Double
    // Addresses may only be used together with pointer or array types.
    //
    // Unlike IrValue, the ir_id of Address could be looked up in
    // Compiler::global_constants and is guaranteed to exist.
    //
    // TODO: allow link-time constants here. this would (only) be useful for
    // a few corner cases in constant folding, e.g.:
    //     int n;
    //     char s[&n != 0]; // or &n + 10 - &n, etc
    //     int main(...) {...}
    Address(String, i64), // ir_id, offset_bytes
    // For struct/union/array, ir_id is a pointer even when is_lvalue=false.
    IrValue(String, bool), // ir_id, is_lvalue
}

impl ConstantOrIrValue {
    fn as_constant_double(&self) -> Option<f64> {
        match self {
            ConstantOrIrValue::I8(v) => Some(*v as f64),
            ConstantOrIrValue::U8(v) => Some(*v as f64),
            ConstantOrIrValue::I16(v) => Some(*v as f64),
            ConstantOrIrValue::U16(v) => Some(*v as f64),
            ConstantOrIrValue::I32(v) => Some(*v as f64),
            ConstantOrIrValue::U32(v) => Some(*v as f64),
            ConstantOrIrValue::I64(v) => Some(*v as f64),
            ConstantOrIrValue::U64(v) => Some(*v as f64),
            ConstantOrIrValue::Float(v) => Some(*v as f64),
            ConstantOrIrValue::Double(v) => Some(*v),
            _ => None,
        }
    }

    fn as_constant_u64(&self) -> Option<u64> {
        match self {
            ConstantOrIrValue::I8(v) => Some(*v as u64),
            ConstantOrIrValue::U8(v) => Some(*v as u64),
            ConstantOrIrValue::I16(v) => Some(*v as u64),
            ConstantOrIrValue::U16(v) => Some(*v as u64),
            ConstantOrIrValue::I32(v) => Some(*v as u64),
            ConstantOrIrValue::U32(v) => Some(*v as u64),
            ConstantOrIrValue::I64(v) => Some(*v as u64),
            ConstantOrIrValue::U64(v) => Some(*v),
            ConstantOrIrValue::Float(v) => Some(*v as u64),
            ConstantOrIrValue::Double(v) => Some(*v as u64),
            _ => None,
        }
    }

    fn is_ir_value(&self) -> bool {
        match self {
            ConstantOrIrValue::IrValue(_, _) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
enum Initializer {
    Expr(QType, ConstantOrIrValue), // does not contain lvalues
    Struct(Vec<Initializer>),
}

impl Scope {
    fn new() -> Scope {
        Scope {
            outer_scope: Box::new(None),
            sue_tag_names_ns: HashMap::new(),
            ordinary_ids_ns: HashMap::new(),
        }
    }

    fn is_file_scope(&self) -> bool {
        self.outer_scope.is_none()
    }

    fn lookup_ordinary_id(
        &self,
        name: &str,
    ) -> Option<(&OrdinaryIdRef, &Scope)> {
        let mut s: &Scope = self;
        loop {
            let r = s.ordinary_ids_ns.get(name);
            if r.is_some() {
                break r.map(|r| (r, s));
            }
            match &*s.outer_scope {
                None => break None,
                Some(outer) => s = outer,
            }
        }
    }

    fn lookup_sue_type(&self, tag: &str) -> Option<(&SueType, &Scope)> {
        let mut s: &Scope = self;
        loop {
            let r = s.sue_tag_names_ns.get(tag);
            if r.is_some() {
                break r.map(|r| (r, s));
            }
            match &*s.outer_scope {
                None => break None,
                Some(outer) => s = outer,
            }
        }
    }

    fn same_as(&self, other: &Scope) -> bool {
        // this works since Scope is neither Copy nor Clone
        ptr::eq(self, other)
    }
}

impl Drop for Scope {
    fn drop(&mut self) {
        self.outer_scope = Box::new(None);
    }
}

struct SwitchDefCtx {
    ctrl_value_tp: QType,
    case_values: HashSet<u64>,
    default_bb_id: String,
    default_case_visited: bool,
}

struct FuncDefCtx {
    func_name: String,
    return_type: QType,

    // user-provided label name => basic block name
    basic_blocks: HashMap<String, String>,
    // user-provided label name => first goto usage loc
    unresolved_labels: HashMap<String, ast::Loc>,

    switch_stack: Vec<SwitchDefCtx>,
    break_bb_stack: Vec<String>,
    continue_bb_stack: Vec<String>,
}

trait IRBuilder {
    fn emit_opaque_struct_type(&mut self, name: &str);

    // struct and union in C are both mapped bo struct in IR. for C unions the
    // generated struct must be packed with u8 so it could have a size.
    // `fields` is supposed to be non-empty.
    fn update_struct_type(
        &mut self,
        name: &str,
        fields: &Vec<SuField>,
        is_union: bool,
    );

    fn create_function(
        &mut self,
        name: &str,
        tp: &QType,
        linkage: Linkage,
        param_ir_ids: &Vec<String>,
    );

    // create a new basic block in the last created function.
    fn create_basic_block(&mut self, name: &str);

    fn set_current_basic_block(&mut self, bb: &str);

    fn get_current_basic_block(&self) -> String;

    fn create_definition(
        &mut self,
        is_global: bool,
        name: &str,
        tp: &QType,
        linkage: Linkage,
        init: &Option<Initializer>,
    );

    // global static constant buffer
    fn create_constant_buffer(&mut self, ir_id: String, buf: Vec<u8>);

    // <ir_id> = <c>
    //
    // local variable initialized with constant literals
    fn create_constant(
        &mut self,
        ir_id: String,
        c: &ConstantOrIrValue,
        tp: &QType,
    );

    // <dst_ir_id> = load <T>, <T>* <src_ir_id>
    //     where src_tp.tp == Type::Pointer(T)
    fn create_load(
        &mut self,
        dst_ir_id: String,
        src_ir_id: String,
        src_tp: &QType,
    );

    // store <T> <src_ir_id>, <T>* <dst_ir_id>
    fn create_store(&mut self, dst_ir_id: String, src_ir_id: String);

    // call void @llvm.memcpy(T* <dst_ir_id>, T* <src_ir_id>, <size>)
    fn create_memcpy(
        &mut self,
        dst_ir_id: String,
        src_ir_id: String,
        size: u32,
        align: u32,
    );

    fn create_cast(
        &mut self,
        dst_ir_id: String,
        dst_tp: &QType,
        src_ir_id: String,
        src_tp: &QType,
    );

    fn create_zext_i1_to_i32(&mut self, dst_ir_id: &str, src_ir_id: &str);

    // <dst_ir_id> = sub 0, <ir_id>
    // <dst_ir_id> = fneg <ir_id>
    fn create_neg(&mut self, dst_ir_id: &str, is_fp: bool, ir_id: &str);

    // bit-wise not. e.g. for i32:
    // <dst_ir_id> = xor -1, <ir_id>
    fn create_not(&mut self, dst_ir_id: &str, ir_id: &str);

    fn create_bin_op(
        &mut self,
        dst_ir_id: String,
        op: ast::Expr_Binary_Op,
        is_signed: bool,
        is_fp: bool,
        left_ir_id: String,
        right_ir_id: String,
    );

    fn create_cmp_op(
        &mut self,
        dst_ir_id: String,
        op: ast::Expr_Binary_Op,
        is_signed: bool,
        is_fp: bool,
        left_ir_id: String,
        right_ir_id: String,
    );

    fn create_ptr_add(
        &mut self,
        dst_ir_id: &str,    // returns i8*
        ptr_ir_id: &str,    // must be i8*
        offset_ir_id: &str, // must be i64
    );

    fn create_call(
        &mut self,
        dst_ir_id: &str,
        func_ir_id: &str, // must be func ptr
        arg_ir_ids: &Vec<String>,
    );

    fn enter_switch(&mut self, ir_id: &str, default_bb_id: &str);

    fn leave_switch(&mut self);

    fn add_switch_case(&mut self, c: &ConstantOrIrValue, bb_id: &str);

    fn create_br(&mut self, bb_id: &str);

    fn create_cond_br(
        &mut self,
        ir_id: &str,
        then_bb_id: &str,
        else_bb_id: &str,
    );

    fn create_return_void(&mut self);

    fn create_return(&mut self, ir_id: &str);

    // use "-" to print to stdout.
    fn print_to_file(&mut self, file_name: &str);
}

struct DummyIRBuilder {}

impl DummyIRBuilder {
    fn new() -> DummyIRBuilder {
        DummyIRBuilder {}
    }
}

impl IRBuilder for DummyIRBuilder {
    fn emit_opaque_struct_type(&mut self, _name: &str) {}

    fn update_struct_type(
        &mut self,
        _name: &str,
        _fields: &Vec<SuField>,
        _is_union: bool,
    ) {
    }

    fn create_function(
        &mut self,
        _name: &str,
        _tp: &QType,
        _linkage: Linkage,
        _param_ir_ids: &Vec<String>,
    ) {
    }

    fn create_basic_block(&mut self, _name: &str) {}

    fn set_current_basic_block(&mut self, _bb: &str) {}

    fn get_current_basic_block(&self) -> String {
        String::new()
    }

    fn create_definition(
        &mut self,
        _is_global: bool,
        _name: &str,
        _tp: &QType,
        _linkage: Linkage,
        _init: &Option<Initializer>,
    ) {
    }

    fn create_constant_buffer(&mut self, _ir_id: String, _buf: Vec<u8>) {}

    fn create_constant(
        &mut self,
        _ir_id: String,
        _c: &ConstantOrIrValue,
        _tp: &QType,
    ) {
    }

    fn create_load(
        &mut self,
        _dst_ir_id: String,
        _src_ir_id: String,
        _src_tp: &QType,
    ) {
    }

    fn create_store(&mut self, _dst_ir_id: String, _src_ir_id: String) {}

    fn create_memcpy(
        &mut self,
        _dst_ir_id: String,
        _src_ir_id: String,
        _size: u32,
        _align: u32,
    ) {
    }

    fn create_cast(
        &mut self,
        _dst_ir_id: String,
        _dst_tp: &QType,
        _src_ir_id: String,
        _src_tp: &QType,
    ) {
    }

    fn create_zext_i1_to_i32(&mut self, _dst_ir_id: &str, _src_ir_id: &str) {}

    fn create_neg(&mut self, _dst_ir_id: &str, _is_fp: bool, _ir_id: &str) {}

    fn create_not(&mut self, _dst_ir_id: &str, _ir_id: &str) {}

    fn create_bin_op(
        &mut self,
        _dst_ir_id: String,
        _op: ast::Expr_Binary_Op,
        _is_signed: bool,
        _is_fp: bool,
        _left_ir_id: String,
        _right_ir_id: String,
    ) {
    }

    fn create_cmp_op(
        &mut self,
        _dst_ir_id: String,
        _op: ast::Expr_Binary_Op,
        _is_signed: bool,
        _is_fp: bool,
        _left_ir_id: String,
        _right_ir_id: String,
    ) {
    }

    fn create_ptr_add(
        &mut self,
        _dst_ir_id: &str,
        _ptr_ir_id: &str,
        _offset_ir_id: &str,
    ) {
    }

    fn create_call(
        &mut self,
        _dst_ir_id: &str,
        _func_ir_id: &str,
        _arg_ir_ids: &Vec<String>,
    ) {
    }

    fn enter_switch(&mut self, _ir_id: &str, _default_bb_id: &str) {}

    fn leave_switch(&mut self) {}

    fn add_switch_case(&mut self, _c: &ConstantOrIrValue, _bb_id: &str) {}

    fn create_br(&mut self, _bb_id: &str) {}

    fn create_cond_br(
        &mut self,
        _ir_id: &str,
        _then_bb_id: &str,
        _else_bb_id: &str,
    ) {
    }

    fn create_return_void(&mut self) {}

    fn create_return(&mut self, _ir_id: &str) {}

    fn print_to_file(&mut self, _file_name: &str) {}
}

#[cfg(feature = "llvm-sys")]
struct LLVMBuilderImpl {
    context: llvm_sys::prelude::LLVMContextRef,
    module: llvm_sys::prelude::LLVMModuleRef,
    builder: llvm_sys::prelude::LLVMBuilderRef,

    next_uuid: u32,

    current_function: llvm_sys::prelude::LLVMValueRef,
    current_basic_block: String,
    basic_blocks: HashMap<String, llvm_sys::prelude::LLVMBasicBlockRef>,
    // key: ir_id
    symbol_table: HashMap<String, llvm_sys::prelude::LLVMValueRef>,

    switch_stack: Vec<llvm_sys::prelude::LLVMValueRef>,
}

#[cfg(feature = "llvm-sys")]
impl LLVMBuilderImpl {
    fn new() -> LLVMBuilderImpl {
        unsafe {
            let context = llvm_sys::core::LLVMContextCreate();
            let module_name = CString::new("c4").unwrap();
            let module = llvm_sys::core::LLVMModuleCreateWithNameInContext(
                module_name.as_ptr(),
                context,
            );
            let builder = llvm_sys::core::LLVMCreateBuilderInContext(context);
            LLVMBuilderImpl {
                context,
                module,
                builder,
                next_uuid: 1_000_000,
                current_function: ptr::null_mut(),
                current_basic_block: String::new(),
                basic_blocks: HashMap::new(),
                symbol_table: HashMap::new(),
                switch_stack: Vec::new(),
            }
        }
    }

    fn get_llvm_type(&self, tp: &Type) -> llvm_sys::prelude::LLVMTypeRef {
        unsafe {
            match tp {
                Type::Void => {
                    llvm_sys::core::LLVMVoidTypeInContext(self.context)
                }
                Type::Char | Type::UnsignedChar => {
                    llvm_sys::core::LLVMInt8TypeInContext(self.context)
                }
                Type::Short | Type::UnsignedShort => {
                    llvm_sys::core::LLVMInt16TypeInContext(self.context)
                }
                Type::Int | Type::UnsignedInt => {
                    llvm_sys::core::LLVMInt32TypeInContext(self.context)
                }
                Type::Long | Type::UnsignedLong => {
                    llvm_sys::core::LLVMInt64TypeInContext(self.context)
                }
                Type::Float => {
                    llvm_sys::core::LLVMFloatTypeInContext(self.context)
                }
                Type::Double => {
                    llvm_sys::core::LLVMDoubleTypeInContext(self.context)
                }
                Type::Struct(su) | Type::Union(su) => {
                    let type_name =
                        CString::new(format!("$.{}", su.uuid)).unwrap();
                    llvm_sys::core::LLVMGetTypeByName(
                        self.module,
                        type_name.as_ptr(),
                    )
                }
                Type::Pointer(tp) => llvm_sys::core::LLVMPointerType(
                    self.get_llvm_type(&tp.tp),
                    0,
                ),
                Type::Array(tp, sz) => {
                    if sz.is_none() {
                        panic!("get_llvm_type() invoked with incomplete array type");
                    }
                    llvm_sys::core::LLVMArrayType(
                        self.get_llvm_type(&tp.tp),
                        sz.unwrap(),
                    )
                }
                Type::Function(tp, params_opt) => {
                    let return_type = self.get_llvm_type(&tp.tp);
                    match params_opt {
                        None => llvm_sys::core::LLVMFunctionType(
                            return_type,
                            ptr::null_mut(),
                            0,
                            true as i32,
                        ),
                        Some(FuncParams::Typed(params, is_vararg)) => {
                            let mut args: Vec<llvm_sys::prelude::LLVMTypeRef> =
                                params
                                    .iter()
                                    .map(|p| self.get_llvm_type(&p.tp.tp))
                                    .collect();
                            llvm_sys::core::LLVMFunctionType(
                                return_type,
                                args.as_mut_ptr(),
                                args.len() as u32,
                                *is_vararg as i32,
                            )
                        }
                        Some(FuncParams::Names(params)) => {
                            let mut args: Vec<llvm_sys::prelude::LLVMTypeRef> =
                                params
                                    .iter()
                                    .map(|_| self.get_llvm_type(&Type::Int))
                                    .collect();
                            llvm_sys::core::LLVMFunctionType(
                                return_type,
                                args.as_mut_ptr(),
                                args.len() as u32,
                                false as i32,
                            )
                        }
                    }
                }
            }
        }
    }

    fn get_llvm_constant(
        &mut self,
        c: &ConstantOrIrValue,
    ) -> (llvm_sys::prelude::LLVMValueRef, QType) {
        let f_int = |src_tp: Type, v: u64, is_signed: bool| {
            let src_tp_llvm = self.get_llvm_type(&src_tp);
            let is_signed = if is_signed { 1 } else { 0 };
            let v = unsafe {
                llvm_sys::core::LLVMConstInt(src_tp_llvm, v, is_signed)
            };
            (v, QType::from(src_tp))
        };
        let f_fp = |src_tp: Type, v: f64| {
            let src_tp_llvm = self.get_llvm_type(&src_tp);
            let v = unsafe { llvm_sys::core::LLVMConstReal(src_tp_llvm, v) };
            (v, QType::from(src_tp))
        };
        use ConstantOrIrValue as C;
        use Type as T;
        match c {
            C::I8(v) => f_int(T::Char, *v as u64, true),
            C::U8(v) => f_int(T::UnsignedChar, *v as u64, false),
            C::I16(v) => f_int(T::Short, *v as u64, true),
            C::U16(v) => f_int(T::UnsignedShort, *v as u64, false),
            C::I32(v) => f_int(T::Int, *v as u64, true),
            C::U32(v) => f_int(T::UnsignedInt, *v as u64, false),
            C::I64(v) => f_int(T::Long, *v as u64, true),
            C::U64(v) => f_int(T::UnsignedLong, *v, false),
            C::Float(v) => f_fp(T::Float, *v as f64),
            C::Double(v) => f_fp(T::Double, *v),
            C::Address(ir_id, offset_bytes) => {
                let src_tp = Type::Pointer(Box::new(QType::from(Type::Void)));
                let src_tp_llvm = self.get_llvm_type(&src_tp);
                let char_ptr_tp = self.get_llvm_type(&Type::Pointer(Box::new(
                    QType::from(Type::Char),
                )));
                let ptr: llvm_sys::prelude::LLVMValueRef =
                    *self.symbol_table.get(ir_id).unwrap();
                let ptr = unsafe {
                    llvm_sys::core::LLVMConstBitCast(ptr, char_ptr_tp)
                };
                let mut offset = unsafe {
                    llvm_sys::core::LLVMConstInt(
                        self.get_llvm_type(&Type::Long),
                        *offset_bytes as u64,
                        1,
                    )
                };
                let src = unsafe {
                    llvm_sys::core::LLVMConstGEP(ptr, &mut offset, 1)
                };
                let src = unsafe {
                    llvm_sys::core::LLVMConstBitCast(src, src_tp_llvm)
                };
                (src, QType::from(src_tp))
            }
            C::IrValue(_, _) => unreachable!(),
        }
    }

    fn get_next_tmp_ir_id(&mut self) -> String {
        let r = self.next_uuid;
        self.next_uuid += 1;
        format!("$.t.{}", r)
    }
}

#[cfg(feature = "llvm-sys")]
impl IRBuilder for LLVMBuilderImpl {
    fn emit_opaque_struct_type(&mut self, name: &str) {
        unsafe {
            let n = CString::new(name).unwrap();
            llvm_sys::core::LLVMStructCreateNamed(self.context, n.as_ptr());
        }
    }

    fn update_struct_type(
        &mut self,
        name: &str,
        fields: &Vec<SuField>,
        _is_union: bool,
    ) {
        unsafe {
            let name_cstr = CString::new(name).unwrap();
            let tp = llvm_sys::core::LLVMGetTypeByName(
                self.module,
                name_cstr.as_ptr(),
            );
            let mut element_types: Vec<llvm_sys::prelude::LLVMTypeRef> = fields
                .iter()
                .map(|su_field| self.get_llvm_type(&su_field.tp.tp))
                .collect();
            llvm_sys::core::LLVMStructSetBody(
                tp,
                element_types.as_mut_ptr(),
                element_types.len() as u32,
                false as i32,
            )
        }
    }

    fn create_function(
        &mut self,
        name: &str,
        tp: &QType,
        linkage: Linkage,
        param_ir_ids: &Vec<String>,
    ) {
        let llvm_tp = self.get_llvm_type(&tp.tp);
        let c_func_name = CString::new(name).unwrap();
        let llvm_func = unsafe {
            llvm_sys::core::LLVMAddFunction(
                self.module,
                c_func_name.as_ptr(),
                llvm_tp,
            )
        };
        self.symbol_table.insert(String::from(name), llvm_func);
        let llvm_linkage = if linkage == Linkage::INTERNAL {
            llvm_sys::LLVMLinkage::LLVMInternalLinkage
        } else {
            llvm_sys::LLVMLinkage::LLVMExternalLinkage
        };
        unsafe {
            llvm_sys::core::LLVMSetLinkage(llvm_func, llvm_linkage);

            let mut llvm_param = llvm_sys::core::LLVMGetFirstParam(llvm_func);
            param_ir_ids.into_iter().for_each(|ir_id| {
                let c_ir_id = CString::new(ir_id.clone()).unwrap();
                llvm_sys::core::LLVMSetValueName2(
                    llvm_param,
                    c_ir_id.as_ptr(),
                    ir_id.len(),
                );
                self.symbol_table.insert(ir_id.clone(), llvm_param);
                llvm_param = llvm_sys::core::LLVMGetNextParam(llvm_param);
            });

            llvm_sys::core::LLVMClearInsertionPosition(self.builder);
        }
        self.current_function = llvm_func;
        self.basic_blocks.clear();
    }

    fn create_basic_block(&mut self, name: &str) {
        let name_c = CString::new(name).unwrap();
        let bb = unsafe {
            llvm_sys::core::LLVMAppendBasicBlockInContext(
                self.context,
                self.current_function,
                name_c.as_ptr(),
            )
        };
        self.basic_blocks.insert(String::from(name), bb);
    }

    fn set_current_basic_block(&mut self, bb: &str) {
        let bb = *self.basic_blocks.get(bb).unwrap();
        unsafe {
            llvm_sys::core::LLVMPositionBuilderAtEnd(self.builder, bb);
        }
    }

    fn get_current_basic_block(&self) -> String {
        if self.current_basic_block.is_empty() {
            unreachable!()
        }
        self.current_basic_block.clone()
    }

    fn create_definition(
        &mut self,
        is_global: bool,
        name: &str,
        tp: &QType,
        linkage: Linkage,
        init: &Option<Initializer>,
    ) {
        let name_c = CString::new(name).unwrap();
        let tp_llvm = self.get_llvm_type(&tp.tp);
        let v = unsafe {
            if is_global {
                llvm_sys::core::LLVMAddGlobal(
                    self.module,
                    tp_llvm,
                    name_c.as_ptr(),
                )
            } else {
                llvm_sys::core::LLVMBuildAlloca(
                    self.builder,
                    tp_llvm,
                    name_c.as_ptr(),
                )
            }
        };
        use llvm_sys::LLVMLinkage as LL;
        let linkage_llvm = match linkage {
            Linkage::EXTERNAL => Some(LL::LLVMExternalLinkage),
            Linkage::INTERNAL => Some(LL::LLVMInternalLinkage),
            Linkage::NONE => None,
        };
        linkage_llvm.map(|ln| unsafe { llvm_sys::core::LLVMSetLinkage(v, ln) });

        use ConstantOrIrValue as C;
        init.as_ref()
            .map(|x| match x {
                Initializer::Struct(_) => unimplemented!(), // TODO: {...} init
                Initializer::Expr(_, C::IrValue(ir_id, false)) => {
                    *self.symbol_table.get(ir_id).unwrap()
                }
                Initializer::Expr(_, c) => self.get_llvm_constant(c).0,
            })
            .map(|value| unsafe {
                if is_global {
                    llvm_sys::core::LLVMSetInitializer(v, value);
                } else {
                    llvm_sys::core::LLVMBuildStore(self.builder, value, v);
                }
            });

        self.symbol_table.insert(String::from(name), v);
    }

    fn create_constant_buffer(&mut self, ir_id: String, buf: Vec<u8>) {
        let v = unsafe {
            llvm_sys::core::LLVMConstStringInContext(
                self.context,
                buf.as_ptr() as *const i8,
                buf.len() as u32,
                1,
            )
        };
        self.symbol_table.insert(ir_id, v);
    }

    fn create_constant(
        &mut self,
        ir_id: String,
        c: &ConstantOrIrValue,
        tp: &QType,
    ) {
        let (src, src_tp) = self.get_llvm_constant(c);
        let src_ir_id = self.get_next_tmp_ir_id();
        self.symbol_table.insert(src_ir_id.clone(), src);
        self.create_cast(ir_id, tp, src_ir_id, &src_tp);
    }

    fn create_load(
        &mut self,
        dst_ir_id: String,
        src_ir_id: String,
        _src_tp: &QType,
    ) {
        let dst_ir_id_c = CString::new(dst_ir_id.clone()).unwrap();
        let src = self.symbol_table.get(&src_ir_id).unwrap();
        let dst = unsafe {
            llvm_sys::core::LLVMBuildLoad(
                self.builder,
                *src,
                dst_ir_id_c.as_ptr(),
            )
        };
        self.symbol_table.insert(dst_ir_id, dst);
    }

    fn create_store(&mut self, dst_ir_id: String, src_ir_id: String) {
        let src = *self.symbol_table.get(&src_ir_id).unwrap();
        let dst = *self.symbol_table.get(&dst_ir_id).unwrap();
        unsafe {
            llvm_sys::core::LLVMBuildStore(self.builder, src, dst);
        }
    }

    fn create_memcpy(
        &mut self,
        dst_ir_id: String,
        src_ir_id: String,
        size: u32,
        align: u32,
    ) {
        let src = self.symbol_table.get(&src_ir_id).unwrap();
        let dst = self.symbol_table.get(&dst_ir_id).unwrap();
        unsafe {
            llvm_sys::core::LLVMBuildMemCpy(
                self.builder,
                *dst,
                align,
                *src,
                align,
                self.get_llvm_constant(&ConstantOrIrValue::U32(size)).0,
            );
        }
    }

    fn create_cast(
        &mut self,
        dst_ir_id: String,
        dst_tp: &QType,
        src_ir_id: String,
        src_tp: &QType,
    ) {
        enum K {
            Int(i32, bool), // num_of_bits, is_signed
            FP(i32),        // num_of_bits
            Ptr,
        }
        let get_kind = |tp: &Type| match tp {
            Type::Char => K::Int(8, true),
            Type::UnsignedChar => K::Int(8, false),
            Type::Short => K::Int(16, true),
            Type::UnsignedShort => K::Int(16, false),
            Type::Int => K::Int(32, true),
            Type::UnsignedInt => K::Int(32, false),
            Type::Long => K::Int(64, true),
            Type::UnsignedLong => K::Int(64, false),
            Type::Float => K::FP(32),
            Type::Double => K::FP(64),
            Type::Pointer(_) => K::Ptr,
            _ => unreachable!(),
        };
        use llvm_sys::LLVMOpcode as O;
        let opcode = match (get_kind(&src_tp.tp), get_kind(&dst_tp.tp)) {
            // int -> int
            (K::Int(nb_src, _), K::Int(nb_dst, _)) if nb_src == nb_dst => None,
            (K::Int(nb_src, _), K::Int(nb_dst, _)) if nb_src > nb_dst => {
                Some(O::LLVMTrunc)
            }
            (K::Int(_, true), K::Int(_, _)) => Some(O::LLVMSExt),
            (K::Int(_, false), K::Int(_, _)) => Some(O::LLVMZExt),
            // int -> fp
            (K::Int(_, true), K::FP(_)) => Some(O::LLVMSIToFP),
            (K::Int(_, false), K::FP(_)) => Some(O::LLVMUIToFP),
            // int -> ptr
            (K::Int(_, _), K::Ptr) => Some(O::LLVMIntToPtr),

            // fp -> int
            (K::FP(_), K::Int(_, true)) => Some(O::LLVMFPToSI),
            (K::FP(_), K::Int(_, false)) => Some(O::LLVMFPToUI),
            // fp -> fp
            (K::FP(nb_src), K::FP(nb_dst)) if nb_src > nb_dst => {
                Some(O::LLVMFPTrunc)
            }
            (K::FP(nb_src), K::FP(nb_dst)) if nb_src < nb_dst => {
                Some(O::LLVMFPExt)
            }
            (K::FP(_), K::FP(_)) => None,
            // fp -> ptr
            (K::FP(_), K::Ptr) => unreachable!(),

            // ptr -> int
            (K::Ptr, K::Int(_, _)) => Some(O::LLVMPtrToInt),
            // ptr -> FP
            (K::Ptr, K::FP(_)) => unreachable!(),
            // ptr -> ptr
            (K::Ptr, K::Ptr) => Some(O::LLVMBitCast),
        };

        let src: llvm_sys::prelude::LLVMValueRef =
            *self.symbol_table.get(&src_ir_id).unwrap();
        let dst_tp_llvm = self.get_llvm_type(&dst_tp.tp);
        let dst_ir_id_c = CString::new(dst_ir_id.clone()).unwrap();
        let dst = match opcode {
            None => src,
            Some(op) => unsafe {
                llvm_sys::core::LLVMBuildCast(
                    self.builder,
                    op,
                    src,
                    dst_tp_llvm,
                    dst_ir_id_c.as_ptr(),
                )
            },
        };
        self.symbol_table.insert(dst_ir_id, dst);
    }

    fn create_zext_i1_to_i32(&mut self, dst_ir_id: &str, src_ir_id: &str) {
        let src = *self.symbol_table.get(src_ir_id).unwrap();
        let dst_ir_id_c = CString::new(dst_ir_id.to_string()).unwrap();
        let dst = unsafe {
            llvm_sys::core::LLVMBuildCast(
                self.builder,
                llvm_sys::LLVMOpcode::LLVMZExt,
                src,
                self.get_llvm_type(&Type::Int),
                dst_ir_id_c.as_ptr(),
            )
        };
        self.symbol_table.insert(dst_ir_id.to_string(), dst);
    }

    fn create_neg(&mut self, dst_ir_id: &str, is_fp: bool, ir_id: &str) {
        let src = *self.symbol_table.get(ir_id).unwrap();
        let dst_ir_id_c = CString::new(dst_ir_id.to_string()).unwrap();
        let dst = unsafe {
            let f = if is_fp {
                llvm_sys::core::LLVMBuildFNeg
            } else {
                llvm_sys::core::LLVMBuildNeg
            };
            f(self.builder, src, dst_ir_id_c.as_ptr())
        };
        self.symbol_table.insert(dst_ir_id.to_string(), dst);
    }

    fn create_not(&mut self, dst_ir_id: &str, ir_id: &str) {
        let src = *self.symbol_table.get(ir_id).unwrap();
        let dst_ir_id_c = CString::new(dst_ir_id.to_string()).unwrap();
        let dst = unsafe {
            llvm_sys::core::LLVMBuildNot(
                self.builder,
                src,
                dst_ir_id_c.as_ptr(),
            )
        };
        self.symbol_table.insert(dst_ir_id.to_string(), dst);
    }

    fn create_bin_op(
        &mut self,
        dst_ir_id: String,
        op: ast::Expr_Binary_Op,
        is_signed: bool,
        is_fp: bool,
        left_ir_id: String,
        right_ir_id: String,
    ) {
        use ast::Expr_Binary_Op as Op;
        use llvm_sys::LLVMOpcode as O;
        let dst_ir_id_c = CString::new(dst_ir_id.clone()).unwrap();
        let left = self.symbol_table.get(&left_ir_id).unwrap();
        let right = self.symbol_table.get(&right_ir_id).unwrap();
        let op = match op {
            Op::BIT_OR => O::LLVMOr,
            Op::XOR => O::LLVMXor,
            Op::BIT_AND => O::LLVMAnd,
            Op::L_SHIFT => O::LLVMShl,
            Op::R_SHIFT if is_signed => O::LLVMAShr,
            Op::R_SHIFT => O::LLVMLShr,
            Op::ADD if is_fp => O::LLVMFAdd,
            Op::ADD => O::LLVMAdd,
            Op::SUB if is_fp => O::LLVMFSub,
            Op::SUB => O::LLVMSub,
            Op::MUL if is_fp => O::LLVMFMul,
            Op::MUL => O::LLVMMul,
            Op::DIV if is_fp => O::LLVMFDiv,
            Op::DIV if is_signed => O::LLVMSDiv,
            Op::DIV => O::LLVMUDiv,
            Op::MOD if is_fp => O::LLVMFRem,
            Op::MOD if is_signed => O::LLVMSRem,
            Op::MOD => O::LLVMURem,
            _ => unreachable!(),
        };
        let dst = unsafe {
            llvm_sys::core::LLVMBuildBinOp(
                self.builder,
                op,
                *left,
                *right,
                dst_ir_id_c.as_ptr(),
            )
        };
        self.symbol_table.insert(dst_ir_id, dst);
    }

    fn create_cmp_op(
        &mut self,
        dst_ir_id: String,
        op: ast::Expr_Binary_Op,
        is_signed: bool,
        is_fp: bool,
        left_ir_id: String,
        right_ir_id: String,
    ) {
        let dst_ir_id_c = CString::new(dst_ir_id.clone()).unwrap();
        let left = *self.symbol_table.get(&left_ir_id).unwrap();
        let right = *self.symbol_table.get(&right_ir_id).unwrap();

        use ast::Expr_Binary_Op as Op;
        let dst = if is_fp {
            use llvm_sys::LLVMRealPredicate as P;
            let pred = match op {
                Op::EQ => P::LLVMRealOEQ,
                Op::NEQ => P::LLVMRealONE,
                Op::LESS => P::LLVMRealOLT,
                Op::GT => P::LLVMRealOGT,
                Op::LEQ => P::LLVMRealOLE,
                Op::GEQ => P::LLVMRealOGE,
                _ => unreachable!(),
            };
            unsafe {
                llvm_sys::core::LLVMBuildFCmp(
                    self.builder,
                    pred,
                    left,
                    right,
                    dst_ir_id_c.as_ptr(),
                )
            }
        } else {
            use llvm_sys::LLVMIntPredicate as P;
            let pred = match op {
                Op::EQ => P::LLVMIntEQ,
                Op::NEQ => P::LLVMIntNE,
                Op::LESS if is_signed => P::LLVMIntSLT,
                Op::GT if is_signed => P::LLVMIntSGT,
                Op::LEQ if is_signed => P::LLVMIntSLE,
                Op::GEQ if is_signed => P::LLVMIntSGE,
                Op::LESS => P::LLVMIntULT,
                Op::GT => P::LLVMIntUGT,
                Op::LEQ => P::LLVMIntULE,
                Op::GEQ => P::LLVMIntUGE,
                _ => unreachable!(),
            };
            unsafe {
                llvm_sys::core::LLVMBuildICmp(
                    self.builder,
                    pred,
                    left,
                    right,
                    dst_ir_id_c.as_ptr(),
                )
            }
        };
        self.symbol_table.insert(dst_ir_id, dst);
    }

    fn create_ptr_add(
        &mut self,
        dst_ir_id: &str,
        ptr_ir_id: &str,
        offset_ir_id: &str,
    ) {
        let dst_ir_id_c = CString::new(dst_ir_id.clone()).unwrap();
        let ptr = *self.symbol_table.get(ptr_ir_id).unwrap();
        let mut offset = *self.symbol_table.get(offset_ir_id).unwrap();
        let dst = unsafe {
            llvm_sys::core::LLVMBuildGEP(
                self.builder,
                ptr,
                &mut offset,
                1,
                dst_ir_id_c.as_ptr(),
            )
        };
        self.symbol_table.insert(dst_ir_id.to_string(), dst);
    }

    fn create_call(
        &mut self,
        dst_ir_id: &str,
        func_ir_id: &str,
        arg_ir_ids: &Vec<String>,
    ) {
        let dst_ir_id_c = CString::new(dst_ir_id.clone()).unwrap();
        let func = *self.symbol_table.get(func_ir_id).unwrap();
        let mut args: Vec<llvm_sys::prelude::LLVMValueRef> = arg_ir_ids
            .into_iter()
            .map(|ir_id| *self.symbol_table.get(ir_id).unwrap())
            .collect();
        let dst = unsafe {
            llvm_sys::core::LLVMBuildCall(
                self.builder,
                func,
                args.as_mut_ptr(),
                args.len() as u32,
                dst_ir_id_c.as_ptr(),
            )
        };
        self.symbol_table.insert(dst_ir_id.to_string(), dst);
    }

    fn enter_switch(&mut self, ir_id: &str, default_bb_id: &str) {
        let v = *self.symbol_table.get(ir_id).unwrap();
        let default_bb = *self.basic_blocks.get(default_bb_id).unwrap();
        let switch_inst = unsafe {
            llvm_sys::core::LLVMBuildSwitch(self.builder, v, default_bb, 10)
        };
        self.switch_stack.push(switch_inst);
    }

    fn leave_switch(&mut self) {
        self.switch_stack.pop();
    }

    fn add_switch_case(&mut self, c: &ConstantOrIrValue, bb_id: &str) {
        let inst = *self.switch_stack.last().unwrap();
        let (value, _) = self.get_llvm_constant(c);
        let bb = *self.basic_blocks.get(bb_id).unwrap();
        unsafe {
            llvm_sys::core::LLVMAddCase(inst, value, bb);
        }
    }

    fn create_br(&mut self, bb_id: &str) {
        let bb = *self.basic_blocks.get(bb_id).unwrap();
        unsafe {
            llvm_sys::core::LLVMBuildBr(self.builder, bb);
        }
    }

    fn create_cond_br(
        &mut self,
        ir_id: &str,
        then_bb_id: &str,
        else_bb_id: &str,
    ) {
        let cond = *self.symbol_table.get(ir_id).unwrap();
        let then_bb = *self.basic_blocks.get(then_bb_id).unwrap();
        let else_bb = *self.basic_blocks.get(else_bb_id).unwrap();
        unsafe {
            llvm_sys::core::LLVMBuildCondBr(
                self.builder,
                cond,
                then_bb,
                else_bb,
            );
        }
    }

    fn create_return_void(&mut self) {
        unsafe {
            llvm_sys::core::LLVMBuildRetVoid(self.builder);
        }
    }

    fn create_return(&mut self, ir_id: &str) {
        let v = self.symbol_table.get(ir_id).unwrap();
        unsafe {
            llvm_sys::core::LLVMBuildRet(self.builder, *v);
        }
    }

    fn print_to_file(&mut self, file_name: &str) {
        let file_name_c = CString::new(file_name).unwrap();
        unsafe {
            let mut err_msg_c: *mut i8 = ptr::null_mut();
            llvm_sys::core::LLVMPrintModuleToFile(
                self.module,
                file_name_c.as_ptr(),
                &mut err_msg_c,
            );
            if !err_msg_c.is_null() {
                let msg = CStr::from_ptr(err_msg_c).to_str().unwrap();
                panic!("Error returned by LLVM backend: {}", msg);
            }
        }
    }
}

#[cfg(feature = "llvm-sys")]
type LLVMBuilder = LLVMBuilderImpl;

#[cfg(not(feature = "llvm-sys"))]
type LLVMBuilder = DummyIRBuilder;

type C4IRBuilder = DummyIRBuilder; // TODO: implement our own IR

struct Compiler<'a> {
    translation_unit: &'a ast::TranslationUnit,
    current_scope: Scope,
    next_uuid: u32,

    // ir_id => binary representation
    // TODO: value should be Either<Vec<u8>, Initializer>
    global_constants: HashMap<String, Vec<u8>>,

    c4ir_builder: C4IRBuilder,
    llvm_builder: LLVMBuilder,
}

type L<'a, T> = (T, &'a ast::Loc);

impl Compiler<'_> {
    fn visit(tu: ast::TranslationUnit) {
        let mut cc = Compiler {
            translation_unit: &tu,
            current_scope: Scope::new(),
            next_uuid: 1_000,
            global_constants: HashMap::new(),
            c4ir_builder: C4IRBuilder::new(),
            llvm_builder: LLVMBuilder::new(),
        };
        for ed in tu.eds.iter() {
            if ed.has_fd() {
                cc.visit_function_def(ed.get_fd())
            } else {
                cc.visit_declaration(ed.get_dl())
            }
        }

        cc.c4ir_builder.print_to_file("-");
        cc.llvm_builder.print_to_file("-");
    }

    fn visit_function_def(&mut self, fd: &ast::FunctionDef) {
        let (scs, rtp) = self.visit_declaration_specifiers(fd.get_dss(), true);
        // unwrap_declarator(is_function_definition=true) enters a new scope
        // without leaving it at the end.
        let (ftp, fname) =
            self.unwrap_declarator(rtp, (fd.get_d(), fd.get_d_loc()), true);
        let (ftp_nq, typed_func_params) = match &ftp.tp {
            Type::Function(rtp, None) => (
                Type::Function(
                    rtp.clone(),
                    Some(FuncParams::Typed(vec![], false)),
                ),
                vec![],
            ),
            Type::Function(_, Some(FuncParams::Typed(params, _))) => {
                // 3.7.1: If the declarator includes a parameter type list, the
                // declaration of each parameter shall include an identifier. No
                // declaration list shall follow.
                params.into_iter().for_each(|p| {
                    if p.name.is_none() {
                        panic!(
                            "{}: Parameter name missing",
                            Compiler::format_loc(fd.get_d_loc())
                        )
                    }
                });
                if !fd.get_dls().is_empty() {
                    let msg = "Function declaration lists shall not be used \
                         when parameters are typed in function declarator";
                    panic!("{}: {}", Compiler::format_loc(fd.get_d_loc()), msg)
                }
                (ftp.tp.clone(), params.clone())
            }
            Type::Function(rtp, Some(FuncParams::Names(names))) => {
                // 3.7.1: The declarator in a function definition specifies the
                // name of the function being defined and the identifiers of its
                // parameters... If the declarator includes an identifier list,
                // the types of the parameters may be declared in a following
                // declaration list. Any parameter that is not declared has type
                // int.
                let mut decls = HashMap::new();
                let mut is_register_decls = HashSet::new();
                // populate `decls`
                fd.get_dls().into_iter().zip(fd.get_dl_locs()).for_each(
                    |(decl, decl_loc)| {
                        use ast::StorageClassSpecifier as SCS;
                        let (param_scs, param_base_tp) = self
                            .visit_declaration_specifiers(
                                decl.get_dss(),
                                !decl.get_ids().is_empty(),
                            );
                        let param_is_register =
                            param_scs.map(|(s, _)| s) == Some(SCS::REGISTER);
                        if param_scs.is_some() && !param_is_register {
                            panic!(
                                "{}: Illegal storage class specifier",
                                Compiler::format_loc(decl_loc)
                            )
                        }
                        decl.get_ids().into_iter().for_each(|id| {
                            if id.init_idx != 0 {
                                panic!(
                                    "{}: Unexpected initializer",
                                    Compiler::format_loc(id.get_d_loc())
                                )
                            }
                            let (param_tp, param_name) = self
                                .unwrap_declarator(
                                    param_base_tp.clone(),
                                    (id.get_d(), id.get_d_loc()),
                                    false, // it's a param decl, not a func def
                                );
                            if decls.contains_key(&param_name) {
                                panic!(
                                    "{}: Redefinition of '{}'",
                                    Compiler::format_loc(id.get_d_loc()),
                                    param_name
                                )
                            }
                            if param_is_register {
                                is_register_decls.insert(param_name.clone());
                            }
                            decls.insert(param_name, param_tp);
                        })
                    },
                );
                let params: Vec<TypedFuncParam> = names
                    .into_iter()
                    .map(|name| TypedFuncParam {
                        is_register: is_register_decls.contains(name),
                        tp: decls.get(name).map_or_else(
                            || QType::from(Type::Int),
                            |tp| tp.clone(),
                        ),
                        name: Some(name.clone()),
                    })
                    .collect();
                (
                    Type::Function(
                        rtp.clone(),
                        Some(FuncParams::Typed(params.clone(), false)),
                    ),
                    params,
                )
            }
            _ => unreachable!(),
        };
        let ftp = QType::from(ftp_nq);
        // `ftp.tp` is now guaranteed to be a
        // Type::Function(..., Some(FuncParams::Typed(...)));
        // `TypedFuncParam.name` is guaranteed to be non-empty.
        self.add_declaration(&fname, &scs, ftp.clone(), true, fd.get_d_loc());
        typed_func_params.iter().for_each(|param| {
            let param_scs = if param.is_register {
                Some((ast::StorageClassSpecifier::REGISTER, fd.get_d_loc()))
            } else {
                None
            };
            // insert the mapping from C param decl `int x` to the IR variable
            // `i32* $.1004` into current_scope.
            self.add_declaration(
                &param.name.clone().unwrap(),
                &param_scs,
                param.tp.clone(),
                false,
                fd.get_d_loc(),
            );
        });
        let linkage = match self
            .current_scope
            .outer_scope
            .as_ref() // for Box
            .as_ref() // for Option
            .unwrap()
            .ordinary_ids_ns
            .get(&fname)
        {
            Some(OrdinaryIdRef::ObjFnRef(_, _, linkage, _)) => linkage.clone(),
            _ => unreachable!(),
        };
        let param_ir_ids = typed_func_params
            .iter()
            .map(|p| p.name.clone().unwrap())
            .map(|name| match self.current_scope.ordinary_ids_ns.get(&name) {
                Some(OrdinaryIdRef::ObjFnRef(ir_id, _, _, _)) => ir_id.clone(),
                _ => unreachable!(),
            })
            .collect();
        self.c4ir_builder
            .create_function(&fname, &ftp, linkage, &param_ir_ids);
        self.llvm_builder
            .create_function(&fname, &ftp, linkage, &param_ir_ids);

        // current_scope assumes arguments are all lvalues, but
        // IRBuilder.create_function creates regular values; i.e. for argument
        // `int x`:
        //   current_scope maintains the mapping "x" => `i32* %.1004`
        //   create_function creates `i32 %.1004`
        // so once we are in the entry basic block, we need to rewrite
        //   current_scope: "x" => `i32* %.1004`
        //   IR: void f(i32 %.1004)
        // to:
        //   current_scope: "x" => `i32* %.1005`
        //   IR: void f(i32 %.1004) {
        //         i32* %.1005 = alloc i32
        //         store %.1004 %.1005
        //       }

        let entry_bb_id = format!("$entry.{}", self.get_next_uuid());
        self.c4ir_builder.create_basic_block(&entry_bb_id);
        self.llvm_builder.create_basic_block(&entry_bb_id);
        self.c4ir_builder.set_current_basic_block(&entry_bb_id);
        self.llvm_builder.set_current_basic_block(&entry_bb_id);

        // argument rewrite as mentioned above
        for param in typed_func_params {
            let name = param.name.unwrap();
            let new_ir_id = self.get_next_ir_id();
            let r = self.current_scope.ordinary_ids_ns.get_mut(&name);
            match r {
                Some(OrdinaryIdRef::ObjFnRef(ir_id, _, _, _)) => {
                    self.c4ir_builder.create_definition(
                        false,
                        &new_ir_id,
                        &param.tp,
                        Linkage::NONE,
                        &None,
                    );
                    self.llvm_builder.create_definition(
                        false,
                        &new_ir_id,
                        &param.tp,
                        Linkage::NONE,
                        &None,
                    );
                    self.c4ir_builder
                        .create_store(new_ir_id.clone(), ir_id.clone());
                    self.llvm_builder
                        .create_store(new_ir_id.clone(), ir_id.clone());
                    mem::replace(ir_id, new_ir_id);
                }
                _ => unreachable!(),
            }
        }

        let rtp = match &ftp.tp {
            Type::Function(rtp, _) => *rtp.clone(),
            _ => unreachable!(),
        };
        let mut func_def_ctx = FuncDefCtx {
            func_name: fname,
            return_type: rtp,
            basic_blocks: HashMap::new(),
            unresolved_labels: HashMap::new(),
            switch_stack: Vec::new(),
            break_bb_stack: Vec::new(),
            continue_bb_stack: Vec::new(),
        };

        let err = self.visit_compound_stmt(fd.get_body(), &mut func_def_ctx);
        if err.is_err() {
            let err = err.unwrap_err();
            panic!("{}: {}", Compiler::format_loc(&err.loc), err.msg)
        }

        func_def_ctx.unresolved_labels.into_iter().last().map(
            |(label, loc)| {
                panic!(
                    "{}: Label '{}' not declared",
                    Compiler::format_loc(&loc),
                    label
                )
            },
        );

        self.leave_scope();
    }

    fn visit_declaration(&mut self, dl: &ast::Declaration) {
        let (storage_class_specifier, qualified_type) = self
            .visit_declaration_specifiers(
                dl.get_dss(),
                !dl.get_ids().is_empty(),
            );

        let scs = storage_class_specifier.map(|(scs, _)| scs);

        use ast::StorageClassSpecifier as SCS;
        if scs == Some(SCS::TYPEDEF) {
            dl.ids.iter().for_each(|id| {
                let (tp, name) = self.unwrap_declarator(
                    qualified_type.clone(),
                    (id.get_d(), id.get_d_loc()),
                    false,
                );
                let old_value = self.current_scope.ordinary_ids_ns.insert(
                    name.clone(),
                    OrdinaryIdRef::TypedefRef(Box::new(tp)),
                );
                if old_value.is_some() {
                    panic!(
                        "{}: Redefinition of typedef {}",
                        Compiler::format_loc(id.get_d_loc()),
                        name
                    );
                }
            });
        } else {
            dl.get_ids().into_iter().for_each(|id| {
                let (qtype, name) = self.unwrap_declarator(
                    qualified_type.clone(),
                    (id.get_d(), id.get_d_loc()),
                    false,
                );
                self.add_declaration(
                    &name,
                    &storage_class_specifier,
                    qtype,
                    false,
                    id.get_d_loc(),
                );
                let id_ref =
                    self.current_scope.ordinary_ids_ns.get(&name).unwrap();
                let (ir_id, qtype, linkage, is_defined) = match id_ref {
                    OrdinaryIdRef::ObjFnRef(
                        ir_id,
                        qtype,
                        linkage,
                        is_defined,
                    ) => (ir_id.clone(), qtype.clone(), *linkage, *is_defined),
                    _ => unreachable!(),
                };

                // 3.1.2.4: An object declared with external or internal
                // linkage, or with the storage-class specifier static has
                // static storage duration
                let is_global = self.current_scope.outer_scope.is_none()
                    || scs == Some(SCS::STATIC);

                let init = if id.init_idx == 0 {
                    Option::None
                } else if is_defined {
                    // covers internal linkage global redefinitions, e.g.:
                    //
                    // static int a = 0;
                    // static int a = 0;
                    //
                    // rest cases should have been checked by add_declaration().
                    panic!(
                        "{}: Redefinition of '{}'",
                        Compiler::format_loc(id.get_init_loc()),
                        name
                    )
                } else {
                    self.current_scope.ordinary_ids_ns.insert(
                        name,
                        OrdinaryIdRef::ObjFnRef(
                            ir_id.clone(),
                            qtype.clone(),
                            linkage,
                            true, // is_defined
                        ),
                    );
                    Some(self.visit_initializer(id.init_idx))
                };

                // TODO: check if `init` is compatible with `qtype`
                // TODO: `qtype` could still change at this point
                //
                // 3.5: If an identifier for an object is declared with no
                // linkage, the type for the object shall be complete by the end
                // of its declarator, or by the end of its init-declarator if it
                // has an initializer.
                //
                // 3.7.2: If the declaration of an identifier for an object is a
                // tentative definition and has internal linkage, the declared
                // type shall not be an incomplete type.

                self.c4ir_builder.create_definition(
                    is_global, &ir_id, &qtype, linkage, &init,
                );
                self.llvm_builder.create_definition(
                    is_global, &ir_id, &qtype, linkage, &init,
                );
            });
        }
    }

    fn visit_declaration_specifiers<'a>(
        &mut self,
        dss: &'a [ast::DeclarationSpecifier],
        try_ref_sue_type: bool, // has declarator or within a cast or sizeof()
    ) -> (Option<L<'a, ast::StorageClassSpecifier>>, QType) {
        let mut storage_class_specifiers: Vec<L<ast::StorageClassSpecifier>> =
            dss.into_iter()
                .filter(|ds| ds.has_scs())
                .map(|ds| (ds.get_scs(), ds.get_loc()))
                .collect();
        let type_specifiers: Vec<L<&ast::TypeSpecifier>> = dss
            .into_iter()
            .filter(|ds| ds.has_ts())
            .map(|ds| (ds.get_ts(), ds.get_loc()))
            .collect();
        let type_qualifiers: Vec<L<ast::TypeQualifier>> = dss
            .into_iter()
            .filter(|ds| ds.has_tq())
            .map(|ds| (ds.get_tq(), ds.get_loc()))
            .collect();

        // 3.5.1: At most one storage-class specifier may be given in the
        // declaration specifiers in a declaration.
        // 3.5.4.3: The storage-class specifier in the declaration specifiers...
        if 1 < storage_class_specifiers.len() {
            panic!(
                "{}: More than one storage class specifier found",
                Compiler::format_loc(storage_class_specifiers[1].1)
            );
        }

        let qualified_type: QType = Compiler::qualify_type(
            &type_qualifiers,
            self.get_type(&type_specifiers, try_ref_sue_type),
        );

        (storage_class_specifiers.pop(), qualified_type)
    }

    fn visit_initializer(&mut self, init_idx: i32) -> Initializer {
        let is_global_decl = self.current_scope.outer_scope.is_none();
        let init = &self.translation_unit.initializers[init_idx as usize];
        match &init.init {
            Some(ast::Initializer_oneof_init::expr(expr)) => {
                let e = &self.translation_unit.exprs[expr.e as usize];
                let (qtype, result) = self.visit_expr(
                    (e, expr.get_e_loc()),
                    true,
                    !is_global_decl,
                );
                let (qtype, result) = self.convert_lvalue_and_func_designator(
                    qtype, result, true, true, true,
                );
                match result {
                    None => panic!(
                        "{}: Global definition initializer must evaluate to \
                         constants",
                        Compiler::format_loc(expr.get_e_loc())
                    ),
                    Some(r) => Initializer::Expr(qtype, r),
                }
            }
            Some(ast::Initializer_oneof_init::field_struct(st)) => {
                Initializer::Struct(
                    st.inits
                        .iter()
                        .map(|idx| self.visit_initializer(*idx))
                        .collect(),
                )
            }
            None => {
                panic!("Invalid AST input: initializer #{} is empty", init_idx)
            }
        }
    }

    // fold_constant=false could be used for type-inference only scenarios, e.g.
    // sizeof(1/0); the 2nd return value could be None if fold_constant=false,
    // or emit_ir=false but the expression is not a constant.
    fn visit_expr(
        &mut self,
        e: L<&ast::Expr>,
        fold_constant: bool,
        emit_ir: bool,
    ) -> (QType, Option<ConstantOrIrValue>) {
        if e.0.e.is_none() {
            panic!(
                "{}: Invalid AST input; Expr.e is missing",
                Compiler::format_loc(e.1)
            )
        }
        match e.0.e.as_ref().unwrap() {
            ast::Expr_oneof_e::id(id) => {
                match self.current_scope.lookup_ordinary_id(id) {
                    None => panic!(
                        "{}: Undefined variable '{}'",
                        Compiler::format_loc(e.1),
                        id
                    ),
                    Some((OrdinaryIdRef::TypedefRef(_), _)) => panic!(
                        "{}: Unexpected type name '{}' in expression",
                        Compiler::format_loc(e.1),
                        id
                    ),
                    Some((OrdinaryIdRef::EnumRef(value), _)) => (
                        QType::from(Type::Int),
                        Some(ConstantOrIrValue::I32(*value)),
                    ),
                    Some((OrdinaryIdRef::ObjFnRef(ir_id, tp, _, _), _)) => {
                        if !emit_ir {
                            (tp.clone(), None) // not a constant expr
                        } else {
                            let v =
                                ConstantOrIrValue::IrValue(ir_id.clone(), true);
                            (tp.clone(), Some(v))
                        }
                    }
                }
            }
            ast::Expr_oneof_e::integer(int) => match (int.signed, int.size) {
                (true, ast::Expr_Integer_Size::INT8) => (
                    QType::from(Type::Char),
                    Some(ConstantOrIrValue::I8(int.n as i8)),
                ),
                (false, ast::Expr_Integer_Size::INT8) => (
                    QType::from(Type::UnsignedChar),
                    Some(ConstantOrIrValue::U8(int.n as u8)),
                ),
                (true, ast::Expr_Integer_Size::INT16) => (
                    QType::from(Type::Short),
                    Some(ConstantOrIrValue::I16(int.n as i16)),
                ),
                (false, ast::Expr_Integer_Size::INT16) => (
                    QType::from(Type::UnsignedShort),
                    Some(ConstantOrIrValue::U16(int.n as u16)),
                ),
                (true, ast::Expr_Integer_Size::INT32) => (
                    QType::from(Type::Int),
                    Some(ConstantOrIrValue::I32(int.n as i32)),
                ),
                (false, ast::Expr_Integer_Size::INT32) => (
                    QType::from(Type::UnsignedInt),
                    Some(ConstantOrIrValue::U32(int.n as u32)),
                ),
                (true, ast::Expr_Integer_Size::INT64) => (
                    QType::from(Type::Long),
                    Some(ConstantOrIrValue::I64(int.n as i64)),
                ),
                (false, ast::Expr_Integer_Size::INT64) => (
                    QType::from(Type::UnsignedLong),
                    Some(ConstantOrIrValue::U64(int.n as u64)),
                ),
            },
            ast::Expr_oneof_e::float(f) => {
                (QType::from(Type::Float), Some(ConstantOrIrValue::Float(*f)))
            }
            ast::Expr_oneof_e::double(d) => (
                QType::from(Type::Double),
                Some(ConstantOrIrValue::Double(*d)),
            ),
            ast::Expr_oneof_e::char(c) => (
                QType::from(Type::Char),
                Some(ConstantOrIrValue::I8(*c as i8)),
            ),
            ast::Expr_oneof_e::wide_char(wc) => (
                QType::from(Type::Short),
                Some(ConstantOrIrValue::I16(*wc as i16)),
            ),
            ast::Expr_oneof_e::string(str) => {
                let mut buf = str.clone().into_bytes();
                buf.push(0);
                let len = buf.len() as u32;

                let ir_id = self.get_next_ir_id();
                self.global_constants.insert(ir_id.clone(), buf.clone());

                self.c4ir_builder
                    .create_constant_buffer(ir_id.clone(), buf.clone());
                self.llvm_builder.create_constant_buffer(ir_id.clone(), buf);

                let tp = QType::from(Type::Array(
                    Box::new(QType::from(Type::Char)),
                    Some(len),
                ));
                (tp, Some(ConstantOrIrValue::Address(ir_id, 0)))
            }
            ast::Expr_oneof_e::wide_string(ws) => {
                let mut buf: Vec<u8> = vec![];
                ws.encode_utf16().for_each(|v| {
                    buf.push(v as u8); // assuming little-endian
                    buf.push((v >> 8) as u8);
                });
                buf.push(0);
                buf.push(0);
                let len = buf.len() as u32;

                let ir_id = self.get_next_ir_id();
                self.global_constants.insert(ir_id.clone(), buf.clone());

                self.c4ir_builder
                    .create_constant_buffer(ir_id.clone(), buf.clone());
                self.llvm_builder.create_constant_buffer(ir_id.clone(), buf);

                let tp = QType::from(Type::Array(
                    Box::new(QType::from(Type::Short)),
                    Some(len),
                ));
                (tp, Some(ConstantOrIrValue::Address(ir_id, 0)))
            }
            ast::Expr_oneof_e::cast(cast) => {
                self.visit_cast_expr((cast, e.1), fold_constant, emit_ir)
            }
            ast::Expr_oneof_e::arr_sub(arr_sub) => self.visit_arr_sub_expr(
                (
                    &self.translation_unit.exprs[arr_sub.arr_idx as usize],
                    arr_sub.get_arr_loc(),
                ),
                (
                    &self.translation_unit.exprs[arr_sub.sub_idx as usize],
                    arr_sub.get_sub_loc(),
                ),
                fold_constant,
                emit_ir,
            ),
            ast::Expr_oneof_e::func_call(func_call) => self
                .visit_func_call_expr(func_call, fold_constant, emit_ir)
                .unwrap_or_else(|err| err.panic()),
            ast::Expr_oneof_e::dot(dot) => {
                let left = &self.translation_unit.exprs[dot.e_idx as usize];
                let left = (left, dot.get_e_loc());
                let field = (dot.get_field(), dot.get_field_loc());
                self.visit_member_access_expr(
                    left,
                    field,
                    true,
                    fold_constant,
                    emit_ir,
                )
                .unwrap_or_else(|err| err.panic())
            }
            ast::Expr_oneof_e::ptr(ptr) => {
                let left = &self.translation_unit.exprs[ptr.e_idx as usize];
                let left = (left, ptr.get_e_loc());
                let field = (ptr.get_field(), ptr.get_field_loc());
                self.visit_member_access_expr(
                    left,
                    field,
                    false,
                    fold_constant,
                    emit_ir,
                )
                .unwrap_or_else(|err| err.panic())
            }
            ast::Expr_oneof_e::sizeof_val(sizeof_val) => {
                let arg = (
                    &self.translation_unit.exprs[sizeof_val.e_idx as usize],
                    sizeof_val.get_e_loc(),
                );
                let (tp, _) = self.visit_expr(arg, false, false);
                let (size, _) =
                    Compiler::get_type_size_and_align_bytes(&tp.tp).unwrap();
                (
                    QType::from(Type::UnsignedLong),
                    Some(ConstantOrIrValue::U64(size as u64)),
                )
            }
            ast::Expr_oneof_e::sizeof_tp(sizeof_tp) => {
                let tp = self.visit_type_name((
                    sizeof_tp.get_tp(),
                    sizeof_tp.get_tp_loc(),
                ));
                let (size, _) =
                    Compiler::get_type_size_and_align_bytes(&tp.tp).unwrap();
                (
                    QType::from(Type::UnsignedLong),
                    Some(ConstantOrIrValue::U64(size as u64)),
                )
            }
            ast::Expr_oneof_e::unary(unary) => {
                let arg = (
                    &self.translation_unit.exprs[unary.e_idx as usize],
                    unary.get_e_loc(),
                );
                let (tp, arg) = self.visit_expr(arg, fold_constant, emit_ir);
                self.visit_unary_op(
                    &tp,
                    arg,
                    unary.get_e_loc(),
                    unary.op,
                    fold_constant,
                    emit_ir,
                )
            }
            ast::Expr_oneof_e::binary(binary) => {
                let left = (
                    &self.translation_unit.exprs[binary.e1_idx as usize],
                    binary.get_e1_loc(),
                );
                let right = (
                    &self.translation_unit.exprs[binary.e2_idx as usize],
                    binary.get_e2_loc(),
                );
                if binary.op == ast::Expr_Binary_Op::LOGIC_AND
                    || binary.op == ast::Expr_Binary_Op::LOGIC_OR
                {
                    self.visit_special_binary_op(
                        left,
                        right,
                        binary.op,
                        fold_constant,
                        emit_ir,
                    )
                } else {
                    let (tp_left, left) =
                        self.visit_expr(left, fold_constant, emit_ir);
                    let (tp_right, right) =
                        self.visit_expr(right, fold_constant, emit_ir);
                    self.visit_simple_binary_op(
                        &tp_left,
                        left,
                        binary.get_e1_loc(),
                        &tp_right,
                        right,
                        binary.get_e2_loc(),
                        binary.op,
                        fold_constant,
                        emit_ir,
                    )
                }
            }
            ast::Expr_oneof_e::ternary(ternary) => {
                let e_cond = (
                    &self.translation_unit.exprs[ternary.cond_idx as usize],
                    ternary.get_cond_loc(),
                );
                let e_then = (
                    &self.translation_unit.exprs[ternary.then_idx as usize],
                    ternary.get_then_loc(),
                );
                let e_else = (
                    &self.translation_unit.exprs[ternary.else_idx as usize],
                    ternary.get_else_loc(),
                );
                self.visit_ternary_op(
                    e_cond,
                    e_then,
                    e_else,
                    fold_constant,
                    emit_ir,
                )
            }
        }
    }

    fn visit_cast_expr(
        &mut self,
        e: L<&ast::Expr_Cast>,
        fold_constant: bool,
        emit_ir: bool,
    ) -> (QType, Option<ConstantOrIrValue>) {
        let dst_tp = self.visit_type_name((e.0.get_tp(), e.0.get_tp_loc()));

        let expr = &self.translation_unit.exprs[e.0.e_idx as usize];
        let (src_tp, v) =
            self.visit_expr((expr, e.0.get_e_loc()), fold_constant, emit_ir);

        // 3.2.2.1: auto conversion of array lvalues
        let (src_tp, v) = self
            .convert_lvalue_and_func_designator(src_tp, v, true, true, true);

        self.cast_expression(src_tp, v, dst_tp, e.1, emit_ir)
    }

    fn visit_arr_sub_expr(
        &mut self,
        arr_e: L<&ast::Expr>,
        sub_e: L<&ast::Expr>,
        fold_constant: bool,
        emit_ir: bool,
    ) -> (QType, Option<ConstantOrIrValue>) {
        let (ptr_tp, ptr) = self.visit_expr(arr_e, fold_constant, emit_ir);
        let (sub_tp, sub) = self.visit_expr(sub_e, fold_constant, emit_ir);
        let (ptr_tp, ptr) = self
            .convert_lvalue_and_func_designator(ptr_tp, ptr, true, true, true);
        let elem_tp = match &ptr_tp.tp {
            Type::Pointer(tp) => match tp.tp {
                Type::Function(_, _) | Type::Void => panic!(
                    "{}: Illegal element type for array subscripting",
                    Compiler::format_loc(arr_e.1)
                ),
                _ => *tp.clone(),
            },
            _ => panic!(
                "{}: Illegal type for array subscripting",
                Compiler::format_loc(arr_e.1)
            ),
        };
        if !sub_tp.is_integral_type() {
            panic!(
                "{}: Array index must be an integral type",
                Compiler::format_loc(sub_e.1)
            )
        }
        let r = if ptr.is_none() || sub.is_none() || !fold_constant {
            None
        } else {
            // visit_cast_expr ensures `ptr` could only be U64, Address, or
            // IrValue, and `sub` must be an integral type, or IrValue.
            use ConstantOrIrValue as C;
            if !emit_ir {
                let sub_c: Option<i64> = match sub.as_ref().unwrap() {
                    C::I8(v) => Some(*v as i64),
                    C::U8(v) => Some(*v as i64),
                    C::I16(v) => Some(*v as i64),
                    C::U16(v) => Some(*v as i64),
                    C::I32(v) => Some(*v as i64),
                    C::U32(v) => Some(*v as i64),
                    C::I64(v) => Some(*v),
                    C::U64(v) => Some(*v as i64),
                    _ => None,
                };
                sub_c.and_then(|s| match (ptr.as_ref().unwrap(), &elem_tp.tp) {
                    (C::Address(ir_id, offset_bytes), Type::Char)
                    | (C::Address(ir_id, offset_bytes), Type::UnsignedChar) => {
                        let offset: usize = (*offset_bytes + s) as usize;
                        let buf = self.global_constants.get(ir_id).unwrap();
                        if buf.len() <= offset {
                            panic!(
                                "{}: Index out of bound",
                                Compiler::format_loc(sub_e.1)
                            )
                        }
                        Some(C::I8(buf[offset] as i8))
                    }
                    // technically we can also constant fold wide strings here
                    _ => None,
                })
            } else {
                // T ptr[sub] = *(ptr + sub)
                let (ptr_tp, ptr) = self.visit_simple_binary_op(
                    &ptr_tp,
                    ptr,
                    arr_e.1,
                    &sub_tp,
                    sub,
                    sub_e.1,
                    ast::Expr_Binary_Op::ADD,
                    fold_constant,
                    emit_ir,
                );
                let (_, r) = self.visit_unary_op(
                    &ptr_tp,
                    ptr,
                    arr_e.1,
                    ast::Expr_Unary_Op::DEREF,
                    fold_constant,
                    emit_ir,
                );
                r
            }
        };
        (elem_tp, r)
    }

    fn visit_func_call_expr(
        &mut self,
        func_call: &ast::Expr_FuncCall,
        fold_constant: bool,
        emit_ir: bool,
    ) -> R<(QType, Option<ConstantOrIrValue>)> {
        let func = &self.translation_unit.exprs[func_call.fn_idx as usize];
        let func = (func, func_call.get_fn_loc());
        // Implicit function declarations are allowed in C89 but not C99.
        let (func_tp, func) = self.visit_expr(func, fold_constant, emit_ir);
        let (func_ptr_tp, func) = self.convert_lvalue_and_func_designator(
            func_tp, func, true, true, true,
        );
        let func_tp = match func_ptr_tp.tp {
            Type::Pointer(tp) => *tp,
            _ => c4_fail!(
                func_call.get_fn_loc(),
                "Function pointer type expected"
            ),
        };
        let (rtp, args_decl) = match func_tp.tp {
            Type::Function(rtp, args_decl) => (*rtp, args_decl),
            _ => unreachable!(),
        };
        match &rtp.tp {
            Type::Struct(_) | Type::Union(_) => {
                unimplemented!() // TODO: return struct / union
            }
            // disallowed on construction
            Type::Function(_, _) | Type::Array(_, _) => unreachable!(),
            _ => (),
        };
        let args: Vec<(QType, Option<String>)> = func_call
            .get_args()
            .into_iter()
            .map(|idx| &self.translation_unit.exprs[*idx as usize])
            .zip(func_call.get_arg_locs())
            .collect::<Vec<L<&ast::Expr>>>()
            .into_iter()
            .map(|arg| {
                let (arg_tp, arg) =
                    self.visit_expr(arg, fold_constant, emit_ir);
                let (arg_tp, arg) = self.convert_lvalue_and_func_designator(
                    arg_tp, arg, true, true, true,
                );
                let arg = arg
                    .map(|c| self.convert_to_ir_value(&arg_tp, c))
                    .map(|c| match c {
                        ConstantOrIrValue::IrValue(ir_id, false) => ir_id,
                        _ => unreachable!(),
                    });
                (arg_tp, arg)
            })
            .collect();
        // 3.3.2.2: If the expression that denotes the called function has a
        // type that does not include a prototype, the integral promotions are
        // performed on each argument and arguments that have type float are
        // promoted to double. These are called the default argument promotions.
        let do_default_arg_promo = |cc: &mut Compiler,
                                    arg: (QType, Option<String>)|
         -> Option<String> {
            let (arg_tp, arg) = arg;
            arg.map(|arg| {
                if arg_tp.is_integral_type() {
                    cc.do_integral_promotion_ir(arg, arg_tp).0
                } else if arg_tp.is_arithmetic_type() {
                    let (_, r) = cc.cast_expression(
                        arg_tp,
                        Some(ConstantOrIrValue::IrValue(arg, false)),
                        QType::from(Type::Double),
                        func_call.get_fn_loc(),
                        true,
                    );
                    match r {
                        Some(ConstantOrIrValue::IrValue(ir_id, false)) => ir_id,
                        _ => unreachable!(),
                    }
                } else {
                    arg
                }
            })
        };
        // 3.3.2.2: If the expression that denotes the called function has a
        // type that includes a prototype, the arguments are implicitly
        // converted, as if by assignment, to the types of the corresponding
        // parameters.
        let cast_arg = |cc: &mut Compiler,
                        dst_tp: &QType,
                        arg: (QType, Option<String>)|
         -> Option<String> {
            let (arg_tp, arg) = arg;
            arg.map(|arg| {
                let vp_ir_id = cc.get_next_ir_id();
                cc.c4ir_builder.create_definition(
                    false,
                    &vp_ir_id,
                    dst_tp,
                    Linkage::NONE,
                    &None,
                );
                cc.llvm_builder.create_definition(
                    false,
                    &vp_ir_id,
                    dst_tp,
                    Linkage::NONE,
                    &None,
                );

                cc.visit_simple_binary_op(
                    dst_tp,
                    Some(ConstantOrIrValue::IrValue(vp_ir_id.clone(), true)),
                    func_call.get_fn_loc(),
                    &arg_tp,
                    Some(ConstantOrIrValue::IrValue(arg, false)),
                    func_call.get_fn_loc(),
                    ast::Expr_Binary_Op::ASSIGN,
                    true,
                    true,
                );

                let v_ir_id = cc.get_next_ir_id();
                cc.c4ir_builder.create_load(
                    v_ir_id.clone(),
                    vp_ir_id.clone(),
                    dst_tp,
                );
                cc.llvm_builder.create_load(
                    v_ir_id.clone(),
                    vp_ir_id.clone(),
                    dst_tp,
                );
                v_ir_id
            })
        };
        let args: Vec<Option<String>> = match args_decl {
            None => args
                .into_iter()
                .map(|arg| do_default_arg_promo(self, arg))
                .collect(),
            Some(FuncParams::Typed(typed_params, is_varargs)) => {
                if args.len() < typed_params.len() {
                    c4_fail!(
                        func_call.get_fn_loc(),
                        "Not enough arguments passed: expecting {}, got {}",
                        typed_params.len(),
                        args.len()
                    )
                } else if args.len() != typed_params.len() && !is_varargs {
                    c4_fail!(
                        func_call.get_fn_loc(),
                        "Too many arguments passed: expecting {}, got {}",
                        typed_params.len(),
                        args.len()
                    )
                }
                let mut to_be_casted_args = args;
                let to_be_promoted_args =
                    to_be_casted_args.split_off(typed_params.len());

                let to_be_casted_args: Vec<Option<String>> = to_be_casted_args
                    .into_iter()
                    .zip(typed_params)
                    .map(|(arg, typed_param)| {
                        cast_arg(self, &typed_param.tp, arg)
                    })
                    .collect();
                to_be_casted_args
                    .into_iter()
                    .chain(
                        to_be_promoted_args
                            .into_iter()
                            .map(|arg| do_default_arg_promo(self, arg)),
                    )
                    .collect()
            }
            Some(FuncParams::Names(names)) => {
                if args.len() != names.len() {
                    c4_fail!(
                        func_call.get_fn_loc(),
                        "Incorrect number of arguments: expecting {}, got {}",
                        names.len(),
                        args.len()
                    )
                }
                args.into_iter()
                    .map(|arg| do_default_arg_promo(self, arg))
                    .collect()
            }
        };

        if !emit_ir || func.is_none() || args.contains(&None) {
            Ok((rtp, None))
        } else {
            let func = match func.unwrap() {
                ConstantOrIrValue::IrValue(ir_id, false) => ir_id,
                _ => unreachable!(),
            };
            let args: Vec<String> =
                args.into_iter().map(|arg| arg.unwrap()).collect();
            let ir_id = self.get_next_ir_id();
            self.c4ir_builder.create_call(&ir_id, &func, &args);
            self.llvm_builder.create_call(&ir_id, &func, &args);
            Ok((rtp, Some(ConstantOrIrValue::IrValue(ir_id, false))))
        }
    }

    fn visit_member_access_expr(
        &mut self,
        left: L<&ast::Expr>,
        field: L<&str>,
        is_dot: bool,
        fold_constant: bool,
        emit_ir: bool,
    ) -> R<(QType, Option<ConstantOrIrValue>)> {
        let left_loc = left.1;
        let (left_tp, left) = self.visit_expr(left, fold_constant, emit_ir);
        let (left_tp, left) = self.convert_lvalue_and_func_designator(
            left_tp, left, !is_dot, true, true,
        );
        let mut is_const = left_tp.is_const;
        let mut is_volatile = left_tp.is_volatile;
        let su_type_is_struct: (&SuType, bool) = match &left_tp.tp {
            Type::Struct(su_type) if is_dot => (su_type.borrow(), true),
            Type::Union(su_type) if is_dot => (su_type.borrow(), false),
            _ if is_dot => c4_fail!(left_loc, "Struct or union expected"),

            Type::Pointer(tp) => {
                is_const = tp.is_const;
                is_volatile = tp.is_volatile;
                match &tp.tp {
                    Type::Struct(su_type) => (su_type.borrow(), true),
                    Type::Union(su_type) => (su_type.borrow(), false),
                    _ => c4_fail!(
                        left_loc,
                        "Pointer to struct or union expected"
                    ),
                }
            }
            _ => c4_fail!(left_loc, "Pointer to struct or union expected"),
        };
        let (su_type, is_struct) = su_type_is_struct;

        let fields = match &su_type.fields {
            None => c4_fail!(
                left_loc,
                "Expression has incomplete struct/union type"
            ),
            Some(fields) => fields,
        };
        // This assumes `field` does not have a bit mask. Things would be more
        // complicated if otherwise.
        let (offset, field_tp) = {
            let prev_fields: Vec<SuField> = fields
                .into_iter()
                .take_while(|f| f.name != Some(field.0.to_string()))
                .map(|f| f.clone())
                .collect();
            if prev_fields.len() >= fields.len() {
                c4_fail!(field.1, "Field '{}' not found", field.0)
            }
            let field_tp = &fields[prev_fields.len()].tp;
            if fields[prev_fields.len()].bit_field_size.is_some() {
                unimplemented!() // TODO: support bit masks (breaks assumptions)
            }
            if is_struct {
                let (offset, _) = if prev_fields.is_empty() {
                    (0, 0)
                } else {
                    Compiler::get_type_size_and_align_bytes(&Type::Struct(
                        Box::new(SuType {
                            fields: Some(prev_fields),
                            uuid: 0, // unused
                        }),
                    ))
                    .unwrap()
                };
                let (_, align) =
                    Compiler::get_type_size_and_align_bytes(&field_tp.tp)
                        .unwrap();
                (Compiler::align_up(offset, align), field_tp)
            } else {
                (0, field_tp)
            }
        };

        let rtp = QType {
            is_const,
            is_volatile,
            tp: field_tp.tp.clone(),
        };
        let r = if left.is_none() || !emit_ir {
            // looks like even clang does not support constant folding here
            None
        } else {
            let left = self.convert_to_ir_value(&left_tp, left.unwrap());
            let (ir_value, is_lvalue) = match left {
                ConstantOrIrValue::IrValue(ir_value, is_lvalue) => {
                    (ir_value, is_lvalue)
                }
                _ => unreachable!(),
            };
            if is_lvalue && !is_dot {
                unreachable!() // convert_lvalue_and_func_designator
            }
            // now ir_value is pointer to struct/union head

            let char_ptr_tp = QType::char_ptr_tp();
            let field_ptr_tp = QType::ptr_tp(rtp.clone());

            let field_ptr_ir_id = {
                // (char *) ir_value
                let head_ptr = self.get_next_ir_id();
                self.c4ir_builder.create_cast(
                    head_ptr.clone(),
                    &char_ptr_tp,
                    ir_value.clone(),
                    &field_ptr_tp,
                );
                self.llvm_builder.create_cast(
                    head_ptr.clone(),
                    &char_ptr_tp,
                    ir_value.clone(),
                    &field_ptr_tp,
                );

                // offset: i64
                let offset = ConstantOrIrValue::I64(offset as i64);
                let offset =
                    self.convert_to_ir_value(&QType::from(Type::Long), offset);
                let offset = match offset {
                    ConstantOrIrValue::IrValue(ir_id, false) => ir_id,
                    _ => unreachable!(),
                };

                // (char *) ir_value + offset
                let field_char_ptr = self.get_next_ir_id();
                self.c4ir_builder.create_ptr_add(
                    &field_char_ptr,
                    &head_ptr,
                    &offset,
                );
                self.llvm_builder.create_ptr_add(
                    &field_char_ptr,
                    &head_ptr,
                    &offset,
                );

                // (T *) ((char *) ir_value + offset)
                let field_ptr = self.get_next_ir_id();
                self.c4ir_builder.create_cast(
                    field_ptr.clone(),
                    &field_ptr_tp,
                    field_char_ptr.clone(),
                    &char_ptr_tp,
                );
                self.llvm_builder.create_cast(
                    field_ptr.clone(),
                    &field_ptr_tp,
                    field_char_ptr.clone(),
                    &char_ptr_tp,
                );

                field_ptr
            };

            if is_dot && !is_lvalue {
                // 3.3.2.3: (for dot) a member of a structure or union object...
                // is an lvalue if the first expression is an lvalue.
                // 3.3.2.3: (for ->) The value... is an lvalue.
                //
                // * (T *) ((char *) ir_value + offset)
                let ir_id = match &rtp.tp {
                    Type::Struct(_) | Type::Union(_) => field_ptr_ir_id,
                    _ => {
                        let derefed = self.get_next_ir_id();
                        self.c4ir_builder.create_load(
                            derefed.clone(),
                            field_ptr_ir_id.clone(),
                            &field_ptr_tp,
                        );
                        self.llvm_builder.create_load(
                            derefed.clone(),
                            field_ptr_ir_id.clone(),
                            &field_ptr_tp,
                        );
                        derefed
                    }
                };
                Some(ConstantOrIrValue::IrValue(ir_id, true))
            } else {
                Some(ConstantOrIrValue::IrValue(field_ptr_ir_id, true))
            }
        };
        Ok((rtp, r))
    }

    fn visit_unary_op(
        &mut self,
        tp: &QType,
        arg: Option<ConstantOrIrValue>,
        loc: &ast::Loc,
        op: ast::Expr_Unary_Op,
        fold_constant: bool,
        emit_ir: bool,
    ) -> (QType, Option<ConstantOrIrValue>) {
        use ast::Expr_Binary_Op as BinOp;
        use ast::Expr_Unary_Op as Op;
        let do_deref_lvalue = match op {
            Op::PREFIX_INC
            | Op::PREFIX_DEC
            | Op::POSTFIX_INC
            | Op::POSTFIX_DEC
            | Op::REF => false,
            _ => true,
        };
        let (arg_tp, arg) = self.convert_lvalue_and_func_designator(
            tp.clone(),
            arg,
            do_deref_lvalue,
            op != Op::REF,
            op != Op::REF,
        );

        match op {
            // ++E is equivalent to (E += 1); --E is analogous to that
            Op::PREFIX_INC | Op::PREFIX_DEC => {
                let bin_op = if op == Op::PREFIX_INC {
                    BinOp::ADD_ASSIGN
                } else {
                    BinOp::SUB_ASSIGN
                };
                self.visit_simple_binary_op(
                    &arg_tp,
                    arg,
                    loc,
                    &QType::from(Type::Int),
                    Some(ConstantOrIrValue::I32(1)),
                    loc,
                    bin_op,
                    fold_constant,
                    emit_ir,
                )
            }
            Op::POSTFIX_INC | Op::POSTFIX_DEC => {
                let (_, ret) = self.convert_lvalue_and_func_designator(
                    arg_tp.clone(),
                    arg.clone(),
                    true,
                    false,
                    false,
                );
                if !emit_ir {
                    (arg_tp, None)
                } else {
                    let bin_op = if op == Op::POSTFIX_INC {
                        BinOp::ADD_ASSIGN
                    } else {
                        BinOp::SUB_ASSIGN
                    };
                    self.visit_simple_binary_op(
                        &arg_tp,
                        arg,
                        loc,
                        &QType::from(Type::Int),
                        Some(ConstantOrIrValue::I32(1)),
                        loc,
                        bin_op,
                        fold_constant,
                        emit_ir,
                    );
                    (arg_tp, ret)
                }
            }
            Op::REF => {
                // TODO: the argument must not be a bit-field
                // TODO: the argument must not be a register variable
                let ptr_tp = QType::ptr_tp(arg_tp);
                if !emit_ir || arg.is_none() {
                    (ptr_tp, None)
                } else {
                    match arg.unwrap() {
                        ConstantOrIrValue::IrValue(ir_id, true) => (
                            ptr_tp,
                            Some(ConstantOrIrValue::IrValue(ir_id, false)),
                        ),
                        _ => panic!(
                            "{}: Expression is not a lvalue",
                            Compiler::format_loc(loc)
                        ),
                    }
                }
            }
            Op::DEREF => {
                let elem_tp = match &arg_tp.tp {
                    Type::Pointer(tp) => *tp.clone(),
                    _ => panic!(
                        "{}: Expression is not a pointer",
                        Compiler::format_loc(loc)
                    ),
                };
                if !emit_ir || arg.is_none() {
                    (elem_tp, None)
                } else {
                    match arg.unwrap() {
                        ConstantOrIrValue::IrValue(ir_id, false) => (
                            elem_tp,
                            Some(ConstantOrIrValue::IrValue(ir_id, true)),
                        ),
                        addr @ ConstantOrIrValue::U64(_) => {
                            match self.convert_to_ir_value(&arg_tp, addr) {
                                ConstantOrIrValue::IrValue(ir_id, false) => (
                                    elem_tp,
                                    Some(ConstantOrIrValue::IrValue(
                                        ir_id, true,
                                    )),
                                ),
                                _ => unreachable!(),
                            }
                        }
                        _ => unreachable!(),
                    }
                }
            }
            Op::POS | Op::NEG => {
                if !arg_tp.is_arithmetic_type() {
                    panic!(
                        "{}: Expression must have arithmetic type",
                        Compiler::format_loc(loc)
                    )
                }
                use ConstantOrIrValue as C;
                let do_neg = op == Op::NEG;
                match arg {
                    Some(C::Address(_, _)) => unreachable!(),

                    Some(C::IrValue(_, true)) => unreachable!(),
                    Some(C::IrValue(ir_id, false)) => {
                        let is_fp = !arg_tp.is_integral_type();
                        let (ir_id, tp) = if is_fp {
                            (ir_id, arg_tp)
                        } else {
                            // 3.3.3.3: The integral promotion is performed
                            self.do_integral_promotion_ir(ir_id, arg_tp)
                        };
                        if do_neg {
                            let new_ir_id = self.get_next_ir_id();
                            self.c4ir_builder
                                .create_neg(&new_ir_id, is_fp, &ir_id);
                            self.llvm_builder
                                .create_neg(&new_ir_id, is_fp, &ir_id);
                            (tp, Some(C::IrValue(new_ir_id, false)))
                        } else {
                            (tp, Some(C::IrValue(ir_id, false)))
                        }
                    }

                    Some(C::Float(v)) if do_neg => (arg_tp, Some(C::Float(-v))),
                    Some(C::Float(v)) => (arg_tp, Some(C::Float(v))),
                    Some(C::Double(v)) if do_neg => {
                        (arg_tp, Some(C::Double(-v)))
                    }
                    Some(C::Double(v)) => (arg_tp, Some(C::Double(v))),

                    Some(C::I64(-0x8000_0000_0000_0000)) => {
                        (arg_tp, Some(C::I64(-0x8000_0000_0000_0000)))
                    }
                    Some(C::I64(v)) if do_neg => (arg_tp, Some(C::I64(!v + 1))),
                    Some(C::I64(v)) => (arg_tp, Some(C::I64(v))),
                    Some(C::U64(0)) => (arg_tp, Some(C::U64(0))),
                    Some(C::U64(v)) if do_neg => (arg_tp, Some(C::U64(!v + 1))),
                    Some(C::U64(v)) => (arg_tp, Some(C::U64(v))),

                    Some(C::U32(v)) if do_neg => {
                        (arg_tp, Some(C::U32(-(v as i64) as u32)))
                    }
                    Some(C::U32(v)) => (arg_tp, Some(C::U32(v))),

                    // 3.3.3.3: The integral promotion is performed
                    Some(c) if do_neg => {
                        let v = -(c.as_constant_u64().unwrap() as i64) as i32;
                        (arg_tp, Some(C::I32(v)))
                    }
                    Some(c) => {
                        let v = c.as_constant_u64().unwrap() as i32;
                        (arg_tp, Some(C::I32(v)))
                    }

                    None => match &arg_tp.tp {
                        Type::Float
                        | Type::Double
                        | Type::UnsignedLong
                        | Type::Long
                        | Type::UnsignedInt => (arg_tp, None),
                        _ => (QType::from(Type::Int), None),
                    },
                }
            }
            Op::BIT_NOT => {
                if !arg_tp.is_integral_type() {
                    panic!(
                        "{}: Cannot apply ~ to non-integral types",
                        Compiler::format_loc(loc)
                    )
                }
                use ConstantOrIrValue as C;
                match arg {
                    None => match &arg_tp.tp {
                        Type::UnsignedLong | Type::Long | Type::UnsignedInt => {
                            (arg_tp, None)
                        }
                        _ => (QType::from(Type::Int), None),
                    },

                    Some(C::Address(_, _))
                    | Some(C::Float(_))
                    | Some(C::Double(_))
                    | Some(C::IrValue(_, true)) => unreachable!(),

                    Some(C::IrValue(ir_id, false)) => {
                        let new_ir_id = self.get_next_ir_id();
                        self.c4ir_builder.create_not(&new_ir_id, &ir_id);
                        self.llvm_builder.create_not(&new_ir_id, &ir_id);
                        (arg_tp, Some(C::IrValue(new_ir_id, false)))
                    }

                    Some(C::U64(v)) => (arg_tp, Some(C::U64(!v))),
                    Some(C::I64(v)) => (arg_tp, Some(C::I64(!v))),
                    Some(C::U32(v)) => (arg_tp, Some(C::U32(!v))),
                    Some(c) => {
                        let v = !c.as_constant_u64().unwrap() as i32;
                        (arg_tp, Some(C::I32(v)))
                    }
                }
            }
            Op::LOGIC_NOT => {
                // 3.3.3.3: The expression !E is equivalent to (0==E).
                self.visit_simple_binary_op(
                    &QType::from(Type::Int),
                    Some(ConstantOrIrValue::I32(0)),
                    loc,
                    &arg_tp,
                    arg,
                    loc,
                    BinOp::EQ,
                    fold_constant,
                    emit_ir,
                )
            }
        }
    }

    // binary ops except && and ||, which perform short-circuit evaluation
    fn visit_simple_binary_op(
        &mut self,
        tp_left: &QType,
        left: Option<ConstantOrIrValue>,
        loc_left: &ast::Loc,
        tp_right: &QType,
        right: Option<ConstantOrIrValue>,
        loc_right: &ast::Loc,
        op: ast::Expr_Binary_Op,
        fold_constant: bool,
        emit_ir: bool,
    ) -> (QType, Option<ConstantOrIrValue>) {
        use ast::Expr_Binary_Op as Op;
        let has_assign = match op {
            Op::ASSIGN
            | Op::MUL_ASSIGN
            | Op::DIV_ASSIGN
            | Op::MOD_ASSIGN
            | Op::ADD_ASSIGN
            | Op::SUB_ASSIGN
            | Op::L_SHIFT_ASSIGN
            | Op::R_SHIFT_ASSIGN
            | Op::BINARY_AND_ASSIGN
            | Op::XOR_ASSIGN
            | Op::BINARY_OR_ASSIGN => true,
            _ => false,
        };
        let (tp_left, left) = self.convert_lvalue_and_func_designator(
            tp_left.clone(),
            left,
            !has_assign,
            true,
            true,
        );
        let (tp_right, right) = self.convert_lvalue_and_func_designator(
            tp_right.clone(),
            right,
            true,
            true,
            true,
        );

        let is_signed = |tp: &QType| match tp.tp {
            Type::UnsignedChar
            | Type::UnsignedShort
            | Type::UnsignedInt
            | Type::UnsignedLong => false,
            _ => true,
        };
        let is_fp = |tp: &QType| match tp.tp {
            Type::Float | Type::Double => true,
            _ => false,
        };

        match op {
            Op::ASSIGN => {
                if tp_left.is_const {
                    panic!(
                        "{}: Cannot modify const-qualified variables",
                        Compiler::format_loc(loc_left)
                    )
                }
                let is_void = |tp: &QType| match tp.tp {
                    Type::Void => true,
                    _ => false,
                };
                // 3.3.16.1: One of the following shall hold:
                match (&tp_left.tp, &tp_right.tp) {
                    // * the left operand has qualified or unqualified
                    //   arithmetic type and the right has arithmetic type;
                    _ if tp_left.is_arithmetic_type()
                        && tp_right.is_arithmetic_type() =>
                    {
                        ()
                    }
                    // * the left operand has a qualified or unqualified version
                    //   of a structure or union type compatible with the type
                    //   of the right;
                    (Type::Struct(_), Type::Struct(_))
                    | (Type::Union(_), Type::Union(_))
                        if Compiler::try_get_composite_type(
                            &tp_left, &tp_right, loc_right,
                        )
                        .is_ok() =>
                    {
                        ()
                    }
                    // * both operands are pointers to qualified or unqualified
                    //   versions of compatible types, and the type pointed to
                    //   by the left has all the qualifiers of the type pointed
                    //   to by the right;
                    (Type::Pointer(tp_l), Type::Pointer(tp_r))
                        if Compiler::try_get_composite_type(
                            tp_l.as_ref(),
                            tp_r.as_ref(),
                            loc_right,
                        )
                        .is_ok()
                            && (tp_l.is_const || !tp_r.is_const)
                            && (tp_l.is_volatile || !tp_r.is_volatile) =>
                    {
                        ()
                    }
                    // * one operand is a pointer to an object or incomplete
                    //   type and the other is a pointer to a qualified or
                    //   unqualified version of void, and the type pointed to by
                    //   the left has all the qualifiers of the type pointed to
                    //   by the right; or
                    (Type::Pointer(tp_l), Type::Pointer(tp_r))
                        if is_void(tp_r)
                            && (tp_l.is_const || !tp_r.is_const)
                            && (tp_l.is_volatile || !tp_r.is_volatile) =>
                    {
                        ()
                    }
                    // * the left operand is a pointer and the right is a null
                    //   pointer constant.
                    (Type::Pointer(_), _)
                        if right.as_ref().and_then(|r| r.as_constant_u64())
                            == Some(0) =>
                    {
                        ()
                    }
                    _ => panic!(
                        "{}: Illegal right hand side value type for ASSIGN",
                        Compiler::format_loc(loc_right)
                    ),
                }
                let (_, right) = self.cast_expression(
                    tp_right,
                    right,
                    tp_left.clone(),
                    loc_right,
                    emit_ir,
                );
                if !emit_ir {
                    return (QType::from(tp_left.tp), None);
                }
                let right =
                    right.map(|c| self.convert_to_ir_value(&tp_left, c));
                // now store `right` to `left`
                let dst_ir_id = match left {
                    Some(ConstantOrIrValue::IrValue(ir_id, true)) => ir_id,
                    _ => unreachable!(),
                };
                let src_ir_id = match &right {
                    Some(ConstantOrIrValue::IrValue(ir_id, false)) => {
                        ir_id.clone()
                    }
                    _ => unreachable!(),
                };
                match &tp_left.tp {
                    Type::Struct(_) | Type::Union(_) => {
                        let (size, align) =
                            Compiler::get_type_size_and_align_bytes(
                                &tp_left.tp,
                            )
                            .unwrap();
                        self.c4ir_builder.create_memcpy(
                            dst_ir_id.clone(),
                            src_ir_id.clone(),
                            size,
                            align,
                        );
                        self.llvm_builder.create_memcpy(
                            dst_ir_id.clone(),
                            src_ir_id.clone(),
                            size,
                            align,
                        );
                        (QType::from(tp_left.tp), right)
                    }
                    _ => {
                        self.c4ir_builder
                            .create_store(dst_ir_id.clone(), src_ir_id.clone());
                        self.llvm_builder
                            .create_store(dst_ir_id.clone(), src_ir_id.clone());
                        (QType::from(tp_left.tp), right)
                    }
                }
            }
            Op::MUL_ASSIGN
            | Op::DIV_ASSIGN
            | Op::MOD_ASSIGN
            | Op::ADD_ASSIGN
            | Op::SUB_ASSIGN
            | Op::L_SHIFT_ASSIGN
            | Op::R_SHIFT_ASSIGN
            | Op::BINARY_AND_ASSIGN
            | Op::XOR_ASSIGN
            | Op::BINARY_OR_ASSIGN => {
                // 3.3.16.2: For the operators += and -= only, either the left
                // operand shall be a pointer to an object type and the right
                // shall have integral type, or the left operand shall have
                // qualified or unqualified arithmetic type and the right shall
                // have arithmetic type.
                // For the other operators, each operand shall have arithmetic
                // type consistent with those allowed by the corresponding
                // binary operator.
                if op == Op::ADD_ASSIGN || op == Op::SUB_ASSIGN {
                    // it's okay to not check the pointed-to type; if it's not
                    // an object, the add/sub operation would fail.
                    if !(tp_left.is_pointer() && tp_right.is_integral_type())
                        && !(tp_left.is_arithmetic_type()
                            && tp_right.is_arithmetic_type())
                    {
                        panic!(
                            "{}: Illegal operand type",
                            Compiler::format_loc(loc_left)
                        )
                    }
                }

                // 3.3.16.2: A compound assignment of the form E1 op = E2
                // differs from the simple assignment expression E1 = E1 op (E2)
                // only in that the lvalue E1 is evaluated only once.
                let simple_op = match op {
                    Op::MUL_ASSIGN => Op::MUL,
                    Op::DIV_ASSIGN => Op::DIV,
                    Op::MOD_ASSIGN => Op::MOD,
                    Op::ADD_ASSIGN => Op::ADD,
                    Op::SUB_ASSIGN => Op::SUB,
                    Op::L_SHIFT_ASSIGN => Op::L_SHIFT,
                    Op::R_SHIFT_ASSIGN => Op::R_SHIFT,
                    Op::BINARY_AND_ASSIGN => Op::BIT_AND,
                    Op::XOR_ASSIGN => Op::XOR,
                    Op::BINARY_OR_ASSIGN => Op::BIT_OR,
                    _ => unreachable!(),
                };
                let (tp_result, result) = self.visit_simple_binary_op(
                    &tp_left,
                    left.clone(),
                    loc_left,
                    &tp_right,
                    right,
                    loc_right,
                    simple_op,
                    fold_constant,
                    emit_ir,
                );
                self.visit_simple_binary_op(
                    &tp_left,
                    left,
                    loc_left,
                    &tp_result,
                    result,
                    loc_left, // left instead of right
                    Op::ASSIGN,
                    fold_constant,
                    emit_ir,
                )
            }
            Op::COMMA => {
                if !emit_ir {
                    (tp_right, None)
                } else {
                    (tp_right, right)
                }
            }
            Op::BIT_OR | Op::XOR | Op::BIT_AND => {
                let err_loc = if !tp_left.is_integral_type() {
                    Some(loc_left)
                } else if !tp_right.is_integral_type() {
                    Some(loc_right)
                } else {
                    None
                };
                err_loc.map(|loc| {
                    panic!(
                        "{}: Operand must have integral type",
                        Compiler::format_loc(loc)
                    )
                });

                fn calc<
                    T: std::ops::BitAnd<Output = T>
                        + std::ops::BitXor<Output = T>
                        + std::ops::BitOr<Output = T>,
                >(
                    op: Op,
                    x: T,
                    y: T,
                ) -> T {
                    if op == Op::BIT_OR {
                        x | y
                    } else if op == Op::XOR {
                        x ^ y
                    } else {
                        x & y
                    }
                }

                use ConstantOrIrValue as C;
                match self
                    .do_arithmetic_conversion(tp_left, left, tp_right, right)
                {
                    (None, None, tp) => (tp, None),

                    (Some(C::I32(x)), Some(C::I32(y)), tp) => {
                        (tp, Some(C::I32(calc(op, x, y))))
                    }
                    (Some(C::U32(x)), Some(C::U32(y)), tp) => {
                        (tp, Some(C::U32(calc(op, x, y))))
                    }
                    (Some(C::I64(x)), Some(C::I64(y)), tp) => {
                        (tp, Some(C::I64(calc(op, x, y))))
                    }
                    (Some(C::U64(x)), Some(C::U64(y)), tp) => {
                        (tp, Some(C::U64(calc(op, x, y))))
                    }

                    (
                        Some(C::IrValue(x_ir_id, false)),
                        Some(C::IrValue(y_ir_id, false)),
                        tp,
                    ) => {
                        let ir_id = self.get_next_ir_id();
                        self.c4ir_builder.create_bin_op(
                            ir_id.clone(),
                            op,
                            is_signed(&tp),
                            is_fp(&tp),
                            x_ir_id.clone(),
                            y_ir_id.clone(),
                        );
                        self.llvm_builder.create_bin_op(
                            ir_id.clone(),
                            op,
                            is_signed(&tp),
                            is_fp(&tp),
                            x_ir_id,
                            y_ir_id,
                        );
                        (tp, Some(C::IrValue(ir_id, false)))
                    }

                    _ => unreachable!(),
                }
            }
            Op::EQ | Op::NEQ | Op::LESS | Op::GT | Op::LEQ | Op::GEQ => {
                use ConstantOrIrValue as C;
                let is_equality_op = op == Op::EQ || op == Op::NEQ;
                let is_null = |c: &Option<C>| match c {
                    Some(C::U64(0)) => true,
                    _ => false,
                };
                fn cmp<T: PartialEq<T> + PartialOrd<T>>(
                    op: Op,
                    x: T,
                    y: T,
                ) -> (QType, Option<C>) {
                    let c = match op {
                        Op::EQ => x == y,
                        Op::NEQ => x != y,
                        Op::LESS => x < y,
                        Op::GT => x > y,
                        Op::LEQ => x <= y,
                        _ => x >= y,
                    };
                    (QType::from(Type::Int), Some(C::I32(c as i32)))
                }
                let (tp_left, left, tp_right, right) =
                    match (&tp_left.tp, &tp_right.tp) {
                        _ if tp_left.is_arithmetic_type()
                            && tp_right.is_arithmetic_type() =>
                        {
                            let (left, right, tp) = self
                                .do_arithmetic_conversion(
                                    tp_left, left, tp_right, right,
                                );
                            (tp.clone(), left, tp, right)
                        }
                        (Type::Pointer(tp_el), Type::Pointer(tp_er))
                            if Compiler::try_get_composite_type(
                                tp_el, tp_er, loc_left,
                            )
                            .is_ok() =>
                        {
                            // 3.3.8 specifies the operands of relational
                            // operators (LESS, GT, LEQ, GEQ) must not be
                            // pointer to functions; but that is allowed for
                            // equality operators (EQ, NEQ) as specified in
                            // 3.3.9.
                            if !is_equality_op && tp_el.is_function() {
                                panic!(
                                    "{}: Illegal type of operands",
                                    Compiler::format_loc(loc_left)
                                );
                            }
                            (tp_left, left, tp_right, right)
                        }
                        (Type::Pointer(tp_el), Type::Pointer(tp_er))
                            if is_equality_op
                                && (tp_el.is_void() || tp_er.is_void()) =>
                        {
                            let (tp_right, right) = self.cast_expression(
                                tp_right,
                                right,
                                tp_left.clone(),
                                loc_right,
                                emit_ir,
                            );
                            (tp_left, left, tp_right, right)
                        }
                        _ if is_equality_op
                            && ((tp_left.is_pointer() && is_null(&right))
                                || (tp_right.is_pointer()
                                    && is_null(&left))) =>
                        {
                            (tp_left, left, tp_right, right)
                        }
                        _ => panic!(
                            "{}: Illegal type of operands",
                            Compiler::format_loc(loc_left)
                        ),
                    };
                if left.is_none() || right.is_none() {
                    return (QType::from(Type::Int), None);
                }
                let (left, right) = (left.unwrap(), right.unwrap());
                let (left, right) = if left.is_ir_value() || right.is_ir_value()
                {
                    (
                        self.convert_to_ir_value(&tp_left, left),
                        self.convert_to_ir_value(&tp_right, right),
                    )
                } else {
                    (left, right)
                };
                let (left, right) = match (&left, &right) {
                    (C::Address(addr_left, _), C::Address(addr_right, _))
                        if addr_left != addr_right =>
                    {
                        if emit_ir {
                            (
                                self.convert_to_ir_value(&tp_left, left),
                                self.convert_to_ir_value(&tp_right, right),
                            )
                        } else {
                            return (QType::from(Type::Int), None);
                        }
                    }
                    _ => (left, right),
                };
                match (left, right) {
                    (C::IrValue(left, false), C::IrValue(right, false)) => {
                        let i1_ir_id = self.get_next_ir_id();
                        self.c4ir_builder.create_cmp_op(
                            i1_ir_id.clone(),
                            op,
                            false,
                            false,
                            left.clone(),
                            right.clone(),
                        );
                        self.llvm_builder.create_cmp_op(
                            i1_ir_id.clone(),
                            op,
                            false,
                            false,
                            left,
                            right,
                        );
                        let ir_id = self.get_next_ir_id();
                        self.c4ir_builder
                            .create_zext_i1_to_i32(&ir_id, &i1_ir_id);
                        self.llvm_builder
                            .create_zext_i1_to_i32(&ir_id, &i1_ir_id);
                        (QType::from(Type::Int), Some(C::IrValue(ir_id, false)))
                    }
                    (C::IrValue(_, _), _) => unreachable!(),
                    (_, C::IrValue(_, _)) => unreachable!(),

                    (C::I8(x), C::I8(y)) => cmp(op, x, y),
                    (C::U8(x), C::U8(y)) => cmp(op, x, y),
                    (C::I16(x), C::I16(y)) => cmp(op, x, y),
                    (C::U16(x), C::U16(y)) => cmp(op, x, y),
                    (C::I32(x), C::I32(y)) => cmp(op, x, y),
                    (C::U32(x), C::U32(y)) => cmp(op, x, y),
                    (C::I64(x), C::I64(y)) => cmp(op, x, y),
                    (C::U64(x), C::U64(y)) => cmp(op, x, y),
                    (C::Float(x), C::Float(y)) => cmp(op, x, y),
                    (C::Double(x), C::Double(y)) => cmp(op, x, y),

                    (C::Address(_, x), C::Address(_, y)) => cmp(op, x, y),
                    (C::Address(_, _), C::I64(0)) => cmp(op, 999, 0),
                    (C::I64(0), C::Address(_, _)) => cmp(op, 0, 999),

                    _ => unreachable!(),
                }
            }
            Op::L_SHIFT | Op::R_SHIFT => {
                let err_loc = if !tp_left.is_integral_type() {
                    Some(loc_left)
                } else if !tp_right.is_integral_type() {
                    Some(loc_right)
                } else {
                    None
                };
                err_loc.map(|loc| {
                    panic!(
                        "{}: Operand must have integral type",
                        Compiler::format_loc(loc)
                    )
                });

                let (tp_left, left) = self.do_integral_promotion(tp_left, left);
                let (tp_right, right) =
                    self.do_integral_promotion(tp_right, right);
                // LLVM requires shl/shr operands to have the same type
                let (tp_right, right) = self.cast_expression(
                    tp_right,
                    right,
                    tp_left.clone(),
                    loc_right,
                    emit_ir,
                );

                if left.is_none() || right.is_none() {
                    return (tp_left, None);
                }
                let (left, right) = (left.unwrap(), right.unwrap());
                let (left, right) = if left.is_ir_value() || right.is_ir_value()
                {
                    (
                        self.convert_to_ir_value(&tp_left, left),
                        self.convert_to_ir_value(&tp_right, right),
                    )
                } else {
                    (left, right)
                };

                let is_shl = op == Op::L_SHIFT;

                use ConstantOrIrValue as C;
                match (left, right) {
                    (
                        C::IrValue(left_ir_id, false),
                        C::IrValue(right_ir_id, false),
                    ) => {
                        let ir_id = self.get_next_ir_id();
                        self.c4ir_builder.create_bin_op(
                            ir_id.clone(),
                            op,
                            is_signed(&tp_left),
                            false,
                            left_ir_id.clone(),
                            right_ir_id.clone(),
                        );
                        self.llvm_builder.create_bin_op(
                            ir_id.clone(),
                            op,
                            is_signed(&tp_left),
                            false,
                            left_ir_id.clone(),
                            right_ir_id.clone(),
                        );
                        (tp_left, Some(C::IrValue(ir_id, false)))
                    }

                    // int
                    (C::I32(x), C::I32(y)) if is_shl => (
                        QType::from(Type::Int),
                        Some(C::I32(x.checked_shl(y as u32).unwrap_or(0))),
                    ),
                    (C::I32(x), C::I32(y)) => (
                        QType::from(Type::Int),
                        Some(C::I32(
                            x.checked_shr(y as u32).unwrap_or(if x >= 0 {
                                0
                            } else {
                                -1
                            }),
                        )),
                    ),
                    // unsigned int
                    (C::U32(x), C::U32(y)) if is_shl => (
                        QType::from(Type::Int),
                        Some(C::U32(x.checked_shl(y).unwrap_or(0))),
                    ),
                    (C::U32(x), C::U32(y)) => (
                        QType::from(Type::Int),
                        Some(C::U32(x.checked_shr(y).unwrap_or(0))),
                    ),
                    // long
                    (C::I64(x), C::I64(y)) if is_shl => (
                        QType::from(Type::Int),
                        Some(C::I64(
                            x.checked_shl(y as u32).unwrap_or(if x >= 0 {
                                0
                            } else {
                                -1
                            }),
                        )),
                    ),
                    (C::I64(x), C::I64(y)) => (
                        QType::from(Type::Int),
                        Some(C::I64(
                            x.checked_shr(y as u32).unwrap_or(if x >= 0 {
                                0
                            } else {
                                -1
                            }),
                        )),
                    ),
                    // unsigned long
                    (C::U64(x), C::U64(y)) if is_shl => (
                        QType::from(Type::Int),
                        Some(C::U64(x.checked_shl(y as u32).unwrap_or(0))),
                    ),
                    (C::U64(x), C::U64(y)) => (
                        QType::from(Type::Int),
                        Some(C::U64(x.checked_shr(y as u32).unwrap_or(0))),
                    ),

                    _ => unreachable!(),
                }
            }
            Op::ADD | Op::SUB => {
                if tp_left.is_arithmetic_type() && tp_right.is_arithmetic_type()
                {
                    let (left, right, tp) = self.do_arithmetic_conversion(
                        tp_left, left, tp_right, right,
                    );
                    if left.is_none() || right.is_none() {
                        return (tp, None);
                    }
                    let (left, right) = (left.unwrap(), right.unwrap());

                    fn calc<
                        T: std::ops::Add<Output = T> + std::ops::Sub<Output = T>,
                    >(
                        op: Op,
                        x: T,
                        y: T,
                    ) -> T {
                        if op == Op::ADD {
                            x + y
                        } else {
                            x - y
                        }
                    }

                    use ConstantOrIrValue as C;
                    match (left, right) {
                        (C::I32(x), C::I32(y)) => {
                            (tp, Some(C::I32(calc(op, x, y))))
                        }
                        (C::U32(x), C::U32(y)) => {
                            (tp, Some(C::U32(calc(op, x, y))))
                        }
                        (C::I64(x), C::I64(y)) => {
                            (tp, Some(C::I64(calc(op, x, y))))
                        }
                        (C::U64(x), C::U64(y)) => {
                            (tp, Some(C::U64(calc(op, x, y))))
                        }
                        (C::Float(x), C::Float(y)) => {
                            (tp, Some(C::Float(calc(op, x, y))))
                        }
                        (C::Double(x), C::Double(y)) => {
                            (tp, Some(C::Double(calc(op, x, y))))
                        }
                        (
                            C::IrValue(x_ir_id, false),
                            C::IrValue(y_ir_id, false),
                        ) => {
                            let ir_id = self.get_next_ir_id();
                            self.c4ir_builder.create_bin_op(
                                ir_id.clone(),
                                op,
                                is_signed(&tp),
                                is_fp(&tp),
                                x_ir_id.clone(),
                                y_ir_id.clone(),
                            );
                            self.llvm_builder.create_bin_op(
                                ir_id.clone(),
                                op,
                                is_signed(&tp),
                                is_fp(&tp),
                                x_ir_id.clone(),
                                y_ir_id.clone(),
                            );
                            (tp, Some(C::IrValue(ir_id, false)))
                        }
                        _ => unreachable!(),
                    }
                } else if tp_left.is_pointer() && tp_right.is_integral_type() {
                    let tp_elem = match &tp_left.tp {
                        Type::Pointer(elem) => *elem.clone(),
                        _ => unreachable!(),
                    };
                    let sz_elem = match Compiler::get_type_size_and_align_bytes(
                        &tp_elem.tp,
                    ) {
                        Some((sz, _)) => sz,
                        None => panic!(
                            "{}: Complete object element type expected",
                            Compiler::format_loc(loc_left)
                        ),
                    };
                    let sz_elem =
                        sz_elem as i64 * if op == Op::ADD { 1 } else { -1 };

                    let (tp_right, right) = self.cast_expression(
                        tp_right,
                        right,
                        QType::from(Type::Long),
                        loc_right,
                        emit_ir,
                    );

                    if left.is_none() || right.is_none() {
                        return (tp_left, None);
                    }
                    let (left, right) = (left.unwrap(), right.unwrap());
                    let (left, right) =
                        if left.is_ir_value() || right.is_ir_value() {
                            (
                                self.convert_to_ir_value(&tp_left, left),
                                self.convert_to_ir_value(&tp_right, right),
                            )
                        } else {
                            (left, right)
                        };

                    use ConstantOrIrValue as C;
                    match (left, right) {
                        (C::IrValue(left, false), C::IrValue(right, false)) => {
                            // ptr + n => (T*) ((void*) ptr + n * sizeof(T))
                            let ptr_ir_id = self.get_next_ir_id();
                            self.c4ir_builder.create_cast(
                                ptr_ir_id.clone(),
                                &QType::char_ptr_tp(),
                                left.clone(),
                                &tp_left,
                            );
                            self.llvm_builder.create_cast(
                                ptr_ir_id.clone(),
                                &QType::char_ptr_tp(),
                                left.clone(),
                                &tp_left,
                            );

                            let sz_ir_id = self.get_next_ir_id();
                            self.c4ir_builder.create_constant(
                                sz_ir_id.clone(),
                                &ConstantOrIrValue::I64(sz_elem),
                                &QType::from(Type::Long),
                            );
                            self.llvm_builder.create_constant(
                                sz_ir_id.clone(),
                                &ConstantOrIrValue::I64(sz_elem),
                                &QType::from(Type::Long),
                            );

                            let offset_ir_id = self.get_next_ir_id();
                            self.c4ir_builder.create_bin_op(
                                offset_ir_id.clone(),
                                Op::MUL,
                                true,
                                false,
                                right.clone(),
                                sz_ir_id.clone(),
                            );
                            self.llvm_builder.create_bin_op(
                                offset_ir_id.clone(),
                                Op::MUL,
                                true,
                                false,
                                right.clone(),
                                sz_ir_id.clone(),
                            );

                            let ir_id = self.get_next_ir_id();
                            self.c4ir_builder.create_ptr_add(
                                &ir_id,
                                &ptr_ir_id,
                                &offset_ir_id,
                            );
                            self.llvm_builder.create_ptr_add(
                                &ir_id,
                                &ptr_ir_id,
                                &offset_ir_id,
                            );

                            self.cast_expression(
                                QType::char_ptr_tp(),
                                Some(C::IrValue(ir_id, false)),
                                tp_left,
                                loc_left,
                                true,
                            )
                        }
                        (C::Address(ir_id, offset), C::I64(y)) => {
                            (tp_left, Some(C::Address(ir_id, offset + y)))
                        }
                        (C::U64(x), C::I64(y)) => {
                            (tp_left, Some(C::U64(x + y as u64)))
                        }
                        _ => unreachable!(),
                    }
                } else if tp_left.is_integral_type()
                    && tp_right.is_pointer()
                    && op == Op::ADD
                {
                    // n + ptr => ptr + n
                    self.visit_simple_binary_op(
                        &tp_right,
                        right,
                        loc_right,
                        &tp_left,
                        left,
                        loc_left,
                        op,
                        fold_constant,
                        emit_ir,
                    )
                } else if tp_left.is_pointer()
                    && tp_right.is_pointer()
                    && op == Op::SUB
                {
                    let long_tp = QType::from(Type::Long);
                    let tp = Compiler::get_composite_type(
                        &tp_left, &tp_right, loc_left,
                    );
                    let tp_elem = match &tp.tp {
                        Type::Pointer(elem) => *elem.clone(),
                        _ => unreachable!(),
                    };
                    let sz_elem = match Compiler::get_type_size_and_align_bytes(
                        &tp_elem.tp,
                    ) {
                        Some((sz, _)) => sz,
                        None => panic!(
                            "{}: Complete object element type expected",
                            Compiler::format_loc(loc_left)
                        ),
                    };

                    if left.is_none() || right.is_none() {
                        return (long_tp, None);
                    }
                    let (left, right) = (left.unwrap(), right.unwrap());
                    let (left, right) =
                        if left.is_ir_value() || right.is_ir_value() {
                            (
                                self.convert_to_ir_value(&tp_left, left),
                                self.convert_to_ir_value(&tp_right, right),
                            )
                        } else {
                            (left, right)
                        };

                    use ConstantOrIrValue as C;
                    match (left, right) {
                        (C::U64(_), C::Address(_, _)) => (long_tp, None),
                        (C::Address(_, _), C::U64(_)) => (long_tp, None),

                        (C::U64(x), C::U64(y)) => (
                            long_tp,
                            Some(C::I64(
                                x.overflowing_sub(y).0 as i64 / sz_elem as i64,
                            )),
                        ),

                        (C::Address(ir_id_x, _), C::Address(ir_id_y, _))
                            if ir_id_x != ir_id_y =>
                        {
                            (long_tp, None)
                        }
                        (
                            C::Address(ir_id, offset_x),
                            C::Address(_, offset_y),
                        ) => (
                            long_tp,
                            Some(C::Address(
                                ir_id,
                                offset_x.overflowing_sub(offset_y).0
                                    / sz_elem as i64,
                            )),
                        ),

                        (
                            C::IrValue(ir_id_x, false),
                            C::IrValue(ir_id_y, false),
                        ) => {
                            // p1 - p2 => ((long*)p1 - (long*)p2) / sizeof(T)
                            let p1 = self.get_next_ir_id();
                            self.c4ir_builder.create_cast(
                                p1.clone(),
                                &long_tp,
                                ir_id_x.clone(),
                                &tp_left,
                            );
                            self.llvm_builder.create_cast(
                                p1.clone(),
                                &long_tp,
                                ir_id_x.clone(),
                                &tp_left,
                            );

                            let p2 = self.get_next_ir_id();
                            self.c4ir_builder.create_cast(
                                p2.clone(),
                                &long_tp,
                                ir_id_y.clone(),
                                &tp_right,
                            );
                            self.llvm_builder.create_cast(
                                p2.clone(),
                                &long_tp,
                                ir_id_y.clone(),
                                &tp_right,
                            );

                            let diff = self.get_next_ir_id();
                            self.c4ir_builder.create_bin_op(
                                diff.clone(),
                                Op::SUB,
                                true,
                                false,
                                p1.clone(),
                                p2.clone(),
                            );
                            self.llvm_builder.create_bin_op(
                                diff.clone(),
                                Op::SUB,
                                true,
                                false,
                                p1.clone(),
                                p2.clone(),
                            );

                            let sz = self.get_next_ir_id();
                            self.c4ir_builder.create_constant(
                                sz.clone(),
                                &C::I64(sz_elem as i64),
                                &long_tp,
                            );
                            self.llvm_builder.create_constant(
                                sz.clone(),
                                &C::I64(sz_elem as i64),
                                &long_tp,
                            );

                            let ir_id = self.get_next_ir_id();
                            self.c4ir_builder.create_bin_op(
                                ir_id.clone(),
                                Op::DIV,
                                true,
                                false,
                                diff.clone(),
                                sz.clone(),
                            );
                            self.llvm_builder.create_bin_op(
                                ir_id.clone(),
                                Op::DIV,
                                true,
                                false,
                                diff.clone(),
                                sz.clone(),
                            );

                            (long_tp, Some(C::IrValue(ir_id, false)))
                        }

                        _ => unreachable!(),
                    }
                } else {
                    panic!(
                        "{}: Invalid operand types",
                        Compiler::format_loc(loc_left)
                    );
                }
            }
            Op::MUL | Op::DIV | Op::MOD => {
                let is_div = op == Op::DIV;
                let is_mod = op == Op::MOD;

                let err_loc = if is_mod && !tp_left.is_integral_type() {
                    Some(loc_left)
                } else if is_mod && !tp_right.is_integral_type() {
                    Some(loc_right)
                } else if !tp_left.is_arithmetic_type() {
                    Some(loc_left)
                } else if !tp_right.is_arithmetic_type() {
                    Some(loc_right)
                } else {
                    None
                };
                err_loc.map(|loc| {
                    panic!(
                        "{}: Invalid operand type",
                        Compiler::format_loc(loc)
                    )
                });

                let (left, right, tp) = self
                    .do_arithmetic_conversion(tp_left, left, tp_right, right);
                if left.is_none() || right.is_none() || !fold_constant {
                    (tp, None)
                } else {
                    let left = left.unwrap();
                    let right = right.unwrap();
                    if is_div || is_mod {
                        if (tp.is_integral_type()
                            && right.as_constant_u64() == Some(0 as u64))
                            || (!tp.is_integral_type()
                                && right.as_constant_double() == Some(0.0))
                        {
                            panic!(
                                "{}: Divisor is 0",
                                Compiler::format_loc(loc_right)
                            )
                        }
                    }
                    use ConstantOrIrValue as C;
                    let c = match (left, right) {
                        (C::I8(x), C::I8(y)) if is_div => C::I8(x / y),
                        (C::I8(x), C::I8(y)) if is_mod => C::I8(x % y),
                        (C::I8(x), C::I8(y)) => C::I8(x * y),

                        (C::U8(x), C::U8(y)) if is_div => C::U8(x / y),
                        (C::U8(x), C::U8(y)) if is_mod => C::U8(x % y),
                        (C::U8(x), C::U8(y)) => C::U8(x * y),

                        (C::I16(x), C::I16(y)) if is_div => C::I16(x / y),
                        (C::I16(x), C::I16(y)) if is_mod => C::I16(x % y),
                        (C::I16(x), C::I16(y)) => C::I16(x * y),

                        (C::U16(x), C::U16(y)) if is_div => C::U16(x / y),
                        (C::U16(x), C::U16(y)) if is_mod => C::U16(x % y),
                        (C::U16(x), C::U16(y)) => C::U16(x * y),

                        (C::I32(x), C::I32(y)) if is_div => C::I32(x / y),
                        (C::I32(x), C::I32(y)) if is_mod => C::I32(x % y),
                        (C::I32(x), C::I32(y)) => C::I32(x * y),

                        (C::U32(x), C::U32(y)) if is_div => C::U32(x / y),
                        (C::U32(x), C::U32(y)) if is_mod => C::U32(x % y),
                        (C::U32(x), C::U32(y)) => C::U32(x * y),

                        (C::I64(x), C::I64(y)) if is_div => C::I64(x / y),
                        (C::I64(x), C::I64(y)) if is_mod => C::I64(x % y),
                        (C::I64(x), C::I64(y)) => C::I64(x * y),

                        (C::U64(x), C::U64(y)) if is_div => C::U64(x / y),
                        (C::U64(x), C::U64(y)) if is_mod => C::U64(x % y),
                        (C::U64(x), C::U64(y)) => C::U64(x * y),

                        (C::Float(x), C::Float(y)) if is_div => C::Float(x / y),
                        (C::Float(x), C::Float(y)) => C::Float(x * y),

                        (C::Double(x), C::Double(y)) if is_div => {
                            C::Double(x / y)
                        }
                        (C::Double(x), C::Double(y)) => C::Double(x * y),

                        (C::IrValue(x, false), C::IrValue(y, false)) => {
                            let ir_id = self.get_next_ir_id();
                            self.c4ir_builder.create_bin_op(
                                ir_id.clone(),
                                op,
                                is_signed(&tp),
                                is_fp(&tp),
                                x.clone(),
                                y.clone(),
                            );
                            self.llvm_builder.create_bin_op(
                                ir_id.clone(),
                                op,
                                is_signed(&tp),
                                is_fp(&tp),
                                x.clone(),
                                y.clone(),
                            );
                            C::IrValue(ir_id, false)
                        }
                        _ => unreachable!(),
                    };
                    (tp, Some(c))
                }
            }
            Op::LOGIC_OR | Op::LOGIC_AND => unreachable!(),
        }
    }

    // for && and ||
    fn visit_special_binary_op(
        &mut self,
        e_left: L<&ast::Expr>,
        e_right: L<&ast::Expr>,
        op: ast::Expr_Binary_Op,
        fold_constant: bool,
        emit_ir: bool,
    ) -> (QType, Option<ConstantOrIrValue>) {
        let check_scalar_type = |tp: &QType, loc: &ast::Loc| {
            if !tp.is_scalar_type() {
                panic!(
                    "{}: Expression must have scalar type",
                    Compiler::format_loc(loc)
                )
            }
        };
        let (left_tp, left) = self.visit_expr(e_left, fold_constant, emit_ir);
        let (left_tp, left) = self.convert_lvalue_and_func_designator(
            left_tp, left, true, true, true,
        );
        check_scalar_type(&left_tp, e_left.1);

        if !fold_constant || left.is_none() {
            // !fold_constant: infer expression type only; ok to return None
            // left.is_none(): !emit_ir but expr is not constant; must ret None
            let (right_tp, _) =
                self.visit_expr(e_right, fold_constant, emit_ir);
            check_scalar_type(&right_tp, e_right.1);
            (QType::from(Type::Int), None)
        } else {
            // emit_ir could be either true or false
            let left = left.unwrap();
            let is_or = op == ast::Expr_Binary_Op::LOGIC_OR;
            use ConstantOrIrValue as C;
            match &left {
                C::I8(_)
                | C::U8(_)
                | C::I16(_)
                | C::U16(_)
                | C::I32(_)
                | C::U32(_)
                | C::I64(_)
                | C::U64(_)
                | C::Float(_)
                | C::Double(_)
                | C::Address(_, _) => {
                    let is_true = left
                        .as_constant_double()
                        .map(|v| v != 0.0)
                        .unwrap_or(true); // address must be non-zero
                    if is_true == is_or {
                        // 1 || right => 1
                        // 0 && right => 0
                        // (but still need to type check `right`)
                        let (right_tp, _) =
                            self.visit_expr(e_right, false, false);
                        check_scalar_type(&right_tp, e_right.1);
                        (
                            QType::from(Type::Int),
                            Some(C::I32(if is_true { 1 } else { 0 })),
                        )
                    } else {
                        // 0 || right => !!right => 0 != right
                        // 1 && right => !!right => 0 != right
                        let (right_tp, right) =
                            self.visit_expr(e_right, fold_constant, emit_ir);
                        self.visit_simple_binary_op(
                            &QType::from(Type::Int),
                            Some(C::I32(0)),
                            e_right.1,
                            &right_tp,
                            right,
                            e_right.1,
                            ast::Expr_Binary_Op::NEQ,
                            fold_constant,
                            emit_ir,
                        )
                    }
                }
                C::IrValue(_, true) => unreachable!(),
                C::IrValue(left_ir_id, false) => {
                    // i32* r = alloca i32
                    let result_ir_id = self.get_next_ir_id();
                    self.c4ir_builder.create_definition(
                        false,
                        &result_ir_id,
                        &QType::from(Type::Int),
                        Linkage::NONE,
                        &None,
                    );
                    self.llvm_builder.create_definition(
                        false,
                        &result_ir_id,
                        &QType::from(Type::Int),
                        Linkage::NONE,
                        &None,
                    );

                    // bool left = eval(left) != 0
                    let left_ir_id = self
                        .visit_cond_expr_ir(
                            left_tp,
                            left_ir_id.clone(),
                            e_left.1,
                        )
                        .unwrap_or_else(|e| e.panic());

                    // true || ... = true
                    // false && ... = false
                    let cond_ir_id = if is_or {
                        left_ir_id
                    } else {
                        let cond_ir_id = self.get_next_ir_id();
                        self.c4ir_builder.create_not(&cond_ir_id, &left_ir_id);
                        self.llvm_builder.create_not(&cond_ir_id, &left_ir_id);
                        cond_ir_id
                    };
                    let short_bb = self.create_bb();
                    let long_bb = self.create_bb();
                    let merge_bb = self.create_bb();
                    self.c4ir_builder.create_cond_br(
                        &cond_ir_id,
                        &short_bb,
                        &long_bb,
                    );
                    self.llvm_builder.create_cond_br(
                        &cond_ir_id,
                        &short_bb,
                        &long_bb,
                    );

                    // short-circuit path: || => 1, && => 0
                    self.c4ir_builder.set_current_basic_block(&short_bb);
                    self.llvm_builder.set_current_basic_block(&short_bb);
                    let short_value = self.convert_to_ir_value(
                        &QType::from(Type::Int),
                        ConstantOrIrValue::I32(if is_or { 1 } else { 0 }),
                    );
                    let short_value_ir_id = match short_value {
                        ConstantOrIrValue::IrValue(ir_id, false) => ir_id,
                        _ => unreachable!(),
                    };
                    self.c4ir_builder.create_store(
                        result_ir_id.clone(),
                        short_value_ir_id.clone(),
                    );
                    self.llvm_builder.create_store(
                        result_ir_id.clone(),
                        short_value_ir_id.clone(),
                    );
                    self.c4ir_builder.create_br(&merge_bb);
                    self.llvm_builder.create_br(&merge_bb);

                    // long path: bool right = eval(right) != 0
                    self.c4ir_builder.set_current_basic_block(&long_bb);
                    self.llvm_builder.set_current_basic_block(&long_bb);
                    let right_ir_id = self
                        .visit_cond_expr(e_right)
                        .unwrap_or_else(|e| e.panic());
                    let right_i32_ir_id = self.get_next_ir_id();
                    self.c4ir_builder
                        .create_zext_i1_to_i32(&right_i32_ir_id, &right_ir_id);
                    self.llvm_builder
                        .create_zext_i1_to_i32(&right_i32_ir_id, &right_ir_id);
                    self.c4ir_builder.create_store(
                        result_ir_id.clone(),
                        right_i32_ir_id.clone(),
                    );
                    self.llvm_builder.create_store(
                        result_ir_id.clone(),
                        right_i32_ir_id.clone(),
                    );
                    self.c4ir_builder.create_br(&merge_bb);
                    self.llvm_builder.create_br(&merge_bb);

                    self.c4ir_builder.set_current_basic_block(&merge_bb);
                    self.llvm_builder.set_current_basic_block(&merge_bb);

                    // i32 result = load i32* r
                    let result_rvalue_ir_id = self.get_next_ir_id();
                    self.c4ir_builder.create_load(
                        result_rvalue_ir_id.clone(),
                        result_ir_id.clone(),
                        &QType::from(Type::Int),
                    );
                    self.llvm_builder.create_load(
                        result_rvalue_ir_id.clone(),
                        result_ir_id.clone(),
                        &QType::from(Type::Int),
                    );
                    (
                        QType::from(Type::Int),
                        Some(ConstantOrIrValue::IrValue(
                            result_rvalue_ir_id,
                            false,
                        )),
                    )
                }
            }
        }
    }

    fn visit_ternary_op(
        &mut self,
        _e_cond: L<&ast::Expr>,
        _e_then: L<&ast::Expr>,
        _e_else: L<&ast::Expr>,
        _fold_constant: bool,
        _emit_ir: bool,
    ) -> (QType, Option<ConstantOrIrValue>) {
        unimplemented!() // TODO: ternary op
    }

    fn visit_stmt(
        &mut self,
        stmt: L<&ast::Statement>,
        ctx: &mut FuncDefCtx,
    ) -> R<()> {
        match stmt.0.stmt.as_ref().unwrap() {
            ast::Statement_oneof_stmt::compound(compound) => {
                self.enter_scope();
                self.visit_compound_stmt(compound, ctx)?;
                self.leave_scope();
                Ok(())
            }
            ast::Statement_oneof_stmt::labeled(labeled) => {
                self.visit_labeled_stmt(labeled, ctx)
            }
            ast::Statement_oneof_stmt::expr(expr) => {
                if expr.e_idx != 0 {
                    let e = &self.translation_unit.exprs[expr.e_idx as usize];
                    let e = (e, expr.get_e_loc());
                    self.visit_expr(e, true, true);
                }
                Ok(())
            }
            ast::Statement_oneof_stmt::if_s(if_s) => {
                self.visit_if_stmt(if_s, ctx)
            }
            ast::Statement_oneof_stmt::switch_s(switch_s) => {
                self.visit_switch_stmt(switch_s, ctx)
            }
            ast::Statement_oneof_stmt::while_s(while_s) => {
                self.visit_while_stmt(while_s, ctx)
            }
            ast::Statement_oneof_stmt::do_while_s(do_while_s) => {
                self.visit_do_while_stmt(do_while_s, ctx)
            }
            ast::Statement_oneof_stmt::for_s(for_s) => {
                self.visit_for_stmt(for_s, ctx)
            }
            ast::Statement_oneof_stmt::goto_s(goto_s) => {
                let bb = match ctx.basic_blocks.get(goto_s.get_id()) {
                    None => {
                        let bb = self.create_bb();
                        ctx.basic_blocks
                            .insert(goto_s.get_id().to_string(), bb.clone());
                        ctx.unresolved_labels.insert(
                            goto_s.get_id().to_string(),
                            goto_s.get_id_loc().clone(),
                        );
                        bb
                    }
                    Some(bb) => bb.clone(),
                };
                self.c4ir_builder.create_br(&bb);
                self.llvm_builder.create_br(&bb);
                Ok(())
            }
            ast::Statement_oneof_stmt::continue_s(_) => {
                match ctx.continue_bb_stack.last() {
                    None => c4_fail!(stmt.1, "Illegal continue statement"),
                    Some(bb_id) => {
                        self.c4ir_builder.create_br(bb_id);
                        self.llvm_builder.create_br(bb_id);
                        Ok(())
                    }
                }
            }
            ast::Statement_oneof_stmt::break_s(_) => {
                match ctx.break_bb_stack.last() {
                    None => c4_fail!(stmt.1, "Illegal break statement"),
                    Some(bb_id) => {
                        self.c4ir_builder.create_br(bb_id);
                        self.llvm_builder.create_br(bb_id);
                        Ok(())
                    }
                }
            }
            ast::Statement_oneof_stmt::return_s(return_s) => {
                self.visit_return_stmt((return_s, stmt.1), ctx)
            }
        }
    }

    // caller should handle scopes for visit_compound_stmt().
    fn visit_compound_stmt(
        &mut self,
        compound: &ast::Statement_Compound,
        ctx: &mut FuncDefCtx,
    ) -> R<()> {
        compound
            .get_dls()
            .into_iter()
            .for_each(|dl| self.visit_declaration(dl));
        let stmts: Vec<L<&ast::Statement>> = compound
            .get_stmt_idxes()
            .into_iter()
            .map(|idx| &self.translation_unit.statements[*idx as usize])
            .zip(compound.get_stmt_locs())
            .collect();
        stmts
            .into_iter()
            .map(|s| self.visit_stmt(s, ctx))
            .filter(|r| r.is_err())
            .next()
            .unwrap_or_else(|| -> R<()> { Ok(()) })
    }

    fn visit_labeled_stmt(
        &mut self,
        labeled: &ast::Statement_Labeled,
        ctx: &mut FuncDefCtx,
    ) -> R<()> {
        match labeled.l.clone().unwrap() {
            ast::Statement_Labeled_oneof_l::id(id) => {
                let bb_id = match (
                    ctx.basic_blocks.get(id.get_id()),
                    ctx.unresolved_labels.get(id.get_id()),
                ) {
                    (None, Some(_)) => unreachable!(),
                    (Some(bb_id), Some(_)) => {
                        ctx.unresolved_labels.remove(id.get_id());
                        bb_id.clone()
                    }
                    (Some(_), None) => c4_fail!(
                        id.get_id_loc(),
                        "Redeclaration of label '{}'",
                        id.get_id()
                    ),
                    (None, None) => self.create_bb(),
                };

                self.c4ir_builder.create_br(&bb_id);
                self.llvm_builder.create_br(&bb_id);
                self.c4ir_builder.set_current_basic_block(&bb_id);
                self.llvm_builder.set_current_basic_block(&bb_id);
                let stmt =
                    &self.translation_unit.statements[id.stmt_idx as usize];
                let stmt = (stmt, id.get_stmt_loc());
                self.visit_stmt(stmt, ctx)
            }
            ast::Statement_Labeled_oneof_l::case_s(case_s) => {
                // 3.6.4.2: The expression of each case label shall be an
                // integral constant expression... The constant expression in
                // each case label is converted to the promoted type of the
                // controlling expression.
                let switch_ctx = match ctx.switch_stack.last() {
                    None => c4_fail!(
                        case_s.get_e_loc(),
                        "Case labels must be within switch statements"
                    ),
                    Some(s) => s,
                };
                let e = &self.translation_unit.exprs[case_s.e_idx as usize];
                let e = (e, case_s.get_e_loc());
                let (e_tp, e) = self.visit_expr(e, true, false);
                let (e_tp, e) = self.cast_expression(
                    e_tp,
                    e,
                    switch_ctx.ctrl_value_tp.clone(),
                    case_s.get_e_loc(),
                    false,
                );
                let e = match e {
                    None => c4_fail!(
                        case_s.get_e_loc(),
                        "Expression does not evaluate to a constant"
                    ),
                    Some(c) => c,
                };
                if !e_tp.is_integral_type() {
                    c4_fail!(
                        case_s.get_e_loc(),
                        "Case labels must be integral values"
                    )
                }
                let e_value = e.as_constant_u64().unwrap();
                if switch_ctx.case_values.contains(&e_value) {
                    c4_fail!(
                        case_s.get_e_loc(),
                        "Duplicate case label {}",
                        e_value
                    )
                }
                ctx.switch_stack
                    .last_mut()
                    .map(|s| s.case_values.insert(e_value));

                let bb_id = self.create_bb();
                self.c4ir_builder.create_br(&bb_id);
                self.llvm_builder.create_br(&bb_id);
                self.c4ir_builder.set_current_basic_block(&bb_id);
                self.llvm_builder.set_current_basic_block(&bb_id);
                self.c4ir_builder.add_switch_case(&e, &bb_id);
                self.llvm_builder.add_switch_case(&e, &bb_id);

                let stmt =
                    &self.translation_unit.statements[case_s.stmt_idx as usize];
                let stmt = (stmt, case_s.get_stmt_loc());
                self.visit_stmt(stmt, ctx)
            }
            ast::Statement_Labeled_oneof_l::default_s(default_s) => {
                let bb_id = match ctx.switch_stack.last() {
                    None => c4_fail!(
                        default_s.get_stmt_loc(),
                        "Default cases must be within switch statements"
                    ),
                    Some(s) if s.default_case_visited => c4_fail!(
                        default_s.get_stmt_loc(),
                        "Duplicate default case"
                    ),
                    Some(s) => s.default_bb_id.clone(),
                };
                ctx.switch_stack
                    .last_mut()
                    .map(|s| s.default_case_visited = true);
                self.c4ir_builder.create_br(&bb_id);
                self.llvm_builder.create_br(&bb_id);
                self.c4ir_builder.set_current_basic_block(&bb_id);
                self.llvm_builder.set_current_basic_block(&bb_id);

                let stmt = &self.translation_unit.statements
                    [default_s.stmt_idx as usize];
                let stmt = (stmt, default_s.get_stmt_loc());
                self.visit_stmt(stmt, ctx)
            }
        }
    }

    fn visit_if_stmt(
        &mut self,
        if_s: &ast::Statement_If,
        ctx: &mut FuncDefCtx,
    ) -> R<()> {
        let cond = &self.translation_unit.exprs[if_s.cond_idx as usize];
        let cond = (cond, if_s.get_cond_loc());
        let cmp_ir_id = self.visit_cond_expr(cond)?;

        let then_bb = self.create_bb();
        let else_bb = self.create_bb();
        let merge_bb = self.create_bb();
        self.c4ir_builder
            .create_cond_br(&cmp_ir_id, &then_bb, &else_bb);
        self.llvm_builder
            .create_cond_br(&cmp_ir_id, &then_bb, &else_bb);

        self.c4ir_builder.set_current_basic_block(&then_bb);
        self.llvm_builder.set_current_basic_block(&then_bb);
        let then_stmt =
            &self.translation_unit.statements[if_s.then_idx as usize];
        let then_stmt = (then_stmt, if_s.get_then_loc());
        self.visit_stmt(then_stmt, ctx)?;
        self.c4ir_builder.create_br(&merge_bb);
        self.llvm_builder.create_br(&merge_bb);

        self.c4ir_builder.set_current_basic_block(&else_bb);
        self.llvm_builder.set_current_basic_block(&else_bb);
        if if_s.else_idx != 0 {
            let else_stmt =
                &self.translation_unit.statements[if_s.else_idx as usize];
            let else_stmt = (else_stmt, if_s.get_else_loc());
            self.visit_stmt(else_stmt, ctx)?;
        }
        self.c4ir_builder.create_br(&merge_bb);
        self.llvm_builder.create_br(&merge_bb);

        self.c4ir_builder.set_current_basic_block(&merge_bb);
        self.llvm_builder.set_current_basic_block(&merge_bb);
        Ok(())
    }

    fn visit_switch_stmt(
        &mut self,
        switch_s: &ast::Statement_Switch,
        ctx: &mut FuncDefCtx,
    ) -> R<()> {
        let default_bb = self.create_bb();
        let break_bb = self.create_bb();

        let e = &self.translation_unit.exprs[switch_s.e_idx as usize];
        let (tp, v) = self.visit_expr((e, switch_s.get_e_loc()), true, true);

        let (tp, v) =
            self.convert_lvalue_and_func_designator(tp, v, true, true, true);
        let v = v.unwrap();
        let ir_id = match &v {
            ConstantOrIrValue::IrValue(x, false) => x.clone(),
            _ => {
                let ir_id = self.get_next_ir_id();
                self.c4ir_builder.create_constant(ir_id.clone(), &v, &tp);
                self.llvm_builder.create_constant(ir_id.clone(), &v, &tp);
                ir_id
            }
        };
        // 3.6.4.2: The integral promotions are performed on the
        // controlling expression.
        let (ir_id, tp) = self.do_integral_promotion_ir(ir_id, tp);

        let switch_def_ctx = SwitchDefCtx {
            ctrl_value_tp: tp,
            case_values: HashSet::new(),
            default_bb_id: default_bb.clone(),
            default_case_visited: false,
        };

        ctx.switch_stack.push(switch_def_ctx);
        ctx.break_bb_stack.push(break_bb.clone());
        self.c4ir_builder.enter_switch(&ir_id, &default_bb);
        self.llvm_builder.enter_switch(&ir_id, &default_bb);

        let body =
            &self.translation_unit.statements[switch_s.body_idx as usize];
        let body = (body, switch_s.get_body_loc());
        self.visit_stmt(body, ctx)?;

        self.c4ir_builder.create_br(&break_bb);
        self.llvm_builder.create_br(&break_bb);
        self.c4ir_builder.set_current_basic_block(&break_bb);
        self.llvm_builder.set_current_basic_block(&break_bb);

        ctx.switch_stack.pop();
        ctx.break_bb_stack.pop();
        self.c4ir_builder.leave_switch();
        self.llvm_builder.leave_switch();

        Ok(())
    }

    fn visit_while_stmt(
        &mut self,
        while_s: &ast::Statement_While,
        ctx: &mut FuncDefCtx,
    ) -> R<()> {
        let cond_bb = self.create_bb();
        let body_bb = self.create_bb();
        let break_bb = self.create_bb();
        self.c4ir_builder.create_br(&cond_bb);
        self.llvm_builder.create_br(&cond_bb);

        self.c4ir_builder.set_current_basic_block(&cond_bb);
        self.llvm_builder.set_current_basic_block(&cond_bb);
        let cond = &self.translation_unit.exprs[while_s.e_idx as usize];
        let cond = (cond, while_s.get_e_loc());
        let cond_ir_id = self.visit_cond_expr(cond)?;
        self.c4ir_builder
            .create_cond_br(&cond_ir_id, &body_bb, &break_bb);
        self.llvm_builder
            .create_cond_br(&cond_ir_id, &body_bb, &break_bb);

        self.c4ir_builder.set_current_basic_block(&body_bb);
        self.llvm_builder.set_current_basic_block(&body_bb);
        ctx.break_bb_stack.push(break_bb.clone());
        ctx.continue_bb_stack.push(cond_bb.clone());
        let body = &self.translation_unit.statements[while_s.body_idx as usize];
        let body = (body, while_s.get_body_loc());
        self.visit_stmt(body, ctx)?;
        self.c4ir_builder.create_br(&cond_bb);
        self.llvm_builder.create_br(&cond_bb);

        self.c4ir_builder.set_current_basic_block(&break_bb);
        self.llvm_builder.set_current_basic_block(&break_bb);
        ctx.break_bb_stack.pop();
        ctx.continue_bb_stack.pop();
        Ok(())
    }

    fn visit_do_while_stmt(
        &mut self,
        do_while_s: &ast::Statement_DoWhile,
        ctx: &mut FuncDefCtx,
    ) -> R<()> {
        let body_bb = self.create_bb();
        let cond_bb = self.create_bb();
        let break_bb = self.create_bb();
        self.c4ir_builder.create_br(&body_bb);
        self.c4ir_builder.create_br(&body_bb);

        self.c4ir_builder.set_current_basic_block(&body_bb);
        self.llvm_builder.set_current_basic_block(&body_bb);
        ctx.break_bb_stack.push(break_bb.clone());
        ctx.continue_bb_stack.push(cond_bb.clone());
        let body =
            &self.translation_unit.statements[do_while_s.body_idx as usize];
        let body = (body, do_while_s.get_body_loc());
        self.visit_stmt(body, ctx)?;
        self.c4ir_builder.create_br(&cond_bb);
        self.llvm_builder.create_br(&cond_bb);

        self.c4ir_builder.set_current_basic_block(&cond_bb);
        self.llvm_builder.set_current_basic_block(&cond_bb);
        ctx.break_bb_stack.pop();
        ctx.continue_bb_stack.pop();
        let cond = &self.translation_unit.exprs[do_while_s.e_idx as usize];
        let cond = (cond, do_while_s.get_e_loc());
        let cond_ir_id = self.visit_cond_expr(cond)?;
        self.c4ir_builder
            .create_cond_br(&cond_ir_id, &body_bb, &break_bb);
        self.llvm_builder
            .create_cond_br(&cond_ir_id, &body_bb, &break_bb);

        self.c4ir_builder.set_current_basic_block(&break_bb);
        self.llvm_builder.set_current_basic_block(&break_bb);
        Ok(())
    }

    fn visit_for_stmt(
        &mut self,
        for_s: &ast::Statement_For,
        ctx: &mut FuncDefCtx,
    ) -> R<()> {
        if for_s.e1_idx != 0 {
            let init = &self.translation_unit.exprs[for_s.e1_idx as usize];
            let init = (init, for_s.get_e1_loc());
            self.visit_expr(init, true, true);
        }

        let cond_bb = self.create_bb();
        let body_bb = self.create_bb();
        let incr_bb = self.create_bb();
        let break_bb = self.create_bb();
        self.c4ir_builder.create_br(&cond_bb);
        self.llvm_builder.create_br(&cond_bb);

        self.c4ir_builder.set_current_basic_block(&cond_bb);
        self.llvm_builder.set_current_basic_block(&cond_bb);
        if for_s.e2_idx != 0 {
            let cond = &self.translation_unit.exprs[for_s.e2_idx as usize];
            let cond = (cond, for_s.get_e2_loc());
            let cond_ir_id = self.visit_cond_expr(cond)?;
            self.c4ir_builder
                .create_cond_br(&cond_ir_id, &body_bb, &break_bb);
            self.llvm_builder
                .create_cond_br(&cond_ir_id, &body_bb, &break_bb);
        } else {
            self.c4ir_builder.create_br(&body_bb);
            self.llvm_builder.create_br(&body_bb);
        }

        self.c4ir_builder.set_current_basic_block(&body_bb);
        self.llvm_builder.set_current_basic_block(&body_bb);
        ctx.break_bb_stack.push(break_bb.clone());
        ctx.continue_bb_stack.push(incr_bb.clone());
        let body = &self.translation_unit.statements[for_s.body_idx as usize];
        let body = (body, for_s.get_body_loc());
        self.visit_stmt(body, ctx)?;
        ctx.break_bb_stack.pop();
        ctx.continue_bb_stack.pop();
        self.c4ir_builder.create_br(&incr_bb);
        self.llvm_builder.create_br(&incr_bb);

        self.c4ir_builder.set_current_basic_block(&incr_bb);
        self.llvm_builder.set_current_basic_block(&incr_bb);
        if for_s.e3_idx != 0 {
            let incr = &self.translation_unit.exprs[for_s.e3_idx as usize];
            let incr = (incr, for_s.get_e3_loc());
            self.visit_expr(incr, true, true);
        }
        self.c4ir_builder.create_br(&cond_bb);
        self.llvm_builder.create_br(&cond_bb);

        self.c4ir_builder.set_current_basic_block(&break_bb);
        self.llvm_builder.set_current_basic_block(&break_bb);
        Ok(())
    }

    fn visit_return_stmt(
        &mut self,
        return_s: L<&ast::Statement_Return>,
        ctx: &mut FuncDefCtx,
    ) -> R<()> {
        let loc = return_s.1;
        let return_s = return_s.0;

        let (tp, r) = if return_s.e_idx == 0 {
            if ctx.return_type.is_void() {
                self.c4ir_builder.create_return_void();
                self.llvm_builder.create_return_void();
                return Ok(());
            } else if ctx.func_name == "main" {
                // 3.6.6.4: If a return statement without an expression
                // is executed, and the value of the function call is
                // used by the caller, the behavior is undefined.
                (QType::from(Type::Int), Some(ConstantOrIrValue::I32(0)))
            } else {
                c4_fail!(loc, "Return value missing");
            }
        } else if ctx.return_type.is_void() {
            c4_fail!(return_s.get_e_loc(), "Unexpected return value");
        } else {
            let e = &self.translation_unit.exprs[return_s.e_idx as usize];
            self.visit_expr((e, return_s.get_e_loc()), true, true)
        };

        let (tp, r) =
            self.convert_lvalue_and_func_designator(tp, r, true, true, true);
        let r = r.unwrap();
        let ir_id = match &r {
            ConstantOrIrValue::IrValue(x, false) => x.clone(),
            _ => {
                let ir_id = self.get_next_ir_id();
                self.c4ir_builder.create_constant(ir_id.clone(), &r, &tp);
                self.llvm_builder.create_constant(ir_id.clone(), &r, &tp);
                ir_id
            }
        };
        if !tp.is_arithmetic_type() && !tp.is_pointer() {
            unimplemented!() // TODO: return struct / union
        }

        // 3.6.6.4: If the expression has a type different from that of
        // the function in which it appears, it is converted as if it
        // were assigned to an object of that type.
        let new_ir_id = self.get_next_ir_id();
        self.c4ir_builder.create_definition(
            false,
            &new_ir_id,
            &ctx.return_type,
            Linkage::NONE,
            &None,
        );
        self.llvm_builder.create_definition(
            false,
            &new_ir_id,
            &ctx.return_type,
            Linkage::NONE,
            &None,
        );
        // T y;
        // y = x;
        // return y;
        self.visit_simple_binary_op(
            &ctx.return_type,
            Some(ConstantOrIrValue::IrValue(new_ir_id.clone(), true)),
            return_s.get_e_loc(),
            &tp,
            Some(ConstantOrIrValue::IrValue(ir_id, false)),
            return_s.get_e_loc(),
            ast::Expr_Binary_Op::ASSIGN,
            true,
            true,
        );
        let ret_ir_id = self.get_next_ir_id();
        self.c4ir_builder.create_load(
            ret_ir_id.clone(),
            new_ir_id.clone(),
            &ctx.return_type,
        );
        self.llvm_builder.create_load(
            ret_ir_id.clone(),
            new_ir_id.clone(),
            &ctx.return_type,
        );
        self.c4ir_builder.create_return(&ret_ir_id);
        self.llvm_builder.create_return(&ret_ir_id);
        Ok(())
    }

    // could be applied to the control expression of `if`, (`do`) `while`,
    // `for`, `?:`, and all operands of `&&` and `||`.
    //
    // returns ir_id of bool.
    fn visit_cond_expr(&mut self, cond: L<&ast::Expr>) -> R<String> {
        let cond_loc = cond.1;
        let (cond_tp, cond) = self.visit_expr(cond, true, true);
        let (cond_tp, cond) = self.convert_lvalue_and_func_designator(
            cond_tp, cond, true, true, true,
        );
        let cond = self.convert_to_ir_value(&cond_tp, cond.unwrap());
        let cond_ir_id = match cond {
            ConstantOrIrValue::IrValue(ir_id, false) => ir_id,
            _ => unreachable!(),
        };
        self.visit_cond_expr_ir(cond_tp, cond_ir_id, cond_loc)
    }

    fn visit_cond_expr_ir(
        &mut self,
        cond_tp: QType,
        cond_ir_id: String,
        cond_loc: &ast::Loc,
    ) -> R<String> {
        // 3.6.4.1, 3.6.5, and 3.3.15 require the cond expression to have scalar
        // type. 3.3.13 and 3.3.14 require both operands to have scalar type.
        let zero_ir_id = self.get_next_ir_id();
        if !cond_tp.is_arithmetic_type() && !cond_tp.is_pointer() {
            c4_fail!(cond_loc, "Scalar type expected")
        } else {
            let zero = ConstantOrIrValue::U64(0);
            self.c4ir_builder.create_constant(
                zero_ir_id.clone(),
                &zero,
                &cond_tp,
            );
            self.llvm_builder.create_constant(
                zero_ir_id.clone(),
                &zero,
                &cond_tp,
            );
        }
        let is_signed = match &cond_tp.tp {
            Type::UnsignedChar
            | Type::UnsignedShort
            | Type::UnsignedInt
            | Type::UnsignedLong => false,
            _ => true,
        };
        let is_fp = cond_tp.is_arithmetic_type() && !cond_tp.is_integral_type();

        let cmp_ir_id = self.get_next_ir_id();
        self.c4ir_builder.create_cmp_op(
            cmp_ir_id.clone(),
            ast::Expr_Binary_Op::NEQ,
            is_signed,
            is_fp,
            cond_ir_id.clone(),
            zero_ir_id.clone(),
        );
        self.llvm_builder.create_cmp_op(
            cmp_ir_id.clone(),
            ast::Expr_Binary_Op::NEQ,
            is_signed,
            is_fp,
            cond_ir_id,
            zero_ir_id,
        );
        Ok(cmp_ir_id)
    }

    fn visit_type_name(&mut self, type_name: L<&ast::TypeName>) -> QType {
        let type_specifiers: Vec<L<&ast::TypeSpecifier>> = type_name
            .0
            .get_sp_qls()
            .into_iter()
            .flat_map(|spql| match &spql.elem {
                Some(ast::TypeName_SpQl_oneof_elem::sp(sp)) => {
                    Some((sp, spql.get_loc())).into_iter()
                }
                _ => None.into_iter(),
            })
            .collect();
        let type_qualifiers: Vec<L<ast::TypeQualifier>> = type_name
            .0
            .get_sp_qls()
            .into_iter()
            .flat_map(|spql| match &spql.elem {
                Some(ast::TypeName_SpQl_oneof_elem::ql(ql)) => {
                    Some((*ql, spql.get_loc())).into_iter()
                }
                _ => None.into_iter(),
            })
            .collect();
        let dst_tp = Compiler::qualify_type(
            &type_qualifiers,
            self.get_type(&type_specifiers, true),
        );
        if type_name.0.get_ad() == ast::AbstractDeclarator::default_instance() {
            dst_tp
        } else {
            self.unwrap_abstract_declarator(
                dst_tp,
                (type_name.0.get_ad(), type_name.0.get_ad_loc()),
            )
        }
    }

    fn get_type(
        &mut self,
        tss: &Vec<L<&ast::TypeSpecifier>>,
        try_ref_sue_type: bool,
    ) -> QType {
        let q = QType::from;
        let cases: Vec<&ast::TypeSpecifier_oneof_s> =
            tss.iter().flat_map(|(ts, _)| ts.s.iter()).collect();
        use ast::TypeSpecifier_oneof_s as TS;
        match cases.as_slice() {
            [TS::void(_)] => q(Type::Void),
            [TS::char(_)] | [TS::signed(_), TS::char(_)] => q(Type::Char),
            [TS::unsigned(_), TS::char(_)] => q(Type::UnsignedChar),
            [TS::short(_)]
            | [TS::signed(_), TS::short(_)]
            | [TS::short(_), TS::int(_)]
            | [TS::signed(_), TS::short(_), TS::int(_)] => q(Type::Short),
            [TS::unsigned(_), TS::short(_)]
            | [TS::unsigned(_), TS::short(_), TS::int(_)] => {
                q(Type::UnsignedShort)
            }
            [TS::int(_)]
            | [TS::signed(_)]
            | [TS::signed(_), TS::int(_)]
            | [] => q(Type::Int),
            [TS::unsigned(_)] | [TS::unsigned(_), TS::int(_)] => {
                q(Type::UnsignedInt)
            }
            [TS::long(_)]
            | [TS::signed(_), TS::long(_)]
            | [TS::long(_), TS::int(_)]
            | [TS::signed(_), TS::long(_), TS::int(_)] => q(Type::Long),
            [TS::unsigned(_), TS::long(_)]
            | [TS::unsigned(_), TS::long(_), TS::int(_)] => {
                q(Type::UnsignedLong)
            }
            [TS::float(_)] => q(Type::Float),
            [TS::double(_)] => q(Type::Double),
            [TS::long(_), TS::double(_)] => q(Type::Double),
            [TS::field_struct(s)] => {
                q(self.get_struct_type((s, tss[0].1), try_ref_sue_type))
            }
            [TS::union(u)] => {
                q(self.get_union_type((u, tss[0].1), try_ref_sue_type))
            }
            [TS::field_enum(e)] => {
                q(self.get_enum_type((e, tss[0].1), try_ref_sue_type))
            }
            [TS::typedef_name(s)] => self.get_typedef_type((s, tss[0].1)),
            _ => panic!(
                "{}: Illegal type specifiers list",
                Compiler::format_loc(tss[0].1)
            ),
        }
    }

    fn get_struct_type(
        &mut self,
        s: L<&ast::TypeSpecifier_Struct>,
        try_ref: bool,
    ) -> Type {
        let name = (s.0.get_name(), s.0.get_name_loc());
        let bodies = s.0.get_bodies().iter().zip(s.0.get_body_locs()).collect();
        self.get_su_type(name, bodies, try_ref, true)
    }

    fn get_union_type(
        &mut self,
        s: L<&ast::TypeSpecifier_Union>,
        try_ref: bool,
    ) -> Type {
        let name = (s.0.get_name(), s.0.get_name_loc());
        let bodies = s.0.get_bodies().iter().zip(s.0.get_body_locs()).collect();
        self.get_su_type(name, bodies, try_ref, false)
    }

    fn get_su_type(
        &mut self,
        name: L<&str>,
        // `struct S {}` is illegal syntax
        bodies: Vec<L<&ast::StructDeclaration>>,
        try_ref: bool,
        is_struct: bool,
    ) -> Type {
        // this case is invalid:
        //      struct S {...};
        //      { struct S; sizeof(struct S); }
        // since the inner `struct S` statement declares a new `struct S` type.
        //
        // but this one is valid:
        //      struct S {...};
        //      { struct S s; sizeof(struct S); }
        // because a body-less struct/union type specifier followed by
        // declarator is a reference, instead of declaration... except when
        // there is nothing to refer to. i.e. this case is also valid:
        //      struct S *sp;
        let try_ref = bodies.is_empty() && try_ref;

        match self.current_scope.lookup_sue_type(name.0) {
            // sanity checks
            //
            // parser should reject this syntax
            _ if name.0.is_empty() && bodies.is_empty() => panic!(
                "{}: struct/union tag and body cannot both be empty",
                Compiler::format_loc(name.1)
            ),
            Some(_) if name.0.is_empty() =>
            // programming error
            {
                panic!("empty tag found in Scope.sue_tag_names_ns")
            }

            // type reference
            Some((SueType::Struct(su_type), _)) if is_struct && try_ref => {
                Type::Struct(Box::new(*su_type.clone()))
            }
            Some((SueType::Union(su_type), _)) if !is_struct && try_ref => {
                Type::Union(Box::new(*su_type.clone()))
            }
            Some(_) if try_ref => panic!(
                "{}: '{}' defined as wrong kind of tag",
                Compiler::format_loc(name.1),
                name.0
            ),

            // type definition - check for possible redefinition errors
            Some((SueType::Struct(su_type), scope))
                if (!is_struct
                    || !bodies.is_empty() && su_type.fields.is_some())
                    && self.current_scope.same_as(scope) =>
            {
                panic!(
                    "{}: redefinition of '{}'",
                    Compiler::format_loc(name.1),
                    name.0
                )
            }
            Some((SueType::Union(su_type), scope))
                if (is_struct
                    || !bodies.is_empty() && su_type.fields.is_some())
                    && self.current_scope.same_as(scope) =>
            {
                panic!(
                    "{}: redefinition of '{}'",
                    Compiler::format_loc(name.1),
                    name.0
                )
            }
            Some((SueType::Enum(_, _), scope))
                if self.current_scope.same_as(scope) =>
            {
                panic!(
                    "{}: redefinition of '{}'",
                    Compiler::format_loc(name.1),
                    name.0
                )
            }

            // type reference - another special case:
            //      struct S {...};
            //      struct S;
            // (which is really just a def which does not conflict with / modify
            // previous defs in the same scope)
            Some((SueType::Struct(su_type), scope))
                if self.current_scope.same_as(scope) && bodies.is_empty() =>
            {
                Type::Struct(Box::new(*su_type.clone()))
            }
            Some((SueType::Union(su_type), scope))
                if self.current_scope.same_as(scope) && bodies.is_empty() =>
            {
                Type::Union(Box::new(*su_type.clone()))
            }

            // type definition
            _ => {
                let mut su_type = SuType {
                    fields: None,
                    uuid: self.get_next_uuid(),
                };
                let tag_name = if name.0.is_empty() {
                    format!("$.{}", su_type.uuid)
                } else {
                    String::from(name.0)
                };
                let ir_type_name = format!("$.{}", su_type.uuid);

                let sue_type = if is_struct {
                    SueType::Struct(Box::new(su_type.clone()))
                } else {
                    SueType::Union(Box::new(su_type.clone()))
                };
                self.current_scope
                    .sue_tag_names_ns
                    .insert(tag_name.clone(), sue_type);

                self.c4ir_builder.emit_opaque_struct_type(&ir_type_name);
                self.llvm_builder.emit_opaque_struct_type(&ir_type_name);

                if !bodies.is_empty() {
                    let f = |&b| self.get_su_field(b);
                    su_type.fields = Some(bodies.iter().flat_map(f).collect());

                    let sue_type = if is_struct {
                        SueType::Struct(Box::new(su_type.clone()))
                    } else {
                        SueType::Union(Box::new(su_type.clone()))
                    };
                    self.current_scope
                        .sue_tag_names_ns
                        .insert(tag_name, sue_type);

                    self.c4ir_builder.update_struct_type(
                        &ir_type_name,
                        su_type.fields.as_ref().unwrap(),
                        !is_struct,
                    );
                    self.llvm_builder.update_struct_type(
                        &ir_type_name,
                        su_type.fields.as_ref().unwrap(),
                        !is_struct,
                    );
                }

                if is_struct {
                    Type::Struct(Box::new(su_type))
                } else {
                    Type::Union(Box::new(su_type))
                }
            }
        }
    }

    fn get_enum_type(
        &mut self,
        s: L<&ast::TypeSpecifier_Enum>,
        try_ref: bool,
    ) -> Type {
        let uuid = self.get_next_uuid();
        let tag_name = if s.0.name.is_empty() {
            format!("$.{}", uuid)
        } else {
            s.0.name.clone()
        };
        // parser sanity check
        if s.0.name.is_empty() && s.0.get_bodies().is_empty() {
            panic!(
                "{}: enum tag and body cannot both be empty",
                Compiler::format_loc(s.1)
            );
        }

        let try_ref = s.0.get_bodies().is_empty() && try_ref;

        match self.current_scope.lookup_sue_type(&tag_name) {
            Some(_) if s.0.name.is_empty() => unreachable!(),

            // type ref
            // 3.5.2.2: Each enumerated type shall be compatible with an integer
            // type; the choice of type is implementation-defined.
            Some((SueType::Enum(_, _), _)) if try_ref => Type::Int,
            Some(_) if try_ref => panic!(
                "{}: '{}' defined as wrong kind of tag",
                Compiler::format_loc(s.1),
                s.0.name
            ),

            // redef errors
            Some((SueType::Enum(is_defined, _), scope))
                if *is_defined
                    && !s.0.get_bodies().is_empty()
                    && self.current_scope.same_as(scope) =>
            {
                panic!(
                    "{}: Redefinition of enum '{}'",
                    Compiler::format_loc(s.1),
                    tag_name
                )
            }
            Some((SueType::Struct(_), scope))
            | Some((SueType::Union(_), scope))
                if self.current_scope.same_as(scope) =>
            {
                panic!(
                    "{}: Redefinition of tag '{}'",
                    Compiler::format_loc(s.1),
                    tag_name
                )
            }

            // type ref
            Some((SueType::Enum(_, _), scope))
                if self.current_scope.same_as(scope)
                    && s.0.get_bodies().is_empty() =>
            {
                Type::Int
            }

            // type def
            _ => {
                self.current_scope.sue_tag_names_ns.insert(
                    tag_name.clone(),
                    SueType::Enum(!s.0.get_bodies().is_empty(), uuid),
                );
                let mut next_body_value = 0;
                s.0.get_bodies().into_iter().for_each(|body| {
                    let name = body.get_name();
                    if name.is_empty() {
                        panic!()
                    }
                    let value = if body.value != 0 {
                        let e =
                            &self.translation_unit.exprs[body.value as usize];
                        let e = (e, body.get_value_loc());
                        let (tp, e) = self.visit_expr(e, true, true);
                        let e = e.unwrap();
                        if !tp.is_integral_type() {
                            panic!(
                                "{}: Integral expression expected",
                                Compiler::format_loc(body.get_value_loc())
                            )
                        }
                        match e.as_constant_u64() {
                            None => panic!(
                                "{}: Constant expression expected",
                                Compiler::format_loc(body.get_value_loc())
                            ),
                            Some(v) => {
                                let v = v as i64;
                                if v < -2147483648_i64 || v > 2147483647_i64 {
                                    panic!(
                                        "{}: Enumerator value out of bound",
                                        Compiler::format_loc(
                                            body.get_value_loc()
                                        )
                                    )
                                }
                                next_body_value = v as i32 + 1;
                                v as i32
                            }
                        }
                    } else {
                        let r = next_body_value;
                        next_body_value += 1;
                        r
                    };

                    match self.current_scope.ordinary_ids_ns.get(name) {
                        None => {
                            self.current_scope.ordinary_ids_ns.insert(
                                name.to_string(),
                                OrdinaryIdRef::EnumRef(value),
                            );
                        }
                        Some(_) => panic!(
                            "{}: Redefinition of identifier '{}'",
                            Compiler::format_loc(body.get_name_loc()),
                            name
                        ),
                    }
                });
                Type::Int
            }
        }
    }

    fn get_typedef_type(&mut self, id: L<&str>) -> QType {
        // It is probably a programming error if this method really panics.
        match self.current_scope.lookup_ordinary_id(id.0) {
            None =>
            // this error should have been captured by parser
            {
                panic!(
                    "{}: Undeclared identifier '{}'",
                    Compiler::format_loc(id.1),
                    id.0
                )
            }
            Some((OrdinaryIdRef::TypedefRef(qtype), _)) => *qtype.clone(),
            Some(_) =>
            // this error should also have been handled elsewhere
            {
                panic!(
                    "{}: Identifier '{}' is not a typedef name",
                    Compiler::format_loc(id.1),
                    id.0
                )
            }
        }
    }

    fn get_su_field(&mut self, sd: L<&ast::StructDeclaration>) -> Vec<SuField> {
        let type_specifiers: Vec<L<&ast::TypeSpecifier>> =
            sd.0.get_sp_qls()
                .iter()
                .filter(|spql| spql.has_sp())
                .map(|spql| (spql.get_sp(), spql.get_loc()))
                .collect();
        let type_qualifiers: Vec<L<ast::TypeQualifier>> =
            sd.0.get_sp_qls()
                .iter()
                .filter(|spql| spql.has_ql())
                .map(|spql| (spql.get_ql(), spql.get_loc()))
                .collect();

        let qualified_type: QType = Compiler::qualify_type(
            &type_qualifiers,
            self.get_type(&type_specifiers, true),
        );

        let r: Vec<SuField> =
            sd.0.ds
                .iter()
                .map(|decl| {
                    let (tp, field_name) = if decl.get_d().get_dd_idx() != 0 {
                        self.unwrap_declarator(
                            qualified_type.clone(),
                            (decl.get_d(), decl.get_d_loc()),
                            false,
                        )
                    } else {
                        (qualified_type.clone(), String::from(""))
                    };
                    // 3.5.2.1: A structure or union shall not contain a member with
                    // incomplete or function type.
                    match Compiler::get_type_size_and_align_bytes(&tp.tp) {
                        None => panic!(
                            "{}: Struct or union contains incomplete type",
                            Compiler::format_loc(sd.1)
                        ),
                        _ => (),
                    }
                    let field_name_opt = if field_name.is_empty() {
                        None
                    } else {
                        Some(field_name)
                    };
                    let bit_field_size: Option<u8> = if decl.e == 0 {
                        None
                    } else {
                        // 3.5.2.1: A bit-field may have type int, unsigned int,
                        // or signed int.
                        match tp.tp {
                            Type::Int | Type::UnsignedInt => (),
                            _ => panic!(
                                "{}: Unexpected bit-field type",
                                Compiler::format_loc(sd.1)
                            ),
                        }
                        let sz = self.get_bitmask_size((
                            &self.translation_unit.exprs[decl.e as usize],
                            decl.get_e_loc(),
                        ));
                        if sz == 0 && field_name_opt.is_none() {
                            panic!(
                                "{}: Named bit-field cannot have zero width",
                                Compiler::format_loc(sd.1)
                            )
                        }
                        Some(sz)
                    };
                    SuField {
                        name: field_name_opt,
                        tp,
                        bit_field_size,
                    }
                })
                .collect();
        r
    }

    fn unwrap_declarator(
        &mut self,
        mut tp: QType,
        d: L<&ast::Declarator>,
        is_function_definition: bool,
    ) -> (QType, String) {
        tp = self.unwrap_pointer(tp, d.0.ptr_idx);
        self.unwrap_direct_declarator(tp, d.0.dd_idx, is_function_definition)
    }

    fn unwrap_direct_declarator(
        &mut self,
        mut tp: QType,
        dd_idx: i32,
        is_function_definition: bool,
    ) -> (QType, String) {
        let dd = &self.translation_unit.direct_declarators[dd_idx as usize];
        use ast::DirectDeclarator_oneof_dd as DD;
        match dd.dd.as_ref().unwrap() {
            DD::id(id) => (tp, id.id.clone()),
            DD::d(d) => {
                self.unwrap_declarator(tp, (d.get_d(), d.get_loc()), false)
            }
            DD::array(array) => {
                match Compiler::get_type_size_and_align_bytes(&tp.tp) {
                    None => panic!(
                        "{}: Array has incomplete element type",
                        Compiler::format_loc(array.get_dd_loc())
                    ),
                    _ => (),
                }
                let size: Option<u32> = if array.size_idx == 0 {
                    None
                } else {
                    Some(self.get_array_size((
                        &self.translation_unit.exprs[array.size_idx as usize],
                        array.get_size_loc(),
                    )))
                };
                tp = QType::from(Type::Array(Box::new(tp), size));
                self.unwrap_direct_declarator(tp, array.dd_idx, false)
            }
            DD::ft(ft) => {
                if tp.is_function() || tp.is_array() {
                    // 3.5.4.3: A function declarator shall not specify a return
                    // type that is a function type or an array type.
                    panic!(
                        "{}: Function cannot return function or array",
                        Compiler::format_loc(ft.get_dd_loc())
                    )
                }
                let pds = ft.get_pds().iter().zip(ft.get_pd_locs()).collect();
                let func_params_opt = self.get_typed_func_params(
                    pds,
                    ft.has_ellipsis,
                    is_function_definition,
                );
                tp = QType::from(Type::Function(Box::new(tp), func_params_opt));
                self.unwrap_direct_declarator(tp, ft.dd_idx, false)
            }
            DD::ids_list(ids_list) => {
                if tp.is_function() || tp.is_array() {
                    // 3.5.4.3: A function declarator shall not specify a return
                    // type that is a function type or an array type.
                    panic!(
                        "{}: Function cannot return function or array",
                        Compiler::format_loc(ids_list.get_dd_loc())
                    )
                }
                if is_function_definition {
                    self.enter_scope();
                }
                let params = if ids_list.ids.is_empty() {
                    None
                } else {
                    Some(FuncParams::Names(ids_list.ids.clone().into_vec()))
                };
                tp = QType::from(Type::Function(Box::new(tp), params));
                self.unwrap_direct_declarator(tp, ids_list.dd_idx, false)
            }
        }
    }

    fn unwrap_abstract_declarator(
        &mut self,
        mut tp: QType,
        ad: L<&ast::AbstractDeclarator>,
    ) -> QType {
        tp = self.unwrap_pointer(tp, ad.0.ptr_idx);
        self.unwrap_direct_abstract_declarator(tp, ad.0.dad_idx)
    }

    fn unwrap_direct_abstract_declarator(
        &mut self,
        mut tp: QType,
        dad_idx: i32,
    ) -> QType {
        if dad_idx == 0 {
            return tp;
        }

        let dad = &self.translation_unit.direct_abstract_declarators
            [dad_idx as usize];
        use ast::DirectAbstractDeclarator_oneof_dad as DAD;
        match dad.dad.as_ref().unwrap() {
            DAD::simple(simple) => self.unwrap_abstract_declarator(
                tp,
                (simple.get_ad(), simple.get_ad_loc()),
            ),
            DAD::array(array) => {
                match Compiler::get_type_size_and_align_bytes(&tp.tp) {
                    None => panic!(
                        "{}: Array has incomplete element type",
                        Compiler::format_loc(array.get_dad_loc())
                    ),
                    _ => (),
                }
                let size: Option<u32> = if array.size_idx == 0 {
                    None
                } else {
                    Some(self.get_array_size((
                        &self.translation_unit.exprs[array.size_idx as usize],
                        array.get_size_loc(),
                    )))
                };
                tp = QType::from(Type::Array(Box::new(tp), size));
                self.unwrap_direct_abstract_declarator(tp, array.dad_idx)
            }
            DAD::func(func) => {
                if tp.is_function() || tp.is_array() {
                    // 3.5.4.3: A function declarator shall not specify a return
                    // type that is a function type or an array type.
                    panic!(
                        "{}: Function cannot return function or array",
                        Compiler::format_loc(func.get_dad_loc())
                    )
                }
                let pds =
                    func.get_pds().iter().zip(func.get_pd_locs()).collect();
                let func_params_opt =
                    self.get_typed_func_params(pds, func.has_ellipsis, false);
                tp = QType::from(Type::Function(Box::new(tp), func_params_opt));
                self.unwrap_direct_abstract_declarator(tp, func.dad_idx)
            }
        }
    }

    fn unwrap_pointer(&self, mut tp: QType, mut ptr_idx: i32) -> QType {
        while ptr_idx != 0 {
            let pointer = &self.translation_unit.pointers[ptr_idx as usize];
            let type_qualifiers: Vec<L<ast::TypeQualifier>> = pointer
                .get_qs()
                .iter()
                .map(|q| q.clone())
                .zip(pointer.get_q_locs())
                .collect();
            tp = QType::from(Type::Pointer(Box::new(tp.clone())));
            tp = Compiler::qualify_type(&type_qualifiers, tp);
            ptr_idx = pointer.ptr_idx;
        }
        tp
    }

    fn get_typed_func_params(
        &mut self,
        pds: Vec<L<&ast::ParamDeclaration>>,
        has_ellipsis: bool,
        is_function_definition: bool,
    ) -> Option<FuncParams> {
        self.enter_scope();
        let v: Vec<TypedFuncParam> = pds
            .into_iter()
            .map(|pd| self.get_typed_func_param(pd))
            .collect();
        if !is_function_definition {
            self.leave_scope();
        } // otherwise: let visit_function_def close the scope
        let is_void = |p: &TypedFuncParam| match p {
            TypedFuncParam {
                is_register: false,
                tp:
                    QType {
                        is_const: false,
                        is_volatile: false,
                        tp: Type::Void,
                    },
                name: None,
            } => true,
            _ => false,
        };
        // TODO: does C89 allow type-only params in function definitions?
        match v.as_slice() {
            // 3.5.4.3: An empty list in a function declarator that...
            // - is part of a function definition:
            //   > specifies that the function has no parameters.
            // - is not part of a function definition:
            //   > specifies that no information about the number or types of
            //     the parameters is supplied.
            [] if !is_function_definition => None,
            [] => Some(FuncParams::Typed(vec![], false)),
            // "int f(void) {...}"
            [p] if is_void(p) && !has_ellipsis => {
                Some(FuncParams::Typed(vec![], false))
            }
            _ => Some(FuncParams::Typed(v, has_ellipsis)),
        }
    }

    fn get_typed_func_param(
        &mut self,
        pd: L<&ast::ParamDeclaration>,
    ) -> TypedFuncParam {
        let mut get_is_register_qtype = |dss| {
            let (scs_opt, qtype) = self.visit_declaration_specifiers(dss, true);
            use ast::StorageClassSpecifier as SCS;
            match scs_opt {
                None => (false, qtype),
                Some((SCS::REGISTER, _)) => (true, qtype),
                Some((_, scs_loc)) => panic!(
                    "{}: Invalid storage class specifier \
                     in function declarator",
                    Compiler::format_loc(scs_loc)
                ),
            }
        };
        use ast::ParamDeclaration_oneof_pd as PD;
        match pd.0.pd.as_ref().unwrap() {
            PD::name(named) => {
                let (is_register, tp) = get_is_register_qtype(named.get_dss());
                let d = (named.get_d(), named.get_d_loc());
                let (tp, name) = self.unwrap_declarator(tp, d, false);
                TypedFuncParam {
                    is_register,
                    tp,
                    name: Some(name),
                }
            }
            PD::type_only(type_only) => {
                let (is_register, tp) =
                    get_is_register_qtype(type_only.get_dss());
                let ad = (type_only.get_ad(), type_only.get_ad_loc());
                let tp = self.unwrap_abstract_declarator(tp, ad);
                TypedFuncParam {
                    is_register,
                    tp,
                    name: None,
                }
            }
            PD::type_only_simple(type_only_simple) => {
                let (is_register, tp) =
                    get_is_register_qtype(type_only_simple.get_dss());
                TypedFuncParam {
                    is_register,
                    tp,
                    name: None,
                }
            }
        }
    }

    fn add_declaration(
        &mut self,
        id: &str,
        l_scs: &Option<L<ast::StorageClassSpecifier>>,
        new_tp: QType,
        pick_outer_scope: bool,
        loc: &ast::Loc,
    ) {
        use ast::StorageClassSpecifier as SCS;
        let scs: Option<SCS> = match l_scs {
            None => None,
            Some((SCS::EXTERN, _)) => Some(SCS::EXTERN),
            Some((SCS::STATIC, _)) => Some(SCS::STATIC),
            Some((other_scs, loc)) => panic!(
                "{}: Illegal storage class specifier {}",
                Compiler::format_loc(loc),
                other_scs.descriptor().name()
            ),
        };
        let next_uuid = self.get_next_uuid();
        let mut scope: &mut Scope = &mut self.current_scope;
        if pick_outer_scope {
            match scope.outer_scope.as_mut() {
                Some(s) => scope = s,
                _ => panic!("programming error"),
            }
        }
        let is_func = match new_tp.tp {
            Type::Function(_, _) => true,
            _ => false,
        };

        let insert_decl =
            |scope: &mut Scope,
             linkage_fn: fn(Option<(&Scope, Linkage)>) -> Linkage| {
                match scope.lookup_ordinary_id(id) {
                    Some((
                        OrdinaryIdRef::ObjFnRef(
                            ir_id,
                            old_tp,
                            old_linkage,
                            is_defined,
                        ),
                        old_scope,
                    )) => {
                        let linkage =
                            linkage_fn(Some((old_scope, old_linkage.clone())));
                        // 3.1.2.6: For an identifier with external or internal
                        // linkage declared in the same scope as another
                        // declaration for that identifier, the type of the
                        // identifier becomes the composite type.
                        let tp = if old_scope.same_as(scope)
                            && linkage != Linkage::NONE
                        {
                            Compiler::get_composite_type(old_tp, &new_tp, loc)
                        } else if old_scope.same_as(scope) {
                            // 3.1.2.2: Identifiers with no linkage denote
                            // unique entities.
                            panic!(
                                "{}: Redefinition of identifier {}",
                                Compiler::format_loc(loc),
                                id
                            );
                        } else {
                            new_tp.clone()
                        };
                        let new_ref = OrdinaryIdRef::ObjFnRef(
                            ir_id.clone(),
                            tp,
                            linkage,
                            *is_defined,
                        );
                        scope.ordinary_ids_ns.insert(String::from(id), new_ref);
                    }
                    Some((_, old_scope)) if old_scope.same_as(scope) => panic!(
                        "{}: Redeclaration of '{}' as different kind of symbol",
                        Compiler::format_loc(loc),
                        id
                    ),
                    _ => {
                        let linkage = linkage_fn(None);
                        let ir_id = if linkage == Linkage::EXTERNAL {
                            String::from(id)
                        } else {
                            format!("${}.{}", id, next_uuid)
                        };
                        scope.ordinary_ids_ns.insert(
                            String::from(id),
                            OrdinaryIdRef::ObjFnRef(
                                ir_id.clone(),
                                new_tp.clone(),
                                linkage,
                                false,
                            ),
                        );
                    }
                }
            };

        if scope.is_file_scope() && scs == Some(SCS::STATIC) {
            // 3.1.2.2: If the declaration of an identifier for an object or a
            // function has file scope and contains the storage-class specifier
            // static, the identifier has internal linkage.
            insert_decl(scope, |_| Linkage::INTERNAL);
        } else if scs == Some(SCS::EXTERN) || (is_func && scs.is_none()) {
            // 3.1.2.2: If the declaration of an identifier for an object or a
            // function contains the storage-class specifier extern, the
            // identifier has the same linkage as any visible declaration of the
            // identifier with file scope. If there is no visible declaration
            // with file scope, the identifier has external linkage.
            //
            // If the declaration of an identifier for a function has no
            // storage-class specifier, its linkage is determined exactly as if
            // it were declared with the storage-class specifier extern.
            insert_decl(scope, |scope_linkage| match scope_linkage {
                Some((scope, linkage)) if scope.is_file_scope() => linkage,
                _ => Linkage::EXTERNAL,
            });
        } else if !is_func && scope.is_file_scope() && scs.is_none() {
            // 3.2.2.2: If the declaration of an identifier for an object has
            // file scope and no storage-class specifier, its linkage is
            // external.
            insert_decl(scope, |_| Linkage::EXTERNAL);
        } else {
            insert_decl(scope, |_| Linkage::NONE);
        }
    }

    fn cast_expression(
        &mut self,
        src_tp: QType,
        v: Option<ConstantOrIrValue>,
        dst_tp: QType,
        loc: &ast::Loc,
        emit_ir: bool,
    ) -> (QType, Option<ConstantOrIrValue>) {
        macro_rules! do_cast {
            ($v:tt, $tp:tt) => {{
                use ConstantOrIrValue as C;
                match $tp {
                    Type::Char => (dst_tp, Some(C::I8(*$v as i8))),
                    Type::UnsignedChar => (dst_tp, Some(C::U8(*$v as u8))),
                    Type::Short => (dst_tp, Some(C::I16(*$v as i16))),
                    Type::UnsignedShort => (dst_tp, Some(C::U16(*$v as u16))),
                    Type::Int => (dst_tp, Some(C::I32(*$v as i32))),
                    Type::UnsignedInt => (dst_tp, Some(C::U32(*$v as u32))),
                    Type::Long => (dst_tp, Some(C::I64(*$v as i64))),
                    Type::UnsignedLong => (dst_tp, Some(C::U64(*$v as u64))),
                    Type::Float => (dst_tp, Some(C::Float(*$v as f32))),
                    Type::Double => (dst_tp, Some(C::Double(*$v as f64))),
                    Type::Pointer(_) => (dst_tp, Some(C::U64(*$v as u64))),
                    _ => unreachable!(),
                }
            }};
        }

        // now cast `v` of `src_tp` into `dst_tp`
        match (&src_tp.tp, &v, &dst_tp.tp) {
            // 3.3.4: Unless the type name specifies void type, the type name
            // shall specify qualified or unqualified scalar type and the
            // operand shall have scalar type.
            (_, _, Type::Void) => (dst_tp, None),
            (Type::Struct(_), _, _)
            | (Type::Union(_), _, _)
            | (Type::Array(_, _), _, _)
            | (Type::Function(_, _), _, _)
            | (_, _, Type::Struct(_))
            | (_, _, Type::Union(_))
            | (_, _, Type::Array(_, _))
            | (_, _, Type::Function(_, _)) => panic!(
                "{}: Cannot cast from/to non-scalar types",
                Compiler::format_loc(loc)
            ),

            (Type::Float, _, Type::Pointer(_))
            | (Type::Double, _, Type::Pointer(_))
            | (Type::Pointer(_), _, Type::Float)
            | (Type::Pointer(_), _, Type::Double) => panic!(
                "{}: Cannot cast floating point values from/to pointers",
                Compiler::format_loc(loc)
            ),

            (_, None, _) => (dst_tp, None),

            (
                Type::Pointer(_),
                Some(ConstantOrIrValue::IrValue(src_ir_id, false)),
                Type::Pointer(_),
            ) => {
                let dst_ir_id = self.get_next_ir_id();
                self.c4ir_builder.create_cast(
                    dst_ir_id.clone(),
                    &dst_tp,
                    src_ir_id.clone(),
                    &src_tp,
                );
                self.llvm_builder.create_cast(
                    dst_ir_id.clone(),
                    &dst_tp,
                    src_ir_id.clone(),
                    &src_tp,
                );
                (dst_tp, Some(ConstantOrIrValue::IrValue(dst_ir_id, false)))
            }
            (Type::Pointer(_), _, Type::Pointer(_)) => (dst_tp, v),

            (_, Some(p @ ConstantOrIrValue::Address(_, _)), _)
            | (_, Some(p @ ConstantOrIrValue::IrValue(_, _)), _) => {
                if !emit_ir {
                    (dst_tp, None)
                } else {
                    let src_ir_id = match p {
                        ConstantOrIrValue::IrValue(ir_id, false) => {
                            ir_id.clone()
                        }
                        ConstantOrIrValue::IrValue(_, true) => {
                            unreachable!() // convert_lvalue_and_func_designator
                        }
                        c => {
                            let ir_id = self.get_next_ir_id();
                            self.c4ir_builder.create_constant(
                                ir_id.clone(),
                                c,
                                &src_tp,
                            );
                            self.llvm_builder.create_constant(
                                ir_id.clone(),
                                c,
                                &src_tp,
                            );
                            ir_id
                        }
                    };
                    let dst_ir_id = self.get_next_ir_id();
                    self.c4ir_builder.create_cast(
                        dst_ir_id.clone(),
                        &dst_tp,
                        src_ir_id.clone(),
                        &src_tp,
                    );
                    self.llvm_builder.create_cast(
                        dst_ir_id.clone(),
                        &dst_tp,
                        src_ir_id,
                        &src_tp,
                    );
                    (dst_tp, Some(ConstantOrIrValue::IrValue(dst_ir_id, false)))
                }
            }

            // compile time constant folding for cast expressions
            (_, Some(ConstantOrIrValue::I8(v)), tp) => do_cast!(v, tp),
            (_, Some(ConstantOrIrValue::U8(v)), tp) => do_cast!(v, tp),
            (_, Some(ConstantOrIrValue::I16(v)), tp) => do_cast!(v, tp),
            (_, Some(ConstantOrIrValue::U16(v)), tp) => do_cast!(v, tp),
            (_, Some(ConstantOrIrValue::I32(v)), tp) => do_cast!(v, tp),
            (_, Some(ConstantOrIrValue::U32(v)), tp) => do_cast!(v, tp),
            (_, Some(ConstantOrIrValue::I64(v)), tp) => do_cast!(v, tp),
            (_, Some(ConstantOrIrValue::U64(v)), tp) => do_cast!(v, tp),
            (_, Some(ConstantOrIrValue::Float(v)), tp) => do_cast!(v, tp),
            (_, Some(ConstantOrIrValue::Double(v)), tp) => do_cast!(v, tp),
        }
    }

    fn get_composite_type(old: &QType, new: &QType, loc: &ast::Loc) -> QType {
        Compiler::try_get_composite_type(old, new, loc).unwrap()
    }

    fn try_get_composite_type(
        old: &QType,
        new: &QType,
        loc: &ast::Loc,
    ) -> Result<QType, String> {
        macro_rules! incompatible_panic {
            () => {{
                let msg = format!(
                    "{}: Type incompatible with previous declaration",
                    Compiler::format_loc(loc)
                );
                return Err(msg);
            }};
            ($msg:expr) => {{
                let msg = format!(
                    "{}: Type incompatible with previous declaration; {}",
                    Compiler::format_loc(loc),
                    $msg
                );
                return Err(msg);
            }};
        };

        // 3.5.3: For two qualified types to be compatible, both shall have the
        // identically qualified version of a compatible type; the order of type
        // qualifiers within a list of specifiers or qualifiers does not affect
        // the specified type.
        if old.is_const != new.is_const || old.is_volatile != new.is_volatile {
            incompatible_panic!();
        }

        let tp = match (&old.tp, &new.tp) {
            (Type::Void, Type::Void) => Type::Void,
            (Type::Char, Type::Char) => Type::Char,
            (Type::UnsignedChar, Type::UnsignedChar) => Type::UnsignedChar,
            (Type::Short, Type::Short) => Type::Short,
            (Type::UnsignedShort, Type::UnsignedShort) => Type::UnsignedShort,
            (Type::Int, Type::Int) => Type::Int,
            (Type::UnsignedInt, Type::UnsignedInt) => Type::UnsignedInt,
            (Type::Long, Type::Long) => Type::Long,
            (Type::UnsignedLong, Type::UnsignedLong) => Type::UnsignedLong,
            (Type::Float, Type::Float) => Type::Float,
            (Type::Double, Type::Double) => Type::Double,
            // 3.1.2.6: Two structure, union, or enumeration types declared in
            // separate translation units are compatible if they have the same
            // number of members, the same member names, and compatible member
            // types;
            //
            // for two structures, the members shall be in the same order;
            // for two enumerations, the members shall have the same values.
            //
            // For an identifier with external or internal linkage declared in
            // the same scope as another declaration for that identifier, the
            // type of the identifier becomes the composite type.
            ////////////////////////////////////////////////////////////////////
            // However, in practice, clang rejects codes like:
            //
            //     extern struct {int n;} x;
            //     extern struct {int n;} x;
            //
            // and:
            //
            //     void f(struct {int n;} x);
            //     void f(struct {int n;} x);
            //
            // But codes like this are accepted:
            //
            //     extern int x[];
            //     extern int x[16];
            //
            // The reason clang gives, is that the two struct types are
            // different. Seems like clang is actually not compliant to the spec
            // in this case, but here we choose to mimic clang's behavior.
            (Type::Struct(tp_left), Type::Struct(tp_right))
            | (Type::Union(tp_left), Type::Union(tp_right)) => {
                match (&tp_left.fields, &tp_right.fields) {
                    (Some(_), Some(_)) => {
                        if tp_left.uuid != tp_right.uuid {
                            incompatible_panic!();
                        }
                        new.tp.clone()
                    }
                    (Some(_), None) => old.tp.clone(),
                    _ => new.tp.clone(),
                }
            }
            (Type::Pointer(tp_left), Type::Pointer(tp_right)) => Type::Pointer(
                Box::new(Compiler::get_composite_type(tp_left, tp_right, loc)),
            ),
            (
                Type::Array(tp_left, sz_left),
                Type::Array(tp_right, sz_right),
            ) => {
                let elem_tp =
                    Compiler::get_composite_type(tp_left, tp_right, loc);
                let sz = match (sz_left, sz_right) {
                    (Some(s1), Some(s2)) if s1 == s2 => Some(*s1),
                    (Some(_), Some(_)) => {
                        incompatible_panic!("different array sizes")
                    }
                    (None, _) => *sz_right,
                    _ => *sz_left,
                };
                Type::Array(Box::new(elem_tp), sz)
            }
            (
                Type::Function(rtp_left, params_left),
                Type::Function(rtp_right, params_right),
            ) => {
                // 3.5.4.3: For two function types to be compatible, both shall
                // specify compatible return types.
                let rtp =
                    Compiler::get_composite_type(rtp_left, rtp_right, loc);
                // 3.7.1: The return type of a function shall be void or an
                // object type other than array.
                match rtp.tp {
                    // should have been captured when unwraping declarators
                    Type::Array(_, _) | Type::Function(_, _) => unreachable!(),
                    _ => (),
                }
                let params_left = Compiler::sanitize_param_types(params_left);
                let params_right = Compiler::sanitize_param_types(params_right);
                let params = match (params_left, params_right) {
                    (
                        Some(FuncParams::Typed(tps_left, is_varargs_left)),
                        Some(FuncParams::Typed(tps_right, is_varargs_right)),
                    ) => {
                        // 3.5.4.3: Moreover, the parameter type lists, if both
                        // are present, shall agree in the number of parameters
                        // and in use of the ellipsis terminator; corresponding
                        // parameters shall have compatible types.
                        //
                        // 3.1.2.6: If both types have parameter type lists, the
                        // type of each parameter in the composite parameter
                        // type list is the composite type of the corresponding
                        // parameters.
                        if tps_left.len() != tps_right.len() {
                            incompatible_panic!("different number of params")
                        }
                        if is_varargs_left != is_varargs_right {
                            incompatible_panic!("different varargs usage")
                        }
                        let tps: Vec<Result<TypedFuncParam, String>> = tps_left
                            .iter()
                            .zip(tps_right)
                            .map(|(tp_left, tp_right)| {
                                if tp_left.is_register != tp_right.is_register {
                                    incompatible_panic!(
                                        "different 'register' usage"
                                    )
                                }
                                let name = tp_right
                                    .name
                                    .clone()
                                    .or(tp_left.name.clone());
                                let param = TypedFuncParam {
                                    is_register: tp_left.is_register,
                                    tp: Compiler::get_composite_type(
                                        &tp_left.tp,
                                        &tp_right.tp,
                                        loc,
                                    ),
                                    name,
                                };
                                Ok(param)
                            })
                            .collect();
                        for tp in &tps {
                            if tp.is_err() {
                                return Err(tp.clone().unwrap_err());
                            }
                        }
                        let tps: Vec<TypedFuncParam> =
                            tps.into_iter().map(|tp| tp.unwrap()).collect();
                        Some(FuncParams::Typed(tps, is_varargs_left))
                    }
                    (
                        Some(FuncParams::Typed(tps, is_varargs)),
                        Some(FuncParams::Names(names)),
                    )
                    | (
                        Some(FuncParams::Names(names)),
                        Some(FuncParams::Typed(tps, is_varargs)),
                    ) => {
                        // 3.5.4.3: If one type has a parameter type list and
                        // the other type is specified by a function definition
                        // that contains a (possibly empty) identifier list,
                        // both shall agree in the number of parameters, and the
                        // type of each prototype parameter shall be compatible
                        // with the type that results from the application of
                        // the default argument promotions to the type of the
                        // corresponding identifier.
                        //
                        // 3.1.2.6: If only one type is a function type with a
                        // parameter type list (a function prototype), the
                        // composite type is a function prototype with the
                        // parameter type list.
                        if tps.len() != names.len() {
                            incompatible_panic!(
                                "number of params does not match"
                            )
                        }
                        Some(FuncParams::Typed(tps, is_varargs))
                    }
                    (Some(x), _) | (_, Some(x)) => Some(x),
                    (None, None) => None,
                };
                Type::Function(Box::new(rtp), params)
            }
            _ => incompatible_panic!(),
        };

        let tp = QType {
            is_const: new.is_const,
            is_volatile: new.is_volatile,
            tp,
        };
        Ok(tp)
    }

    fn get_type_size_and_align_bytes(tp: &Type) -> Option<(u32, u32)> {
        match tp {
            Type::Void => None,
            Type::Char | Type::UnsignedChar => Some((1, 1)),
            Type::Short | Type::UnsignedShort => Some((2, 2)),
            Type::Int | Type::UnsignedInt => Some((4, 4)),
            Type::Long | Type::UnsignedLong => Some((8, 8)),
            Type::Float => Some((4, 4)),
            Type::Double => Some((8, 8)),
            Type::Struct(body) => body.fields.as_ref().and_then(|fs| {
                let mut sz: u32 = 0;
                let mut align: u32 = 0;
                let mut bit_field_quota: u32 = 0;
                for f in fs {
                    match Compiler::get_type_size_and_align_bytes(&f.tp.tp) {
                        // get_su_field ensures struct/union does not contain
                        // fields of incomplete type.
                        None => unreachable!(),
                        Some((f_sz, f_align)) => {
                            align = max(align, f_align);
                            match f.bit_field_size {
                                None => {
                                    sz = Compiler::align_up(sz, f_align) + f_sz
                                }
                                // 3.5.2.1: a bit-field with a width of 0
                                // indicates that no further bit-field is to be
                                // packed into the unit in which the previous
                                // bit-field, if any, was placed.
                                Some(0) => {
                                    bit_field_quota = 0;
                                }
                                Some(x) if (x as u32) <= bit_field_quota => {
                                    bit_field_quota -= x as u32;
                                }
                                Some(x) => {
                                    // bit field type and mask size were checked
                                    // in get_su_field.
                                    sz = Compiler::align_up(sz, f_align) + f_sz;
                                    bit_field_quota = f_sz - x as u32;
                                }
                            }
                        }
                    }
                }
                // 3.5.2.1: There may also be unnamed padding at the end of a
                // structure or union, as necessary to achieve the appropriate
                // alignment were the structure or union to be a member of an
                // array.
                sz = Compiler::align_up(sz, align);
                Some((sz, align))
            }),
            Type::Union(body) => body.fields.as_ref().and_then(|fs| {
                let sz_align_vec: Vec<(u32, u32)> = fs
                    .into_iter()
                    .map(|f| {
                        Compiler::get_type_size_and_align_bytes(&f.tp.tp)
                            .unwrap()
                    })
                    .collect();
                let sz =
                    (&sz_align_vec).into_iter().map(|p| p.0).max().unwrap();
                let align =
                    (&sz_align_vec).into_iter().map(|p| p.1).max().unwrap();
                let sz = Compiler::align_up(sz, align);
                Some((sz, align))
            }),
            Type::Pointer(_) => Some((8, 8)),
            Type::Array(elem, elem_cnt_opt) => elem_cnt_opt.map(|elem_cnt| {
                // elem type completeness is checked in
                // unwrap(_abstract)_declarator.
                let (elem_sz, elem_align) =
                    Compiler::get_type_size_and_align_bytes(&elem.tp).unwrap();
                (elem_sz * elem_cnt, elem_align)
            }),
            Type::Function(_, _) => None,
        }
    }

    // returns smallest number that is >=n and mod(align)==0.
    fn align_up(n: u32, align: u32) -> u32 {
        if align == 0 {
            panic!()
        }
        let rem = n & (align - 1);
        if rem == 0 {
            n
        } else {
            (n ^ rem) + align
        }
    }

    // 3.5.4.3: For each parameter declared with function or array type, its
    // type for these comparisons is the one that results from conversion to a
    // pointer type, as in 3.7.1; For each parameter declared with qualified
    // type, its type for these comparisons is the unqualified version of its
    // declared type.
    fn sanitize_param_types(params: &Option<FuncParams>) -> Option<FuncParams> {
        let q = QType::from;
        match params {
            Some(FuncParams::Typed(tps, is_varargs)) => {
                Some(FuncParams::Typed(
                    tps.iter()
                        .map(|tp| TypedFuncParam {
                            is_register: tp.is_register,
                            tp: q(match &tp.tp.tp {
                                Type::Array(elem_tp, _) => {
                                    Type::Pointer(elem_tp.clone())
                                }
                                Type::Function(_, _) => {
                                    Type::Pointer(Box::new(q(tp.tp.tp.clone())))
                                }
                                x => x.clone(),
                            }),
                            name: tp.name.clone(),
                        })
                        .collect(),
                    *is_varargs,
                ))
            }
            x => x.clone(),
        }
    }

    fn qualify_type(
        tqs: &Vec<L<ast::TypeQualifier>>,
        mut qtype: QType,
    ) -> QType {
        for &(q, loc) in tqs {
            use ast::TypeQualifier as TQ;
            let is_const = qtype.is_const && q == TQ::CONST;
            let is_volatile = qtype.is_volatile && q == TQ::VOLATILE;

            // 3.5.3: The same type qualifier shall not appear more than once in
            // the same specifier list or qualifier list, either directly or via
            // one or more typedef s.
            if is_const == qtype.is_const && is_volatile == qtype.is_volatile {
                panic!(
                    "{}: Duplicate '{}' type qualifier",
                    Compiler::format_loc(loc),
                    format!("{:?}", q).to_lowercase()
                );
            }

            qtype.is_const = is_const;
            qtype.is_volatile = is_volatile;
        }
        qtype
    }

    // 3.2.2.1:
    //
    // Except when it is the operand of
    //     the sizeof operator,
    //     the unary & operator,
    //     the ++ operator,
    //     the -- operator,
    // or the left operand of
    //     the . operator or
    //     an assignment operator,
    // an lvalue that does not have array type is converted to the value stored
    // in the designated object (and is no longer an lvalue). If the lvalue has
    // qualified type, the value has the unqualified version of the type of the
    // lvalue; otherwise the value has the type of the lvalue. If the lvalue has
    // an incomplete type and does not have array type, the behavior is
    // undefined.
    //
    // Except when it is the operand of
    //     the sizeof operator or
    //     the unary & operator,
    // or
    //     is a character string literal used to initialize an array of
    //         character type, or
    //     is a wide string literal used to initialize an array with element
    //         type compatible with wchar_t,
    // an lvalue that has type ``array of type'' is converted to an expression
    // that has type ``pointer to type'' that points to the initial member of
    // the array object and is not an lvalue.
    //
    // A function designator is an expression that has function type. Except
    // when it is the operand of
    //     the sizeof operator or
    //     the unary & operator,
    // a function designator with type ``function returning type'' is converted
    // to an expression that has type ``pointer to function returning type''.
    fn convert_lvalue_and_func_designator(
        &mut self,
        tp: QType,
        expr: Option<ConstantOrIrValue>,
        do_deref_lvalue: bool,
        do_arr_to_ptr: bool,
        do_fun_to_ptr: bool,
    ) -> (QType, Option<ConstantOrIrValue>) {
        use ConstantOrIrValue as C;
        match (tp.tp.clone(), expr.clone()) {
            // do_arr_to_ptr
            (Type::Array(t, _), Some(C::IrValue(ir_id, true)))
                if do_arr_to_ptr =>
            {
                let tp = QType::from(Type::Pointer(t));
                (tp, Some(C::IrValue(ir_id, false)))
            }
            (Type::Array(t, _), None) if do_arr_to_ptr => {
                let tp = QType::from(Type::Pointer(t));
                (tp, None)
            }
            (Type::Array(_, _), _) if do_arr_to_ptr => unreachable!(),
            (Type::Array(_, _), _) => (tp, expr),

            // do_fun_to_ptr
            (Type::Function(_, _), Some(C::IrValue(ir_id, _)))
                if do_fun_to_ptr =>
            {
                let tp = QType::from(Type::Pointer(Box::new(tp)));
                // ir_id should already have func ptr type in IR
                (tp, Some(C::IrValue(ir_id, false)))
            }
            (Type::Function(_, _), None) if do_fun_to_ptr => {
                let tp = QType::from(Type::Pointer(Box::new(tp)));
                (tp, None)
            }
            (Type::Function(_, _), _) if do_fun_to_ptr => unreachable!(),
            (Type::Function(_, _), _) => (tp, expr),

            // do_deref_lvalue
            (t @ Type::Struct(_), Some(C::IrValue(ir_id, true)))
            | (t @ Type::Union(_), Some(C::IrValue(ir_id, true)))
                if do_deref_lvalue =>
            {
                (QType::from(t), Some(C::IrValue(ir_id, false)))
            }
            (t, Some(C::IrValue(ir_id, true))) if do_deref_lvalue => {
                let dst_ir_id = self.get_next_ir_id();
                self.c4ir_builder.create_load(
                    dst_ir_id.clone(),
                    ir_id.clone(),
                    &tp,
                );
                self.llvm_builder.create_load(
                    dst_ir_id.clone(),
                    ir_id.clone(),
                    &tp,
                );
                (QType::from(t), Some(C::IrValue(dst_ir_id, false)))
            }
            _ => (tp, expr),
        }
    }

    fn convert_to_ir_value(
        &mut self,
        tp: &QType,
        c: ConstantOrIrValue,
    ) -> ConstantOrIrValue {
        match c {
            ConstantOrIrValue::IrValue(_, _) => c,
            ConstantOrIrValue::Address(_, _) => {
                // This could return lvalue ir values, which breaks a few
                // assumptions in the code
                unimplemented!() // TODO: string literals and struct init
            }
            _ => {
                let ir_id = self.get_next_ir_id();
                self.c4ir_builder.create_constant(ir_id.clone(), &c, tp);
                self.llvm_builder.create_constant(ir_id.clone(), &c, tp);
                ConstantOrIrValue::IrValue(ir_id, false)
            }
        }
    }

    fn get_bitmask_size(&mut self, e: L<&ast::Expr>) -> u8 {
        let sz = self.get_array_size(e);
        if sz > 32 {
            // 3.5.2.1: A bit-field may have type int, unsigned int, or
            // signed int.
            panic!(
                "{}: Bitmask size must be within the [0, 32] range",
                Compiler::format_loc(e.1)
            )
        }
        sz as u8
    }

    fn get_array_size(&mut self, e: L<&ast::Expr>) -> u32 {
        let (tp, r) = self.visit_expr(e, true, false);
        let r = match r {
            None => panic!(
                "{}: Array size is not a constant",
                Compiler::format_loc(e.1)
            ),
            Some(t) => t,
        };

        match tp.tp {
            Type::Void
            | Type::Float
            | Type::Double
            | Type::Struct(_)
            | Type::Union(_)
            | Type::Pointer(_)
            | Type::Array(_, _)
            | Type::Function(_, _) => {
                panic!("{}: Illegal expression type", Compiler::format_loc(e.1))
            }
            _ => (),
        }
        use ConstantOrIrValue as C;
        let sz = match r {
            C::I8(v) => v as u64,
            C::U8(v) => v as u64,
            C::I16(v) => v as u64,
            C::U16(v) => v as u64,
            C::I32(v) => v as u64,
            C::U32(v) => v as u64,
            C::I64(v) => v as u64,
            C::U64(v) => v,
            C::Float(_)
            | C::Double(_)
            | C::Address(_, _)
            | C::IrValue(_, _) => panic!(
                "{}: Array size is not a constant",
                Compiler::format_loc(e.1)
            ),
        };
        if sz > 0x7fff_ffff {
            panic!(
                "{}: Array size must be within the [0, INT_MAX] range",
                Compiler::format_loc(e.1)
            )
        } else {
            sz as u32
        }
    }

    fn do_arithmetic_conversion(
        &mut self,
        tp_x: QType,
        x: Option<ConstantOrIrValue>,
        tp_y: QType,
        y: Option<ConstantOrIrValue>,
    ) -> (Option<ConstantOrIrValue>, Option<ConstantOrIrValue>, QType) {
        if !tp_x.is_arithmetic_type() || !tp_y.is_arithmetic_type() {
            panic!(
                "programming error: do_arithmetic_conversion() only accepts \
                 arithmetic types"
            )
        }

        let return_none = x.is_none() || y.is_none();
        use ConstantOrIrValue as C;
        let get_dummy_value = |tp: &QType| match tp.tp {
            Type::Char => C::I8(0),
            Type::UnsignedChar => C::U8(0),
            Type::Short => C::I16(0),
            Type::UnsignedShort => C::U16(0),
            Type::Int => C::I32(0),
            Type::UnsignedInt => C::U32(0),
            Type::Long => C::I64(0),
            Type::UnsignedLong => C::U64(0),
            Type::Float => C::Float(0.0),
            Type::Double => C::Double(0.0),
            _ => unreachable!(),
        };
        let (tp_x, x) = self
            .convert_lvalue_and_func_designator(tp_x, x, true, false, false);
        let x = x.unwrap_or_else(|| get_dummy_value(&tp_x));
        let (tp_y, y) = self
            .convert_lvalue_and_func_designator(tp_y, y, true, false, false);
        let y = y.unwrap_or_else(|| get_dummy_value(&tp_y));

        let r = match (&x, &y) {
            (C::IrValue(_, true), _) | (_, C::IrValue(_, true)) => {
                unreachable!() // convert_lvalue_and_func_designator
            }
            (C::IrValue(ir_id_x, false), C::IrValue(ir_id_y, false)) => {
                let (new_ir_id_x, new_ir_id_y, new_tp) = self
                    .do_arithmetic_conversion_ir(
                        tp_x,
                        ir_id_x.clone(),
                        tp_y,
                        ir_id_y.clone(),
                    );
                let new_x = C::IrValue(new_ir_id_x, false);
                let new_y = C::IrValue(new_ir_id_y, false);
                (new_x, new_y, new_tp)
            }
            (C::IrValue(_, _), _) => {
                let new_ir_id_y = self.get_next_ir_id();
                self.c4ir_builder.create_constant(
                    new_ir_id_y.clone(),
                    &y,
                    &tp_y,
                );
                self.llvm_builder.create_constant(
                    new_ir_id_y.clone(),
                    &y,
                    &tp_y,
                );
                let (new_x, new_y, new_tp) = self.do_arithmetic_conversion(
                    tp_x,
                    Some(x),
                    tp_y,
                    Some(C::IrValue(new_ir_id_y, false)),
                );
                (new_x.unwrap(), new_y.unwrap(), new_tp)
            }
            (_, C::IrValue(_, _)) => {
                let new_ir_id_x = self.get_next_ir_id();
                self.c4ir_builder.create_constant(
                    new_ir_id_x.clone(),
                    &x,
                    &tp_x,
                );
                self.llvm_builder.create_constant(
                    new_ir_id_x.clone(),
                    &x,
                    &tp_x,
                );
                let (new_x, new_y, new_tp) = self.do_arithmetic_conversion(
                    tp_x,
                    Some(C::IrValue(new_ir_id_x, false)),
                    tp_y,
                    Some(y),
                );
                (new_x.unwrap(), new_y.unwrap(), new_tp)
            }
            (C::Double(_), _) => (
                x,
                C::Double(y.as_constant_double().unwrap()),
                QType::from(Type::Double),
            ),
            (_, C::Double(_)) => (
                C::Double(x.as_constant_double().unwrap()),
                y,
                QType::from(Type::Double),
            ),
            (C::Float(_), _) => (
                x,
                C::Float(y.as_constant_double().unwrap() as f32),
                QType::from(Type::Float),
            ),
            (_, C::Float(_)) => (
                C::Float(x.as_constant_double().unwrap() as f32),
                y,
                QType::from(Type::Float),
            ),
            (C::U64(_), _) => (
                x,
                C::U64(y.as_constant_u64().unwrap()),
                QType::from(Type::UnsignedLong),
            ),
            (_, C::U64(_)) => (
                C::U64(x.as_constant_u64().unwrap()),
                y,
                QType::from(Type::UnsignedLong),
            ),
            (C::I64(_), _) => (
                x,
                C::I64(y.as_constant_u64().unwrap() as i64),
                QType::from(Type::Long),
            ),
            (_, C::I64(_)) => (
                C::I64(x.as_constant_u64().unwrap() as i64),
                y,
                QType::from(Type::Long),
            ),
            (C::U32(_), _) => (
                x,
                C::U32(y.as_constant_u64().unwrap() as u32),
                QType::from(Type::UnsignedInt),
            ),
            (_, C::U32(_)) => (
                C::U32(x.as_constant_u64().unwrap() as u32),
                y,
                QType::from(Type::UnsignedInt),
            ),
            _ => (
                C::I32(x.as_constant_u64().unwrap() as i32),
                C::I32(y.as_constant_u64().unwrap() as i32),
                QType::from(Type::Int),
            ),
        };
        if return_none {
            (None, None, r.2)
        } else {
            (Some(r.0), Some(r.1), r.2)
        }
    }

    // x and y must both be of arithmetic types, and shall not be lvalues.
    fn do_arithmetic_conversion_ir(
        &mut self,
        tp_x: QType,
        ir_id_x: String,
        tp_y: QType,
        ir_id_y: String,
    ) -> (String, String, QType) {
        if !tp_x.is_arithmetic_type() || !tp_y.is_arithmetic_type() {
            panic!(
                "programming error: do_arithmetic_conversion_ir() only accepts \
                 arithmetic types"
            )
        }

        let do_cast = |cc: &mut Compiler,
                       ir_id_old: String,
                       tp_old: &QType,
                       tp_new: Type|
         -> (String, QType) {
            let ir_id_new = cc.get_next_ir_id();
            let tp_new = QType::from(tp_new);
            cc.c4ir_builder.create_cast(
                ir_id_new.clone(),
                &tp_new,
                ir_id_old.clone(),
                tp_old,
            );
            cc.llvm_builder.create_cast(
                ir_id_new.clone(),
                &tp_new,
                ir_id_old.clone(),
                tp_old,
            );
            (ir_id_new, tp_new)
        };

        match (&tp_x.tp, &tp_y.tp) {
            // 3.2.1.5: First, if either operand has type long double, the other
            // operand is converted to long double. Otherwise, if either operand
            // has type double, the other operand is converted to double.
            (Type::Double, Type::Double) => {
                (ir_id_x, ir_id_y, QType::from(Type::Double))
            }
            (Type::Double, _) => {
                let (new_ir_id_y, tp) =
                    do_cast(self, ir_id_y, &tp_y, Type::Double);
                (ir_id_x, new_ir_id_y, tp)
            }
            (_, Type::Double) => {
                let (new_ir_id_x, tp) =
                    do_cast(self, ir_id_x, &tp_x, Type::Double);
                (new_ir_id_x, ir_id_y, tp)
            }
            // 3.2.1.5: Otherwise, if either operand has type float, the other
            // operand is converted to float.
            (Type::Float, Type::Float) => {
                (ir_id_x, ir_id_y, QType::from(Type::Float))
            }
            (Type::Float, _) => {
                let (new_ir_id_y, tp) =
                    do_cast(self, ir_id_y, &tp_y, Type::Float);
                (ir_id_x, new_ir_id_y, tp)
            }
            (_, Type::Float) => {
                let (new_ir_id_x, tp) =
                    do_cast(self, ir_id_x, &tp_x, Type::Float);
                (new_ir_id_x, ir_id_y, tp)
            }
            // 3.2.1.5: Otherwise, the integral promotions are performed on both
            // operands.
            _ => {
                let (ir_id_x, tp_x) =
                    self.do_integral_promotion_ir(ir_id_x, tp_x);
                let (ir_id_y, tp_y) =
                    self.do_integral_promotion_ir(ir_id_y, tp_y);
                match (&tp_x.tp, &tp_y.tp) {
                    // 3.2.1.5: Then the following rules are applied: If either
                    // operand has type unsigned long int, the other operand is
                    // converted to unsigned long int.
                    (Type::UnsignedLong, Type::UnsignedLong) => {
                        (ir_id_x, ir_id_y, QType::from(Type::UnsignedLong))
                    }
                    (Type::UnsignedLong, _) => {
                        let (new_ir_id_y, tp) =
                            do_cast(self, ir_id_y, &tp_y, Type::UnsignedLong);
                        (ir_id_x, new_ir_id_y, tp)
                    }
                    (_, Type::UnsignedLong) => {
                        let (new_ir_id_x, tp) =
                            do_cast(self, ir_id_x, &tp_x, Type::UnsignedLong);
                        (new_ir_id_x, ir_id_y, tp)
                    }
                    // 3.2.1.5: Otherwise, if one operand has type long int and
                    // the other has type unsigned int, if a long int can
                    // represent all values of an unsigned int, the operand of
                    // type unsigned int is converted to long int; if a long int
                    // cannot represent all the values of an unsigned int, both
                    // operands are converted to unsigned long int. Otherwise,
                    // if either operand has type long int, the other operand is
                    // converted to long int.
                    (Type::Long, Type::Long) => {
                        (ir_id_x, ir_id_y, QType::from(Type::Long))
                    }
                    (Type::Long, _) => {
                        let (new_ir_id_y, tp) =
                            do_cast(self, ir_id_y, &tp_y, Type::Long);
                        (ir_id_x, new_ir_id_y, tp)
                    }
                    (_, Type::Long) => {
                        let (new_ir_id_x, tp) =
                            do_cast(self, ir_id_x, &tp_x, Type::Long);
                        (new_ir_id_x, ir_id_y, tp)
                    }
                    // 3.2.1.5: Otherwise, if either operand has type unsigned
                    // int, the other operand is converted to unsigned int.
                    (Type::UnsignedInt, Type::UnsignedInt) => {
                        (ir_id_x, ir_id_y, QType::from(Type::UnsignedInt))
                    }
                    (Type::UnsignedInt, _) => {
                        let (new_ir_id_y, tp) =
                            do_cast(self, ir_id_y, &tp_y, Type::UnsignedInt);
                        (ir_id_x, new_ir_id_y, tp)
                    }
                    (_, Type::UnsignedInt) => {
                        let (new_ir_id_x, tp) =
                            do_cast(self, ir_id_x, &tp_x, Type::UnsignedInt);
                        (new_ir_id_x, ir_id_y, tp)
                    }
                    // 3.2.1.5: Otherwise, both operands have type int.
                    //
                    // since this went through integral promotion, they could
                    // only be int if control flow reaches here.
                    _ => (ir_id_x, ir_id_y, QType::from(Type::Int)),
                }
            }
        }
    }

    fn do_integral_promotion(
        &mut self,
        tp: QType,
        c: Option<ConstantOrIrValue>,
    ) -> (QType, Option<ConstantOrIrValue>) {
        if !tp.is_integral_type() {
            panic!(
                "programming error: cannot do integral promotion on \
                 non-integral types"
            )
        }
        let (tp, c) =
            self.convert_lvalue_and_func_designator(tp, c, true, true, true);
        let int_tp = QType::from(Type::Int);
        use ConstantOrIrValue as C;
        match c {
            None => {
                let tp = match &tp.tp {
                    Type::Char
                    | Type::UnsignedChar
                    | Type::Short
                    | Type::UnsignedShort => Type::Int,
                    _ => tp.tp,
                };
                (QType::from(tp), None)
            }

            Some(C::IrValue(ir_id, false)) => {
                let (ir_id, tp) = self.do_integral_promotion_ir(ir_id, tp);
                (tp, Some(C::IrValue(ir_id, false)))
            }
            Some(C::IrValue(_, true)) => unreachable!(),

            Some(C::I8(x)) => (int_tp, Some(C::I32(x as i32))),
            Some(C::U8(x)) => (int_tp, Some(C::I32(x as i32))),
            Some(C::I16(x)) => (int_tp, Some(C::I32(x as i32))),
            Some(C::U16(x)) => (int_tp, Some(C::I32(x as i32))),

            c => (tp, c),
        }
    }

    // 3.2.1.1: A char, a short int, or an int bit-field, or their signed or
    // unsigned varieties, or an object that has enumeration type, may be used
    // in an expression wherever an int or unsigned int may be used. If an int
    // can represent all values of the original type, the value is converted to
    // an int; otherwise it is converted to an unsigned int.
    fn do_integral_promotion_ir(
        &mut self,
        ir_id: String,
        tp: QType,
    ) -> (String, QType) {
        if !tp.is_integral_type() {
            panic!(
                "programming error: cannot do integral promotion on \
                 non-integral types"
            )
        }
        match &tp.tp {
            Type::Char
            | Type::UnsignedChar
            | Type::Short
            | Type::UnsignedShort => {
                let ir_id_new = self.get_next_ir_id();
                let tp_new = QType::from(Type::Int);
                self.c4ir_builder.create_cast(
                    ir_id_new.clone(),
                    &tp_new,
                    ir_id.clone(),
                    &tp,
                );
                self.llvm_builder.create_cast(
                    ir_id_new.clone(),
                    &tp_new,
                    ir_id.clone(),
                    &tp,
                );
                (ir_id_new, tp_new)
            }
            _ => (ir_id, tp),
        }
    }

    fn format_loc(loc: &ast::Loc) -> String {
        loc.get_levels()
            .get(0)
            .map_or(String::from("<unknown location>"), |r| {
                format!("{}:{}:{}", r.file_name, r.line_begin, r.col_begin)
            })
    }

    fn enter_scope(&mut self) {
        let old_scope = mem::replace(&mut self.current_scope, Scope::new());
        self.current_scope.outer_scope = Box::new(Some(old_scope));
    }

    fn leave_scope(&mut self) {
        let outer_scope =
            mem::replace(&mut self.current_scope.outer_scope, Box::new(None));
        match *outer_scope {
            None => panic!("Cannot leave file scope {:?}", self.current_scope),
            Some(s) => self.current_scope = s,
        }
    }

    fn get_next_uuid(&mut self) -> u32 {
        let r = self.next_uuid;
        self.next_uuid += 1;
        r
    }

    fn get_next_ir_id(&mut self) -> String {
        format!("$.{}", self.get_next_uuid())
    }

    fn create_bb(&mut self) -> String {
        let bb_id = format!("$bb.{}", self.get_next_uuid());
        self.c4ir_builder.create_basic_block(&bb_id);
        self.llvm_builder.create_basic_block(&bb_id);
        bb_id
    }

    fn get_current_bb(&self) -> String {
        let _ = self.c4ir_builder.get_current_basic_block();
        self.llvm_builder.get_current_basic_block()
    }
}

fn main() {
    let input_path: Option<String> = env::args().into_iter().skip(1).next();

    let parse =
        |input| ::protobuf::parse_from_reader::<ast::TranslationUnit>(input);

    let protobuf_result = match input_path {
        None => parse(&mut io::stdin()),
        Some(p) => parse(&mut File::open(p).unwrap()),
    };
    let translation_unit = match protobuf_result {
        Ok(tu) => tu,
        Err(e) => panic!(e.to_string()),
    };
    Compiler::visit(translation_unit);
}
