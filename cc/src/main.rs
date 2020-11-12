use protobuf::{Message, ProtobufEnum};
use std::borrow::Borrow;
use std::cmp::{max, min};
use std::collections::{HashMap, HashSet, VecDeque};
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

    fn is_struct_type(&self) -> bool {
        match self.tp {
            Type::Struct(_) => true,
            _ => false,
        }
    }

    fn is_union_type(&self) -> bool {
        match self.tp {
            Type::Union(_) => true,
            _ => false,
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

    fn is_char_arr(&self) -> bool {
        match &self.tp {
            Type::Array(elem_tp, _) => match &elem_tp.tp {
                Type::Char => true,
                _ => false,
            },
            _ => false,
        }
    }

    fn is_wchar_arr(&self) -> bool {
        match &self.tp {
            Type::Array(elem_tp, _) => match &elem_tp.tp {
                Type::Short => true,
                _ => false,
            },
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
    // StrAddresses may only be used together with pointer or array types.
    //
    // Unlike IrValue, the ir_id of StrAddress could be looked up in
    // Compiler::str_constants and is guaranteed to exist.
    StrAddress(String, i64), // ir_id, offset_bytes
    // Like IrValue, but for variables that have link time address.
    //
    // is_lvalue=True: the value at addr (ir_id + offset_bytes)
    // is_lvalue=False: the value of addr (ir_id + offset_bytes)
    HasAddress(String, i64, bool), // ir_id, offset_bytes, is_lvalue
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
    Struct(VecDeque<Initializer>, u32), // zero_padding_bytes: u32
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

    fn lookup_sue_type_by_uuid(&self, uuid: u32) -> Option<Type> {
        let mut scope: &Scope = &self;
        loop {
            let su_type = scope.sue_tag_names_ns.values().into_iter().find_map(
                |t| match t {
                    SueType::Struct(b) if b.uuid == uuid => {
                        Some(Type::Struct(b.clone()))
                    }
                    SueType::Union(b) if b.uuid == uuid => {
                        Some(Type::Union(b.clone()))
                    }
                    _ => None,
                },
            );
            if su_type.is_some() {
                break su_type;
            }
            match &*scope.outer_scope {
                None => break None,
                Some(outer) => scope = outer,
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

struct StructFieldOffset {
    offset: u32,

    // only meaningful if bit_field_mask != 0; field actual value is:
    //
    // without sign extension (using unsigned shr):
    //
    //   (v & bit_field_mask) >> bit_field_offset
    //
    // or:
    //   v & (bit_field_mask >> bit_field_offset)
    //
    // with sign extension (using arithmetic shr):
    //
    //      (v & bit_field_mask)
    //   << bit_field_rem_bits
    //   >> bit_field_rem_bits
    //   >> bit_field_offset
    bit_field_offset: u8,
    bit_field_mask: u32,
    bit_field_rem_bits: u8,
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

    // allocs should be placed in entry blocks.
    fn set_entry_basic_block(&mut self, bb: &str);

    fn create_definition(
        &mut self,
        is_global: bool,
        name: &str,
        tp: &QType,
        linkage: Linkage,
        init: &Option<Initializer>,
    );

    // global static constant buffer
    fn create_constant_buffer(&mut self, ir_id: &str, buf: Vec<u8>);

    // <ir_id> = <c>
    //
    // local variable initialized with constant literals
    fn create_constant(
        &mut self,
        ir_id: &str,
        c: &ConstantOrIrValue,
        tp: &QType,
    );

    // <dst_ir_id> = load <T>, <T>* <src_ir_id>
    //     where src_tp.tp == Type::Pointer(T)
    fn create_load(&mut self, dst_ir_id: &str, src_ir_id: &str, src_tp: &QType);

    // store <T> <src_ir_id>, <T>* <dst_ir_id>
    fn create_store(&mut self, dst_ir_id: &str, src_ir_id: &str);

    // call void @llvm.memcpy(T* <dst_ir_id>, T* <src_ir_id>, <size>)
    fn create_memcpy(
        &mut self,
        dst_ir_id: &str,
        src_ir_id: &str,
        size: u32,
        align: u32,
    );

    fn create_cast(
        &mut self,
        dst_ir_id: &str,
        dst_tp: &QType,
        src_ir_id: &str,
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
        dst_ir_id: &str,
        op: ast::Expr_Binary_Op,
        is_signed: bool,
        is_fp: bool,
        left_ir_id: &str,
        right_ir_id: &str,
    );

    fn create_cmp_op(
        &mut self,
        dst_ir_id: &str,
        op: ast::Expr_Binary_Op,
        is_signed: bool,
        is_fp: bool,
        left_ir_id: &str,
        right_ir_id: &str,
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

    fn create_va_start(&mut self, ir_id: &str);

    fn create_va_arg(&mut self, dst_ir_id: &str, ir_id: &str, tp: &QType);

    fn create_va_end(&mut self, ir_id: &str);

    fn create_va_copy(&mut self, dst_ir_id: &str, src_ir_id: &str);

    fn create_return_void(&mut self);

    fn create_return(&mut self, ir_id: &str);

    // use "-" to print to stdout.
    fn print_to_file(&mut self, file_name: &str);

    fn write_bitcode_to_file(&mut self, file_name: &str);
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

    fn set_entry_basic_block(&mut self, _bb: &str) {}

    fn create_definition(
        &mut self,
        _is_global: bool,
        _name: &str,
        _tp: &QType,
        _linkage: Linkage,
        _init: &Option<Initializer>,
    ) {
    }

    fn create_constant_buffer(&mut self, _ir_id: &str, _buf: Vec<u8>) {}

    fn create_constant(
        &mut self,
        _ir_id: &str,
        _c: &ConstantOrIrValue,
        _tp: &QType,
    ) {
    }

    fn create_load(
        &mut self,
        _dst_ir_id: &str,
        _src_ir_id: &str,
        _src_tp: &QType,
    ) {
    }

    fn create_store(&mut self, _dst_ir_id: &str, _src_ir_id: &str) {}

    fn create_memcpy(
        &mut self,
        _dst_ir_id: &str,
        _src_ir_id: &str,
        _size: u32,
        _align: u32,
    ) {
    }

    fn create_cast(
        &mut self,
        _dst_ir_id: &str,
        _dst_tp: &QType,
        _src_ir_id: &str,
        _src_tp: &QType,
    ) {
    }

    fn create_zext_i1_to_i32(&mut self, _dst_ir_id: &str, _src_ir_id: &str) {}

    fn create_neg(&mut self, _dst_ir_id: &str, _is_fp: bool, _ir_id: &str) {}

    fn create_not(&mut self, _dst_ir_id: &str, _ir_id: &str) {}

    fn create_bin_op(
        &mut self,
        _dst_ir_id: &str,
        _op: ast::Expr_Binary_Op,
        _is_signed: bool,
        _is_fp: bool,
        _left_ir_id: &str,
        _right_ir_id: &str,
    ) {
    }

    fn create_cmp_op(
        &mut self,
        _dst_ir_id: &str,
        _op: ast::Expr_Binary_Op,
        _is_signed: bool,
        _is_fp: bool,
        _left_ir_id: &str,
        _right_ir_id: &str,
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

    fn create_va_start(&mut self, _ir_id: &str) {}

    fn create_va_arg(&mut self, _dst_ir_id: &str, _ir_id: &str, _tp: &QType) {}

    fn create_va_end(&mut self, _ir_id: &str) {}

    fn create_va_copy(&mut self, _dst_ir_id: &str, _src_ir_id: &str) {}

    fn create_return_void(&mut self) {}

    fn create_return(&mut self, _ir_id: &str) {}

    fn print_to_file(&mut self, _file_name: &str) {}

    fn write_bitcode_to_file(&mut self, _file_name: &str) {}
}

#[cfg(feature = "llvm-sys")]
struct LLVMBuilderImpl {
    context: llvm_sys::prelude::LLVMContextRef,
    module: llvm_sys::prelude::LLVMModuleRef,
    builder: llvm_sys::prelude::LLVMBuilderRef,

    next_uuid: u32,

    current_function: llvm_sys::prelude::LLVMValueRef,
    current_basic_block: String,
    entry_block: String,
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
            let mut r = LLVMBuilderImpl {
                context,
                module,
                builder,
                next_uuid: 1_000_000,
                current_function: ptr::null_mut(),
                current_basic_block: String::new(),
                entry_block: String::new(),
                basic_blocks: HashMap::new(),
                symbol_table: HashMap::new(),
                switch_stack: Vec::new(),
            };
            r.init();
            r
        }
    }

    fn init(&mut self) {
        {
            let fn_tp = {
                let ptr_tp = TypedFuncParam {
                    is_register: false,
                    tp: QType::char_ptr_tp(),
                    name: None,
                };
                Type::Function(
                    Box::new(QType::from(Type::Void)),
                    Some(FuncParams::Typed(vec![ptr_tp], false)),
                )
            };
            let fn_tp_llvm = self.get_llvm_type(&fn_tp);

            let fn_name_c = CString::new("llvm.va_start").unwrap();
            let fn_llvm = unsafe {
                llvm_sys::core::LLVMAddFunction(
                    self.module,
                    fn_name_c.as_ptr(),
                    fn_tp_llvm,
                )
            };
            self.symbol_table
                .insert("llvm.va_start".to_string(), fn_llvm);

            let fn_name_c = CString::new("llvm.va_end").unwrap();
            let fn_llvm = unsafe {
                llvm_sys::core::LLVMAddFunction(
                    self.module,
                    fn_name_c.as_ptr(),
                    fn_tp_llvm,
                )
            };
            self.symbol_table.insert("llvm.va_end".to_string(), fn_llvm);
        }

        {
            let fn_name_c = CString::new("llvm.va_copy").unwrap();
            let fn_tp = {
                let ptr_tp = TypedFuncParam {
                    is_register: false,
                    tp: QType::char_ptr_tp(),
                    name: None,
                };
                Type::Function(
                    Box::new(QType::from(Type::Void)),
                    Some(FuncParams::Typed(
                        vec![ptr_tp.clone(), ptr_tp],
                        false,
                    )),
                )
            };
            let fn_tp_llvm = self.get_llvm_type(&fn_tp);
            let fn_llvm = unsafe {
                llvm_sys::core::LLVMAddFunction(
                    self.module,
                    fn_name_c.as_ptr(),
                    fn_tp_llvm,
                )
            };
            self.symbol_table
                .insert("llvm.va_copy".to_string(), fn_llvm);
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
                        CString::new(format!(".{}", su.uuid)).unwrap();
                    llvm_sys::core::LLVMGetTypeByName(
                        self.module,
                        type_name.as_ptr(),
                    )
                }
                Type::Pointer(tp) => llvm_sys::core::LLVMPointerType(
                    if !tp.is_void() {
                        self.get_llvm_type(&tp.tp)
                    } else {
                        llvm_sys::core::LLVMInt8TypeInContext(self.context)
                    },
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
            C::StrAddress(ir_id, offset_bytes)
            | C::HasAddress(ir_id, offset_bytes, false) => {
                let char_ptr_tp = QType::char_ptr_tp().tp;
                let char_ptr_tp_llvm = self.get_llvm_type(&char_ptr_tp);
                let ptr: llvm_sys::prelude::LLVMValueRef =
                    *self.symbol_table.get(ir_id).unwrap();
                let ptr = unsafe {
                    llvm_sys::core::LLVMConstBitCast(ptr, char_ptr_tp_llvm)
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
                (src, QType::from(char_ptr_tp))
            }
            C::HasAddress(_, _, true) => unreachable!(),
            C::IrValue(_, _) => unreachable!(),
        }
    }

    // TODO: init_tp should be a &QType, not Option. Otherwise nested structs
    // and array of structs won't work when we use LLVM bitcode format IR. Today
    // we workaround this issue by using LLVM .ll format IR.
    fn get_llvm_constant_init(
        &mut self,
        init: &Initializer,
        init_tp: Option<&QType>,
    ) -> llvm_sys::prelude::LLVMValueRef {
        use ConstantOrIrValue as C;
        match init {
            Initializer::Expr(_, C::IrValue(ir_id, false)) => {
                *self.symbol_table.get(ir_id).unwrap()
            }
            Initializer::Expr(_, c)
                if init_tp.map(|t| t.is_pointer()) == Some(true)
                    && c.as_constant_u64() == Some(0) =>
            {
                let init_tp = self.get_llvm_type(&init_tp.unwrap().tp);
                unsafe { llvm_sys::core::LLVMConstNull(init_tp) }
            }
            Initializer::Expr(_, c @ C::HasAddress(_, _, false))
                if init_tp.map(|t| t.is_pointer()) == Some(true) =>
            {
                let pre_cast = self.get_llvm_constant(c).0;
                // get_llvm_constant() does not know tp, so for HasAddress it
                // will always return i8* values, so a cast is needed here
                let init_tp = self.get_llvm_type(&init_tp.unwrap().tp);
                let name_c = CString::new("").unwrap();
                unsafe {
                    llvm_sys::core::LLVMBuildCast(
                        self.builder,
                        llvm_sys::LLVMOpcode::LLVMBitCast,
                        pre_cast,
                        init_tp,
                        name_c.as_ptr(),
                    )
                }
            }
            Initializer::Expr(_, c) => self.get_llvm_constant(c).0,
            Initializer::Struct(inits, _)
                if inits.is_empty() && init_tp.is_some() =>
            {
                let llvm_tp = self.get_llvm_type(&init_tp.unwrap().tp);
                unsafe { llvm_sys::core::LLVMConstNull(llvm_tp) }
            }
            Initializer::Struct(inits, zero_padding_bytes) => {
                let mut vals: Vec<llvm_sys::prelude::LLVMValueRef> = inits
                    .into_iter()
                    .map(|init| {
                        let init_tp: Option<&QType> =
                            match init_tp.map(|t| &t.tp) {
                                // ad-hoc fix for array; probably need the same
                                // for struct/union.
                                Some(Type::Array(t, _)) => Some(t.borrow()),
                                _ => None,
                            };
                        self.get_llvm_constant_init(init, init_tp)
                    })
                    .collect();
                if *zero_padding_bytes > 0 {
                    let zero_padding_tp = self.get_llvm_type(&Type::Array(
                        Box::new(QType::from(Type::Char)),
                        Some(*zero_padding_bytes),
                    ));
                    let zero_padding = unsafe {
                        llvm_sys::core::LLVMConstNull(zero_padding_tp)
                    };
                    vals.push(zero_padding);
                }
                unsafe {
                    if init_tp.is_none() {
                        llvm_sys::core::LLVMConstStruct(
                            vals.as_mut_ptr(),
                            vals.len() as u32,
                            0,
                        )
                    } else if init_tp.map(|t| t.is_array()) == Some(true) {
                        let elem_tp: QType = match &init_tp.unwrap().tp {
                            Type::Array(elem_tp, _) => *elem_tp.clone(),
                            _ => unreachable!(),
                        };
                        llvm_sys::core::LLVMConstArray(
                            self.get_llvm_type(&elem_tp.tp),
                            vals.as_mut_ptr(),
                            vals.len() as u32,
                        )
                    } else {
                        llvm_sys::core::LLVMConstNamedStruct(
                            self.get_llvm_type(&init_tp.unwrap().tp),
                            vals.as_mut_ptr(),
                            vals.len() as u32,
                        )
                    }
                }
            }
        }
    }

    fn get_next_tmp_ir_id(&mut self) -> String {
        let r = self.next_uuid;
        self.next_uuid += 1;
        format!("t.{}", r)
    }

    fn sanitize_basic_blocks(&mut self) {
        for bb in self.basic_blocks.values() {
            unsafe {
                let mut v = llvm_sys::core::LLVMGetFirstInstruction(*bb);
                while !v.is_null()
                    && llvm_sys::core::LLVMIsATerminatorInst(v).is_null()
                {
                    v = llvm_sys::core::LLVMGetNextInstruction(v)
                }
                // v is now the last terminator inst, or null
                if !v.is_null() {
                    v = llvm_sys::core::LLVMGetNextInstruction(v)
                }
                while !v.is_null() {
                    let t = v;
                    v = llvm_sys::core::LLVMGetNextInstruction(v);
                    llvm_sys::core::LLVMInstructionEraseFromParent(t);
                }
            }
        }
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
        is_union: bool,
    ) {
        unsafe {
            let name_cstr = CString::new(name).unwrap();
            let tp = llvm_sys::core::LLVMGetTypeByName(
                self.module,
                name_cstr.as_ptr(),
            );
            let mut element_types: Vec<llvm_sys::prelude::LLVMTypeRef> = fields
                .iter()
                .map(|su_field| {
                    if !is_union && su_field.bit_field_size.is_some() {
                        unreachable!() // struct must be packed first
                    }
                    self.get_llvm_type(&su_field.tp.tp)
                })
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
        let linkage = if linkage == Linkage::INTERNAL {
            Linkage::INTERNAL
        } else {
            Linkage::EXTERNAL
        };
        self.create_definition(true, name, tp, linkage, &None);
        let llvm_func = *self.symbol_table.get(name).unwrap();
        unsafe {
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
        self.sanitize_basic_blocks();
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
        self.current_basic_block = bb.to_string();
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

    fn set_entry_basic_block(&mut self, bb: &str) {
        self.entry_block = bb.to_string();
    }

    fn create_definition(
        &mut self,
        is_global: bool,
        name: &str,
        tp: &QType,
        linkage: Linkage,
        init: &Option<Initializer>,
    ) {
        // C allows global external tentative defs of arrays of unknown size:
        //   int x[];
        // as long as it's redeclared later with a size:
        //   int x[];
        //   int x[10];
        // on seeing the first def, clang implicitly gives it a size of 1. Since
        // this seems to be an undefined behavior, we mimick clang's behavior
        // here but without signaling a warning.
        let tp = match &tp.tp {
            Type::Array(elem_tp, None)
                if is_global && linkage == Linkage::EXTERNAL =>
            {
                QType {
                    is_const: tp.is_const,
                    is_volatile: tp.is_volatile,
                    tp: Type::Array(elem_tp.clone(), Some(1)),
                }
            }
            _ => tp.clone(),
        };

        let name_c = CString::new(name).unwrap();
        let tp_llvm = self.get_llvm_type(&tp.tp);
        let v = unsafe {
            if is_global
                && self.symbol_table.contains_key(name)
                && init.is_none()
            {
                *self.symbol_table.get(name).unwrap()
            } else if is_global {
                if tp.is_function() {
                    llvm_sys::core::LLVMAddFunction(
                        self.module,
                        name_c.as_ptr(),
                        tp_llvm,
                    )
                } else {
                    let new_v = llvm_sys::core::LLVMAddGlobal(
                        self.module,
                        tp_llvm,
                        name_c.as_ptr(),
                    );
                    self.symbol_table.get(name).map(|old_v| {
                        llvm_sys::core::LLVMReplaceAllUsesWith(*old_v, new_v);
                        llvm_sys::core::LLVMDeleteGlobal(*old_v);
                        llvm_sys::core::LLVMSetValueName2(
                            new_v,
                            name_c.as_ptr(),
                            name.len(),
                        );
                    });
                    new_v
                }
            } else {
                let cur_bb = self.get_current_basic_block();
                let entry_bb = self.entry_block.clone();
                self.set_current_basic_block(&entry_bb);
                let v = llvm_sys::core::LLVMBuildAlloca(
                    self.builder,
                    tp_llvm,
                    name_c.as_ptr(),
                );
                self.set_current_basic_block(&cur_bb);
                v
            }
        };
        use llvm_sys::LLVMLinkage as LL;
        let linkage_llvm = match linkage {
            Linkage::EXTERNAL => Some(LL::LLVMExternalLinkage),
            Linkage::INTERNAL => Some(LL::LLVMInternalLinkage),
            Linkage::NONE => None,
        };
        linkage_llvm.map(|ln| unsafe { llvm_sys::core::LLVMSetLinkage(v, ln) });

        // TODO: this does not yet work for:
        //   nested structs;
        //   partially initialized structs;
        //   partially initialized arrays.
        init.as_ref()
            .map(|x| self.get_llvm_constant_init(x, Some(&tp)))
            .map(|value| unsafe {
                if is_global {
                    llvm_sys::core::LLVMSetInitializer(v, value);
                } else {
                    // Looks like store works even for aggregate types. Another
                    // option would be to create a global const and memcpy.
                    llvm_sys::core::LLVMBuildStore(self.builder, value, v);
                }
            });

        self.symbol_table.insert(String::from(name), v);
    }

    fn create_constant_buffer(&mut self, ir_id: &str, buf: Vec<u8>) {
        let ir_id_c = CString::new(ir_id).unwrap();
        let v = unsafe {
            let v = llvm_sys::core::LLVMAddGlobal(
                self.module,
                self.get_llvm_type(&Type::Array(
                    Box::new(QType::from(Type::Char)),
                    Some(buf.len() as u32),
                )),
                ir_id_c.as_ptr(),
            );
            llvm_sys::core::LLVMSetLinkage(
                v,
                llvm_sys::LLVMLinkage::LLVMLinkerPrivateLinkage,
            );
            let s = llvm_sys::core::LLVMConstStringInContext(
                self.context,
                buf.as_ptr() as *const i8,
                buf.len() as u32,
                1,
            );
            llvm_sys::core::LLVMSetInitializer(v, s);
            v
        };
        self.symbol_table.insert(ir_id.to_string(), v);
    }

    fn create_constant(
        &mut self,
        ir_id: &str,
        c: &ConstantOrIrValue,
        tp: &QType,
    ) {
        let (src, src_tp) = self.get_llvm_constant(c);
        let src_ir_id = self.get_next_tmp_ir_id();
        self.symbol_table.insert(src_ir_id.clone(), src);
        self.create_cast(ir_id, tp, &src_ir_id, &src_tp);
    }

    fn create_load(
        &mut self,
        dst_ir_id: &str,
        src_ir_id: &str,
        _src_tp: &QType,
    ) {
        let dst_ir_id_c = CString::new(dst_ir_id).unwrap();
        let src = self.symbol_table.get(src_ir_id).unwrap();
        let dst = unsafe {
            llvm_sys::core::LLVMBuildLoad(
                self.builder,
                *src,
                dst_ir_id_c.as_ptr(),
            )
        };
        self.symbol_table.insert(dst_ir_id.to_string(), dst);
    }

    fn create_store(&mut self, dst_ir_id: &str, src_ir_id: &str) {
        let src = *self.symbol_table.get(src_ir_id).unwrap();
        let dst = *self.symbol_table.get(dst_ir_id).unwrap();
        unsafe {
            llvm_sys::core::LLVMBuildStore(self.builder, src, dst);
        }
    }

    fn create_memcpy(
        &mut self,
        dst_ir_id: &str,
        src_ir_id: &str,
        size: u32,
        align: u32,
    ) {
        let src = self.symbol_table.get(src_ir_id).unwrap();
        let dst = self.symbol_table.get(dst_ir_id).unwrap();
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
        dst_ir_id: &str,
        dst_tp: &QType,
        src_ir_id: &str,
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
            *self.symbol_table.get(src_ir_id).unwrap();
        let dst_tp_llvm = self.get_llvm_type(&dst_tp.tp);
        let dst_ir_id_c = CString::new(dst_ir_id).unwrap();
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
        self.symbol_table.insert(dst_ir_id.to_string(), dst);
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
        dst_ir_id: &str,
        op: ast::Expr_Binary_Op,
        is_signed: bool,
        is_fp: bool,
        left_ir_id: &str,
        right_ir_id: &str,
    ) {
        use ast::Expr_Binary_Op as Op;
        use llvm_sys::LLVMOpcode as O;
        let dst_ir_id_c = CString::new(dst_ir_id).unwrap();
        let left = self.symbol_table.get(left_ir_id).unwrap();
        let right = self.symbol_table.get(right_ir_id).unwrap();
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
        self.symbol_table.insert(dst_ir_id.to_string(), dst);
    }

    fn create_cmp_op(
        &mut self,
        dst_ir_id: &str,
        op: ast::Expr_Binary_Op,
        is_signed: bool,
        is_fp: bool,
        left_ir_id: &str,
        right_ir_id: &str,
    ) {
        let dst_ir_id_c = CString::new(dst_ir_id).unwrap();
        let left = *self.symbol_table.get(left_ir_id).unwrap();
        let right = *self.symbol_table.get(right_ir_id).unwrap();

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
        self.symbol_table.insert(dst_ir_id.to_string(), dst);
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

    fn create_va_start(&mut self, ir_id: &str) {
        let fn_llvm = *self.symbol_table.get("llvm.va_start").unwrap();
        let empty_str_c = CString::new("").unwrap();
        let ap = *self.symbol_table.get(ir_id).unwrap();
        let ap = unsafe {
            llvm_sys::core::LLVMBuildCast(
                self.builder,
                llvm_sys::LLVMOpcode::LLVMBitCast,
                ap,
                self.get_llvm_type(&QType::char_ptr_tp().tp),
                empty_str_c.as_ptr(),
            )
        };
        let mut args = vec![ap];
        unsafe {
            llvm_sys::core::LLVMBuildCall(
                self.builder,
                fn_llvm,
                args.as_mut_ptr(),
                1,
                empty_str_c.as_ptr(),
            )
        };
    }

    fn create_va_arg(&mut self, dst_ir_id: &str, ir_id: &str, tp: &QType) {
        let dst_ir_id_c = CString::new(dst_ir_id.clone()).unwrap();
        let ap = *self.symbol_table.get(ir_id).unwrap();
        let tp_llvm = self.get_llvm_type(&tp.tp);
        let dst = unsafe {
            llvm_sys::core::LLVMBuildVAArg(
                self.builder,
                ap,
                tp_llvm,
                dst_ir_id_c.as_ptr(),
            )
        };
        self.symbol_table.insert(dst_ir_id.to_string(), dst);
    }

    fn create_va_end(&mut self, ir_id: &str) {
        let fn_llvm = *self.symbol_table.get("llvm.va_end").unwrap();
        let empty_str_c = CString::new("").unwrap();
        let ap = *self.symbol_table.get(ir_id).unwrap();
        let ap = unsafe {
            llvm_sys::core::LLVMBuildCast(
                self.builder,
                llvm_sys::LLVMOpcode::LLVMBitCast,
                ap,
                self.get_llvm_type(&QType::char_ptr_tp().tp),
                empty_str_c.as_ptr(),
            )
        };
        let mut args = vec![ap];
        unsafe {
            llvm_sys::core::LLVMBuildCall(
                self.builder,
                fn_llvm,
                args.as_mut_ptr(),
                1,
                empty_str_c.as_ptr(),
            )
        };
    }

    fn create_va_copy(&mut self, dst_ir_id: &str, src_ir_id: &str) {
        let fn_llvm = *self.symbol_table.get("llvm.va_copy").unwrap();
        let empty_str_c = CString::new("").unwrap();
        let dst_ap = *self.symbol_table.get(dst_ir_id).unwrap();
        let src_ap = *self.symbol_table.get(src_ir_id).unwrap();
        let dst_ap = unsafe {
            llvm_sys::core::LLVMBuildCast(
                self.builder,
                llvm_sys::LLVMOpcode::LLVMBitCast,
                dst_ap,
                self.get_llvm_type(&QType::char_ptr_tp().tp),
                empty_str_c.as_ptr(),
            )
        };
        let src_ap = unsafe {
            llvm_sys::core::LLVMBuildCast(
                self.builder,
                llvm_sys::LLVMOpcode::LLVMBitCast,
                src_ap,
                self.get_llvm_type(&QType::char_ptr_tp().tp),
                empty_str_c.as_ptr(),
            )
        };
        let mut args = vec![dst_ap, src_ap];
        unsafe {
            llvm_sys::core::LLVMBuildCall(
                self.builder,
                fn_llvm,
                args.as_mut_ptr(),
                2,
                empty_str_c.as_ptr(),
            )
        };
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
        self.sanitize_basic_blocks();

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

    fn write_bitcode_to_file(&mut self, file_name: &str) {
        self.sanitize_basic_blocks();

        let file_name_c = CString::new(file_name).unwrap();
        let r = unsafe {
            llvm_sys::bit_writer::LLVMWriteBitcodeToFile(
                self.module,
                file_name_c.as_ptr(),
            )
        };
        if r != 0 {
            panic!("Error code returned by LLVM backend: {}", r)
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
    str_constants: HashMap<String, Vec<u8>>,
    // value: ir_id
    has_link_time_addr: HashSet<String>,

    c4ir_builder: C4IRBuilder,
    llvm_builder: LLVMBuilder,
}

type L<'a, T> = (T, &'a ast::Loc);

impl Compiler<'_> {
    fn visit(tu: ast::TranslationUnit, pretty_print_ir: bool) {
        let mut cc = Compiler {
            translation_unit: &tu,
            current_scope: Scope::new(),
            next_uuid: 1_000,
            str_constants: HashMap::new(),
            has_link_time_addr: HashSet::new(),
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

        if pretty_print_ir {
            cc.llvm_builder.print_to_file("-");
        } else {
            cc.llvm_builder.write_bitcode_to_file("-");
        }
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
            // `i32* %.1004` into current_scope.
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
        self.has_link_time_addr.insert(fname.clone());

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

        let entry_bb_id = format!("entry.{}", self.get_next_uuid());
        self.c4ir_builder.create_basic_block(&entry_bb_id);
        self.llvm_builder.create_basic_block(&entry_bb_id);
        self.c4ir_builder.set_current_basic_block(&entry_bb_id);
        self.llvm_builder.set_current_basic_block(&entry_bb_id);
        self.c4ir_builder.set_entry_basic_block(&entry_bb_id);
        self.llvm_builder.set_entry_basic_block(&entry_bb_id);

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
                    self.c4ir_builder.create_store(&new_ir_id, &ir_id);
                    self.llvm_builder.create_store(&new_ir_id, &ir_id);
                    *ir_id = new_ir_id;
                }
                _ => unreachable!(),
            }
        }

        let body_bb_id = self.create_bb();
        self.c4ir_builder.set_current_basic_block(&body_bb_id);
        self.llvm_builder.set_current_basic_block(&body_bb_id);

        let rtp = match &ftp.tp {
            Type::Function(rtp, _) => *rtp.clone(),
            _ => unreachable!(),
        };
        let mut func_def_ctx = FuncDefCtx {
            func_name: fname,
            return_type: rtp.clone(),
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
        // handles funcs without explicit returns, e.g.:
        //
        // void f1() {}
        // int f2() {}
        // int f3() {
        //   goto END;
        // END:
        //   (void) 999;
        // }
        if rtp.is_void() {
            self.c4ir_builder.create_return_void();
            self.llvm_builder.create_return_void();
        } else {
            let alloc_ir_id = self.get_next_ir_id();
            self.c4ir_builder.create_definition(
                false,
                &alloc_ir_id,
                &rtp,
                Linkage::NONE,
                &None,
            );
            self.llvm_builder.create_definition(
                false,
                &alloc_ir_id,
                &rtp,
                Linkage::NONE,
                &None,
            );
            let ptr_tp = QType::ptr_tp(rtp);
            let r_ir_id = self.get_next_ir_id();
            self.c4ir_builder
                .create_load(&r_ir_id, &alloc_ir_id, &ptr_tp);
            self.llvm_builder
                .create_load(&r_ir_id, &alloc_ir_id, &ptr_tp);
            self.c4ir_builder.create_return(&r_ir_id);
            self.llvm_builder.create_return(&r_ir_id);
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

        self.c4ir_builder.set_current_basic_block(&entry_bb_id);
        self.llvm_builder.set_current_basic_block(&entry_bb_id);
        self.c4ir_builder.create_br(&body_bb_id);
        self.llvm_builder.create_br(&body_bb_id);

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
                let is_global =
                    linkage != Linkage::NONE || scs == Some(SCS::STATIC);

                if is_global {
                    self.has_link_time_addr.insert(ir_id.clone());
                }

                let mut init = if id.init_idx == 0 {
                    // 3.7.2: tentative definition
                    // 3.5.7: If an object that has static storage duration is
                    // not initialized explicitly, it is initialized implicitly
                    // as if every member that has arithmetic type were assigned
                    // 0 and every member that has pointer type were assigned a
                    // null pointer constant.
                    if ((self.current_scope.is_file_scope()
                        && (scs == None || scs == Some(SCS::STATIC)))
                        || (!self.current_scope.is_file_scope() && is_global))
                        && !qtype.is_function()
                        && !is_defined
                    {
                        let zero = if qtype.is_scalar_type() {
                            Initializer::Expr(
                                QType::from(Type::Int),
                                ConstantOrIrValue::I32(0),
                            )
                        } else {
                            Initializer::Struct(VecDeque::new(), 0)
                        };
                        Some(zero)
                    } else {
                        Option::None
                    }
                } else if linkage != Linkage::NONE
                    && !self.current_scope.is_file_scope()
                {
                    // 3.5.7: If the declaration of an identifier has block
                    // scope, and the identifier has external or internal
                    // linkage, there shall be no initializer for the
                    // identifier.
                    panic!(
                        "{}: Unexpected initializer for variable {}",
                        Compiler::format_loc(id.get_d_loc()),
                        name
                    )
                } else if is_defined {
                    // covers global redefinitions, e.g.:
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
                        name.clone(),
                        OrdinaryIdRef::ObjFnRef(
                            ir_id.clone(),
                            qtype.clone(),
                            linkage,
                            true, // is_defined
                        ),
                    );
                    // 3.5.7: All the expressions in an initializer for an
                    // object that has static storage duration or in an
                    // initializer list for an object that has aggregate or
                    // union type shall be constant expressions.
                    let emit_ir = !is_global && qtype.is_scalar_type();
                    Some(self.visit_initializer(id.init_idx, emit_ir))
                };

                let init = init.as_mut().map(|init| {
                    let emit_ir = !is_global && qtype.is_scalar_type();
                    let r = self
                        .sanitize_initializer(
                            &qtype,
                            init,
                            id.get_d_loc(),
                            emit_ir,
                        )
                        .unwrap_or_else(|err| err.panic());
                    // 3.5.7: There shall be no more initializers in an
                    // initializer list than there are objects to be
                    // initialized.
                    match init {
                        Initializer::Struct(xs, _) if !xs.is_empty() => {
                            panic!(
                                "{}: Redundant initializer element",
                                Compiler::format_loc(id.get_init_loc())
                            );
                        }
                        _ => (),
                    }

                    r
                });

                // 3.5.7: If an array of unknown size is initialized, its size
                // is determined by the number of initializers provided for its
                // members. At the end of its initializer list, the array no
                // longer has incomplete type.
                let qtype = match (&qtype.tp, &init) {
                    (
                        Type::Array(elem_tp, None),
                        Some(Initializer::Struct(inits, 0)),
                    ) if id.init_idx != 0 => QType {
                        is_const: qtype.is_const,
                        is_volatile: qtype.is_volatile,
                        tp: Type::Array(
                            elem_tp.clone(),
                            Some(inits.len() as u32),
                        ),
                    },
                    _ => qtype,
                };
                match self.current_scope.ordinary_ids_ns.get_mut(&name).unwrap()
                {
                    OrdinaryIdRef::ObjFnRef(_, tp, _, _) => {
                        *tp = qtype.clone();
                    }
                    _ => unreachable!(),
                }

                // 3.5: If an identifier for an object is declared with no
                // linkage, the type for the object shall be complete by the end
                // of its declarator, or by the end of its init-declarator if it
                // has an initializer.
                if linkage == Linkage::NONE
                    && Compiler::get_type_size_and_align_bytes(&qtype.tp)
                        .is_none()
                {
                    panic!(
                        "{}: Incomplete type for variable {}",
                        Compiler::format_loc(id.get_init_loc()),
                        name
                    )
                }

                // 3.7.2: A declaration of an identifier for an object that has
                // file scope without an initializer, and without a
                // storage-class specifier or with the storage-class specifier
                // static, constitutes a tentative definition.
                //
                // 3.7.2: If the declaration of an identifier for an object is a
                // tentative definition and has internal linkage, the declared
                // type shall not be an incomplete type.
                let is_tentative = !qtype.is_function()
                    && self.current_scope.is_file_scope()
                    && id.init_idx == 0
                    && (scs.is_none() || scs == Some(SCS::STATIC));
                if is_tentative
                    && linkage == Linkage::INTERNAL
                    && Compiler::get_type_size_and_align_bytes(&qtype.tp)
                        .is_none()
                {
                    panic!(
                        "{}: Incomplete type for variable {}",
                        Compiler::format_loc(id.get_d_loc()),
                        name
                    )
                }

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

    fn visit_initializer(
        &mut self,
        init_idx: i32,
        emit_ir: bool,
    ) -> Initializer {
        let init = &self.translation_unit.initializers[init_idx as usize];
        match &init.init {
            Some(ast::Initializer_oneof_init::expr(expr)) => {
                let e = &self.translation_unit.exprs[expr.e as usize];
                let (qtype, result) =
                    self.visit_expr((e, expr.get_e_loc()), true, emit_ir);
                let (qtype, result) = self.convert_lvalue_and_func_designator(
                    qtype, result, true, true, true, emit_ir,
                );
                match result {
                    None => panic!(
                        "{}: Initializer shall be a constant expression",
                        Compiler::format_loc(expr.get_e_loc())
                    ),
                    Some(r) => Initializer::Expr(qtype, r),
                }
            }
            Some(ast::Initializer_oneof_init::field_struct(st)) => {
                Initializer::Struct(
                    st.inits
                        .iter()
                        .map(|idx| self.visit_initializer(*idx, emit_ir))
                        .collect(),
                    0,
                )
            }
            None => {
                panic!("Invalid AST input: initializer #{} is empty", init_idx)
            }
        }
    }

    fn sanitize_initializer(
        &mut self,
        qtype: &QType,
        init: &mut Initializer,
        loc: &ast::Loc,
        emit_ir: bool,
    ) -> R<Initializer> {
        // 3.5.7: The type of the entity to be initialized shall be an object
        // type or an array of unknown size.
        if Compiler::get_type_size_and_align_bytes(&qtype.tp).is_none()
            && !qtype.is_array()
        {
            let entity_type_err_msg =
                "Complete object type or array of unknown size expected";
            c4_fail!(loc, entity_type_err_msg)
        }

        // 3.5.7 constraints + semantics:

        // * The initializer for a scalar shall be a single expression,
        //   optionally enclosed in braces. The initial value of the object is
        //   that of the expression; the same type constraints and conversions
        //   as for simple assignment apply.
        if qtype.is_scalar_type() {
            let (tp, c) = match init {
                Initializer::Expr(tp, c) => (tp, c),
                Initializer::Struct(inits, _) => {
                    let single_expr_err_msg =
                        "Initializer for scalar must be a single expression";
                    if inits.len() != 1 {
                        c4_fail!(loc, single_expr_err_msg)
                    }
                    match inits.get_mut(0).unwrap() {
                        Initializer::Expr(tp, c) => (tp, c),
                        _ => c4_fail!(loc, single_expr_err_msg),
                    }
                }
            };
            Compiler::check_types_for_assign(qtype, &tp, &Some(c.clone()), loc);

            let (tp, c) = self.cast_expression(
                tp.clone(),
                Some(c.clone()),
                qtype.clone(),
                loc,
                emit_ir,
            );
            if c.is_none() {
                c4_fail!(loc, "Initializer is not a compile time constant")
            }
            return Ok(Initializer::Expr(tp, c.unwrap()));
        }

        // - The initializer for a structure or union object that has automatic
        //   storage duration either shall be an initializer list as described
        //   below, or shall be a single expression that has compatible
        //   structure or union type. In the latter case, the initial value of
        //   the object is that of the expression.
        //
        // in other words, cases like this are supported:
        //
        //   void f() {
        //     struct S s1;
        //     struct S s2 = s1;
        //   }
        //
        // - An array of character type may be initialized by a character string
        //   literal, optionally enclosed in braces. Successive characters of
        //   the character string literal (including the terminating null
        //   character if there is room or if the array is of unknown size)
        //   initialize the members of the array.
        //
        // (similarly for wchar_t / wide string literlas)
        let mut str_lit_inits: Option<VecDeque<Initializer>> = None;
        match init {
            Initializer::Expr(tp, _) if !qtype.is_array() => {
                if Compiler::try_get_composite_type(qtype, tp, loc).is_ok() {
                    return Ok(init.clone());
                }
                c4_fail!(loc, "Incompatible initializer type")
            }
            Initializer::Expr(_, c) => {
                if !qtype.is_char_arr() && !qtype.is_wchar_arr() {
                    c4_fail!(loc, "Brace-enclosed initializer expected")
                }
                // C89 spec explicitly states initializers with excess elements,
                // e.g.:
                //   char s[2] = {'h', 'e', 'l', 'l', 'o', \0};
                // are disallowed, but string literal initializers, e.g.:
                //   char s[2] = "hello";
                // are not mentioned. Here we silently truncate in the second
                // case but fail in the first case.
                let arr_len = match &qtype.tp {
                    Type::Array(_, len) => *len,
                    _ => None,
                };

                let str_ir_id = match c {
                    ConstantOrIrValue::StrAddress(ir_id, 0) => ir_id,
                    _ => c4_fail!(
                        loc,
                        "Brace-enclosed initializer or string literal expected"
                    ),
                };
                let str_buf = self.str_constants.get(str_ir_id).unwrap();
                str_lit_inits = if qtype.is_char_arr() {
                    let mut inits: VecDeque<Initializer> = str_buf
                        .into_iter()
                        .map(|c| ConstantOrIrValue::I8(*c as i8))
                        .map(|c| Initializer::Expr(QType::from(Type::Char), c))
                        .take(arr_len.unwrap_or(u32::MAX) as usize)
                        .collect();
                    // seems like this is also implicitly allowed:
                    //   char s[10] = "hi";
                    arr_len.map(|len| {
                        for _ in inits.len()..(len as usize) {
                            inits.push_back(Initializer::Expr(
                                QType::from(Type::Char),
                                ConstantOrIrValue::I8(0),
                            ));
                        }
                    });
                    Some(inits)
                } else {
                    // qtype.is_wchar_arr() == true
                    let mut inits: VecDeque<Initializer> = VecDeque::new();
                    for i in (0..str_buf.len()).step_by(2) {
                        // assuming little-endian
                        let c =
                            str_buf[i] as i16 | ((str_buf[i + 1] as i16) << 8);
                        inits.push_back(Initializer::Expr(
                            QType::from(Type::Short),
                            ConstantOrIrValue::I16(c),
                        ))
                    }
                    arr_len.map(|len| inits.truncate(len as usize));
                    arr_len.map(|len| {
                        for _ in inits.len()..(len as usize) {
                            inits.push_back(Initializer::Expr(
                                QType::from(Type::Short),
                                ConstantOrIrValue::I16(0),
                            ));
                        }
                    });
                    Some(inits)
                };
            }
            Initializer::Struct(_, 0) => (),
            Initializer::Struct(_, _) => unreachable!(),
        };

        // After this point `init` must be braced.
        // i.e. no more Initializer::Expr(...)

        fn get_inits<'a, 'b: 'a>(
            str_lit_inits: &'a mut Option<VecDeque<Initializer>>,
            init: &'b mut Initializer,
        ) -> &'a mut VecDeque<Initializer> {
            if str_lit_inits.is_some() {
                return str_lit_inits.as_mut().unwrap();
            }
            match init {
                Initializer::Struct(x, 0) => x,
                _ => unreachable!(),
            }
        }
        macro_rules! get_inits {
            () => {
                get_inits(&mut str_lit_inits, init)
            };
        }

        match &qtype.tp {
            // - A brace-enclosed initializer for a union object initializes the
            //   member that appears first in the declaration list of the union
            //   type.
            Type::Union(su_type) => match &su_type.fields {
                Some(fs) if fs.len() > 0 => {
                    let union_sz =
                        Compiler::get_type_size_and_align_bytes(&qtype.tp)
                            .unwrap()
                            .0;
                    let field_sz =
                        Compiler::get_type_size_and_align_bytes(&fs[0].tp.tp)
                            .unwrap()
                            .0;
                    let zero_padding_bytes = union_sz - field_sz;

                    if get_inits!().len() != 1 {
                        c4_fail!(loc, "Redundant initializer element")
                    }
                    let init = get_inits!().get_mut(0).unwrap();

                    let init = self
                        .sanitize_initializer(&fs[0].tp, init, loc, emit_ir)?;
                    let init = match init {
                        Initializer::Expr(_, _) => Initializer::Struct(
                            VecDeque::from(vec![init]),
                            zero_padding_bytes,
                        ),
                        Initializer::Struct(inits, padding) => {
                            Initializer::Struct(
                                inits,
                                padding + zero_padding_bytes,
                            )
                        }
                    };
                    Ok(init)
                }
                None => c4_fail!(loc, "Incomplete union type"),
                _ => unreachable!(),
            },
            Type::Array(elem_tp, arr_len) => {
                let arr_len = arr_len.unwrap_or(get_inits!().len() as u32);
                let inits: R<VecDeque<Initializer>> = (0..arr_len)
                    .flat_map(|_| get_inits!().pop_front())
                    .map(|mut init| {
                        self.sanitize_initializer(
                            elem_tp.as_ref(),
                            &mut init,
                            loc,
                            emit_ir,
                        )
                    })
                    .collect();
                let inits = inits?;
                let padding = if arr_len > inits.len() as u32 {
                    let elem_sz =
                        Compiler::get_type_size_and_align_bytes(&elem_tp.tp)
                            .unwrap()
                            .0;
                    (arr_len - inits.len() as u32) * elem_sz
                } else {
                    0
                };
                Ok(Initializer::Struct(inits, padding))
            }
            Type::Struct(su_type) => match &su_type.fields {
                None => c4_fail!(loc, "Incomplete struct type"),
                Some(fs) if fs.len() == 0 => unreachable!(),
                Some(fs) => {
                    let mut cur_byte: u8 = 0;
                    let mut cur_byte_rem_bits = 0;
                    // cur_byte_rem_bits == 0 implies cur_bf_rem_bytes == 0

                    let mut cur_bf_rem_bytes = 0;
                    let mut struct_rem_bytes =
                        Compiler::get_type_size_and_align_bytes(&qtype.tp)
                            .unwrap()
                            .0;

                    let mut new_inits: VecDeque<Initializer> = VecDeque::new();

                    for field in fs {
                        // when no more initializers: zero-init rest fields
                        if get_inits!().is_empty() {
                            // if previous bit field is not finished yet:
                            // zero-init remaining bits
                            if cur_byte_rem_bits != 0 {
                                cur_byte <<= cur_bf_rem_bytes;
                                new_inits.push_back(Initializer::Expr(
                                    QType::from(Type::Char),
                                    ConstantOrIrValue::U8(cur_byte),
                                ));
                                struct_rem_bytes -= 1;
                            }

                            break;
                        }

                        // * All unnamed structure or union members are ignored
                        //   during initialization.
                        let inner_init = if field.name.is_none()
                            || field.bit_field_size == Some(0)
                        {
                            None
                        } else {
                            get_inits!().pop_front() // must be non-empty
                        };

                        // flush current byte if needed
                        let flush_cur_byte = cur_byte_rem_bits != 0
                            && (field.bit_field_size.is_none()
                                || field.bit_field_size == Some(0)
                                || field.bit_field_size.unwrap()
                                    > cur_byte_rem_bits + cur_bf_rem_bytes * 8);
                        if field.bit_field_size == Some(0) || flush_cur_byte {
                            // flush cur_byte
                            if cur_byte_rem_bits != 0 {
                                new_inits.push_back(Initializer::Expr(
                                    QType::from(Type::UnsignedChar),
                                    ConstantOrIrValue::U8(cur_byte),
                                ));
                                for _ in 0..cur_bf_rem_bytes {
                                    new_inits.push_back(Initializer::Expr(
                                        QType::from(Type::UnsignedChar),
                                        ConstantOrIrValue::U8(0),
                                    ));
                                }
                                cur_byte = 0;
                                cur_byte_rem_bits = 0;
                                struct_rem_bytes -= cur_bf_rem_bytes as u32 + 1;
                                cur_bf_rem_bytes = 0;
                            }
                        }

                        if cur_byte_rem_bits == 0 {
                            // insert padding to align `field`
                            let struct_bytes =
                                Compiler::get_type_size_and_align_bytes(
                                    &qtype.tp,
                                )
                                .unwrap()
                                .0;
                            let align =
                                Compiler::get_type_size_and_align_bytes(
                                    &field.tp.tp,
                                )
                                .unwrap()
                                .1;
                            while (struct_bytes - struct_rem_bytes) % align != 0
                            {
                                new_inits.push_back(Initializer::Expr(
                                    QType::from(Type::UnsignedChar),
                                    ConstantOrIrValue::U8(0),
                                ));
                                struct_rem_bytes -= 1;
                            }
                        }

                        match field.bit_field_size {
                            // bit field size 0 is just used to indicate bit
                            // field packing should stop; it does not consume
                            // initializers
                            Some(0) => continue,
                            // continue packing into previous bit field if there
                            // is still space
                            Some(mut sz) => {
                                let mut n = {
                                    use ConstantOrIrValue as C;
                                    let mut init = inner_init.unwrap_or(
                                        Initializer::Expr(
                                            QType::from(Type::Int),
                                            C::I32(0),
                                        ),
                                    );
                                    let init = self.sanitize_initializer(
                                        &field.tp, &mut init, loc, emit_ir,
                                    )?;
                                    let c = match init {
                                        Initializer::Expr(_, c) => c,
                                        _ => unreachable!(),
                                    };
                                    match c {
                                        C::I32(n) => n as u32,
                                        C::U32(n) => n,
                                        C::IrValue(_, _) => c4_fail!(
                                            loc,
                                            "Compile time constant expected"
                                        ),
                                        _ => unreachable!(),
                                    }
                                };

                                if cur_bf_rem_bytes == 0 {
                                    cur_byte = 0;
                                    cur_byte_rem_bits = 8;
                                    // bit fields must be i32 or u32
                                    cur_bf_rem_bytes = 3; // 4 - 1
                                }

                                // now pack `sz` bits of `n` into remaining
                                // space of the current bit field
                                while sz > 0 {
                                    let p1_n_bits = min(sz, cur_byte_rem_bits);
                                    cur_byte |= (n as u8) << (8 - p1_n_bits)
                                        >> (8 - p1_n_bits)
                                        << (8 - cur_byte_rem_bits);
                                    cur_byte_rem_bits -= p1_n_bits;
                                    sz -= p1_n_bits;
                                    n >>= p1_n_bits;

                                    if cur_byte_rem_bits == 0 {
                                        new_inits.push_back(Initializer::Expr(
                                            QType::from(Type::UnsignedChar),
                                            ConstantOrIrValue::U8(cur_byte),
                                        ));
                                        struct_rem_bytes -= 1;
                                        cur_byte = 0;

                                        if cur_bf_rem_bytes > 0 {
                                            // should never underflow
                                            cur_bf_rem_bytes -= 1;
                                            cur_byte_rem_bits = 8;
                                        }
                                    }
                                }
                            }
                            // field has no bf size: proceed normally
                            None => {
                                let field_sz =
                                    Compiler::get_type_size_and_align_bytes(
                                        &field.tp.tp,
                                    )
                                    .unwrap()
                                    .0;

                                let inner_init = inner_init.unwrap();
                                match (field.tp.is_scalar_type(), inner_init) {
                                    (false, x @ Initializer::Expr(_, _)) => {
                                        get_inits!().push_front(x);

                                        let new_init = self
                                            .sanitize_initializer(
                                                &field.tp, init, loc, emit_ir,
                                            )?;
                                        new_inits.push_back(new_init);
                                        struct_rem_bytes -= field_sz;
                                    }
                                    (_, mut inner_init) => {
                                        let new_init = self
                                            .sanitize_initializer(
                                                &field.tp,
                                                &mut inner_init,
                                                loc,
                                                emit_ir,
                                            )?;
                                        new_inits.push_back(new_init);
                                        struct_rem_bytes -= field_sz;
                                    }
                                }
                            }
                        }
                    }

                    if cur_byte_rem_bits > 0 {
                        new_inits.push_back(Initializer::Expr(
                            QType::from(Type::UnsignedChar),
                            ConstantOrIrValue::U8(cur_byte),
                        ));
                        struct_rem_bytes -= 1;
                    }

                    // do not check for redundant initializers here; remaining
                    // initializers may be for following fields of the outer
                    // struct
                    Ok(Initializer::Struct(new_inits, struct_rem_bytes))
                }
            },
            _ => unreachable!(),
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
                        if self.has_link_time_addr.contains(ir_id) {
                            let v = ConstantOrIrValue::HasAddress(
                                ir_id.clone(),
                                0,
                                true,
                            );
                            (tp.clone(), Some(v))
                        } else if !emit_ir {
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
                let mut buf = str.clone();
                buf.push(0);
                let len = buf.len() as u32;

                let ir_id = self.get_next_ir_id();
                self.str_constants.insert(ir_id.clone(), buf.clone());

                self.c4ir_builder
                    .create_constant_buffer(&ir_id, buf.clone());
                self.llvm_builder.create_constant_buffer(&ir_id, buf);

                let tp = QType::from(Type::Array(
                    Box::new(QType::from(Type::Char)),
                    Some(len),
                ));
                (tp, Some(ConstantOrIrValue::StrAddress(ir_id, 0)))
            }
            ast::Expr_oneof_e::wide_string(ws) => {
                let mut buf: Vec<u8> = ws.clone();
                buf.push(0);
                buf.push(0);
                let len = buf.len() as u32;

                let ir_id = self.get_next_ir_id();
                self.str_constants.insert(ir_id.clone(), buf.clone());

                self.c4ir_builder
                    .create_constant_buffer(&ir_id, buf.clone());
                self.llvm_builder.create_constant_buffer(&ir_id, buf);

                let tp = QType::from(Type::Array(
                    Box::new(QType::from(Type::Short)),
                    Some(len),
                ));
                (tp, Some(ConstantOrIrValue::StrAddress(ir_id, 0)))
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
                    self.visit_logical_binary_op(
                        left,
                        right,
                        binary.op,
                        fold_constant,
                        emit_ir,
                    )
                } else if binary.op == ast::Expr_Binary_Op::ASSIGN {
                    self.visit_assign_expr(left, right, fold_constant, emit_ir)
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
                .unwrap_or_else(|e| e.panic())
            }
            ast::Expr_oneof_e::builtin_offsetof(offsetof) => {
                let tp = self.visit_type_name((
                    offsetof.get_tp(),
                    offsetof.get_tp_loc(),
                ));
                let offset = match Compiler::get_struct_layout(
                    &tp.tp,
                    Some(offsetof.get_field()),
                )
                .3
                {
                    Some((_, offset)) if offset.bit_field_mask == 0 => offset,
                    Some(_) => panic!(
                        "{}: Cannot get offset for bit-field '{}'",
                        Compiler::format_loc(offsetof.get_field_loc()),
                        offsetof.get_field()
                    ),
                    None => panic!(
                        "{}: Field '{}' not found",
                        Compiler::format_loc(offsetof.get_field_loc()),
                        offsetof.get_field()
                    ),
                };
                // 4.1.5: and offsetof... expends to an integral constant
                // expression that has type size_t
                (
                    QType::from(Type::UnsignedLong),
                    Some(ConstantOrIrValue::U64(offset.offset as u64)),
                )
            }
            ast::Expr_oneof_e::builtin_va_start(va_start) => {
                if !emit_ir {
                    panic!(
                        "{}: Not a const expression",
                        Compiler::format_loc(e.1)
                    );
                }
                let ap = (
                    &self.translation_unit.exprs[va_start.ap_idx as usize],
                    va_start.get_ap_loc(),
                );
                let (tp_ap, ap) = self.visit_expr(ap, fold_constant, emit_ir);
                let ap = self.convert_to_ir_value(&tp_ap, ap.unwrap());
                let ap_ir_id = match ap {
                    ConstantOrIrValue::IrValue(ir_id, _) => ir_id,
                    _ => unreachable!(),
                };
                self.c4ir_builder.create_va_start(&ap_ir_id);
                self.llvm_builder.create_va_start(&ap_ir_id);
                (QType::from(Type::Void), Some(ConstantOrIrValue::I32(0)))
            }
            ast::Expr_oneof_e::builtin_va_arg(va_arg) => {
                if !emit_ir {
                    panic!(
                        "{}: Not a const expression",
                        Compiler::format_loc(e.1)
                    );
                }
                let ap = (
                    &self.translation_unit.exprs[va_arg.ap_idx as usize],
                    va_arg.get_ap_loc(),
                );
                let (tp_ap, ap) = self.visit_expr(ap, fold_constant, emit_ir);
                let ap = self.convert_to_ir_value(&tp_ap, ap.unwrap());
                let ap_ir_id = match ap {
                    ConstantOrIrValue::IrValue(ir_id, _) => ir_id,
                    _ => unreachable!(),
                };
                let tp_arg = self
                    .visit_type_name((va_arg.get_tp(), va_arg.get_tp_loc()));
                let dst_ir_id = self.get_next_ir_id();
                self.c4ir_builder
                    .create_va_arg(&dst_ir_id, &ap_ir_id, &tp_arg);
                self.llvm_builder
                    .create_va_arg(&dst_ir_id, &ap_ir_id, &tp_arg);
                (tp_arg, Some(ConstantOrIrValue::IrValue(dst_ir_id, false)))
            }
            ast::Expr_oneof_e::builtin_va_end(va_end) => {
                if !emit_ir {
                    panic!(
                        "{}: Not a const expression",
                        Compiler::format_loc(e.1)
                    );
                }
                let ap = (
                    &self.translation_unit.exprs[va_end.ap_idx as usize],
                    va_end.get_ap_loc(),
                );
                let (tp_ap, ap) = self.visit_expr(ap, fold_constant, emit_ir);
                let ap = self.convert_to_ir_value(&tp_ap, ap.unwrap());
                let ap_ir_id = match ap {
                    ConstantOrIrValue::IrValue(ir_id, _) => ir_id,
                    _ => unreachable!(),
                };
                self.c4ir_builder.create_va_end(&ap_ir_id);
                self.llvm_builder.create_va_end(&ap_ir_id);
                (QType::from(Type::Void), Some(ConstantOrIrValue::I32(0)))
            }
            ast::Expr_oneof_e::builtin_va_copy(va_copy) => {
                if !emit_ir {
                    panic!(
                        "{}: Not a const expression",
                        Compiler::format_loc(e.1)
                    );
                }
                let dst = (
                    &self.translation_unit.exprs[va_copy.dst_idx as usize],
                    va_copy.get_dst_loc(),
                );
                let src = (
                    &self.translation_unit.exprs[va_copy.src_idx as usize],
                    va_copy.get_src_loc(),
                );
                let (tp_dst, dst) =
                    self.visit_expr(dst, fold_constant, emit_ir);
                let (tp_src, src) =
                    self.visit_expr(src, fold_constant, emit_ir);
                let dst = self.convert_to_ir_value(&tp_dst, dst.unwrap());
                let src = self.convert_to_ir_value(&tp_src, src.unwrap());
                let dst_ir_id = match dst {
                    ConstantOrIrValue::IrValue(ir_id, _) => ir_id,
                    _ => unreachable!(),
                };
                let src_ir_id = match src {
                    ConstantOrIrValue::IrValue(ir_id, _) => ir_id,
                    _ => unreachable!(),
                };
                self.c4ir_builder.create_va_copy(&dst_ir_id, &src_ir_id);
                self.llvm_builder.create_va_copy(&dst_ir_id, &src_ir_id);
                (QType::from(Type::Void), Some(ConstantOrIrValue::I32(0)))
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
        let (src_tp, v) = self.convert_lvalue_and_func_designator(
            src_tp, v, true, true, true, emit_ir,
        );

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
        let (ptr_tp, ptr) = self.convert_lvalue_and_func_designator(
            ptr_tp, ptr, true, true, true, emit_ir,
        );
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
            // visit_cast_expr ensures `ptr` could only be U64, StrAddress,
            // HasAddress, or IrValue, and `sub` must be an integral type,
            // HasAddress, or IrValue.
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
                    (C::StrAddress(ir_id, offset_bytes), Type::Char)
                    | (
                        C::StrAddress(ir_id, offset_bytes),
                        Type::UnsignedChar,
                    ) => {
                        let offset: usize = (*offset_bytes + s) as usize;
                        let buf = self.str_constants.get(ir_id).unwrap();
                        if buf.len() <= offset {
                            panic!(
                                "{}: Index out of bound",
                                Compiler::format_loc(sub_e.1)
                            )
                        }
                        Some(C::I8(buf[offset] as i8))
                    }
                    // technically we can also constant fold wide strings here
                    (C::HasAddress(ir_id, offset_bytes, false), _) => {
                        let elem_sz =
                            match Compiler::get_type_size_and_align_bytes(
                                &elem_tp.tp,
                            ) {
                                None => panic!(
                                    "{}: Element has incomplete type",
                                    Compiler::format_loc(arr_e.1)
                                ),
                                Some((sz, _)) => sz as i64,
                            };
                        let offset: i64 = *offset_bytes + elem_sz * s;
                        Some(C::HasAddress(ir_id.clone(), offset, true))
                    }
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

    // TODO: struct / union (passing + returning; also visit_return_stmt)
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
        let func = func.map(|f| self.convert_to_ir_value(&func_tp, f));
        let (func_ptr_tp, func) = self.convert_lvalue_and_func_designator(
            func_tp, func, true, true, true, emit_ir,
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
            // disallowed on construction
            Type::Function(_, _) | Type::Array(_, _) => unreachable!(),
            _ => (),
        };
        // bool: is_null_constant
        let args: Vec<(QType, Option<String>, bool)> = func_call
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
                    arg_tp, arg, true, true, true, emit_ir,
                );
                let is_null_constant =
                    arg.as_ref().and_then(|c| c.as_constant_u64()) == Some(0);
                let arg = arg
                    .map(|c| self.convert_to_ir_value(&arg_tp, c))
                    .map(|c| match c {
                        ConstantOrIrValue::IrValue(ir_id, false) => ir_id,
                        _ => unreachable!(),
                    });
                (arg_tp, arg, is_null_constant)
            })
            .collect();

        // 3.3.2.2: If the expression that denotes the called function has a
        // type that does not include a prototype, the integral promotions are
        // performed on each argument and arguments that have type float are
        // promoted to double. These are called the default argument promotions.
        let do_default_arg_promo = |cc: &mut Compiler,
                                    arg: (QType, Option<String>, bool)|
         -> Option<String> {
            let (arg_tp, arg, _) = arg;
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
                        arg: (QType, Option<String>, bool)|
         -> Option<String> {
            let (arg_tp, arg, is_null_constant) = arg;
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

                let arg = Some(ConstantOrIrValue::IrValue(arg, false));
                let (arg_tp, arg) = if is_null_constant && dst_tp.is_pointer() {
                    cc.cast_expression(
                        arg_tp,
                        arg,
                        dst_tp.clone(),
                        func_call.get_fn_loc(),
                        emit_ir,
                    )
                } else {
                    (arg_tp, arg)
                };

                cc.visit_simple_binary_op(
                    dst_tp,
                    Some(ConstantOrIrValue::IrValue(vp_ir_id.clone(), true)),
                    func_call.get_fn_loc(),
                    &arg_tp,
                    arg,
                    func_call.get_fn_loc(),
                    ast::Expr_Binary_Op::ASSIGN,
                    true,
                    true,
                );

                let v_ir_id = cc.get_next_ir_id();
                cc.c4ir_builder.create_load(&v_ir_id, &vp_ir_id, dst_tp);
                cc.llvm_builder.create_load(&v_ir_id, &vp_ir_id, dst_tp);
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
            if rtp.is_void() {
                self.c4ir_builder.create_call("", &func, &args);
                self.llvm_builder.create_call("", &func, &args);
                self.c4ir_builder.create_constant(
                    &ir_id,
                    &ConstantOrIrValue::I8(0),
                    &QType::from(Type::Char),
                );
                self.llvm_builder.create_constant(
                    &ir_id,
                    &ConstantOrIrValue::I8(0),
                    &QType::from(Type::Char),
                );
            } else if rtp.is_struct_type() || rtp.is_union_type() {
                let call_ret_ir_id = self.get_next_ir_id();
                self.c4ir_builder.create_call(&call_ret_ir_id, &func, &args);
                self.llvm_builder.create_call(&call_ret_ir_id, &func, &args);
                self.c4ir_builder.create_definition(
                    false,
                    &ir_id,
                    &rtp,
                    Linkage::NONE,
                    &None,
                );
                self.llvm_builder.create_definition(
                    false,
                    &ir_id,
                    &rtp,
                    Linkage::NONE,
                    &None,
                );
                self.c4ir_builder.create_store(&ir_id, &call_ret_ir_id);
                self.llvm_builder.create_store(&ir_id, &call_ret_ir_id);
            } else {
                self.c4ir_builder.create_call(&ir_id, &func, &args);
                self.llvm_builder.create_call(&ir_id, &func, &args);
            }
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
            left_tp, left, !is_dot, true, true, emit_ir,
        );
        let mut is_const = left_tp.is_const;
        let mut is_volatile = left_tp.is_volatile;
        let su_type_is_struct: (&Type, &SuType, bool) = match &left_tp.tp {
            Type::Struct(su_type) if is_dot => {
                (&left_tp.tp, su_type.borrow(), true)
            }
            Type::Union(su_type) if is_dot => {
                (&left_tp.tp, su_type.borrow(), false)
            }
            _ if is_dot => c4_fail!(left_loc, "Struct or union expected"),

            Type::Pointer(tp) => {
                is_const = tp.is_const;
                is_volatile = tp.is_volatile;
                match &tp.tp {
                    Type::Struct(su_type) => (&tp.tp, su_type.borrow(), true),
                    Type::Union(su_type) => (&tp.tp, su_type.borrow(), false),
                    _ => c4_fail!(
                        left_loc,
                        "Pointer to struct or union expected"
                    ),
                }
            }
            _ => c4_fail!(left_loc, "Pointer to struct or union expected"),
        };
        let (su_tp, su_type, is_struct) = su_type_is_struct;
        // this supports cases like:
        //   struct S;
        //   struct P { struct S *sp; } p;
        //   struct S { int n; };
        //   p.sp->n;
        let (su_tp, su_type) = if su_type.fields.is_some() {
            (su_tp.clone(), su_type.clone())
        } else {
            match self.current_scope.lookup_sue_type_by_uuid(su_type.uuid) {
                Some(Type::Struct(b)) => (Type::Struct(b.clone()), *b),
                Some(Type::Union(b)) => (Type::Union(b.clone()), *b),
                _ => (su_tp.clone(), su_type.clone()),
            }
        };

        let fields = match &su_type.fields {
            None => c4_fail!(
                left_loc,
                "Expression has incomplete struct/union type"
            ),
            Some(fs) => (fs),
        };
        let (field_tp, offset) = if is_struct {
            match Compiler::get_struct_layout(&su_tp, Some(field.0)).3 {
                Some(x) => x,
                None => c4_fail!(field.1, "Field '{}' not found", field.0),
            }
        } else {
            let zero_offset = StructFieldOffset {
                offset: 0,
                bit_field_offset: 0,
                bit_field_mask: 0,
                bit_field_rem_bits: 0,
            };
            match fields
                .into_iter()
                .find(|f| f.name.as_deref() == Some(field.0))
            {
                Some(f) => (f.tp.clone(), zero_offset),
                None => c4_fail!(field.1, "Field '{}' not found", field.0),
            }
        };

        let rtp = QType {
            is_const,
            is_volatile,
            tp: field_tp.tp,
        };
        let r = if left.is_none() || !emit_ir {
            match left {
                Some(ConstantOrIrValue::HasAddress(
                    ir_id,
                    offset_bytes,
                    true,
                )) if offset.bit_field_mask == 0 => {
                    Some(ConstantOrIrValue::HasAddress(
                        ir_id,
                        offset_bytes + offset.offset as i64,
                        true,
                    ))
                }
                // looks like even clang does not support constant folding here
                _ => None,
            }
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
                    &head_ptr,
                    &char_ptr_tp,
                    &ir_value,
                    &field_ptr_tp,
                );
                self.llvm_builder.create_cast(
                    &head_ptr,
                    &char_ptr_tp,
                    &ir_value,
                    &field_ptr_tp,
                );

                // offset: i64
                let offset = ConstantOrIrValue::I64(offset.offset as i64);
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
                    &field_ptr,
                    &field_ptr_tp,
                    &field_char_ptr,
                    &char_ptr_tp,
                );
                self.llvm_builder.create_cast(
                    &field_ptr,
                    &field_ptr_tp,
                    &field_char_ptr,
                    &char_ptr_tp,
                );

                field_ptr
            };

            if (is_dot && !is_lvalue) || offset.bit_field_mask != 0 {
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
                            &derefed,
                            &field_ptr_ir_id,
                            &field_ptr_tp,
                        );
                        self.llvm_builder.create_load(
                            &derefed,
                            &field_ptr_ir_id,
                            &field_ptr_tp,
                        );
                        derefed
                    }
                };

                let r_ir_id = if offset.bit_field_mask == 0 {
                    ir_id
                } else {
                    let mut get_ir_id = |tp, c| match self
                        .convert_to_ir_value(&QType::from(tp), c)
                    {
                        ConstantOrIrValue::IrValue(ir_id, false) => ir_id,
                        _ => unreachable!(),
                    };
                    let mask_ir_id = get_ir_id(
                        Type::UnsignedInt,
                        ConstantOrIrValue::U32(offset.bit_field_mask),
                    );
                    let shl_bits_ir_id = get_ir_id(
                        Type::UnsignedChar,
                        ConstantOrIrValue::U8(offset.bit_field_rem_bits),
                    );
                    let shr_bits_ir_id = get_ir_id(
                        Type::UnsignedChar,
                        ConstantOrIrValue::U8(
                            offset.bit_field_rem_bits + offset.bit_field_offset,
                        ),
                    );

                    let is_signed = match &rtp.tp {
                        Type::Int => true,
                        Type::UnsignedInt => false,
                        _ => unreachable!(),
                    };

                    let masked_ir_id = self.get_next_ir_id();
                    self.c4ir_builder.create_bin_op(
                        &masked_ir_id,
                        ast::Expr_Binary_Op::BIT_AND,
                        is_signed,
                        false,
                        &ir_id,
                        &mask_ir_id,
                    );
                    self.llvm_builder.create_bin_op(
                        &masked_ir_id,
                        ast::Expr_Binary_Op::BIT_AND,
                        is_signed,
                        false,
                        &ir_id,
                        &mask_ir_id,
                    );

                    let shl_ir_id = self.get_next_ir_id();
                    self.c4ir_builder.create_bin_op(
                        &shl_ir_id,
                        ast::Expr_Binary_Op::L_SHIFT,
                        is_signed,
                        false,
                        &masked_ir_id,
                        &shl_bits_ir_id,
                    );
                    self.llvm_builder.create_bin_op(
                        &shl_ir_id,
                        ast::Expr_Binary_Op::L_SHIFT,
                        is_signed,
                        false,
                        &masked_ir_id,
                        &shl_bits_ir_id,
                    );

                    let r_ir_id = self.get_next_ir_id();
                    self.c4ir_builder.create_bin_op(
                        &r_ir_id,
                        ast::Expr_Binary_Op::R_SHIFT,
                        is_signed,
                        false,
                        &shl_ir_id,
                        &shr_bits_ir_id,
                    );
                    self.llvm_builder.create_bin_op(
                        &r_ir_id,
                        ast::Expr_Binary_Op::R_SHIFT,
                        is_signed,
                        false,
                        &shl_ir_id,
                        &shr_bits_ir_id,
                    );

                    r_ir_id
                };
                Some(ConstantOrIrValue::IrValue(r_ir_id, false))
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
            emit_ir,
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
                    emit_ir,
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
                // C89 requires the argument must not be a bit-field or a
                // register variable. If the argument is a:
                //
                // - bit-field: Since in our implementation bit fields are not
                //   lvalues, semantic analysis would fail.
                // - register variable: Semantic analysis would succeed, which
                //   is in fact incorrect per C89 spec requirements.
                let ptr_tp = QType::ptr_tp(arg_tp);

                use ConstantOrIrValue as C;
                match arg {
                    None => (ptr_tp, None),
                    Some(C::HasAddress(ir_id, offset_bytes, true)) => (
                        ptr_tp,
                        Some(C::HasAddress(ir_id, offset_bytes, false)),
                    ),
                    _ if !emit_ir => (ptr_tp, None),

                    Some(C::IrValue(ir_id, true)) => {
                        (ptr_tp, Some(C::IrValue(ir_id, false)))
                    }
                    _ => panic!(
                        "{}: Expression is not a lvalue",
                        Compiler::format_loc(loc)
                    ),
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

                use ConstantOrIrValue as C;
                match arg {
                    None => (elem_tp, None),
                    Some(C::HasAddress(ir_id, offset_bytes, false)) => (
                        elem_tp,
                        Some(C::HasAddress(ir_id, offset_bytes, true)),
                    ),
                    Some(C::StrAddress(ir_id, offset_bytes)) => {
                        let is_char = match &elem_tp.tp {
                            Type::Char => true,
                            Type::Short => false,
                            _ => unreachable!(),
                        };
                        let s = self.str_constants.get(&ir_id).unwrap();
                        let idx = offset_bytes as usize;
                        let c = if is_char {
                            C::I8(s[idx] as i8)
                        } else {
                            C::I16(s[idx] as i16 | ((s[idx + 1] as i16) << 8))
                        };
                        (elem_tp, Some(c))
                    }
                    _ if !emit_ir => (elem_tp, None),

                    Some(C::IrValue(ir_id, false)) => {
                        (elem_tp, Some(C::IrValue(ir_id, true)))
                    }
                    Some(addr @ C::U64(_)) => {
                        match self.convert_to_ir_value(&arg_tp, addr) {
                            C::IrValue(ir_id, false) => {
                                (elem_tp, Some(C::IrValue(ir_id, true)))
                            }
                            _ => unreachable!(),
                        }
                    }
                    _ => unreachable!(),
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
                    Some(C::StrAddress(_, _)) => unreachable!(),

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

                    Some(C::StrAddress(_, _))
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
            emit_ir,
        );
        let (tp_right, right) = self.convert_lvalue_and_func_designator(
            tp_right.clone(),
            right,
            true,
            true,
            true,
            emit_ir,
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
                Compiler::check_types_for_assign(
                    &tp_left, &tp_right, &right, loc_right,
                );
                let (_, right) = if tp_right.is_scalar_type() {
                    self.cast_expression(
                        tp_right,
                        right,
                        tp_left.clone(),
                        loc_right,
                        emit_ir,
                    )
                } else {
                    (tp_right, right)
                };
                if !emit_ir {
                    return (QType::from(tp_left.tp), None);
                }
                let left = left.map(|c| self.convert_to_ir_value(&tp_left, c));
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
                        self.c4ir_builder
                            .create_memcpy(&dst_ir_id, &src_ir_id, size, align);
                        self.llvm_builder
                            .create_memcpy(&dst_ir_id, &src_ir_id, size, align);
                        (QType::from(tp_left.tp), right)
                    }
                    _ => {
                        self.c4ir_builder.create_store(&dst_ir_id, &src_ir_id);
                        self.llvm_builder.create_store(&dst_ir_id, &src_ir_id);
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
                match self.do_arithmetic_conversion(
                    tp_left, left, tp_right, right, emit_ir,
                ) {
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
                            &ir_id,
                            op,
                            is_signed(&tp),
                            is_fp(&tp),
                            &x_ir_id,
                            &y_ir_id,
                        );
                        self.llvm_builder.create_bin_op(
                            &ir_id,
                            op,
                            is_signed(&tp),
                            is_fp(&tp),
                            &x_ir_id,
                            &y_ir_id,
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
                    Some(C::Float(_)) | Some(C::Double(_)) => false,
                    Some(c) => c.as_constant_u64() == Some(0),
                    _ => false,
                };
                let de_qualify = |tp: &QType| QType::from(tp.tp.clone());
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
                                    tp_left, left, tp_right, right, emit_ir,
                                );
                            (tp.clone(), left, tp, right)
                        }
                        // 3.3.8, 3.3.9: both operands are pointers to qualified
                        // or unqualified versions of compatible types
                        (Type::Pointer(tp_el), Type::Pointer(tp_er))
                            if Compiler::try_get_composite_type(
                                &de_qualify(tp_el),
                                &de_qualify(tp_er),
                                loc_left,
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
                            let (tp_left, left) = if is_null(&left) {
                                // now `right` is the pointer type
                                (tp_right.clone(), Some(C::U64(0)))
                            } else {
                                (tp_left.clone(), left)
                            };
                            let (tp_right, right) = if is_null(&right) {
                                // now `left` is the pointer type
                                (tp_left.clone(), Some(C::U64(0)))
                            } else {
                                (tp_right.clone(), right)
                            };
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
                    (
                        C::StrAddress(addr_left, _),
                        C::StrAddress(addr_right, _),
                    ) if addr_left != addr_right => {
                        if emit_ir {
                            (
                                self.convert_to_ir_value(&tp_left, left),
                                self.convert_to_ir_value(&tp_right, right),
                            )
                        } else {
                            return (QType::from(Type::Int), None);
                        }
                    }
                    (
                        C::HasAddress(ir_id_left, _, false),
                        C::HasAddress(ir_id_right, _, false),
                    ) if ir_id_left == ir_id_right => (left, right),
                    (C::HasAddress(_, _, false), C::U64(0))
                    | (C::U64(0), C::HasAddress(_, _, false)) => (left, right),
                    (C::HasAddress(_, _, _), _)
                    | (_, C::HasAddress(_, _, _)) => {
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
                            &i1_ir_id,
                            op,
                            is_signed(&tp_left),
                            is_fp(&tp_left),
                            &left,
                            &right,
                        );
                        self.llvm_builder.create_cmp_op(
                            &i1_ir_id,
                            op,
                            is_signed(&tp_left),
                            is_fp(&tp_left),
                            &left,
                            &right,
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

                    (C::StrAddress(_, x), C::StrAddress(_, y)) => cmp(op, x, y),
                    (C::StrAddress(_, _), C::U64(0)) => cmp(op, 999, 0),
                    (C::U64(0), C::StrAddress(_, _)) => cmp(op, 0, 999),

                    (
                        C::HasAddress(_, x, false),
                        C::HasAddress(_, y, false),
                    ) => cmp(op, x, y),
                    (C::HasAddress(_, _, false), C::U64(0)) => cmp(op, 999, 0),
                    (C::U64(0), C::HasAddress(_, _, false)) => cmp(op, 0, 999),

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

                let (tp_left, left) =
                    self.do_integral_promotion(tp_left, left, emit_ir);
                let (tp_right, right) =
                    self.do_integral_promotion(tp_right, right, emit_ir);
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
                            &ir_id,
                            op,
                            is_signed(&tp_left),
                            false,
                            &left_ir_id,
                            &right_ir_id,
                        );
                        self.llvm_builder.create_bin_op(
                            &ir_id,
                            op,
                            is_signed(&tp_left),
                            false,
                            &left_ir_id,
                            &right_ir_id,
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
                        QType::from(Type::UnsignedInt),
                        Some(C::U32(x.checked_shl(y).unwrap_or(0))),
                    ),
                    (C::U32(x), C::U32(y)) => (
                        QType::from(Type::UnsignedInt),
                        Some(C::U32(x.checked_shr(y).unwrap_or(0))),
                    ),
                    // long
                    (C::I64(x), C::I64(y)) if is_shl => (
                        QType::from(Type::Long),
                        Some(C::I64(
                            x.checked_shl(y as u32).unwrap_or(if x >= 0 {
                                0
                            } else {
                                -1
                            }),
                        )),
                    ),
                    (C::I64(x), C::I64(y)) => (
                        QType::from(Type::Long),
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
                        QType::from(Type::UnsignedLong),
                        Some(C::U64(x.checked_shl(y as u32).unwrap_or(0))),
                    ),
                    (C::U64(x), C::U64(y)) => (
                        QType::from(Type::UnsignedLong),
                        Some(C::U64(x.checked_shr(y as u32).unwrap_or(0))),
                    ),

                    _ => unreachable!(),
                }
            }
            Op::ADD | Op::SUB => {
                if tp_left.is_arithmetic_type() && tp_right.is_arithmetic_type()
                {
                    let (left, right, tp) = self.do_arithmetic_conversion(
                        tp_left, left, tp_right, right, emit_ir,
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
                                &ir_id,
                                op,
                                is_signed(&tp),
                                is_fp(&tp),
                                &x_ir_id,
                                &y_ir_id,
                            );
                            self.llvm_builder.create_bin_op(
                                &ir_id,
                                op,
                                is_signed(&tp),
                                is_fp(&tp),
                                &x_ir_id,
                                &y_ir_id,
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
                    let tp_elem = match &tp_elem.tp {
                        Type::Struct(b) | Type::Union(b)
                            if b.fields.is_none() =>
                        {
                            self.current_scope
                                .lookup_sue_type_by_uuid(b.uuid)
                                .map(QType::from)
                                .unwrap_or(tp_elem)
                        }
                        _ => tp_elem,
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
                                &ptr_ir_id,
                                &QType::char_ptr_tp(),
                                &left,
                                &tp_left,
                            );
                            self.llvm_builder.create_cast(
                                &ptr_ir_id,
                                &QType::char_ptr_tp(),
                                &left,
                                &tp_left,
                            );

                            let sz_ir_id = self.get_next_ir_id();
                            self.c4ir_builder.create_constant(
                                &sz_ir_id,
                                &ConstantOrIrValue::I64(sz_elem),
                                &QType::from(Type::Long),
                            );
                            self.llvm_builder.create_constant(
                                &sz_ir_id,
                                &ConstantOrIrValue::I64(sz_elem),
                                &QType::from(Type::Long),
                            );

                            let offset_ir_id = self.get_next_ir_id();
                            self.c4ir_builder.create_bin_op(
                                &offset_ir_id,
                                Op::MUL,
                                true,
                                false,
                                &right,
                                &sz_ir_id,
                            );
                            self.llvm_builder.create_bin_op(
                                &offset_ir_id,
                                Op::MUL,
                                true,
                                false,
                                &right,
                                &sz_ir_id,
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
                        (C::StrAddress(ir_id, offset), C::I64(y)) => (
                            tp_left,
                            Some(C::StrAddress(ir_id, offset + y * sz_elem)),
                        ),
                        (C::HasAddress(ir_id, offset, false), C::I64(y)) => (
                            tp_left,
                            Some(C::HasAddress(
                                ir_id,
                                offset + y * sz_elem,
                                false,
                            )),
                        ),
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
                    // 3.3.6: both operands are pointers to qualified or
                    // unqualified versions of compatible object types
                    let (tp_left, tp_right) = {
                        let mut tp_left = tp_left;
                        let mut tp_right = tp_right;
                        match &mut tp_left.tp {
                            Type::Pointer(elem_tp) => {
                                elem_tp.is_const = false;
                                elem_tp.is_volatile = false;
                            }
                            _ => unreachable!(),
                        };
                        match &mut tp_right.tp {
                            Type::Pointer(elem_tp) => {
                                elem_tp.is_const = false;
                                elem_tp.is_volatile = false;
                            }
                            _ => unreachable!(),
                        };
                        (tp_left, tp_right)
                    };
                    let tp = Compiler::get_composite_type(
                        &tp_left, &tp_right, loc_left,
                    );

                    let tp_elem = match &tp.tp {
                        Type::Pointer(elem) => *elem.clone(),
                        _ => unreachable!(),
                    };
                    // special case for void* minus void*
                    let tp_elem = match &tp_elem.tp {
                        Type::Void => QType::from(Type::Char),
                        _ => tp_elem,
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
                        (C::U64(_), C::StrAddress(_, _)) => (long_tp, None),
                        (C::StrAddress(_, _), C::U64(_)) => (long_tp, None),

                        (C::U64(x), C::U64(y)) => (
                            long_tp,
                            Some(C::I64(
                                x.overflowing_sub(y).0 as i64 / sz_elem as i64,
                            )),
                        ),

                        (
                            C::StrAddress(ir_id_x, _),
                            C::StrAddress(ir_id_y, _),
                        ) if ir_id_x != ir_id_y => (long_tp, None),
                        (
                            C::StrAddress(_, offset_x),
                            C::StrAddress(_, offset_y),
                        ) => (
                            long_tp,
                            Some(C::I64(
                                offset_x.overflowing_sub(offset_y).0
                                    / sz_elem as i64,
                            )),
                        ),

                        (
                            C::HasAddress(ir_id_x, offset_x, false),
                            C::HasAddress(ir_id_y, offset_y, false),
                        ) => {
                            if ir_id_x != ir_id_y {
                                (long_tp, None)
                            } else {
                                (
                                    long_tp,
                                    Some(C::I64(
                                        offset_x.overflowing_sub(offset_y).0
                                            / sz_elem as i64,
                                    )),
                                )
                            }
                        }

                        (
                            C::IrValue(ir_id_x, false),
                            C::IrValue(ir_id_y, false),
                        ) => {
                            // p1 - p2 => ((long*)p1 - (long*)p2) / sizeof(T)
                            let p1 = self.get_next_ir_id();
                            self.c4ir_builder
                                .create_cast(&p1, &long_tp, &ir_id_x, &tp_left);
                            self.llvm_builder
                                .create_cast(&p1, &long_tp, &ir_id_x, &tp_left);

                            let p2 = self.get_next_ir_id();
                            self.c4ir_builder.create_cast(
                                &p2, &long_tp, &ir_id_y, &tp_right,
                            );
                            self.llvm_builder.create_cast(
                                &p2, &long_tp, &ir_id_y, &tp_right,
                            );

                            let diff = self.get_next_ir_id();
                            self.c4ir_builder.create_bin_op(
                                &diff,
                                Op::SUB,
                                true,
                                false,
                                &p1,
                                &p2,
                            );
                            self.llvm_builder.create_bin_op(
                                &diff,
                                Op::SUB,
                                true,
                                false,
                                &p1,
                                &p2,
                            );

                            let sz = self.get_next_ir_id();
                            self.c4ir_builder.create_constant(
                                &sz,
                                &C::I64(sz_elem as i64),
                                &long_tp,
                            );
                            self.llvm_builder.create_constant(
                                &sz,
                                &C::I64(sz_elem as i64),
                                &long_tp,
                            );

                            let ir_id = self.get_next_ir_id();
                            self.c4ir_builder.create_bin_op(
                                &ir_id,
                                Op::DIV,
                                true,
                                false,
                                &diff,
                                &sz,
                            );
                            self.llvm_builder.create_bin_op(
                                &ir_id,
                                Op::DIV,
                                true,
                                false,
                                &diff,
                                &sz,
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

                let (left, right, tp) = self.do_arithmetic_conversion(
                    tp_left, left, tp_right, right, emit_ir,
                );
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
                                &ir_id,
                                op,
                                is_signed(&tp),
                                is_fp(&tp),
                                &x,
                                &y,
                            );
                            self.llvm_builder.create_bin_op(
                                &ir_id,
                                op,
                                is_signed(&tp),
                                is_fp(&tp),
                                &x,
                                &y,
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
    fn visit_logical_binary_op(
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
            left_tp, left, true, true, true, emit_ir,
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
                | C::StrAddress(_, _)
                | C::HasAddress(_, _, false) => {
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
                C::HasAddress(_, _, true) => unreachable!(),
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
                    self.c4ir_builder
                        .create_store(&result_ir_id, &short_value_ir_id);
                    self.llvm_builder
                        .create_store(&result_ir_id, &short_value_ir_id);
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
                    self.c4ir_builder
                        .create_store(&result_ir_id, &right_i32_ir_id);
                    self.llvm_builder
                        .create_store(&result_ir_id, &right_i32_ir_id);
                    self.c4ir_builder.create_br(&merge_bb);
                    self.llvm_builder.create_br(&merge_bb);

                    self.c4ir_builder.set_current_basic_block(&merge_bb);
                    self.llvm_builder.set_current_basic_block(&merge_bb);

                    // i32 result = load i32* r
                    let result_rvalue_ir_id = self.get_next_ir_id();
                    self.c4ir_builder.create_load(
                        &result_rvalue_ir_id,
                        &result_ir_id,
                        &QType::from(Type::Int),
                    );
                    self.llvm_builder.create_load(
                        &result_rvalue_ir_id,
                        &result_ir_id,
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

    // specialized entry point for assign exprs; this is a wrapper around
    // visit_simple_binary_op() that handles writing to bit fields.
    fn visit_assign_expr(
        &mut self,
        left: L<&ast::Expr>,
        right: L<&ast::Expr>,
        fold_constant: bool,
        emit_ir: bool,
    ) -> (QType, Option<ConstantOrIrValue>) {
        let loc_left = left.1;
        let loc_right = right.1;

        // if left hand side is not even a member access expr: fallback to
        // simple assign expr
        let (struct_expr, field, is_dot) = match left.0.e.as_ref().unwrap() {
            ast::Expr_oneof_e::dot(dot) => {
                let s = &self.translation_unit.exprs[dot.e_idx as usize];
                let s = (s, dot.get_e_loc());
                let f = (dot.get_field(), dot.get_field_loc());
                (s, f, true)
            }
            ast::Expr_oneof_e::ptr(ptr) => {
                let s = &self.translation_unit.exprs[ptr.e_idx as usize];
                let s = (s, ptr.get_e_loc());
                let f = (ptr.get_field(), ptr.get_field_loc());
                (s, f, false)
            }
            _ => {
                let (tp_left, left) =
                    self.visit_expr(left, fold_constant, emit_ir);
                let (tp_right, right) =
                    self.visit_expr(right, fold_constant, emit_ir);
                return self.visit_simple_binary_op(
                    &tp_left,
                    left,
                    loc_left,
                    &tp_right,
                    right,
                    loc_right,
                    ast::Expr_Binary_Op::ASSIGN,
                    fold_constant,
                    emit_ir,
                );
            }
        };

        // if left hand side is not a bit field access expr: fallback to simple
        // assign expr
        //
        // similar to visit_member_access_expr(), if the left hand side member
        // access expr has incomplete struct type, extra type lookup is needed
        let (tp_struct_expr, _) = self.visit_expr(struct_expr, false, false);
        let offset = match &tp_struct_expr.tp {
            Type::Struct(b) if is_dot && b.fields.is_some() => {
                Compiler::get_struct_layout(&tp_struct_expr.tp, Some(field.0)).3
            }
            Type::Struct(b) if is_dot && b.fields.is_none() => {
                match self.current_scope.lookup_sue_type_by_uuid(b.uuid) {
                    Some(t) => Compiler::get_struct_layout(&t, Some(field.0)).3,
                    None => panic!(
                        "{}: Expression has incomplete struct type",
                        Compiler::format_loc(left.1)
                    ),
                }
            }
            Type::Pointer(tp) if !is_dot => match &tp.tp {
                Type::Struct(b) if b.fields.is_some() => {
                    Compiler::get_struct_layout(&tp.tp, Some(field.0)).3
                }
                Type::Struct(b) if b.fields.is_none() => {
                    match self.current_scope.lookup_sue_type_by_uuid(b.uuid) {
                        Some(t) => {
                            Compiler::get_struct_layout(&t, Some(field.0)).3
                        }
                        None => panic!(
                            "{}: Expression has incomplete struct type",
                            Compiler::format_loc(left.1)
                        ),
                    }
                }
                _ => None,
            },
            _ => None,
        };
        let (tp_field, offset) = match offset {
            Some(x) if x.1.bit_field_mask != 0 => x,
            Some(_) | None => {
                let (tp_left, left) =
                    self.visit_expr(left, fold_constant, emit_ir);
                let (tp_right, right) =
                    self.visit_expr(right, fold_constant, emit_ir);
                return self.visit_simple_binary_op(
                    &tp_left,
                    left,
                    loc_left,
                    &tp_right,
                    right,
                    loc_right,
                    ast::Expr_Binary_Op::ASSIGN,
                    fold_constant,
                    emit_ir,
                );
            }
        };

        // when x is a bit field of struct s:
        //      s.x = e (or s->x = e)
        // will be converted to:
        //      $v = e & (mask >> offset);
        //      *ptr = (*ptr & ~mask) | $v << offset;
        //      $v
        let (tp_struct_expr, struct_expr) =
            self.visit_expr(struct_expr, fold_constant, emit_ir);
        let (tp_struct_expr, struct_expr) = self
            .convert_lvalue_and_func_designator(
                tp_struct_expr,
                struct_expr,
                !is_dot,
                true,
                true,
                emit_ir,
            );
        let (tp_right, right) = self.visit_expr(right, fold_constant, emit_ir);
        let (tp_right, right) = self.convert_lvalue_and_func_designator(
            tp_right, right, true, true, true, emit_ir,
        );
        let src_ir_id = {
            if tp_struct_expr.is_const {
                panic!(
                    "{}: Cannot modify const-qualified variables",
                    Compiler::format_loc(loc_left)
                )
            }
            Compiler::check_types_for_assign(
                &tp_field, &tp_right, &right, loc_right,
            );
            let (_, right) = self.cast_expression(
                tp_right,
                right,
                tp_field.clone(),
                loc_right,
                emit_ir,
            );
            if !emit_ir {
                return (tp_field, None);
            }
            let right = right.map(|c| self.convert_to_ir_value(&tp_field, c));
            match &right {
                Some(ConstantOrIrValue::IrValue(ir_id, false)) => ir_id.clone(),
                _ => unreachable!(),
            }
        };
        let field_ptr_ir_id = {
            let struct_expr = struct_expr
                .map(|c| self.convert_to_ir_value(&tp_struct_expr, c));
            let struct_ptr_ir_id = match struct_expr {
                Some(ConstantOrIrValue::IrValue(ir_id, _)) if is_dot => ir_id,
                Some(ConstantOrIrValue::IrValue(ir_id, false)) if !is_dot => {
                    ir_id
                }
                _ => unreachable!(),
            };

            let struct_i8_ptr_ir_id = self.get_next_ir_id();
            // src_tp is only a marker for ir builder that indicates it's a
            // ptr->ptr conversion
            self.c4ir_builder.create_cast(
                &struct_i8_ptr_ir_id,
                &QType::char_ptr_tp(),
                &struct_ptr_ir_id,
                &QType::char_ptr_tp(),
            );
            self.llvm_builder.create_cast(
                &struct_i8_ptr_ir_id,
                &QType::char_ptr_tp(),
                &struct_ptr_ir_id,
                &QType::char_ptr_tp(),
            );

            let offset_ir_id = match self.convert_to_ir_value(
                &QType::from(Type::Long),
                ConstantOrIrValue::I64(offset.offset as i64),
            ) {
                ConstantOrIrValue::IrValue(ir_id, false) => ir_id,
                _ => unreachable!(),
            };

            let field_i8_ptr_ir_id = self.get_next_ir_id();
            self.c4ir_builder.create_ptr_add(
                &field_i8_ptr_ir_id,
                &struct_i8_ptr_ir_id,
                &offset_ir_id,
            );
            self.llvm_builder.create_ptr_add(
                &field_i8_ptr_ir_id,
                &struct_i8_ptr_ir_id,
                &offset_ir_id,
            );

            let field_ptr_ir_id = self.get_next_ir_id();
            self.c4ir_builder.create_cast(
                &field_ptr_ir_id,
                &QType::ptr_tp(tp_field.clone()),
                &field_i8_ptr_ir_id,
                &QType::char_ptr_tp(),
            );
            self.llvm_builder.create_cast(
                &field_ptr_ir_id,
                &QType::ptr_tp(tp_field.clone()),
                &field_i8_ptr_ir_id,
                &QType::char_ptr_tp(),
            );

            field_ptr_ir_id
        };

        let is_signed = match &tp_field.tp {
            Type::Int => true,
            _ => false,
        };

        // $v = e & (mask >> offset);
        let v_ir_id = {
            let mask = ConstantOrIrValue::U32(
                offset.bit_field_mask >> offset.bit_field_offset,
            );
            let mask_ir_id = match self
                .convert_to_ir_value(&QType::from(Type::UnsignedInt), mask)
            {
                ConstantOrIrValue::IrValue(ir_id, false) => ir_id,
                _ => unreachable!(),
            };

            let v_ir_id = self.get_next_ir_id();
            self.c4ir_builder.create_bin_op(
                &v_ir_id,
                ast::Expr_Binary_Op::BIT_AND,
                is_signed,
                false,
                &src_ir_id,
                &mask_ir_id,
            );
            self.llvm_builder.create_bin_op(
                &v_ir_id,
                ast::Expr_Binary_Op::BIT_AND,
                is_signed,
                false,
                &src_ir_id,
                &mask_ir_id,
            );

            v_ir_id
        };
        // *ptr = (*ptr & ~mask) | $v << offset;
        {
            let orig_ir_id = self.get_next_ir_id();
            self.c4ir_builder.create_load(
                &orig_ir_id,
                &field_ptr_ir_id,
                &tp_field,
            );
            self.llvm_builder.create_load(
                &orig_ir_id,
                &field_ptr_ir_id,
                &tp_field,
            );

            let mask = ConstantOrIrValue::U32(!offset.bit_field_mask);
            let mask_ir_id = match self.convert_to_ir_value(&tp_field, mask) {
                ConstantOrIrValue::IrValue(ir_id, false) => ir_id,
                _ => unreachable!(),
            };

            // $left = *ptr & ~mask
            let left_ir_id = self.get_next_ir_id();
            self.c4ir_builder.create_bin_op(
                &left_ir_id,
                ast::Expr_Binary_Op::BIT_AND,
                is_signed,
                false,
                &orig_ir_id,
                &mask_ir_id,
            );
            self.llvm_builder.create_bin_op(
                &left_ir_id,
                ast::Expr_Binary_Op::BIT_AND,
                is_signed,
                false,
                &orig_ir_id,
                &mask_ir_id,
            );

            let offset = ConstantOrIrValue::U8(offset.bit_field_offset);
            let offset_ir_id = match self
                .convert_to_ir_value(&QType::from(Type::UnsignedChar), offset)
            {
                ConstantOrIrValue::IrValue(ir_id, false) => ir_id,
                _ => unreachable!(),
            };

            // $right = $v << offset
            let right_ir_id = self.get_next_ir_id();
            self.c4ir_builder.create_bin_op(
                &right_ir_id,
                ast::Expr_Binary_Op::L_SHIFT,
                is_signed,
                false,
                &v_ir_id,
                &offset_ir_id,
            );
            self.llvm_builder.create_bin_op(
                &right_ir_id,
                ast::Expr_Binary_Op::L_SHIFT,
                is_signed,
                false,
                &v_ir_id,
                &offset_ir_id,
            );

            // $t = $left | $right
            let ir_id = self.get_next_ir_id();
            self.c4ir_builder.create_bin_op(
                &ir_id,
                ast::Expr_Binary_Op::BIT_OR,
                is_signed,
                false,
                &left_ir_id,
                &right_ir_id,
            );
            self.llvm_builder.create_bin_op(
                &ir_id,
                ast::Expr_Binary_Op::BIT_OR,
                is_signed,
                false,
                &left_ir_id,
                &right_ir_id,
            );

            // *ptr = $t
            self.c4ir_builder.create_store(&field_ptr_ir_id, &ir_id);
            self.llvm_builder.create_store(&field_ptr_ir_id, &ir_id);
        }
        // $r = $v << (rem_bits + offset) >> (rem_bits + offset)
        let r_ir_id = if is_signed {
            let shift_bits = ConstantOrIrValue::U8(
                offset.bit_field_rem_bits + offset.bit_field_offset,
            );
            let shift_bits_ir_id = match self.convert_to_ir_value(
                &QType::from(Type::UnsignedChar),
                shift_bits,
            ) {
                ConstantOrIrValue::IrValue(ir_id, false) => ir_id,
                _ => unreachable!(),
            };

            let shl_ir_id = self.get_next_ir_id();
            self.c4ir_builder.create_bin_op(
                &shl_ir_id,
                ast::Expr_Binary_Op::L_SHIFT,
                true,
                false,
                &v_ir_id,
                &shift_bits_ir_id,
            );
            self.llvm_builder.create_bin_op(
                &shl_ir_id,
                ast::Expr_Binary_Op::L_SHIFT,
                true,
                false,
                &v_ir_id,
                &shift_bits_ir_id,
            );

            let shr_ir_id = self.get_next_ir_id();
            self.c4ir_builder.create_bin_op(
                &shr_ir_id,
                ast::Expr_Binary_Op::R_SHIFT,
                true,
                false,
                &shl_ir_id,
                &shift_bits_ir_id,
            );
            self.llvm_builder.create_bin_op(
                &shr_ir_id,
                ast::Expr_Binary_Op::R_SHIFT,
                true,
                false,
                &shl_ir_id,
                &shift_bits_ir_id,
            );

            shr_ir_id
        } else {
            v_ir_id
        };
        // $r
        (tp_field, Some(ConstantOrIrValue::IrValue(r_ir_id, false)))
    }

    fn visit_ternary_op(
        &mut self,
        e_cond: L<&ast::Expr>,
        e_then: L<&ast::Expr>,
        e_else: L<&ast::Expr>,
        fold_constant: bool,
        emit_ir: bool,
    ) -> R<(QType, Option<ConstantOrIrValue>)> {
        use ConstantOrIrValue as C;
        if emit_ir {
            let cond_ir_id = self.visit_cond_expr(e_cond)?;

            let entry_bb = self.get_current_bb();
            let then_bb = self.create_bb();
            let else_bb = self.create_bb();
            let merge_bb = self.create_bb();

            self.c4ir_builder.set_current_basic_block(&then_bb);
            self.llvm_builder.set_current_basic_block(&then_bb);
            let (then_tp, then_c) =
                self.visit_expr(e_then, fold_constant, emit_ir);
            let (then_tp, then_c) = self.convert_lvalue_and_func_designator(
                then_tp, then_c, true, true, true, emit_ir,
            );
            let final_then_bb = self.get_current_bb();

            self.c4ir_builder.set_current_basic_block(&else_bb);
            self.llvm_builder.set_current_basic_block(&else_bb);
            let (else_tp, else_c) =
                self.visit_expr(e_else, fold_constant, emit_ir);
            let (else_tp, else_c) = self.convert_lvalue_and_func_designator(
                else_tp, else_c, true, true, true, emit_ir,
            );
            let final_else_bb = self.get_current_bb();

            let rtp = self.get_ternary_expr_rtp(
                &then_tp, &then_c, &else_tp, &else_c, e_else.1,
            )?;

            // emit the rest IR

            self.c4ir_builder.set_current_basic_block(&entry_bb);
            self.llvm_builder.set_current_basic_block(&entry_bb);
            let r_ir_id = self.get_next_ir_id();
            if !rtp.is_void() {
                self.c4ir_builder.create_definition(
                    false,
                    &r_ir_id,
                    &rtp,
                    Linkage::NONE,
                    &None,
                );
                self.llvm_builder.create_definition(
                    false,
                    &r_ir_id,
                    &rtp,
                    Linkage::NONE,
                    &None,
                );
            }
            self.c4ir_builder
                .create_cond_br(&cond_ir_id, &then_bb, &else_bb);
            self.llvm_builder
                .create_cond_br(&cond_ir_id, &then_bb, &else_bb);

            self.c4ir_builder.set_current_basic_block(&final_then_bb);
            self.llvm_builder.set_current_basic_block(&final_then_bb);
            let then_c = then_c.map(|c| self.convert_to_ir_value(&then_tp, c));
            if !rtp.is_void() {
                let (_, then_c) = self.cast_expression(
                    then_tp,
                    then_c,
                    rtp.clone(),
                    e_then.1,
                    true,
                );
                match then_c.unwrap() {
                    C::IrValue(ir_id, false) if rtp.is_scalar_type() => {
                        self.c4ir_builder.create_store(&r_ir_id, &ir_id);
                        self.llvm_builder.create_store(&r_ir_id, &ir_id);
                    }
                    C::IrValue(ir_id, false) => {
                        let (size, align) =
                            match Compiler::get_type_size_and_align_bytes(
                                &rtp.tp,
                            ) {
                                Some(x) => x,
                                None => {
                                    c4_fail!(e_then.1, "Complete type expected")
                                }
                            };
                        self.c4ir_builder
                            .create_memcpy(&r_ir_id, &ir_id, size, align);
                        self.llvm_builder
                            .create_memcpy(&r_ir_id, &ir_id, size, align);
                    }
                    _ => unreachable!(),
                }
            }
            self.c4ir_builder.create_br(&merge_bb);
            self.llvm_builder.create_br(&merge_bb);

            self.c4ir_builder.set_current_basic_block(&final_else_bb);
            self.llvm_builder.set_current_basic_block(&final_else_bb);
            let else_c = else_c.map(|c| self.convert_to_ir_value(&else_tp, c));
            if !rtp.is_void() {
                let (_, else_c) = self.cast_expression(
                    else_tp,
                    else_c,
                    rtp.clone(),
                    e_else.1,
                    true,
                );
                match else_c.unwrap() {
                    C::IrValue(ir_id, false) if rtp.is_scalar_type() => {
                        self.c4ir_builder.create_store(&r_ir_id, &ir_id);
                        self.llvm_builder.create_store(&r_ir_id, &ir_id);
                    }
                    C::IrValue(ir_id, false) => {
                        let (size, align) =
                            match Compiler::get_type_size_and_align_bytes(
                                &rtp.tp,
                            ) {
                                Some(x) => x,
                                None => {
                                    c4_fail!(e_else.1, "Complete type expected")
                                }
                            };
                        self.c4ir_builder
                            .create_memcpy(&r_ir_id, &ir_id, size, align);
                        self.llvm_builder
                            .create_memcpy(&r_ir_id, &ir_id, size, align);
                    }
                    _ => unreachable!(),
                }
            }
            self.c4ir_builder.create_br(&merge_bb);
            self.llvm_builder.create_br(&merge_bb);

            self.c4ir_builder.set_current_basic_block(&merge_bb);
            self.llvm_builder.set_current_basic_block(&merge_bb);
            if !rtp.is_void() {
                let ptr_rtp = QType::ptr_tp(rtp.clone());
                let ir_id = self.get_next_ir_id();
                self.c4ir_builder.create_load(&ir_id, &r_ir_id, &ptr_rtp);
                self.llvm_builder.create_load(&ir_id, &r_ir_id, &ptr_rtp);
                Ok((rtp, Some(C::IrValue(ir_id, false))))
            } else {
                Ok((rtp, Some(C::I32(0)))) // dummy value
            }
        } else {
            let (cond_tp, cond) =
                self.visit_expr(e_cond, fold_constant, emit_ir);
            let (cond_tp, cond) = self.convert_lvalue_and_func_designator(
                cond_tp, cond, true, true, true, emit_ir,
            );
            // 3.3.15: The first operand shall have scalar type.
            if !cond_tp.is_scalar_type() {
                c4_fail!(e_cond.1, "Scalar type expected")
            }
            // cond => (cond != 0) => 1 or 0
            let (_, cond) = self.visit_simple_binary_op(
                &QType::from(Type::Int),
                Some(C::I32(0)),
                e_cond.1,
                &cond_tp,
                cond,
                e_cond.1,
                ast::Expr_Binary_Op::NEQ,
                fold_constant,
                emit_ir,
            );

            let (then_tp, then_c) =
                self.visit_expr(e_then, fold_constant, emit_ir);
            let (then_tp, then_c) = self.convert_lvalue_and_func_designator(
                then_tp, then_c, true, true, true, emit_ir,
            );

            let (else_tp, else_c) =
                self.visit_expr(e_else, fold_constant, emit_ir);
            let (else_tp, else_c) = self.convert_lvalue_and_func_designator(
                else_tp, else_c, true, true, true, emit_ir,
            );

            let rtp = self.get_ternary_expr_rtp(
                &then_tp, &then_c, &else_tp, &else_c, e_else.1,
            )?;

            if rtp.is_void() {
                return Ok((rtp, Some(C::I32(0)))); // dummy value
            }

            let (_, then_c) = self.cast_expression(
                then_tp,
                then_c,
                rtp.clone(),
                e_then.1,
                emit_ir,
            );
            let (_, else_c) = self.cast_expression(
                else_tp,
                else_c,
                rtp.clone(),
                e_else.1,
                emit_ir,
            );

            let r = match cond {
                None => None,
                Some(C::I32(1)) => then_c,
                Some(C::I32(0)) => else_c,
                _ => unreachable!(),
            };
            Ok((rtp, r))
        }
    }

    fn get_ternary_expr_rtp(
        &mut self,
        then_tp: &QType,
        then_c: &Option<ConstantOrIrValue>,
        else_tp: &QType,
        else_c: &Option<ConstantOrIrValue>,
        loc: &ast::Loc,
    ) -> R<QType> {
        use ConstantOrIrValue as C;
        let is_null = |c: &Option<C>| -> bool {
            match c {
                None => false,
                Some(C::Float(_)) | Some(C::Double(_)) => false,
                Some(c) => c.as_constant_u64() == Some(0),
            }
        };
        let is_void_ptr = |tp: &QType| -> bool {
            match &tp.tp {
                Type::Pointer(tp) => match &tp.tp {
                    Type::Void => true,
                    _ => false,
                },
                _ => false,
            }
        };

        let rtp = if then_tp.is_arithmetic_type()
            && else_tp.is_arithmetic_type()
        {
            self.do_arithmetic_conversion(
                then_tp.clone(),
                None,
                else_tp.clone(),
                None,
                false,
            )
            .2
        } else if then_tp.is_pointer()
            && (is_null(else_c) || is_void_ptr(else_tp))
        {
            then_tp.clone()
        } else if (is_null(then_c) || is_void_ptr(then_tp))
            && else_tp.is_pointer()
        {
            else_tp.clone()
        } else if then_tp.is_pointer() && else_tp.is_pointer() {
            // 3.3.15: Furthermore, if both operands are pointers to compatible
            // types or differently qualified versions of a compatible type, the
            // result has the composite type
            let (then_tp, else_tp) = {
                let mut then_tp = then_tp.clone();
                let mut else_tp = else_tp.clone();
                match &mut then_tp.tp {
                    Type::Pointer(elem_tp) => {
                        elem_tp.is_const = false;
                        elem_tp.is_volatile = false;
                    }
                    _ => unreachable!(),
                };
                match &mut else_tp.tp {
                    Type::Pointer(elem_tp) => {
                        elem_tp.is_const = false;
                        elem_tp.is_volatile = false;
                    }
                    _ => unreachable!(),
                };
                (then_tp, else_tp)
            };
            let rtp = Compiler::try_get_composite_type(&then_tp, &else_tp, loc);
            if rtp.is_ok() {
                rtp.unwrap()
            } else {
                c4_fail!(loc, "Incompatible expression type")
            }
        } else {
            let rtp = Compiler::try_get_composite_type(then_tp, else_tp, loc);
            if rtp.is_ok() {
                rtp.unwrap()
            } else {
                c4_fail!(loc, "Incompatible expression type")
            }
        };
        Ok(rtp)
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
                    (None, None) => {
                        let bb_id = self.create_bb();
                        ctx.basic_blocks
                            .insert(id.get_id().to_string(), bb_id.clone());
                        bb_id
                    }
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

        let (tp, v) = self
            .convert_lvalue_and_func_designator(tp, v, true, true, true, true);
        let v = v.unwrap();
        let v = self.convert_to_ir_value(&tp, v);
        let ir_id = match &v {
            ConstantOrIrValue::IrValue(x, false) => x.clone(),
            _ => unreachable!(),
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

        // last case may not have explicit break
        self.c4ir_builder.create_br(&break_bb);
        self.llvm_builder.create_br(&break_bb);

        // default case may be absent
        self.c4ir_builder.set_current_basic_block(&default_bb);
        self.llvm_builder.set_current_basic_block(&default_bb);
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
        self.llvm_builder.create_br(&body_bb);

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

        // before converting return value to ir value, check if it's null
        // pointer; if yes then change its return type to make sure it could
        // pass type checking
        let (tp, r) = if ctx.return_type.is_pointer()
            && r.as_ref().and_then(|r| r.as_constant_u64()) == Some(0)
        {
            (ctx.return_type.clone(), r)
        } else {
            (tp, r)
        };

        let (tp, r) = self
            .convert_lvalue_and_func_designator(tp, r, true, true, true, true);
        let r = r.unwrap();
        let r = self.convert_to_ir_value(&tp, r);
        let ir_id = match &r {
            ConstantOrIrValue::IrValue(x, false) => x.clone(),
            _ => unreachable!(),
        };
        if !tp.is_arithmetic_type()
            && !tp.is_pointer()
            && !tp.is_struct_type()
            && !tp.is_union_type()
        {
            unimplemented!()
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
        self.c4ir_builder
            .create_load(&ret_ir_id, &new_ir_id, &ctx.return_type);
        self.llvm_builder
            .create_load(&ret_ir_id, &new_ir_id, &ctx.return_type);
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
            cond_tp, cond, true, true, true, true,
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
            self.c4ir_builder
                .create_constant(&zero_ir_id, &zero, &cond_tp);
            self.llvm_builder
                .create_constant(&zero_ir_id, &zero, &cond_tp);
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
            &cmp_ir_id,
            ast::Expr_Binary_Op::NEQ,
            is_signed,
            is_fp,
            &cond_ir_id,
            &zero_ir_id,
        );
        self.llvm_builder.create_cmp_op(
            &cmp_ir_id,
            ast::Expr_Binary_Op::NEQ,
            is_signed,
            is_fp,
            &cond_ir_id,
            &zero_ir_id,
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
            // added in C99 and used by glibc headers
            [TS::unsigned(_), TS::long(_), TS::long(_)]
            | [TS::unsigned(_), TS::long(_), TS::long(_), TS::int(_)] => {
                q(Type::UnsignedLong)
            }
            [TS::long(_), TS::long(_)]
            | [TS::long(_), TS::long(_), TS::int(_)] => q(Type::Long),
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
            old_sue_type => {
                let uuid = match old_sue_type {
                    // struct S;
                    // struct S {...};
                    Some((SueType::Struct(su_type), scope))
                        if self.current_scope.same_as(scope) =>
                    {
                        su_type.uuid
                    }
                    Some((SueType::Union(su_type), scope))
                        if self.current_scope.same_as(scope) =>
                    {
                        su_type.uuid
                    }
                    _ => self.get_next_uuid(),
                };

                let mut su_type = SuType { fields: None, uuid };
                let tag_name = if name.0.is_empty() {
                    format!(".{}", su_type.uuid)
                } else {
                    String::from(name.0)
                };
                let ir_type_name = format!(".{}", su_type.uuid);

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

                    let packed_fields = if is_struct {
                        let tp = QType::from(Type::Struct(Box::from(
                            su_type.clone(),
                        )));
                        let (tp, _, _, _) =
                            Compiler::get_struct_layout(&tp.tp, None);
                        match tp {
                            Type::Struct(su_type) => su_type.fields.unwrap(),
                            _ => unreachable!(),
                        }
                    } else {
                        // let's just assume union and bit fields are never
                        // used together
                        let tp = QType::from(Type::Union(Box::from(
                            su_type.clone(),
                        )));
                        let tp_sz =
                            Compiler::get_type_size_and_align_bytes(&tp.tp)
                                .unwrap()
                                .0;

                        let (max_align_field, max_align_field_sz) = su_type
                            .fields
                            .as_ref()
                            .unwrap()
                            .iter()
                            .map(|f| {
                                let (sz, align) =
                                    Compiler::get_type_size_and_align_bytes(
                                        &f.tp.tp,
                                    )
                                    .unwrap();
                                (f, sz, align)
                            })
                            .max_by_key(|t| t.2)
                            .map(|(f, sz, _)| (f.clone(), sz))
                            .unwrap();

                        let mut fs = vec![max_align_field];
                        if tp_sz > max_align_field_sz {
                            fs.push(SuField {
                                name: None,
                                tp: QType::from(Type::Array(
                                    Box::new(QType::from(Type::Char)),
                                    Some(tp_sz - max_align_field_sz),
                                )),
                                bit_field_size: None,
                            });
                        }
                        fs
                    };
                    self.c4ir_builder.update_struct_type(
                        &ir_type_name,
                        &packed_fields,
                        !is_struct,
                    );
                    self.llvm_builder.update_struct_type(
                        &ir_type_name,
                        &packed_fields,
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
            format!(".{}", uuid)
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
            Some((OrdinaryIdRef::TypedefRef(qtype), _)) => {
                let su_uuid = match &qtype.tp {
                    Type::Struct(body) if body.fields.is_none() => body.uuid,
                    Type::Union(body) if body.fields.is_none() => body.uuid,
                    _ => return *qtype.clone(),
                };

                // this is for supporting cases like:
                //   typedef struct S T;
                //   struct S {...};
                //   { T t; ... }
                let su_type: Option<Box<SuType>> =
                    match self.current_scope.lookup_sue_type_by_uuid(su_uuid) {
                        Some(Type::Struct(b)) => Some(b),
                        Some(Type::Union(b)) => Some(b),
                        _ => None,
                    };

                let mut r = *qtype.clone();
                if su_type.is_some() {
                    match &mut r.tp {
                        Type::Struct(b) => *b = su_type.unwrap(),
                        Type::Union(b) => *b = su_type.unwrap(),
                        _ => (),
                    }
                }
                r
            }
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
                    // 3.5.2.1: A structure or union shall not contain a member
                    // with incomplete or function type.
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
                        if sz == 0 && field_name_opt.is_some() {
                            panic!(
                                "{}: Named bit-field cannot have zero width",
                                Compiler::format_loc(sd.1)
                            )
                        }
                        Some(sz)
                    };
                    // unnamed fields is a C11 feature.
                    if field_name_opt.is_none()
                        && bit_field_size.is_none()
                        && !tp.is_struct_type()
                        && !tp.is_union_type()
                    {
                        panic!(
                            "{}: Unnamed field must be struct or union",
                            Compiler::format_loc(sd.1)
                        )
                    }
                    SuField {
                        name: field_name_opt,
                        tp,
                        bit_field_size,
                    }
                })
                .collect();
        r
    }

    // The way we pack bit fields is different from (and less efficient than)
    // clang's behavior. For example, this struct:
    //
    //      struct S {char c; int n:3};
    //
    // with clang, sizeof(struct S) == 4;
    // with our implementation, sizeof(struct S) == 8.
    //
    // Note that the same bit field packing algorithm is also used in other
    // places, e.g. sanitize_initializer().
    fn get_struct_layout(
        tp: &Type,
        field_name: Option<&str>,
    ) -> (Type, u32, u32, Option<(QType, StructFieldOffset)>) {
        let fields = match tp {
            Type::Struct(su_type) => su_type.fields.as_ref().unwrap(),
            _ => unreachable!(),
        };

        let mut selected_field: Option<(QType, StructFieldOffset)> = None;
        let mut check_selected_field =
            |field: &SuField, offset: u32, bit_field_quota: u8| {
                let bit_field_quota = if bit_field_quota != 0 {
                    bit_field_quota
                } else {
                    32
                };
                if field.name.is_some() && field.name.as_deref() == field_name {
                    let offset = if field.bit_field_size.is_none() {
                        StructFieldOffset {
                            offset,
                            bit_field_offset: 0,
                            bit_field_mask: 0,
                            bit_field_rem_bits: 0,
                        }
                    } else {
                        let bit_field_offset = 32 - bit_field_quota;
                        let bit_field_rem_bits =
                            bit_field_quota - field.bit_field_size.unwrap();
                        let bit_field_mask = u32::MAX << bit_field_rem_bits
                            >> bit_field_rem_bits
                            >> bit_field_offset
                            << bit_field_offset;
                        StructFieldOffset {
                            offset,
                            bit_field_offset,
                            bit_field_mask,
                            bit_field_rem_bits,
                        }
                    };
                    selected_field = Some((field.tp.clone(), offset));
                }
            };

        let unnamed_char_field = SuField {
            name: None,
            tp: QType::from(Type::Char),
            bit_field_size: None,
        };

        let mut size: u32 = 0;
        let mut align: u32 = 1;
        let mut bit_field_quota: u8 = 0;
        let mut new_fields: Vec<SuField> = Vec::new();
        for field in fields {
            // get_su_field ensures struct/union does not contain fields of
            // incomplete type.
            let (f_sz, f_align) =
                Compiler::get_type_size_and_align_bytes(&field.tp.tp).unwrap();
            // bit field with width=0 does not affect whole struct align, but
            // does affect field align (at least that's clang's behavior)
            //
            // 3.5.2.1: a bit-field with a width of 0 indicates that no further
            // bit-field is to be packed into the unit in which the previous
            // bit-field, if any, was placed.
            if field.bit_field_size != Some(0) {
                align = max(align, f_align);
            }
            if field.bit_field_size != Some(0)
                && field.bit_field_size.map(|s| s < bit_field_quota)
                    == Some(true)
            {
                check_selected_field(field, size, bit_field_quota);
                bit_field_quota -= field.bit_field_size.unwrap();
            } else {
                bit_field_quota = 0;
                let aligned = Compiler::align_up(size, f_align);
                for _ in 0..aligned - size {
                    new_fields.push(unnamed_char_field.clone());
                }
                size = aligned;

                check_selected_field(field, size, bit_field_quota);

                if field.bit_field_size != Some(0) {
                    if field.bit_field_size.is_none() {
                        let f = SuField {
                            name: field.name.clone(),
                            tp: field.tp.clone(),
                            bit_field_size: None,
                        };
                        new_fields.push(f);
                    } else {
                        // bit field type and mask size were checked in
                        // get_su_field.
                        for _ in 0..4 {
                            new_fields.push(unnamed_char_field.clone());
                        }
                        bit_field_quota = 32 - field.bit_field_size.unwrap();
                    }
                    size += f_sz;
                }
            }
        }

        // 3.5.2.1: There may also be unnamed padding at the end of a structure
        // or union, as necessary to achieve the appropriate alignment were the
        // structure or union to be a member of an array.
        let aligned = Compiler::align_up(size, align);
        for _ in 0..aligned - size {
            new_fields.push(unnamed_char_field.clone());
        }

        let su_type = SuType {
            fields: Some(new_fields),
            uuid: 0,
        };
        (Type::Struct(Box::new(su_type)), size, align, selected_field)
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
        // C89 does not allow type-only params in function definitions; this
        // case will be checked in visit_function_def().
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
        // 3.7.1: A declaration of a parameter as ``array of type'' shall be
        // adjusted to ``pointer to type'', and a declaration of a parameter as
        // ``function returning type'' shall be adjusted to ``pointer to
        // function returning type''.
        fn func_arr_to_ptr(tp: QType) -> QType {
            QType {
                is_const: tp.is_const,
                is_volatile: tp.is_volatile,
                tp: match tp.tp {
                    Type::Array(elem_tp, _) => Type::Pointer(elem_tp),
                    tp @ Type::Function(_, _) => {
                        Type::Pointer(Box::new(QType::from(tp)))
                    }
                    tp => tp,
                },
            }
        }

        use ast::ParamDeclaration_oneof_pd as PD;
        match pd.0.pd.as_ref().unwrap() {
            PD::name(named) => {
                let (is_register, tp) = get_is_register_qtype(named.get_dss());
                let d = (named.get_d(), named.get_d_loc());
                let (tp, name) = self.unwrap_declarator(tp, d, false);
                TypedFuncParam {
                    is_register,
                    tp: func_arr_to_ptr(tp),
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
                    tp: func_arr_to_ptr(tp),
                    name: None,
                }
            }
            PD::type_only_simple(type_only_simple) => {
                let (is_register, tp) =
                    get_is_register_qtype(type_only_simple.get_dss());
                TypedFuncParam {
                    is_register,
                    tp: func_arr_to_ptr(tp),
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
            Some((SCS::REGISTER, _)) => None,
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
                        let ir_id = if linkage == Linkage::EXTERNAL || is_func {
                            String::from(id)
                        } else {
                            format!("{}.{}", id, next_uuid)
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

    fn check_types_for_assign(
        tp_left: &QType,
        tp_right: &QType,
        right: &Option<ConstantOrIrValue>,
        loc_right: &ast::Loc,
    ) {
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
                    &QType::from(tp_l.tp.clone()),
                    &QType::from(tp_r.tp.clone()),
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
                if (is_void(tp_l) || is_void(tp_r))
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
                self.c4ir_builder
                    .create_cast(&dst_ir_id, &dst_tp, &src_ir_id, &src_tp);
                self.llvm_builder
                    .create_cast(&dst_ir_id, &dst_tp, &src_ir_id, &src_tp);
                (dst_tp, Some(ConstantOrIrValue::IrValue(dst_ir_id, false)))
            }
            (Type::Pointer(_), _, Type::Pointer(_)) => (dst_tp, v),

            (_, Some(p @ ConstantOrIrValue::StrAddress(_, _)), _)
            | (_, Some(p @ ConstantOrIrValue::HasAddress(_, _, _)), _)
            | (_, Some(p @ ConstantOrIrValue::IrValue(_, _)), _) => {
                if !emit_ir {
                    (dst_tp, None)
                } else {
                    let src_ir_id = match p {
                        ConstantOrIrValue::IrValue(ir_id, false) => {
                            ir_id.clone()
                        }
                        ConstantOrIrValue::HasAddress(_, _, true)
                        | ConstantOrIrValue::IrValue(_, true) => {
                            unreachable!() // convert_lvalue_and_func_designator
                        }
                        c => {
                            let ir_id = self.get_next_ir_id();
                            self.c4ir_builder
                                .create_constant(&ir_id, c, &src_tp);
                            self.llvm_builder
                                .create_constant(&ir_id, c, &src_tp);
                            ir_id
                        }
                    };
                    let dst_ir_id = self.get_next_ir_id();
                    self.c4ir_builder
                        .create_cast(&dst_ir_id, &dst_tp, &src_ir_id, &src_tp);
                    self.llvm_builder
                        .create_cast(&dst_ir_id, &dst_tp, &src_ir_id, &src_tp);
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
            Type::Struct(body) => body.fields.as_ref().and_then(|_| {
                let (_, sz, align, _) = Compiler::get_struct_layout(tp, None);
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
            let is_const = qtype.is_const || q == TQ::CONST;
            let is_volatile = qtype.is_volatile || q == TQ::VOLATILE;

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
        emit_ir: bool,
    ) -> (QType, Option<ConstantOrIrValue>) {
        use ConstantOrIrValue as C;
        match (tp.tp.clone(), expr.clone()) {
            // do_arr_to_ptr
            (Type::Array(t, sz), Some(C::IrValue(src_ir_id, true)))
                if do_arr_to_ptr =>
            {
                let src_tp =
                    QType::ptr_tp(QType::from(Type::Array(t.clone(), sz)));
                let dst_tp = QType::from(Type::Pointer(t));
                let dst_ir_id = self.get_next_ir_id();
                self.c4ir_builder
                    .create_cast(&dst_ir_id, &dst_tp, &src_ir_id, &src_tp);
                self.llvm_builder
                    .create_cast(&dst_ir_id, &dst_tp, &src_ir_id, &src_tp);
                (dst_tp, Some(C::IrValue(dst_ir_id, false)))
            }
            (Type::Array(t, _), Some(C::HasAddress(ir_id, offset, true)))
                if do_arr_to_ptr =>
            {
                let tp = QType::from(Type::Pointer(t));
                (tp, Some(C::HasAddress(ir_id, offset, false)))
            }
            (Type::Array(t, _), None) if do_arr_to_ptr => {
                let tp = QType::from(Type::Pointer(t));
                (tp, None)
            }
            (Type::Array(t, _), addr @ Some(C::StrAddress(_, _)))
            | (Type::Array(t, _), addr @ Some(C::HasAddress(_, _, false)))
                if do_arr_to_ptr =>
            {
                let tp = QType::from(Type::Pointer(t));
                (tp, addr)
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
            (Type::Function(_, _), Some(c @ C::HasAddress(_, _, _)))
                if do_fun_to_ptr =>
            {
                match self.convert_to_ir_value(&tp, c) {
                    C::IrValue(ir_id, _) => {
                        let tp = QType::from(Type::Pointer(Box::new(tp)));
                        (tp, Some(C::IrValue(ir_id, false)))
                    }
                    _ => unreachable!(),
                }
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
            (t, Some(C::HasAddress(_, _, true)))
                if do_deref_lvalue && !emit_ir =>
            {
                (QType::from(t), None)
            }
            (t @ Type::Struct(_), Some(c @ C::HasAddress(_, _, true)))
            | (t @ Type::Union(_), Some(c @ C::HasAddress(_, _, true)))
                if do_deref_lvalue =>
            {
                // get address
                let ptr_ir_id = match self.convert_to_ir_value(&tp, c) {
                    C::IrValue(ir_id, true) => ir_id,
                    _ => unreachable!(),
                };

                (QType::from(t), Some(C::IrValue(ptr_ir_id, false)))
            }
            (t, Some(c @ C::HasAddress(_, _, true))) if do_deref_lvalue => {
                // get address
                let ptr_ir_id = match self.convert_to_ir_value(&tp, c) {
                    C::IrValue(ir_id, true) => ir_id,
                    _ => unreachable!(),
                };
                // and load
                let ptr_tp = QType::ptr_tp(tp.clone());
                let ir_id = self.get_next_ir_id();
                self.c4ir_builder.create_load(&ir_id, &ptr_ir_id, &ptr_tp);
                self.llvm_builder.create_load(&ir_id, &ptr_ir_id, &ptr_tp);

                (QType::from(t), Some(C::IrValue(ir_id, false)))
            }
            (t, Some(C::IrValue(ir_id, true))) if do_deref_lvalue => {
                let dst_ir_id = self.get_next_ir_id();
                self.c4ir_builder.create_load(&dst_ir_id, &ir_id, &tp);
                self.llvm_builder.create_load(&dst_ir_id, &ir_id, &tp);
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
        use ConstantOrIrValue as C;
        match c {
            C::IrValue(_, _) => c,
            C::StrAddress(ir_id, offset_bytes) => {
                let old_ptr_ir_id = self.get_next_ir_id();
                self.c4ir_builder.create_cast(
                    &old_ptr_ir_id,
                    &QType::char_ptr_tp(),
                    &ir_id,
                    tp,
                );
                self.llvm_builder.create_cast(
                    &old_ptr_ir_id,
                    &QType::char_ptr_tp(),
                    &ir_id,
                    tp,
                );

                let offset_ir_id = self.get_next_ir_id();
                self.c4ir_builder.create_constant(
                    &offset_ir_id,
                    &C::I64(offset_bytes),
                    &QType::from(Type::Long),
                );
                self.llvm_builder.create_constant(
                    &offset_ir_id,
                    &C::I64(offset_bytes),
                    &QType::from(Type::Long),
                );

                let ptr_ir_id = self.get_next_ir_id();
                self.c4ir_builder.create_ptr_add(
                    &ptr_ir_id,
                    &old_ptr_ir_id,
                    &offset_ir_id,
                );
                self.llvm_builder.create_ptr_add(
                    &ptr_ir_id,
                    &old_ptr_ir_id,
                    &offset_ir_id,
                );

                let ir_id = self.get_next_ir_id();
                self.c4ir_builder.create_cast(
                    &ir_id,
                    tp,
                    &ptr_ir_id,
                    &QType::char_ptr_tp(),
                );
                self.llvm_builder.create_cast(
                    &ir_id,
                    tp,
                    &ptr_ir_id,
                    &QType::char_ptr_tp(),
                );
                C::IrValue(ir_id, false)
            }
            C::HasAddress(ir_id, offset_bytes, is_lvalue) => {
                let ptr_tp = if is_lvalue {
                    // when is_lvalue, tp is T but ir_id is T*
                    QType::ptr_tp(tp.clone())
                } else {
                    // otherwise tp is T* and ir_id is T*
                    tp.clone()
                };
                match self.convert_to_ir_value(
                    &ptr_tp,
                    C::StrAddress(ir_id, offset_bytes),
                ) {
                    C::IrValue(ptr_ir_id, false) => {
                        C::IrValue(ptr_ir_id, is_lvalue)
                    }
                    _ => unreachable!(),
                }
            }
            _ => {
                let ir_id = self.get_next_ir_id();
                self.c4ir_builder.create_constant(&ir_id, &c, tp);
                self.llvm_builder.create_constant(&ir_id, &c, tp);
                C::IrValue(ir_id, false)
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
            | C::StrAddress(_, _)
            | C::HasAddress(_, _, _)
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
        emit_ir: bool,
    ) -> (Option<ConstantOrIrValue>, Option<ConstantOrIrValue>, QType) {
        if !tp_x.is_arithmetic_type() || !tp_y.is_arithmetic_type() {
            panic!(
                "programming error: do_arithmetic_conversion() only accepts \
                 arithmetic types"
            )
        }

        use ConstantOrIrValue as C;
        let mut check_link_time_constant = |tp, c| match c {
            Some(C::HasAddress(_, _, _)) if !emit_ir => None,
            Some(c @ C::HasAddress(_, _, _)) => {
                Some(self.convert_to_ir_value(tp, c))
            }
            c => c,
        };
        let x = check_link_time_constant(&tp_x, x);
        let y = check_link_time_constant(&tp_y, y);

        let return_none = x.is_none() || y.is_none();
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
        let (tp_x, x) = self.convert_lvalue_and_func_designator(
            tp_x, x, true, false, false, emit_ir,
        );
        let x = x.unwrap_or_else(|| get_dummy_value(&tp_x));
        let (tp_y, y) = self.convert_lvalue_and_func_designator(
            tp_y, y, true, false, false, emit_ir,
        );
        let y = y.unwrap_or_else(|| get_dummy_value(&tp_y));

        let r = match (&x, &y) {
            (C::IrValue(_, true), _) | (_, C::IrValue(_, true)) => {
                unreachable!() // convert_lvalue_and_func_designator
            }
            (C::HasAddress(_, _, _), _) | (_, C::HasAddress(_, _, _)) => {
                unreachable!() // check_link_time_constant
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
                self.c4ir_builder.create_constant(&new_ir_id_y, &y, &tp_y);
                self.llvm_builder.create_constant(&new_ir_id_y, &y, &tp_y);
                let (new_x, new_y, new_tp) = self.do_arithmetic_conversion(
                    tp_x,
                    Some(x),
                    tp_y,
                    Some(C::IrValue(new_ir_id_y, false)),
                    emit_ir,
                );
                (new_x.unwrap(), new_y.unwrap(), new_tp)
            }
            (_, C::IrValue(_, _)) => {
                let new_ir_id_x = self.get_next_ir_id();
                self.c4ir_builder.create_constant(&new_ir_id_x, &x, &tp_x);
                self.llvm_builder.create_constant(&new_ir_id_x, &x, &tp_x);
                let (new_x, new_y, new_tp) = self.do_arithmetic_conversion(
                    tp_x,
                    Some(C::IrValue(new_ir_id_x, false)),
                    tp_y,
                    Some(y),
                    emit_ir,
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
            cc.c4ir_builder
                .create_cast(&ir_id_new, &tp_new, &ir_id_old, tp_old);
            cc.llvm_builder
                .create_cast(&ir_id_new, &tp_new, &ir_id_old, tp_old);
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
        emit_ir: bool,
    ) -> (QType, Option<ConstantOrIrValue>) {
        if !tp.is_integral_type() {
            panic!(
                "programming error: cannot do integral promotion on \
                 non-integral types"
            )
        }
        use ConstantOrIrValue as C;
        let c = match c {
            Some(C::HasAddress(_, _, _)) if !emit_ir => None,
            Some(c @ C::HasAddress(_, _, _)) => {
                Some(self.convert_to_ir_value(&tp, c))
            }
            c => c,
        };
        let (tp, c) = self.convert_lvalue_and_func_designator(
            tp, c, true, true, true, emit_ir,
        );
        let int_tp = QType::from(Type::Int);
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
            Some(C::IrValue(_, true))
            | Some(C::StrAddress(_, _))
            | Some(C::HasAddress(_, _, _)) => unreachable!(),

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
                self.c4ir_builder
                    .create_cast(&ir_id_new, &tp_new, &ir_id, &tp);
                self.llvm_builder
                    .create_cast(&ir_id_new, &tp_new, &ir_id, &tp);
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
        format!(".{}", self.get_next_uuid())
    }

    fn create_bb(&mut self) -> String {
        let bb_id = format!("bb.{}", self.get_next_uuid());
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
    let pretty_print_ir = env::args()
        .into_iter()
        .find(|x| x == "-emit-llvm")
        .is_some();
    let input_path: Option<String> = env::args()
        .into_iter()
        .filter(|x| x != "-emit-llvm")
        .skip(1)
        .next();

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
    Compiler::visit(translation_unit, pretty_print_ir);
}
