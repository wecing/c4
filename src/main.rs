use protobuf::ProtobufEnum;
use std::cmp::max;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::ffi::CString;
use std::io;
use std::mem;
use std::ptr;

mod ast;

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

    #[allow(dead_code)]
    Enum(Box<EnumType>),

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

#[derive(Debug, Clone)]
struct EnumType {}

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

    #[allow(dead_code)]
    Enum(Box<EnumType>),
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

    #[allow(dead_code)]
    EnumRef(Box<EnumType>),

    // ObjFnRef(ir_id, tp, linkage, is_defined)
    // since it's a lvalue, the IR id refers to a pointer to `tp`.
    ObjFnRef(String, QType, Linkage, bool),
}

#[derive(Debug, Clone)]
enum ConstantOrIrValue {
    I8(i8),
    U8(u8),
    I16(i16),
    U16(u16),
    I32(i32),
    U32(u32),
    I64(i64),
    U64(u64),
    Float(f32),
    Double(f64),
    // Addresses may only be used together with pointer or array types.
    //
    // Unlike IrValue, the ir_id of Address could be looked up in
    // Compiler::global_constants and is guaranteed to exist.
    Address(String, i64),  // ir_id, offset_bytes
    IrValue(String, bool), // ir_id, is_lvalue
}

#[derive(Debug, Clone)]
enum Initializer {
    Expr(QType, ConstantOrIrValue),
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

    fn create_cast(
        &mut self,
        dst_ir_id: String,
        dst_tp: &QType,
        src_ir_id: String,
        src_tp: &QType,
    );
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

    fn create_cast(
        &mut self,
        _dst_ir_id: String,
        _dst_tp: &QType,
        _src_ir_id: String,
        _src_tp: &QType,
    ) {
    }
}

#[cfg(feature = "llvm-sys")]
struct LLVMBuilderImpl {
    context: llvm_sys::prelude::LLVMContextRef,
    module: llvm_sys::prelude::LLVMModuleRef,
    builder: llvm_sys::prelude::LLVMBuilderRef,

    next_uuid: u32,

    current_function: llvm_sys::prelude::LLVMValueRef,
    basic_blocks: HashMap<String, llvm_sys::prelude::LLVMBasicBlockRef>,
    // key: ir_id
    symbol_table: HashMap<String, llvm_sys::prelude::LLVMValueRef>,
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
                basic_blocks: HashMap::new(),
                symbol_table: HashMap::new(),
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
                Type::Enum(_) => {
                    llvm_sys::core::LLVMInt32TypeInContext(self.context)
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
                Initializer::Expr(_, C::IrValue(ir_id, is_lvalue)) => {
                    let value = *self.symbol_table.get(ir_id).unwrap();
                    if *is_lvalue {
                        let name_c =
                            CString::new(self.get_next_tmp_ir_id()).unwrap();
                        unsafe {
                            llvm_sys::core::LLVMBuildLoad(
                                self.builder,
                                value,
                                name_c.as_ptr(),
                            )
                        }
                    } else {
                        value
                    }
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
                    let msg =
                        "Function declaration lists shall not be used \
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
                            .visit_declaration_specifiers(decl.get_dss(), true);
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
            self.add_declaration(
                &param.name.clone().unwrap(),
                &param_scs,
                param.tp.clone(),
                false,
                fd.get_d_loc(),
            );
        });
        let linkage = match self.current_scope.ordinary_ids_ns.get(&fname) {
            Some(OrdinaryIdRef::ObjFnRef(_, _, linkage, _)) => linkage.clone(),
            _ => unreachable!(),
        };
        let param_ir_ids = typed_func_params
            .into_iter()
            .map(|p| p.name.unwrap())
            .map(|name| match self.current_scope.ordinary_ids_ns.get(&name) {
                Some(OrdinaryIdRef::ObjFnRef(ir_id, _, _, _)) => ir_id.clone(),
                _ => unreachable!(),
            })
            .collect();
        self.c4ir_builder
            .create_function(&fname, &ftp, linkage, &param_ir_ids);
        self.llvm_builder
            .create_function(&fname, &ftp, linkage, &param_ir_ids);

        let entry_bb_id = format!("$entry.{}", self.get_next_uuid());
        self.c4ir_builder.create_basic_block(&entry_bb_id);
        self.llvm_builder.create_basic_block(&entry_bb_id);
        self.c4ir_builder.set_current_basic_block(&entry_bb_id);
        self.llvm_builder.set_current_basic_block(&entry_bb_id);

        fd.get_body()
            .get_dls()
            .into_iter()
            .for_each(|dl| self.visit_declaration(dl));
        // struct FuncDefCtx {
        //   // user-provided label name => basic block name
        //   basic_blocks: HashMap<String, String>,
        //   unresolved_labels: HashSet<String>
        // }
        fd.get_body()
            .get_stmt_idxes()
            .into_iter()
            .map(|idx| &self.translation_unit.statements[*idx as usize])
            .for_each(|_stmt| {
                unimplemented!() // TODO: implement statements
            });

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

    // TODO: `size_required` is a bad choice of name. either rename it to
    //       something like `has_declarator`, or ensure the struct is defined
    //       in get_su_type().
    fn visit_declaration_specifiers<'a>(
        &mut self,
        dss: &'a [ast::DeclarationSpecifier],
        size_required: bool,
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
            self.get_type(&type_specifiers, size_required),
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
                    Some((OrdinaryIdRef::EnumRef(_), _)) => {
                        unimplemented!() // TODO: support enums
                    }
                    Some((OrdinaryIdRef::ObjFnRef(ir_id, tp, _, _), _)) => {
                        if fold_constant {
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

                let ir_id = format!("$.{}", self.get_next_uuid());
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

                let ir_id = format!("$.{}", self.get_next_uuid());
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
            ast::Expr_oneof_e::func_call(_) => unimplemented!(),
            ast::Expr_oneof_e::dot(_) => unimplemented!(),
            ast::Expr_oneof_e::ptr(_) => unimplemented!(),
            ast::Expr_oneof_e::sizeof_val(_) => unimplemented!(),
            ast::Expr_oneof_e::sizeof_tp(_) => unimplemented!(),
            ast::Expr_oneof_e::unary(_) => unimplemented!(),
            ast::Expr_oneof_e::binary(_) => unimplemented!(),
            ast::Expr_oneof_e::ternary(_) => unimplemented!(),
            // TODO: implement all expressions
        }
    }

    fn visit_cast_expr(
        &mut self,
        e: L<&ast::Expr_Cast>,
        fold_constant: bool,
        emit_ir: bool,
    ) -> (QType, Option<ConstantOrIrValue>) {
        let type_specifiers: Vec<L<&ast::TypeSpecifier>> =
            e.0.get_tp()
                .get_sp_qls()
                .into_iter()
                .flat_map(|spql| match &spql.elem {
                    Some(ast::TypeName_SpQl_oneof_elem::sp(sp)) => {
                        Some((sp, spql.get_loc())).into_iter()
                    }
                    _ => None.into_iter(),
                })
                .collect();
        let type_qualifiers: Vec<L<ast::TypeQualifier>> =
            e.0.get_tp()
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
        let dst_tp = self.unwrap_abstract_declarator(
            dst_tp,
            (e.0.get_tp().get_ad(), e.0.get_tp().get_ad_loc()),
        );

        let expr = &self.translation_unit.exprs[e.0.e_idx as usize];
        let (src_tp, v) =
            self.visit_expr((expr, e.0.get_e_loc()), fold_constant, emit_ir);

        // 3.2.2.1: auto conversion of array lvalues
        let (src_tp, v) = Compiler::convert_array_lvalue(src_tp, v);

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
                Compiler::format_loc(e.1)
            ),

            (Type::Float, _, Type::Pointer(_))
            | (Type::Double, _, Type::Pointer(_))
            | (Type::Pointer(_), _, Type::Float)
            | (Type::Pointer(_), _, Type::Double) => panic!(
                "{}: Cannot cast floating point values from/to pointers",
                Compiler::format_loc(e.1)
            ),

            (_, None, _) => (dst_tp, None),
            (Type::Enum(_), _, _) | (_, _, Type::Enum(_)) => unimplemented!(),

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
                        ConstantOrIrValue::IrValue(ir_id, true) => {
                            // lvalues need to be deref-ed first
                            let dst_ir_id = self.get_next_ir_id();
                            self.c4ir_builder.create_load(
                                dst_ir_id.clone(),
                                ir_id.clone(),
                                &src_tp,
                            );
                            self.llvm_builder.create_load(
                                dst_ir_id.clone(),
                                ir_id.clone(),
                                &src_tp,
                            );
                            dst_ir_id
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

    fn visit_arr_sub_expr(
        &mut self,
        arr_e: L<&ast::Expr>,
        sub_e: L<&ast::Expr>,
        fold_constant: bool,
        emit_ir: bool,
    ) -> (QType, Option<ConstantOrIrValue>) {
        let (ptr_tp, ptr) = self.visit_expr(arr_e, fold_constant, emit_ir);
        let (sub_tp, sub) = self.visit_expr(sub_e, fold_constant, emit_ir);
        let (ptr_tp, ptr) = Compiler::convert_array_lvalue(ptr_tp, ptr);
        let elem_tp = match ptr_tp.tp {
            Type::Pointer(tp) => match tp.tp {
                Type::Function(_, _) | Type::Void => panic!(
                    "{}: Illegal element type for array subscripting",
                    Compiler::format_loc(arr_e.1)
                ),
                _ => *tp,
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
                // TODO: T a[b] = * (T *) ((void *)a + (b * sizeof(T)))
                unimplemented!()
            }
        };
        (elem_tp, r)
    }

    fn get_type(
        &mut self,
        tss: &Vec<L<&ast::TypeSpecifier>>,
        size_required: bool,
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
                q(self.get_struct_type((s, tss[0].1), size_required))
            }
            [TS::union(u)] => {
                q(self.get_union_type((u, tss[0].1), size_required))
            }
            [TS::field_enum(e)] => q(self.get_enum_type((e, tss[0].1))),
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
        size_required: bool,
    ) -> Type {
        let name = (s.0.get_name(), s.0.get_name_loc());
        let bodies = s.0.get_bodies().iter().zip(s.0.get_body_locs()).collect();
        self.get_su_type(name, bodies, size_required, true)
    }

    fn get_union_type(
        &mut self,
        s: L<&ast::TypeSpecifier_Union>,
        size_required: bool,
    ) -> Type {
        let name = (s.0.get_name(), s.0.get_name_loc());
        let bodies = s.0.get_bodies().iter().zip(s.0.get_body_locs()).collect();
        self.get_su_type(name, bodies, size_required, false)
    }

    fn get_su_type(
        &mut self,
        name: L<&str>,
        // `struct S {}` is illegal syntax
        bodies: Vec<L<&ast::StructDeclaration>>,
        size_required: bool,
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
        let try_ref = bodies.is_empty() && size_required;

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
            Some((SueType::Enum(_), scope))
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
            Some((SueType::Struct(su_type), scope))
                if self.current_scope.same_as(scope) =>
            {
                Type::Struct(Box::new(*su_type.clone()))
            }
            Some((SueType::Union(su_type), scope))
                if self.current_scope.same_as(scope) =>
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

    fn get_enum_type(&mut self, _s: L<&ast::TypeSpecifier_Enum>) -> Type {
        unimplemented!() // TODO: implement enum
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
                let names = FuncParams::Names(ids_list.ids.clone().into_vec());
                tp = QType::from(Type::Function(Box::new(tp), Some(names)));
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

    fn get_composite_type(old: &QType, new: &QType, loc: &ast::Loc) -> QType {
        macro_rules! incompatible_panic {
            () => {
                panic!(
                    "{}: Type incompatible with previous declaration",
                    Compiler::format_loc(loc)
                )
            };
            ($msg:expr) => {
                panic!(
                    "{}: Type incompatible with previous declaration; {}",
                    Compiler::format_loc(loc),
                    $msg
                )
            };
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
            (Type::Enum(_tp_left), Type::Enum(_tp_right)) => {
                unimplemented!() // TODO: implement enum
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
                    Type::Array(_, _) => panic!(
                        "{}: Function cannot return array types",
                        Compiler::format_loc(loc)
                    ),
                    Type::Function(_, _) => panic!(
                        "{}: Function cannot return function types",
                        Compiler::format_loc(loc)
                    ),
                    _ => {}
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
                        let tps: Vec<TypedFuncParam> = tps_left
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
                                TypedFuncParam {
                                    is_register: tp_left.is_register,
                                    tp: Compiler::get_composite_type(
                                        &tp_left.tp,
                                        &tp_right.tp,
                                        loc,
                                    ),
                                    name,
                                }
                            })
                            .collect();
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

        QType {
            is_const: new.is_const,
            is_volatile: new.is_volatile,
            tp,
        }
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
                Some((
                    (&sz_align_vec).into_iter().map(|p| p.0).max().unwrap(),
                    (&sz_align_vec).into_iter().map(|p| p.1).max().unwrap(),
                ))
            }),
            Type::Enum(_) => unimplemented!(),
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

    // 3.2.2.1: Except when it is the operand of the sizeof operator or the
    // unary & operator, or is a character string literal used to initialize an
    // array of character type, or is a wide string literal used to initialize
    // an array with element type compatible with wchar_t, an lvalue that has
    // type ``array of type'' is converted to an expression that has type
    // ``pointer to type'' that points to the initial member of the array object
    // and is not an lvalue.
    fn convert_array_lvalue(
        tp: QType,
        expr: Option<ConstantOrIrValue>,
    ) -> (QType, Option<ConstantOrIrValue>) {
        match (tp.tp.clone(), expr.clone()) {
            (
                Type::Array(t, _),
                Some(ConstantOrIrValue::IrValue(ir_id, true)),
            ) => (
                QType {
                    is_const: tp.is_const,
                    is_volatile: tp.is_volatile,
                    tp: Type::Pointer(t),
                },
                Some(ConstantOrIrValue::IrValue(ir_id, false)),
            ),
            _ => (tp, expr),
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
            Type::Enum(_) => unimplemented!(), // TODO: support enums
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
}

fn main() {
    let protobuf_result =
        ::protobuf::parse_from_reader::<ast::TranslationUnit>(&mut io::stdin());
    let translation_unit = match protobuf_result {
        Ok(tu) => tu,
        Err(e) => panic!(String::from(e.description())),
    };
    Compiler::visit(translation_unit);
}
