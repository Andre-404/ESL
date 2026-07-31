#pragma once
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "BridgeObjects.h"

// LLVM mirror of every runtime object layout, checked against the real C++ types at startup
// Declarations of every function the runtime exports, and the IR-level helpers built on top
class runtime_bridge {
    llvm::IRBuilder<>& _b;
    llvm::Module& _mod;
    llvm::DataLayout _DL;
    std::array<llvm::StructType*, +bridge_ty::_count> _types{};
    llvm::GlobalVariable* _gc_flag = nullptr;
public:
    runtime_bridge(llvm::IRBuilder<>& b, llvm::Module& mod, const llvm::DataLayout& DL);

    llvm::IRBuilder<>& b() const { return _b; }
    llvm::Module& mod() const { return _mod; }
    llvm::LLVMContext& ctx() const { return _b.getContext(); }
    const llvm::DataLayout& DL() const { return _DL; }

    // An ESL Value: an opaque pointer in addrspace 1
    static llvm::Type* eslValType(llvm::LLVMContext& ctx);
    llvm::Type* eslValType() const { return eslValType(_b.getContext()); }

    // Layouts
    llvm::StructType* sty(bridge_ty ty) const { return _types[+ty]; }
    // Byte offset of field `idx`, and the alignment an access to it may assume.
    uint64_t field_off(bridge_ty ty, unsigned idx) const;
    llvm::Align field_align(bridge_ty ty, unsigned idx) const;
    // Where the flexible array member behind `ty` starts (== sizeof of the C++ type).
    uint64_t trailing_off(bridge_ty ty) const;

    // Runtime + helper functions only, by name
    llvm::Function* fn(const std::string& name) const;
    llvm::CallInst* call(
        const std::string& name, llvm::ArrayRef<llvm::Value*> args, const llvm::Twine& valName = ""
    ) const;

    // Handle factories. These decode the NaN-boxed Value; they do NOT typecheck it.
    // For checked construction use RuntimeTypecheck::checked_*.
    RtString str(llvm::Value* val) const;
    RtArr arr(llvm::Value* val) const;
    RtClosure closure(llvm::Value* val) const;
    RtInst inst(llvm::Value* val) const;
    // Classes and rt_arr_stores are never NaN-boxed, so this takes the raw pointer.
    CompClass klass(llvm::Value* rawPtr) const;
    RtArrStore arr_store(llvm::Value* rawPtr) const;

    // The gc::managed header as a constant, for compiler-emitted objects.
    llvm::Constant* const_header(gc::move_state state, object::rt_type type) const;

    // The flag the collector raises to ask threads to take the slow path. Handed to gc_init and
    // read by the write barrier; goes through here so the name only exists in one place.
    llvm::GlobalVariable* gc_flag() const { return _gc_flag; }
private:
    void create_types();
    void declare_runtime_functions();
    void build_ir_helpers();
    // Aborts if any mirrored layout disagrees with the C++ type it mirrors.
    void verify_layouts() const;

    // Creates the function, points the builder at a fresh entry block, runs `body`, verifies it.
    llvm::Function* define_fn(
        const std::string& name, llvm::FunctionType* ty, llvm::GlobalValue::LinkageTypes linkage,
        const std::function<void(llvm::Function*)>& body
    );
};
