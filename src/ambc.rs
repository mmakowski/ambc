extern mod rustc = "rustc#0.9";

use rustc::lib::llvm::llvm;
use rustc::lib::llvm::{False};

fn main() {
    unsafe {
        let llvm_context = llvm::LLVMContextCreate();

        let llvm_module = "mod1".with_c_str(|buf| {
            llvm::LLVMModuleCreateWithNameInContext(buf, llvm_context)
        });

        let builder = llvm::LLVMCreateBuilderInContext(llvm_context);
        let int_type = llvm::LLVMIntTypeInContext(llvm_context, 64);
        let func_type = llvm::LLVMFunctionType(int_type, [int_type].as_ptr(), 1, False);
        let func = "f1".with_c_str(|buf| { llvm::LLVMAddFunction(llvm_module, buf, func_type) });
        let block = "f1_start".with_c_str(|buf| { llvm::LLVMAppendBasicBlockInContext(llvm_context, func, buf) });
        llvm::LLVMPositionBuilderAtEnd(builder, block);
        let ret_val = llvm::LLVMConstInt(int_type, 42, False);
        llvm::LLVMBuildRet(builder, ret_val);

        llvm::LLVMDumpModule(llvm_module);
    }
}
