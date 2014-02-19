extern mod rustc = "rustc#0.9";

use rustc::lib::llvm::llvm;
use rustc::lib::llvm::{Bool, False, True};

fn main() {
    unsafe {
        let llvm_context = llvm::LLVMContextCreate();

        let llvm_module = "mod1".with_c_str(|buf| {
            llvm::LLVMModuleCreateWithNameInContext(buf, llvm_context)
        });

        let builder = llvm::LLVMCreateBuilderInContext(llvm_context);
        let double_type = llvm::LLVMDoubleTypeInContext(llvm_context);
        let func_type = llvm::LLVMFunctionType(double_type, [double_type].as_ptr(), 1, False);
        let func = "f1".with_c_str(|buf| { llvm::LLVMAddFunction(llvm_module, buf, func_type) });
        
        llvm::LLVMDumpModule(llvm_module);
    }
}
