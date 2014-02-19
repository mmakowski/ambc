extern mod rustc = "rustc#0.9";

use rustc::lib::llvm::llvm;

fn main() {
    unsafe {
        // Create our first global context.
        let llvm_context = llvm::LLVMContextCreate();

        // Create our module `module1` and attach our context.
        let llvm_module = "mod1".with_c_str(|buf| {
            llvm::LLVMModuleCreateWithNameInContext(buf, llvm_context)
        });

        // Create a useless builder.
        let builder = llvm::LLVMCreateBuilderInContext(llvm_context);

        // Dump the output of the LLVM module in IR format.
        llvm::LLVMDumpModule(llvm_module);
    }
}
