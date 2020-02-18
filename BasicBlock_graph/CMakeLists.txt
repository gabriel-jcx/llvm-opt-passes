add_llvm_library(LLVMBasicBlock_graph MODULE 
   BasicBlock_graph.cpp
   DEPENDS
   PLUGIN_TOOL
   opt
   )
