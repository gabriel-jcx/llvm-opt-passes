add_llvm_library(Loop_Para MODULE
   parallelize.cpp
   DEPENDS
   intrinsics_gen
   PLUGIN_TOOL
   opt
   )
