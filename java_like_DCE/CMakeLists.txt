add_llvm_library(java_like_DCE MODULE 
   F_Status.cpp
   DCE.cpp
   java_like_DCE_pass.cpp
   DEPENDS
   intrinsics_gen
   PLUGIN_TOOL
   opt
   )
