# llvm-opt-passes

There are a total of 3 passes.

BasicBlock_graph generates the control flow graph of a program based on Basic Blocks.
java_like_DCE implements java like dead code elimination using liveness analysis.
Loop_Para paralleize loops without dependencies using LLVM IR and OpenMP apis.
