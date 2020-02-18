#include "llvm/Pass.h"
#include "llvm/IR/Function.h"
#include "DCE.cpp"
// Note:
//       1. An instruction can only be removed from parent when nothing else is using.
// This file register for the pass, the main DCE sit in the DCE.cpp file
using namespace llvm;
namespace {
   struct java_like_DCE : public FunctionPass{
      static char ID;
      java_like_DCE(): FunctionPass(ID){
         //Constructor for class don't know what's needed yet! XD
         //Guess i don't have to do anything here in this struct;
      }
      bool runOnFunction(Function &F) override{
         bool modified = deadcodeElimination(F).eliminate();

         return modified;

      }
   };
}
char java_like_DCE::ID = 1;
static RegisterPass<java_like_DCE> X("java_like_DCE","watsup");
