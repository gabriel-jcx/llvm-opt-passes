#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/SmallVector.h"

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Use.h"
#include "llvm/IR/Value.h"
#include "llvm/Transforms/Scalar.h"
#define MY_DEBUG 0

using namespace llvm;
namespace{
   struct InstInfo{
      bool isalive = false;
   };
   class deadcodeElimination{
      private:
         // A bitVector that uses to keep track of the alive instructions
         SmallVector<Instruction*,128> Inst_BitVector;
         Function &F;
         MapVector<Instruction*,InstInfo> Inst_Map;
         inline bool isalive(Instruction *I) {return Inst_Map[I].isalive;}
         int inst_count;

      public:
         deadcodeElimination(Function &F): F(F){
            inst_count = 0;
            // Gather info about the function

            for(Function::iterator b = F.begin(); b != F.end(); ++b){
               inst_count += (*b).size();
            }

            // iterate over the Basic Blocks
            Inst_Map.reserve(inst_count);
            for(Instruction &i : instructions(F)){ 
               //instructions is in "instIterator.h"
               if(is_always_alive(i)){
                  errs() << "Marking " << i << "   alive\n";
                  mark_instruction_alive(&i);
               }
            }
         };
         // Determine if the instruction can be always alive
         void mark_instruction_alive(Instruction *inst){
            InstInfo &i_info = Inst_Map[inst];
            if(i_info.isalive) return;
            i_info.isalive = true;
            Inst_BitVector.push_back(inst);
         }
         // Here we need to add logic to determine if a instruciton
         // may cause exception behavior where the instruction causes
         // seg fault or divided by 0 exceptions.
         bool may_cause_exception(Instruction &i){
            if(i.getOpcode() == Instruction::UDiv or
                  i.getOpcode() == Instruction::SDiv or
                  i.getOpcode() == Instruction::URem or
                  i.getOpcode() == Instruction::SRem){
               Value *divisor = i.getOperand(1);
               auto *ConstInt = dyn_cast<ConstantInt>(divisor);
               return !ConstInt or ConstInt->isZero();
               //if(isa<DivideInst>(i))
            }
            return false;
         }
         bool is_always_alive(Instruction &inst){
            if(may_cause_exception(inst))
               return true; 
            if(inst.mayHaveSideEffects() || inst.mayReadOrWriteMemory()){
               errs() << inst << "  may have side effects\n";
               return true;
            }
            if(isa<ReturnInst>(inst)) return true;
            if(not inst.isTerminator()) return false;
            //if(isa<BranchInst>(inst) or isa<SwitchInst>(inst))
            //   return false;
            return true;
         }
         bool remove_instruction(){
            errs() << "The size of the Inst_BitVector is " << Inst_BitVector.size() << "\n";
            // Reuse the BitVector to reduce memory usage for removing instructions
            // The following loop is used to print all the infomation about
            // the instruction for inspecting if its alive or not.
            for(Function::iterator fi = F.begin();fi!=F.end();++fi){
               BasicBlock &temp_bb = *fi;
               for (BasicBlock::iterator bi = temp_bb.begin();
                     bi != temp_bb.end(); ++bi){
                  errs() << *bi << "    status:";
                  errs() << Inst_Map[&*bi].isalive << "\n";
               }
            }
            errs() << "\nPrinting out the Current status of all instructions!\n";
            for(Instruction &i : instructions(F)){
               if(isalive(&i)) continue;
               if(MY_DEBUG){
                  errs() << "Pushing the instruction " << i << " for elimination!!!\n";
               }
               Inst_BitVector.push_back(&i);
               i.dropAllReferences();
            }
            // Removing!!!
            errs() << "The number of Insts to remove: " << Inst_BitVector.size() << '\n';
            errs() << "Ready to remove instructions!\n";
            for (Instruction *&i : Inst_BitVector){
               //Shouldn't need to check the use_empty here
               if(!i->use_empty()){
                  i->replaceAllUsesWith(UndefValue::get(i->getType()));
               }
               errs() << "Removing instruction: " << *i << '\n';
               i->eraseFromParent();
            }
            return (Inst_BitVector.size() == 0);
         }
         // Function gradually go through the bitvector and find all instruction
         // that might be alive and pop from the bitvector, all the remaining
         // Instruction in the bitvector is dead instruction for elimination.
         void find_alive_instructions(){
            errs() << "Instructions that are in the BitVectors are: \n";
            while(Inst_BitVector.size() != 0){
               Instruction *inst_alive = Inst_BitVector.pop_back_val();
               errs() << *inst_alive << '\n';
               // Iterating over operands: OI = operand iterator
               for(Use &operand_iter:inst_alive->operands()){ 
                  if(Instruction *temp_inst = dyn_cast<Instruction>(operand_iter))
                     mark_instruction_alive(temp_inst);
               }
            }
            errs() << '\n';
         }

         bool eliminate(){
            errs() << "Total number of instructions for function " 
               << F.getName() << " is: "  << inst_count << "\n";
            find_alive_instructions();
            bool res = remove_instruction();
            return res;
         }
   };
}
