#include "llvm/ADT/Statistic.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>
#include <unordered_map>

#define DEBUG_TYPE "BasicBlock_graph"
using namespace llvm;
STATISTIC(BasicBlockCounter, "Counts number of basic blocks");
namespace {
  
   struct BasicBlock_graph : public BasicBlockPass{
      static char ID;
      int count;
      std::unordered_map<unsigned long int,unsigned long int> nodes;
      //llvm::raw_fd_ostream *outfile;
      //std::error_code EC;
      BasicBlock_graph() : BasicBlockPass(ID){
      //   fp = fopen("bb_cfg.dot","w");
         freopen("temp.dot","w",stdout);
         //outfile = new raw_fd_ostream(llvm::StringRef("bb_cfg.dot"),EC, llvm::sys::fs::OF_None);
         count = 0;
      }
      bool doInitialization(Function &F) override{
         outs() << "digraph \"Graph for basic block\" {\n"; 
         outs() <<"\tlabel=\"CFG for basic block\";\n";
      }
      bool runOnBasicBlock(BasicBlock &BB) override {
         ++BasicBlockCounter;
         ++count;
         fprintf(stderr,"Successfully cast as string %u\n",&BB);
         errs() << "BasicBlock #" << BasicBlockCounter << '\n';
         errs() << "BasicBlock_graph:" << BB.getName() << '\n';
         outs() << "\tNode" << &BB << " [shape=record,label=\"{"<<BB.getName()<<":\\l ";
         //outs() << BB.getTerminator() << '\n';
         //outs() << BB.getInstList().getName();
         //BasicBlock::iterator e = BB.end();
         for (BasicBlock::iterator i = BB.begin(), e=BB.end(); i!=e; ++i){
            Instruction *ii = &*i;
            //fprintf(stdout,"%s\n",(char *)*ii);
            //outfile << *ii << "\n"; 
            outs() << *ii << "\\l ";
            //outs() << i->getOpcodeName() << " " << i->getOperand(0)->getName() << '\n';
         }
         outs() << "}\"];\n";
         errs() << "The current Basic Block is: " << BB.getName() << '\n';
         if(BB.getSinglePredecessor() == nullptr){
            errs() << "Multiple Predecessor\n";
         }else{
            errs() << "Predecessor of the current block is: ";
            BasicBlock *bb = BB.getSinglePredecessor();
            unsigned long int BB_addr = reinterpret_cast<unsigned long int>(&BB);
            unsigned long int bb_addr = reinterpret_cast<unsigned long int>(&*bb); 
            std::unordered_map<unsigned long int, unsigned long int>::const_iterator i = nodes.find(bb_addr);
            if(i == nodes.end()){
               errs() << "Not found!\n";
               nodes.insert({bb_addr,BB_addr});
               outs() << "\tNode" << &*bb << " -> " << "Node" << &BB << ";\n";
            }
            else{
               if(i->second == BB_addr){
                  errs() << "AH HA!\n";
               }else{
                  outs() << "\tNode" << &*bb << " -> " << "Node" << &BB << ";\n";
               }
            }
            errs().write_escaped(BB.getSinglePredecessor()->getName());
            errs() << '\n';
            
         }
         if(BB.getSingleSuccessor() != nullptr){
            errs() << "Successor of the current block is: ";
            errs().write_escaped(BB.getSingleSuccessor()->getName());
            BasicBlock *bb = BB.getSingleSuccessor();
            unsigned long int BB_addr = reinterpret_cast<unsigned long int>(&BB);
            unsigned long int bb_addr = reinterpret_cast<unsigned long int>(&*bb); 
            std::unordered_map<unsigned long int, unsigned long int>::const_iterator i = nodes.find(BB_addr);
            if(i == nodes.end()){
               errs() << "Not found!\n";
               nodes.insert({BB_addr,bb_addr});
               outs() << "\tNode" << &BB << " -> " << "Node" << &*bb << ";\n";
            }
            else{
               if(i->second == BB_addr){
                  errs() << "AH HA!\n";
               }else{
                  outs() << "\tNode" << &BB << " -> " << "Node" << &*bb << ";\n";
               }
            }
            errs() << '\n';
         }
         errs() << '\n';
         return false;
      }
      bool doFinalization(Function &F) override{
         //std::ofstream file;
         //file.open("BasicGraph_dot");  
         outs() << "}";
         errs() << "Finished!!!\n";
      }
   };
}
char BasicBlock_graph::ID = 0;
static RegisterPass<BasicBlock_graph> X("BasicBlock_graph","My Own Pass");
