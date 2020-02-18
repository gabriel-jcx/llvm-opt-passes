#include "llvm/ADT/MapVector.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/BasicBlock.h"
using namespace llvm;
namespace{
   using StatusMap = DenseMap<BasicBlock *, bool>;
   class DFStatus: public StatusMap{
      //friend class deadcodeElimination;
      //Function &F;
      //status(Function &F): F(F){};
      public:
         std::pair<StatusMap::iterator,bool> insert(BasicBlock *BB){
            return StatusMap::insert(std::make_pair(BB,true));
         }
         void completed(BasicBlock *BB){
            (*this)[BB] = false;
         }
         //void (BasicBlock *BB){
         //
         //};
         bool isalive(BasicBlock *BB){
            auto iter = find(BB);
            return iter != end() && iter->second;
         }
   }Status;
}
