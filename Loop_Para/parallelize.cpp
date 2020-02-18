#include "llvm/Pass.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Analysis/LoopInfo.h"
#include "llvm/Analysis/ScalarEvolution.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/DomTreeUpdater.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"

using namespace llvm;

FunctionType *kmpc_microTy = nullptr;
StructType *ident_t = nullptr;
FunctionType *kmpc_fork_FuncTy = nullptr;
int loop_upper_bound = 0; // this value is then calculated by adding trip count to lb
int loop_stride = 1; // Assumption: This value is 1!
int loop_lower_bound = 0; // Assumption: This lower bound start with 0!

FunctionType *get_or_create_kmpc_micro_ptrTy(LLVMContext &Context){
    if(kmpc_microTy == nullptr){
        auto *Int32PtrTy = PointerType::getUnqual(Type::getInt32Ty(Context));
        Type *microTy_Params[] = {Int32PtrTy,Int32PtrTy};
        kmpc_microTy = FunctionType::get(Type::getVoidTy(Context),
                microTy_Params,true);
    }
    return kmpc_microTy;
}
PointerType *get_kmpc_micro_ptrTy(LLVMContext &Context){
    return PointerType::getUnqual(get_or_create_kmpc_micro_ptrTy(Context));
}

namespace{
    struct Parallelize: public ModulePass{
        private:
            bool modified;
            Constant *DefaultOpenMPPSource;
            Constant *DefaultOpenMPLocation;

            // Constant for omp function definitions so that
            // the functions can be referred later
            Constant *kmpc_fork_const;
            Constant *omp_outlined_const;
            Constant *kmpc_init_const;
            Constant *kmpc_barrier_const;
            Constant *kmpc_fini_const;

            // Function type for the omp_outlined function
            // & Function.
            FunctionType *Outlined_FuncTy;
            Function *Outlined_Fn;

            // This vector would hold all the basic blocks in the OMP functions.
            std::vector<BasicBlock*> OMP_BBs;

            // This vector would hold all the Basic Blocks within the loop
            // in the original function
            std::vector<BasicBlock*> Loop_BBs;
            BasicBlock *Omp_For_End_BB;
            BasicBlock *Loop_Parent;
        public:
            static char ID;
            Parallelize(): ModulePass(ID){
                // Initialize all variables here if needed
            }

            /* Class Function Declarations */
            void Declare_Funcs(Module &M, StructType*);
            Constant *Global_Decal_kmpc(Module &M, StructType*);
            StructType *Gen_Struct(Module& M);
            Function *Format_Func_To_Task(Module* M, CallInst *Call);
            void Create_Basic_Blocks(Module *M);

            // Don't think we need getAnalysis Usage yet.
            // For now, most analysis Wrapper Pass are implemented on Function Pass
            // While, our pass is a Module Pass
            void getAnalysisUsage(AnalysisUsage &AU) const override{
                AU.setPreservesCFG();
                AU.addRequired<LoopInfoWrapperPass>();
                AU.addRequired<ScalarEvolutionWrapperPass>();
            }
            bool doInitialization(Module &M) override{

                // Generate the ident_t used for _kmpc_fork_call_
                ident_t = Gen_Struct(M);
                Declare_Funcs(M,ident_t);
                // Create the default location for the 
                DataLayout DL(&M);
                uint64_t Alignment = DL.getTypeAllocSize(Type::getInt8Ty(M.getContext()));
                const std::string DefaultLocStr = ";unknown;unknown;0;0;;";
                StringRef DefaultLoc(DefaultLocStr.c_str(), DefaultLocStr.size()+1);
                Constant *C = ConstantDataArray::getString(
                        M.getContext(),DefaultLoc,false);
                auto *GV = new GlobalVariable(M, C->getType(),true,
                        GlobalVariable::PrivateLinkage, C, ".str",nullptr,
                        GlobalValue::NotThreadLocal);
                GV->setAlignment(Alignment);
                GV->setUnnamedAddr(GlobalVariable::UnnamedAddr::Global);
                DefaultOpenMPPSource = cast<Constant>(GV);
                DefaultOpenMPPSource = ConstantExpr::getBitCast(
                        DefaultOpenMPPSource, Type::getInt8PtrTy(M.getContext()));

                DefaultOpenMPLocation = Global_Decal_kmpc(M,ident_t);
                LLVMContext &c = M.getContext();
                auto *VoidTy = Type::getVoidTy(c);
                auto *Int32PtrTy = Type::getInt32PtrTy(c);

                Outlined_FuncTy = FunctionType::get(VoidTy,{Int32PtrTy,Int32PtrTy},false);
                Outlined_Fn = Function::Create(Outlined_FuncTy,
                        GlobalValue::InternalLinkage,".omp_outlined.",M);
                omp_outlined_const = cast<Constant>(Outlined_Fn);
                omp_outlined_const = ConstantExpr::getBitCast(
                        omp_outlined_const,get_kmpc_micro_ptrTy(M.getContext()));

                // Generate the Global location

                return true;
            }
            bool runOnModule(Module &M) override{
                errs() << "Running on Module\n";
                modified = true;
                Type *Int32Ty = Type::getInt32Ty(M.getContext());
                Type *VoidTy = Type::getVoidTy(M.getContext());
                Type *Int32PtrTy = Type::getInt32PtrTy(M.getContext());
                CallInst *call = nullptr; //CallIns//t::Create(VoidTy, nullptr,{});

                // Looping through the First and only function to
                // obtain information about the loop
                // e.g. loop depth 
                Module::iterator F = M.begin();
                LoopInfo *LI = &getAnalysis<LoopInfoWrapperPass>(*F).getLoopInfo();

                /* Printing the Loop Infos from Loop Analysis Pass */
                errs() << "***********************LOOP INFO START****************************\n";
                LI->print(errs());
                errs() << "***********************LOOP INFO END****************************\n";

                // Dumn flag for only counting the loop depth for once
                bool first = false;
                for(Function::iterator BB = F->begin(); BB!=F->end();++BB){
                    Loop* l = LI->getLoopFor(&*BB);
                    if(l == nullptr) continue;
                    errs() << l->getStartLoc() << "\n";
                    
                    errs() << "Pushing \"" <<BB->getName()<< "\" into the loop BasicBlock Vector\n";
                    Loop_BBs.push_back(&*BB);
                    if(not first){
                        first = true;
                        ScalarEvolution *SE=&getAnalysis<ScalarEvolutionWrapperPass>(*F).getSE();
                        if (l != nullptr) {
                            // getSmallConstantTripCount returns the maximum trip count of the loop
                            // if it is a single-exit loop 
                            //
                            Loop_Parent = &*--BB;
                            BB++;
                            errs() << "Loop parent is \n" << *Loop_Parent;
                            loop_upper_bound = 
                                SE->getSmallConstantTripCount(l)-2+loop_lower_bound;
                        }
                    }
                    for(BasicBlock::iterator BI = BB->begin(); BI != BB->end(); ++BI){
                        // add code here to analyze the loop and get info
                    }                            
                }                                
                /* Need to analyze the loop here to retrive lower bound and upper bound */
                auto &for_cond_BB = Loop_BBs[0];
                for(auto &inst : *for_cond_BB){
                    if(isa<PHINode>(inst)){
                        PHINode *phi = cast<PHINode>(&inst);
                        loop_lower_bound = cast<ConstantInt>(phi->getIncomingValue(0))->getSExtValue();
                    }

                }
                for(auto &F : *M.begin()){
                    errs() <<  F << "\n";
                }

                // Modify here for real loop parallelization
                BasicBlock::iterator BI = M.begin()->begin()->begin(); 
                Function *fork = cast<Function>(kmpc_fork_const); // dont need this?
                Instruction &inst = *BI;
                IRBuilder<> builder(&inst);
                std::vector<Value*> fork_args = {
                    DefaultOpenMPLocation,
                    ConstantInt::get(Int32Ty,0,true),
                    omp_outlined_const
                        // This last argument is incorrect i think!!
                };
                call = builder.CreateCall(kmpc_fork_const,fork_args);

                Function *extract = Format_Func_To_Task(&M,call);


                for(Function::iterator BB = F->begin(); BB!=F->end();++BB){
                    errs() << &*BB << " = " << Loop_Parent<< "\n";
                    if(&*BB == Loop_Parent){
                        errs() << "Reachinged here\n";
                        IRBuilder<> build(&*BB);
                        Instruction *inst = BB->getTerminator();
                        BranchInst *new_branch = build.CreateBr(&*++BB);
                        inst->eraseFromParent();
                        break;
                    }
                }
                // Loop over the created threaded function
                // ********* FOR DEBUGGING PURPOSE *********
                errs() << "**********************Extracted Func Start**************************" << "\n";
                errs() << *extract << "\n";
                errs() << "**********************Extracted Func End**************************" << "\n";
                for(auto &F : *M.begin()){
                    errs() << F;
                }

                errs() << "finished\n";                  
                return modified;                         
            }                                            
    };

}// anon namespace end



void Parallelize::Create_Basic_Blocks(Module *M){
    LLVMContext &C = M->getContext();
    std::vector<std::string> BB_Names = {"entry", "cond.true","cond.false","cond.end",
        "omp.loop.exit"};
    for(auto &BB_iter:Loop_BBs){
        errs() << *BB_iter << "\n";
    }
    for(auto &BB_iter: BB_Names){
        BasicBlock* BB = BasicBlock::Create(C, BB_iter,Outlined_Fn,nullptr);
        if(BB_iter == "omp.loop.exit"){
            for(auto &loop_BB:Loop_BBs){
                loop_BB->moveBefore(BB);
                OMP_BBs.push_back(&*loop_BB);
            }
            Omp_For_End_BB = BasicBlock::Create(C,
                    "omp.inner.for.end",Outlined_Fn,BB);
            OMP_BBs.push_back(Omp_For_End_BB);

        }
        OMP_BBs.push_back(BB);
    }

}

// Setup the omp_outlined function in detail
Function *Parallelize::Format_Func_To_Task(Module* M, CallInst *Call){
    Type *Int32Ty = Type::getInt32Ty(M->getContext());
    std::vector<Value*> LoadedArgs;
    for(auto& arg:Call->arg_operands()){
        LoadedArgs.push_back(arg); 
    }
    LLVMContext &C = M->getContext();
    DataLayout DL(M);
    StringRef Arg_Names[] = {".global_tid",".bound_tid"};
    auto *Int32PtrTy = Type::getInt32PtrTy(M->getContext());
    Create_Basic_Blocks(M);

    std::vector<Value*> out_args;
    for (auto &arg: Outlined_Fn->args()){
        arg.setName(Arg_Names[out_args.size()]);
        out_args.push_back(&arg);
    }
    Outlined_Fn->setLinkage(GlobalValue::InternalLinkage);

    // Setting the attributes based on the parallelized IR
    Outlined_Fn->addFnAttr(Attribute::NoInline);
    Outlined_Fn->addFnAttr(Attribute::NoRecurse);
    Outlined_Fn->addFnAttr(Attribute::NoUnwind);
    Outlined_Fn->addFnAttr(Attribute::UWTable);
    std::vector<Type*> Outlined_Fn_params;
    for(auto& param:Outlined_Fn->getFunctionType()->params()){
        Outlined_Fn_params.push_back(param);
    }

    /* Construction the entry Basic Block */

    auto *Entry_BB = *OMP_BBs.begin();

    // Creating the IRBuilder for the entry Basic Block
    IRBuilder<> Builder(Entry_BB);
    AllocaInst *global_tid_alloc = Builder.CreateAlloca(
            Int32PtrTy,nullptr,".global_tid..addr");
    global_tid_alloc->setAlignment(8);
    StoreInst *global_tid_store = Builder.CreateStore(
            Outlined_Fn->args().begin(),global_tid_alloc, false);
    global_tid_store->setAlignment(8);
    AllocaInst *bound_tid_alloc = Builder.CreateAlloca(
            Int32PtrTy,nullptr,".bound_tid..addr");
    bound_tid_alloc->setAlignment(8);
    StoreInst *bound_tid_store = Builder.CreateStore(
            Outlined_Fn->args().begin()+1,bound_tid_alloc, false);
    bound_tid_store->setAlignment(8);

    // iv: iteration variable
    // lb: lower bound
    // ub: upper bound
    // stride: should always be 1?
    // Initialize the names of omp variables
    std::vector<std::string> omp_vars_names = {//".omp.iv",
        ".omp.lb",".omp.ub",".omp.stride",".omp.is_last"};

    // This is a temporary variable to hold the values for omp varaibles
    std::vector<int> omp_var_values = {0,loop_upper_bound,1,0};


    auto val_iter = omp_var_values.begin();
    std::vector<Value*> omp_vars;
    for (auto &var_name:omp_vars_names){
        AllocaInst *inst = Builder.CreateAlloca(
                Int32Ty,nullptr,var_name);
        inst->setAlignment(4);
        Value *val = ConstantInt::get(Int32Ty,*val_iter++,true);

        // ************ IMPORTANT NOTE *************
        // push_back alloca instruction for parameters!!
        // This passed in as a parameter would create a pointer to the var
        omp_vars.push_back(inst); 

        StoreInst *omp_var_store = Builder.CreateStore(val,inst, false);
        omp_var_store->setAlignment(4);

    }

    //Initialize parameter for _kmpc_for_static_init_4 and make the function call
    auto *tid_load = Builder.CreateAlignedLoad(global_tid_alloc,8); 
    // AllocaInst is also a type of Value?
    auto *tid_load_param = Builder.CreateAlignedLoad(tid_load,4);
    Constant *init_ident = Global_Decal_kmpc(*M,ident_t);

    std::vector<Value*> init_args = {
        init_ident, /* Source code location*/
        tid_load_param, /* Global thread id of the thread*/
        ConstantInt::get(Int32Ty,34,true), /* Scheduling Type */ // ???? 34, WTH??
        omp_vars[3], /* Pointer to is_last */
        omp_vars[0], /* Pointer to the lower bound */
        omp_vars[1], /* Pointer to the upper bound */
        omp_vars[2], /* Pointer to the stride */
        ConstantInt::get(Int32Ty,1,true), /* Loop Increment */
        ConstantInt::get(Int32Ty,1,true) /* Chunk size */
    };
    CallInst *init_call = Builder.CreateCall(kmpc_init_const,init_args);
    LoadInst *ub_load = Builder.CreateLoad(Int32Ty,omp_vars[1]);
    ub_load->setAlignment(4); 
    Value *icmp_sgt = Builder.CreateICmpSGT(
            ub_load,ConstantInt::get(Int32Ty,loop_upper_bound,true),"cmp");


    Builder.CreateCondBr(icmp_sgt,OMP_BBs[1],OMP_BBs[2]);
    int OMP_BB_Count = 1;
    /* Finish Constructing the entry Basic Block for the omp_outlined Function */


    IRBuilder<> Cond_True_Builder(OMP_BBs[OMP_BB_Count]);
    Cond_True_Builder.CreateBr(OMP_BBs[OMP_BB_Count+2]); //jump over the next block

    OMP_BB_Count++;
    IRBuilder<> Cond_False_Builder(OMP_BBs[OMP_BB_Count]);
    auto *Upper_Bound_Load = Cond_False_Builder.CreateLoad(Int32Ty, omp_vars[1]);
    Upper_Bound_Load->setAlignment(4);

    Cond_False_Builder.CreateBr(OMP_BBs[OMP_BB_Count+1]);

    /* Constructing the cond.end Basic Block */
    OMP_BB_Count++;
    IRBuilder<> Cond_End_Builder(OMP_BBs[OMP_BB_Count]);
    auto *Phi_inst = Cond_End_Builder.CreatePHI(Int32Ty,loop_upper_bound,"cond");
    Phi_inst->addIncoming(ConstantInt::get(Int32Ty,loop_upper_bound,true),OMP_BBs[OMP_BB_Count-2]);
    Phi_inst->addIncoming(Upper_Bound_Load,OMP_BBs[OMP_BB_Count-1]);
    auto *store_ub_inst = Cond_End_Builder.CreateStore(Phi_inst,omp_vars[1],false);
    store_ub_inst->setAlignment(4);
    auto *load_lb_inst = Cond_End_Builder.CreateLoad(Int32Ty,omp_vars[0],false);
    load_lb_inst->setAlignment(4);

    /* Since using PhiNode to store the IV value, maybe we can savely remove this */
    //auto *store_iv_inst = Cond_End_Builder.CreateStore(load_lb_inst,omp_vars[0],false);
    //store_iv_inst->setAlignment(4);
    
    Cond_End_Builder.CreateBr(OMP_BBs[OMP_BB_Count+1]);

    // *************************** Note *********************************
    // Maybe making the following renaming into a function is better 
    // Or using a loop or a better design would be better in terms of readability
    // For now, This looks like hard coding a little

    while(OMP_BBs[++OMP_BB_Count]->getName() != Loop_BBs[0]->getName()); // Incrementing the count while not for.cond
    IRBuilder<> For_Cond_Builder(OMP_BBs[OMP_BB_Count]);
    OMP_BBs[OMP_BB_Count]->setName("omp.inner."+OMP_BBs[OMP_BB_Count]->getName());
    
    // get the current basicblock processing, and loop throught it
    auto *BB = OMP_BBs[OMP_BB_Count];
     
    PHINode* prev_phi;
    PHINode* phi_inst;
    // Logic to re-create the Phi node based on the previous PhiNode
    for(auto &iterator: *BB){
        errs() << iterator << "\n";
        auto *inst = &iterator;
        if(isa<PHINode>(inst)){
            prev_phi = cast<PHINode>(inst);
            std::vector<BasicBlock*> phi_bbs;
            std::vector<Value*> phi_vals;
            phi_inst = PHINode::Create(Int32Ty,0,".omp.iv",prev_phi);
            for(unsigned int i = 0; i < prev_phi->getNumIncomingValues();++i){
                if(prev_phi->getIncomingBlock(i)==Loop_Parent){ // Need to be done
                    phi_bbs.push_back(OMP_BBs[OMP_BB_Count-1]);
                    phi_vals.push_back(load_lb_inst);
                    continue;
                }
                phi_bbs.push_back(prev_phi->getIncomingBlock(i));
                phi_vals.push_back(prev_phi->getIncomingValue(i));
            }
            for(unsigned int i = 0; i < phi_bbs.size(); ++i){
                phi_inst->addIncoming(phi_vals[i],phi_bbs[i]);
            }


            /******************************* Note *********************************
             * There is a bug related with use_begin() and use_end(), it would
             * always point to the same element somehow, believed to be a bug
             * existd in the list implementation in the llvm project
             *********************************************************************/


            // Can't really erase prev_phi here 
            // Since there is another use of the prev_phi inside the for body
            // Need to process the User instructions
            while(not prev_phi->use_empty()){
                errs() << *prev_phi->user_back() << "\n";
                if(isa<CmpInst>(prev_phi->user_back())){
                    errs() << "Assertion should not be here multiple times\n";
                    auto *cmp_inst = cast<CmpInst>(prev_phi->user_back());
                    auto *temp_inst = For_Cond_Builder.CreateAlignedLoad(Int32Ty,omp_vars[1],4);
                    temp_inst->moveBefore(cmp_inst);
                    errs() << *BB << "\n";
                    BranchInst *BB_Term = cast<BranchInst>(BB->getTerminator());
                    Instruction *cond_inst = cast<Instruction>(For_Cond_Builder.CreateICmpSLE(phi_inst,temp_inst));

                    cond_inst->moveBefore(BB_Term);
                    BB_Term->setCondition(cond_inst);
                    cmp_inst->eraseFromParent();
                    

                }else{
                    prev_phi->user_back()->replaceUsesOfWith(prev_phi,phi_inst);

                }
            }
            // The Use of this phi node should be empty at this point
            errs() << "******************DEBUG*******************\n";
            for(auto U = prev_phi->use_begin();U != prev_phi->use_end();++U){
                errs() << **U << "\n";
            }
            errs() << "******************DEBUG*******************\n";
        }
    }
    if(prev_phi->use_empty())
        prev_phi->eraseFromParent();

    // The terminator of a Basic Block is garanteed a branch instruction right?
    errs() << *OMP_BBs[OMP_BB_Count] << "\n";
    BranchInst *BB_Term =cast<BranchInst>(OMP_BBs[OMP_BB_Count]->getTerminator());

    // Create a new branch instruction and delete the old branch instruction
    For_Cond_Builder.CreateCondBr(BB_Term->getCondition(),
            OMP_BBs[OMP_BB_Count+1], Omp_For_End_BB);
    BB_Term->eraseFromParent();
    
    OMP_BB_Count++; //Assumption!! for_body Basic Block is always the next 

    //while(OMP_BBs[++OMP_BB_Count]->getName() != "for.body"); // Incrementing the count while not for.body
    BasicBlock::iterator prev_begin = OMP_BBs[OMP_BB_Count]->begin();
    IRBuilder<> For_Body_Builder(OMP_BBs[OMP_BB_Count],prev_begin);
    OMP_BBs[OMP_BB_Count]->setName("omp.inner."+OMP_BBs[OMP_BB_Count]->getName());
    Value* IV_Mul_Stride = For_Body_Builder.CreateNSWMul(phi_inst,ConstantInt::get(Int32Ty,loop_stride,true),"mul");
    Value* IV_Add_lb = For_Body_Builder.CreateAdd(IV_Mul_Stride,ConstantInt::get(Int32Ty,loop_lower_bound,true),"add");

    // Looping through the previous BasicBlock to replace omp.iv use with Added value
    for(;prev_begin != OMP_BBs[OMP_BB_Count]->end();prev_begin++){
        for(unsigned int i = 0;i < prev_begin->getNumOperands();i++){
            Value *val = prev_begin->getOperand(i);
            if(val == phi_inst){
                prev_begin->replaceUsesOfWith(val,IV_Add_lb);
            }
        }
    }

    
    while(OMP_BBs[++OMP_BB_Count] != Loop_BBs.back()); // Incrementing the count while not for.inc
    OMP_BBs[OMP_BB_Count]->setName("omp.inner."+OMP_BBs[OMP_BB_Count]->getName());

    OMP_BB_Count++;

    IRBuilder<> For_End_Builder(OMP_BBs[OMP_BB_Count]);
    For_End_Builder.CreateBr(OMP_BBs[OMP_BB_Count+1]);

    OMP_BB_Count++;
    IRBuilder<> Omp_Exit_Builder(OMP_BBs[OMP_BB_Count]);
    std::vector<Value*> kmpc_for_fini_args{
        init_ident, /* the ident_t passed into the kmpc_for_init function*/
        tid_load_param,
    };
    Omp_Exit_Builder.CreateCall(kmpc_fini_const,kmpc_for_fini_args);
    Constant *barrier_ident = Global_Decal_kmpc(*M,ident_t);
    Omp_Exit_Builder.CreateCall(kmpc_barrier_const,{barrier_ident,tid_load_param});

    Omp_Exit_Builder.CreateRet(nullptr);


    return Outlined_Fn;
}


Constant* Parallelize::Global_Decal_kmpc(Module &M, StructType* ident_t){
    auto *Int32Ty = Type::getInt32Ty(M.getContext());
    std::vector<Constant *> Members = {
        ConstantInt::get(Int32Ty, 0, true), ConstantInt::get(Int32Ty, 2, true),
        ConstantInt::get(Int32Ty, 0, true), ConstantInt::get(Int32Ty, 0, true),
        DefaultOpenMPPSource};
    Constant *C2 = ConstantStruct::get(ident_t, Members);
    auto *GV2 = new GlobalVariable(M, C2->getType(), true, GlobalValue::PrivateLinkage,
            C2, "", nullptr, GlobalValue::NotThreadLocal);
    GV2->setUnnamedAddr(GlobalValue::UnnamedAddr::Global);
    GV2->setAlignment(8);
    return cast<Constant>(GV2);
}
void Parallelize::Declare_Funcs(Module &M,StructType *ident_t){
    auto *VoidTy = Type::getVoidTy(M.getContext());
    PointerType *identPtr = PointerType::get(ident_t,0);
    auto *Int32Ty = Type::getInt32Ty(M.getContext());
    auto *Int32PtrTy = Type::getInt32PtrTy(M.getContext());


    // Init function decal for _kmpc_fork_call
    Type *kmpc_fork_TypeParams[] = {identPtr, Int32Ty,
        get_kmpc_micro_ptrTy(M.getContext())};
    kmpc_fork_FuncTy = FunctionType::get(VoidTy,
            kmpc_fork_TypeParams,true);
    kmpc_fork_const = M.getOrInsertFunction(
            "__kmpc_fork_call",kmpc_fork_FuncTy);

    // Init function decal for _kmpc_fork_static_fini
    Type *kmpc_fini_TypeParams[] = {identPtr, Int32Ty};
    FunctionType *kmpc_fini_FuncTy = FunctionType::get(VoidTy,
            kmpc_fini_TypeParams,false);
    kmpc_fini_const = M.getOrInsertFunction(
            "__kmpc_for_static_fini", kmpc_fini_FuncTy
            );
    // Init function decal for _kmpc_fork_static_init
    Type *kmpc_static_init_TypeParams[] = {identPtr,   Int32Ty,    Int32Ty,
        Int32PtrTy, Int32PtrTy, Int32PtrTy,
        Int32PtrTy, Int32Ty,    Int32Ty};
    FunctionType *kmpc_static_init_FuncTy = FunctionType::get(
            VoidTy, kmpc_static_init_TypeParams, false);
    kmpc_init_const = M.getOrInsertFunction(
            "__kmpc_for_static_init_4",kmpc_static_init_FuncTy);

    // Init function decal for _kmpc_barrier
    Type *kmpc_barrier_TypeParams[] = {identPtr, Int32Ty};
    FunctionType *kmpc_barrier_FuncTy = FunctionType::get(VoidTy,
            kmpc_barrier_TypeParams,false);
    kmpc_barrier_const = M.getOrInsertFunction(
            "__kmpc_barrier", kmpc_barrier_FuncTy);


}

/* Function to generate the struct type required for OpenMP function calls
 * It hard codes the name of the struct to ident_t for now! */
StructType* Parallelize::Gen_Struct(Module &M){
    Type *int_type = Type::getInt32Ty(M.getContext());
    PointerType *p_type = Type::getInt8PtrTy(M.getContext(),0);
    StructType* struct_created = StructType::create("struct.ident_t",
            int_type,int_type,int_type,int_type, p_type);
    return struct_created;
}

char Parallelize::ID = 1;
static RegisterPass<Parallelize> X("parallelize","Booyah!!!!!");
