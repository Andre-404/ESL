#include "TypeInference.h"



struct SSAVar {
    std::shared_ptr<CFG::VarDecl> decl;
};