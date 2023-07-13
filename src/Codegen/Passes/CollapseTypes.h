#include "ASTToTypedAST.h"
#include "../TypedAST/Types.h"
#include "../../Includes/unorderedDense.h"

namespace passes {
    namespace TypeCollapser {
        class TypeCollapser : public typedAST::TypedASTVisitor {
        public:
            TypeCollapser();
            bool hadError;
            void run(typedAST::Function program, vector<File *> srcFiles);

            #pragma region Visitor
            void visitVarDecl(typedAST::VarDecl *decl);
            void visitVarRead(typedAST::VarRead *expr);
            void visitVarStore(typedAST::VarStore *expr);
            void visitVarReadNative(typedAST::VarReadNative *expr);
            void visitArithmeticExpr(typedAST::ArithmeticExpr *expr);
            void visitComparisonExpr(typedAST::ComparisonExpr *expr);
            void visitUnaryExpr(typedAST::UnaryExpr *expr);
            void visitLiteralExpr(typedAST::LiteralExpr *expr);
            void visitHashmapExpr(typedAST::HashmapExpr *expr);
            void visitArrayExpr(typedAST::ArrayExpr *expr);
            void visitCollectionGet(typedAST::CollectionGet *expr);
            void visitCollectionSet(typedAST::CollectionSet *expr);
            void visitConditionalExpr(typedAST::ConditionalExpr *expr);
            void visitCallExpr(typedAST::CallExpr *expr);
            void visitInvokeExpr(typedAST::InvokeExpr *expr);
            void visitNewExpr(typedAST::NewExpr *expr);
            void visitAsyncExpr(typedAST::AsyncExpr *expr);
            void visitAwaitExpr(typedAST::AwaitExpr *expr);
            void visitCreateClosureExpr(typedAST::CreateClosureExpr *expr);
            void visitFuncDecl(typedAST::FuncDecl *expr);
            void visitReturnStmt(typedAST::ReturnStmt *expr);
            void visitRangeExpr(typedAST::RangeExpr *expr);
            void visitUncondJump(typedAST::UncondJump *expr);
            void visitIfStmt(typedAST::IfStmt *expr);
            void visitWhileStmt(typedAST::WhileStmt *expr);
            void visitSwitchStmt(typedAST::SwitchStmt *expr);
            void visitClassDecl(typedAST::ClassDecl *expr);
            void visitInstGet(typedAST::InstGet *expr);
            void visitInstSuperGet(typedAST::InstSuperGet *expr);
            void visitInstSet(typedAST::InstSet *expr);
            #pragma endregion
        private:
            ankerl::unordered_dense::set<types::tyPtr> collapsedTypes;
            //Types that are in the progress of being collapsed
            ankerl::unordered_dense::set<types::tyPtr> inProgress;
            ankerl::unordered_dense::set<types::TypeFlag> opaqueTypes;

            void collapseType(types::tyPtr toCollapse);

            // Collapses all types that aren't considered opaque
            std::unordered_set<types::tyPtr> shallowCollapse(types::tyPtr toCollapse);

            void collapseFunctionTy(std::shared_ptr<types::FunctionType> fnTy);
            void collapseClassTy(std::shared_ptr<types::ClassType> classTy);
            void collapseFutureTy(std::shared_ptr<types::FutureType> futureTy);
            void collapseUnionTy(std::shared_ptr<types::TypeUnion> typeUnion);
            void collapseDeferredCall(std::shared_ptr<types::CallReturnDeferred> deferredCall);
            void collapseDeferredInstGet(std::shared_ptr<types::InstanceGetDeferred> deferredInstGet);
            void collapseDeferredAwait(std::shared_ptr<types::FutureAwaitDeferred> deferredAwait);

            void error(Token token, string msg);
        };
    }
}
