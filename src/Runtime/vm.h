#pragma once
#include "../codegen/codegenDefs.h"
#include "../Objects/objects.h"
#include "thread.h"
#include <condition_variable>

namespace runtime {
	string expectedType(string msg, Value val);
	
	class VM {
	public:
		VM(compileCore::Compiler* compiler);
		void execute();
		void mark(memory::GarbageCollector* gc);
		bool allThreadsPaused();
		// Used by all threads
		vector<Globalvar> globals;
		vector<File*> sourceFiles;
		// Main code block, all function look into this vector at some offset
		Chunk code;
		// For adding/removing threads
		std::mutex mtx;
		vector<Thread*> childThreads;

		// For pausing threads during gc run
		std::mutex pauseMtx;
		std::condition_variable mainThreadCv;
		std::condition_variable childThreadsCv;
		std::atomic<byte> threadsPaused;
		Thread* mainThread;
	};

}
