#pragma once
#include "../moduleDefs.h"

namespace errorHandler {
	void showCompileErrors();

	void addCompileError(string msg, Token token);
	void addSystemError(string msg);
	bool hasErrors();
    vector<string> convertCompilerErrorsToJson();
    void printRuntimeError(string func, string file, uint32_t line, uint32_t col, bool isInline);
}