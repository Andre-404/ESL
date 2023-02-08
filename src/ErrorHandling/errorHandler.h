#pragma once
#include "../common.h"
#include "../moduleDefs.h"

namespace errorHandler {
	void showCompileErrors();

	void addCompileError(string msg, Token token);
	void addSystemError(string msg);
	bool hasErrors();
}