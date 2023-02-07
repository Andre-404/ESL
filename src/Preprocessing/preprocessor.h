#pragma once
#include "../common.h"
#include "scanner.h"
#include <unordered_map>
#include <unordered_set>
#include <memory>
#include <tuple>

namespace preprocessing {
	using std::unordered_set;
	using std::unordered_map;
	using std::unique_ptr;
	using std::pair;

	class Preprocessor {
	public:
		Preprocessor();
		~Preprocessor();
		void preprocessProject(string mainFilePath);

		vector<CSLModule*> getSortedUnits() { return sortedUnits; }
	private:
		string projectRootPath;
		Scanner scanner;
		CSLModule* curUnit;

		unordered_map<string, CSLModule*> allUnits;
		vector<CSLModule*> sortedUnits;
		
		vector<pair<Token, Token>> processDirectives(CSLModule* unit);

		CSLModule* scanFile(string unitName);
		void topsort(CSLModule* unit);
	};

}