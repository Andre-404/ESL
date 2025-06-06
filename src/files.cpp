#include "common.h"
#include <filesystem>
#include <fstream>
#include <sstream>

string readFile(char* path) {
    std::filesystem::path p(path);
    if (std::filesystem::exists(p)) {
        std::stringstream ss;
        std::ifstream file(path);
        ss << file.rdbuf();
        file.close();
        return ss.str();
    }
    else {
        std::cout << "Couldn't open file " << path << "\n";
        return "";
    }
}

string readFile(const char* path) {
    return readFile((char*)path);
}

string readFile(const string& path) {
    return readFile(path.c_str());
}