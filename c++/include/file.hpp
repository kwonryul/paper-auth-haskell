#include <fstream>
#include <sstream>
#include <string>

std::string readFileIntoString(const std::string& filePath) {
    std::ifstream fileStream(filePath, std::ios::in);
    if (!fileStream.is_open())
        return "";
    std::stringstream stringStream;
    stringStream << fileStream.rdbuf();
    fileStream.close();
    return stringStream.str();
}