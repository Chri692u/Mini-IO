#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <dirent.h>

// Equivalent to printStr in Haskell
void printStr(const char* str) {
    printf("%s\n", str);
}

// Equivalent to readStr in Haskell
void readStr(char* buffer, int bufferSize) {
    fgets(buffer, bufferSize, stdin);
    buffer[strcspn(buffer, "\n")] = 0;
}

// Equivalent to readFile in Haskell
char* readFile(const char* filename) {
    FILE* file = fopen(filename, "r");
    if (file == NULL) {
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    long length = ftell(file);
    fseek(file, 0, SEEK_SET);

    char* buffer = malloc(length + 1);
    if (buffer == NULL) {
        fclose(file);
        return NULL;
    }

    fread(buffer, 1, length, file);
    buffer[length] = '\0';

    fclose(file);
    return buffer;
}

// Equivalent to writeFile in Haskell
void createFile(const char* filename) {
    FILE* file = fopen(filename, "w");
    if (file != NULL) {
        fclose(file);
    }
}

// Equivalent to deleteFile in Haskell
void removeFile(const char* filename) {
    remove(filename);
}