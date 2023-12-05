#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/stat.h>
#include <direct.h>

// Equivalent to currentDirectory in Haskell
char* currentDirectory() {
    char* buffer = malloc(1024);
    getcwd(buffer, 1024);
    return buffer;
}

// Equivalent to listDirectory in Haskell
char** listDirectory(const char* path) {
    DIR* dir = opendir(path);
    if (dir == NULL) {
        return NULL;
    }

    struct dirent* entry;
    char** files = malloc(1024 * sizeof(char*));
    int i = 0;
    while ((entry = readdir(dir)) != NULL) {
        files[i] = malloc(strlen(entry->d_name) + 1);
        strcpy(files[i], entry->d_name);
        i++;
    }
    closedir(dir);

    files[i] = NULL;
    return files;
}

// Equivalent to changeDirectory in Haskell
int changeDirectory(const char* dirname) {
    DIR* dir = opendir(".");
    if (dir == NULL) {
        return -1;
    }

    struct dirent* entry;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, dirname) == 0) {
            closedir(dir);
            return chdir(dirname);
        }
    }

    closedir(dir);
    return -1;
}

// Function to create a directory
int createDirectory(const char *path) {
    if (_mkdir(path) == -1) {
        return -1; // Return -1 on failure
    }
    return 0; // Return 0 on success
}

// Function to remove a directory
int removeDirectory(const char *path) {
    if (_rmdir(path) == -1) {
        return -1; // Return -1 on failure
    }
    return 0; // Return 0 on success
}