#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/stat.h>

// Function to get the size of a file
long getFileSize(const char *path) {
    struct stat st;
    if (stat(path, &st) == -1) {
        return -1; // Return -1 on failure
    }
    if (S_ISREG(st.st_mode)) {
        return st.st_size; // Return the size of the file
    }
    return -1; // Return -1 if the path is not a file
}

// Function to get the size of a directory
long getDirectorySize(const char *path) {
    DIR *dir;
    struct dirent *entry;
    long totalSize = 0;

    if ((dir = opendir(path)) == NULL) {
        return -1; // Return -1 on failure
    }

    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") != 0 && strcmp(entry->d_name, "..") != 0) {
            char subPath[4096];
            snprintf(subPath, sizeof(subPath), "%s/%s", path, entry->d_name);

            struct stat st;
            if (stat(subPath, &st) == -1) {
                closedir(dir);
                return -1; // Return -1 on failure
            }

            if (S_ISREG(st.st_mode)) {
                totalSize += st.st_size;
            } else if (S_ISDIR(st.st_mode)) {
                long dirSize = getDirectorySize(subPath);
                if (dirSize == -1) {
                    closedir(dir);
                    return -1; // Return -1 on failure
                }
                totalSize += dirSize;
            }
        }
    }

    closedir(dir);
    return totalSize; // Return the total size of the directory
}