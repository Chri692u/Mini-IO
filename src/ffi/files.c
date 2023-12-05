#include <stdio.h>

// function to open a file
FILE* open_file(const char* filename, const char* mode) {
    FILE* file = fopen(filename, mode);
    if (file == NULL) {
        printf("Failed to open file: %s\n", filename);
        return NULL;
    }
    return file;
}

// function to close a file
void close_file(FILE* file) {
    if (file != NULL) {
        fclose(file);
    }
}

// function to append data to a file
void append_to_file(FILE* file, const char* data) {
    if (file != NULL) {
        fputs(data, file);
    }
}