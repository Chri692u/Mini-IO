
/* LINUX JOBS (NOT TESTED)
remove ifdef and split code later*/
#ifdef _WIN32
    // Windows-specific code
    #include <windows.h>
#else
    // Unix-specific code
    #include <unistd.h>
    #include <sys/wait.h>
    #include <sys/types.h>
    #include <stdlib.h>

    // Prototypes
    int c_fork(void);
    void c_exec(char* program);
    int c_wait(int pid);

    // Unix fork
    int c_fork() {
        return fork();
    }

    // Unix exec
    void c_exec(char* program) {
        char* argv[] = { program, NULL };
        execvp(program, argv);
    }

    // Unix wait
    int c_wait(int pid) {
        int status;
        waitpid(pid, &status, 0);
        return WEXITSTATUS(status);
    }

#endif