```c
#include <sys/types.h>
#include <sys/wait.h>
#include <stdio.h>
#include <pthread.h>


void helper(void) {
        exit(42);
}

int main() {
        int kirito = 0;
        pid_t pid = fork();
        if (pid) {
                wait(&kirito);
        } else {
                helper();
        }
        printf("%d %d\n", pid, kirito);
        printf("%d %d\n", pid, WEXITSTATUS(kirito));
}
```

`fork` 在父进程返回子进程的 id，在失败的时候返回 -1，在子进程返回 0

`wait(&status)` 等待一个状态，并不指定是哪个子进程返回的状态，取第一个
通常状态存储在  status 的低 16 位的高 8 位，用 `WEXITSTATUS` 取得状态