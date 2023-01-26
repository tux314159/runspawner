#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

// We take a single line of input over a stdin; it is just
// program followed by arguments. The resulting process' stdin
// will also be taken over stdin. stdout and stderr are sent
// back over stdout and stderr.
// A single newline character is sent over the pipe to indicate
// completion.
// We also take care of establishing the pipe.
// NOTE that we do not do much error-handling at all; in addition,
// as the input is expected to be TRUSTED we do not check for buffer
// overflows; the maximum length of the command is 1023 chars.
// NOTE we require a user with UID 1000 and a group with GID 1000
// with NO PRIVELIGES to exist.

#define BUFSZ 1024

const char job_pipe_path[] = "/var/lib/pheidippides-job-pipe";

// NOTE: returns number of bytes read INCLUDING newline.
size_t readline(int fd, char *buf, size_t max_n)
{
	int n = 0, x;
	while ((x = read(fd, buf + (n++), 1))) {
		fprintf(stderr, "n=%d\n", n);
		if (buf[n - 1] == '\n') {
			buf[n - 1] = '\0';
			break;
		}
	}
	return n - !x;
}

int main(void)
{
	mkfifo(job_pipe_path, O_RDWR);
	printf("\n");
	fflush(stdout);
	int job_ppe = open(job_pipe_path, O_WRONLY);

	int inpipe[2], outpipe[2], errpipe[2];

	char cmd[BUFSZ];
	int n;
	while ((n = readline(STDIN_FILENO, cmd, BUFSZ-1))) {
		fflush(stdin);
		cmd[n - 1] = '\0';
		pipe(inpipe);
		pipe(outpipe);
		pipe(errpipe);
		pid_t cpid = fork();
		if (!cpid) {
			//dup2(inpipe[0], STDIN_FILENO);
			dup2(outpipe[1], STDOUT_FILENO);
			dup2(errpipe[1], STDERR_FILENO);
			close(STDIN_FILENO);
			close(job_ppe);

			int nargs = 0;
			char *argv[BUFSZ];
			memset(argv, 0, sizeof(*argv) * BUFSZ);
			for (int i = 0; i < n; i++) {
				if (cmd[i] == ' ') {
					cmd[i] = '\0';
					argv[++nargs] = cmd + i + 1;
				}
			}
			argv[0] = cmd;
			argv[nargs + 1] = NULL;

			setgid(1000);
			setuid(1000);
			execvp(cmd, argv);
			close(inpipe[0]);
			close(outpipe[1]);
			close(errpipe[1]);
			_exit(0);
		}
		wait(NULL);

		close(inpipe[0]);
		close(outpipe[1]);
		close(errpipe[1]);
		
		char buf[BUFSZ];
		while ((n = read(outpipe[0], buf, BUFSZ))) {
			write(STDOUT_FILENO, buf, n);
		}
		while ((n = read(errpipe[0], buf, BUFSZ))) {
			write(STDERR_FILENO, buf, n);
		}
		write(job_ppe, "\n", 1);

		close(inpipe[1]);
		close(outpipe[0]);
		close(errpipe[0]);
	}

	close(job_ppe);
	unlink(job_pipe_path);
}
