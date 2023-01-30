#include <fcntl.h>
#include <termios.h>
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
		if (buf[n - 1] == '\n') {
			buf[n - 1] = '\0';
			break;
		}
	}
	return n - !x;
}

int main(void)
{
	// Put ourselves into raw mode.
	struct termios raw;
	tcgetattr(STDIN_FILENO, &raw);
	raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
	raw.c_oflag &= ~(OPOST);
	raw.c_cflag |= (CS8);
	raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
	tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
	// Create the job fifo.
	mkfifo(job_pipe_path, O_RDWR);
	// We are ready.
	write(STDOUT_FILENO, "\n", 1);

	int job_ppe = open(job_pipe_path, O_WRONLY);

	char cmd[BUFSZ];
	int n;
	while ((n = readline(STDIN_FILENO, cmd, BUFSZ-1))) {
		cmd[n - 1] = '\0';
		pid_t cpid = fork();
		if (!cpid) {
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
			_exit(0);
		}
		wait(NULL);
		fsync(STDOUT_FILENO);
		write(job_ppe, "\n", 1);
	}

	close(job_ppe);
	unlink(job_pipe_path);
}
