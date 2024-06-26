#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>

/*
 * We take a single line of input over a stdin; it is just program
 * followed by arguments. The resulting process' stdin will also be taken
 * over stdin. stdout and stderr are sent back over stdout and stderr.
 * A single newline character is sent over the pipe to indicate completion.
 * We also take care of establishing the pipe.
 *
 * WARN we do not do much error-handling at all; in addition, as the input
 * is expected to be TRUSTED we do not check for buffer overflows; the
 * maximum length of the command is 1023 chars.
 *
 * NOTE we require a user with UID 1000 and a group with GID 1000 with NO
 * PRIVELIGES to exist.
 */

#define BUFSZ 1024

const char job_pipe_path[] = "/var/lib/runspawner-job-pipe";

int main(void)
{
	// Put ourselves into raw mode
	struct termios raw;
	tcgetattr(STDIN_FILENO, &raw);
	raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
	raw.c_oflag &= ~(OPOST);
	raw.c_cflag |= (CS8);
	raw.c_lflag &= ~(ECHO | IEXTEN | ISIG);
	tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);

	// We are ready, drop permissions
	write(STDOUT_FILENO, "\n", 1);
	int job_ppe = open(job_pipe_path, O_WRONLY);
	setgid(1000);
	setuid(1000);

	char cmd[BUFSZ];
	int n;
	while ((n = read(STDIN_FILENO, cmd, BUFSZ-1))) {
		cmd[n - 1] = '\0';
		system(cmd);
		fsync(STDOUT_FILENO);
		write(job_ppe, "\n", 1);
		fsync(job_ppe);
	}

	close(job_ppe);
}
