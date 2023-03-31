#include "ceditor.h"
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>

struct termios orig_termios;

char readChar(){
	char c;
	int nread = read(STDIN_FILENO, &c, 1);
	if(nread == -1)	return '\0';
	return c;
}

char readCharBlocking(){
	int nread;
  	char c;
  	while ((nread = read(STDIN_FILENO, &c, 1)) != 1 && errno != EAGAIN) {
    	if (nread == -1) return '\0';
  	}
  	return c;
}

int writeBuffer(const char* buf, int size){
	return write(STDOUT_FILENO, buf, size);
}

void exitRawMode() {
	// setting termios attributes to original
  	tcsetattr(STDIN_FILENO, TCSAFLUSH, &orig_termios);
}

void enterRawMode() {
	// getting termios attributes
	tcgetattr(STDIN_FILENO, &orig_termios);

	// set attibutes to original incase program exit
	atexit(exitRawMode);

	// updating termios attibutes
  	struct termios raw = orig_termios;
	raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
	// ~IXON 	: Disable Ctrl-S, Ctrl-Q
	// ~ICRNL	: Disable Ctrl-M
  	raw.c_oflag &= ~(OPOST);
	// ~OPOST	: Disable output processing
  	raw.c_cflag |= (CS8);
  	raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
	// ~ECHO 	: don't print each key to terminal
	// ~ICANON 	: reading byte by byte
	// ~ISIG	: stop Ctrl-C, Ctrl-Z signals
	// ~IEXTEN 	: disable Ctrl-V
  	raw.c_cc[VMIN] = 0;
	// minimum bytes to be read before read returns
  	raw.c_cc[VTIME] = 1;
	// maximum time before read returns
  	
	// setting termios attributes
	tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}