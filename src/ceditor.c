// includes ----------------------------------------------
#include "ceditor.h"
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <assert.h>

// global structs ----------------------------------------
struct termios orig_termios;

// syscall wrappers --------------------------------------
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

int editorReadKey() {
  	int nread;
  	char c;
  	while ((nread = read(STDIN_FILENO, &c, 1)) != 1) {
    	if (nread == -1 && errno != EAGAIN) return -1;
  	}

  	if (c == '\x1b') {
    	char seq[3];

    	if (read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
   		if (read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';

    	if (seq[0] == '[') {
      		if (seq[1] >= '0' && seq[1] <= '9') {
        		if (read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
        		if (seq[2] == '~') {
          			switch (seq[1]) {
            			case '1': return HOME_KEY;
            			case '3': return DEL_KEY;
            			case '4': return END_KEY;
            			case '5': return PAGE_UP;
            			case '6': return PAGE_DOWN;
            			case '7': return HOME_KEY;
            			case '8': return END_KEY;
          			}
        		}
      		} else {
        		switch (seq[1]) {
          			case 'A': return ARROW_UP;
          			case 'B': return ARROW_DOWN;
          			case 'C': return ARROW_RIGHT;
          			case 'D': return ARROW_LEFT;
          			case 'H': return HOME_KEY;
          			case 'F': return END_KEY;
        		}
      		}
    	} else if (seq[0] == '0') {
      		switch (seq[1]) {
        		case 'H': return HOME_KEY;
        		case 'F': return END_KEY;
      		}
    	}
    	return '\x1b';
  	} else {
    	return c;
  	}
}

int writeBuffer(const char* buf, int size){
	return write(STDOUT_FILENO, buf, size);
}

// terminal -----------------------------------------------
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

int getWindowCols(){
	struct winsize ws;
	if(ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0){
		return 0;
	}
	return ws.ws_col;
}

int getWindowRows(){
	struct winsize ws;
	if(ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0){
		return 0;
	}
	return ws.ws_row;
}
