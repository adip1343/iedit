ceditor.o : ceditor.c
	$(CC) -c ceditor.c -Wall -Wextra -pedantic -stdc99 `idris --include`

editor : ceditor.o *.idr
	idris -o editor Editor.idr

clean : 
	rm *.o *.ibc editor

