ceditor.o : ceditor.c
	$(CC) -c ceditor.c -Wall -Wextra -pedantic -std=c99 `idris

editor : ceditor.o *.idr
	idris -o editor Editor.idr -p contrib

clean : 
	rm *.o *.ibc editor