ceditor.o : ceditor.c
	$(CC) -c ceditor.c `idris --include`

editor : ceditor.o *.idr
	idris -o editor Editor.idr -p contrib

clean : 
	rm *.o *.ibc editor