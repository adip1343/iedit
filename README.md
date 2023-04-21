# iedit
Interactive code editor written in Idris for Idris. We use `Control.ST` to implement text editor as type safe state machine. 

## Compiling



```bash
git clone https://github.com/adip1343/iedit.git
cd ./iedit/src
make editor
```

## Usage
You can open a file to edit using 
```bash
./editor /path/to/file
```
### Keybindings

| Key    | Usage                |
| ------ | -------------------- |
| Ctrl S | Save file            |
| Ctrl Q | Exit text editor     |
| Ctrl C | Case split           |
| Ctrl D | Add clause           |
| Ctrl O | Obvious proof search |
| Ctrl T | Show Type            |

To be able to use interactive editing make sure Idris REPL is running.

## References

1. [kilo](https://github.com/antirez/kilo/blob/master/kilo.c) : Small text editor written in C
2. [Interactive editing in Idris](https://docs.idris-lang.org/en/latest/tutorial/interactive.html) documentation
3. [`Control.ST`](https://docs.idris-lang.org/en/latest/st/introduction.html) documentation
