# FortranShell
Interactive console program in which to edit, compile, and run Fortran code, tested on Windows and Linux.

Compile with `gfortran -o fedit.exe buffer.f90 xbuffer_code.f90`

Sample session:

```
c:\fortran\test>fedit.exe
> print*,2**10
> run
        1024
> list
 print*,2**10
> clear
> list
> implicit none
> integer :: i=2,j=5
> print*,"i,j,i**j=",i,j,i**j
> run
 i,j,i**j=           2           5          32
> k = i**j
> run
junk.f90:4:1:

    4 | k = i**j
      | 1
Error: Symbol 'k' at (1) has no IMPLICIT type
> list
 implicit none
 integer :: i=2,j=5
 print*,"i,j,i**j=",i,j,i**j
 k = i**j
> q
STOP done -- code saved in junk.f90
```

Entering "help" or "h" gives these directions:
```
 enter a line of Fortran code. Otherwise, ...
 quit or q to quit
 run or r to compile and run executable
 runold or ro to run executable again without recompiling
 compile to compile without running executable
 list or l for a program listing with line numbers
 barelist or bl for a program listing without line numbers
 del to delete all code; del i to delete line i ; del i j to delete lines i to j
 line n <code> to replace the current line n with <code>
 insert n <code> to move lines n on down by 1 and put <code> on line n
```
`fedit.exe foo.f90` pulls in code from `foo.f90`. Otherwise program starts blank. In most cases
an ```end``` is supplied automatically and need not be supplied by the user.
