# FortranShell
Interactive console program in which to edit, compile, and run Fortran code, tested on Windows and Linux.

Compile with `gfortran -o fedit.exe buffer.f90 xbuffer_code.f90`

Sample session:

```
> print*,2**5;
          32
> del
> imp
> integer :: i
> do i=1,4
> print*,i,i**2
> end do
> list
1: implicit none
2: integer :: i
3: do i=1,4
4: print*,i,i**2
5: end do
> run
           1           1
           2           4
           3           9
           4          16
> q
STOP done -- code saved in temp.f90
```

Entering "help" or "h" gives these directions:
```
 enter a line of Fortran code. Otherwise, ...
 quit or q to quit
 run or r to compile and run executable
 a line of code terminated by ; causes the program to be compiled and run
 runold or ro to run executable again without recompiling
 compile to compile without running executable
 list or l for a program listing with line numbers
 barelist or bl for a program listing without line numbers
 del to delete all code; del i to delete line i ; del i j to delete lines i to j
 line n <code> to replace the current line n with <code>
 insert n <code> to move lines n on down by 1 and put <code> on line n
 imp is expanded to implicit none
```
`fedit.exe foo.f90` pulls in code from `foo.f90`. Otherwise program starts blank. In most cases
an ```end``` is supplied automatically and need not be supplied by the user.
