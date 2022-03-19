# FortranShell
Windows CMD shell in which to edit, compile, and run Fortran code

Compile with `gfortran -o fedit.exe xread_code.f90`

Sample session:

```
c:\fortran\test>fedit
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
STOP done -- code saved in junk.f90```
