program read_code
! 03/19/2022 09:45 AM allows the user to run executable again without recompiling
! 03/18/2022 10:25 PM read Fortran code from terminal, compile and run it
! to do:
! number the lines of code in the listing -- also have a "barelist"
! store the lines of code in an array of strings
! let user include code from another file
! let user delete a range of lines, including n1:n2, n: or :n
! let user edit a line
! let user insert a line
! let user comment out or uncomment a set of lines
! make it work on Linux
! allow the code to be linked with specified object or library files to create an executable
! indent code in if blocks and do loops
! expand p x,y to print*,x,y
! add end associate statements as needed
! check if each entered line of code is invalid and reject it if it is -- are there tools for this?
! let user make a block out of a group of lines
! allow tools such as Met Office stylist to analyze the code, or other tools to indent it
! move use statements and declarations with :: automatically to the write place
implicit none
character (len=*), parameter :: src = "temp.f90", full_src="junk.f90", &
                                executable = "a.exe"
character (len=1000)         :: text, code_line
integer, parameter           :: iu = 10, ju = 11, ku = 12
integer                      :: ierr,stat
open (unit=iu,file=full_src,iostat=stat)
if (stat == 0) close (iu,status="delete")
open (unit=iu,file=src,action="write",status="replace")
do
   write (*,"('> ')",advance="no")
   read (*,"(a)") text
   if (text == "quit" .or. text == "q") then
      stop "done -- code saved in " // trim(full_src)
   else if (text == "help" .or. text == "h") then
      print*,"quit or q to quit"
      print*,"run or r to compile and run executable"
      print*,"runold or ro to run executable again without recompiling"
      print*,"compile to compile without running executable"
      print*,"list or l for a program listing"
      print*,"clear or c to delete code and start over"
   else if (text == "clear" .or. text == "c") then
      close (iu,status="delete")
      open (unit=iu,file=src,action="write",status="replace")
   else if (text == "list" .or. text == "l") then
      close (iu)
      open (unit=ju,file=src,action="read")
      do
         read (ju,"(a)",iostat=ierr) code_line
         if (ierr == 0) then
            write (*,*) trim(code_line)
         else
            exit
         end if
      end do
      close (ju)
      open (unit=iu,file=src,action="write",position="append")
   else if (text == "run" .or. text == "r") then
      call compile()
      if (file_exists(executable)) call execute_command_line(executable)
   else if (text == "compile") then
      call compile()
   else if (text == "runold" .or. text == "ro") then
      if (file_exists(executable)) then
         call execute_command_line(executable)
      else
         print "(a,' does not exist')",trim(executable)
      end if
   else
      write (iu,"(a)") text ! add text to source file
!      print "(a)",trim(text)
   end if
end do
contains
!
subroutine compile()
integer :: stat, ku
call execute_command_line("copy " // src // " " // full_src // " > nul")
call execute_command_line("echo end >> " // full_src)
open (newunit=ku,file=executable,iostat=stat)
if (stat == 0) close(ku,status="delete") ! delete executable if it exists
call execute_command_line("gfortran " // trim(full_src))
end subroutine compile
!
function file_exists(file_name) result(exists)
character (len=*), intent(in) :: file_name
logical                       :: exists
inquire(file=file_name,exist=exists)
end function file_exists
!
end program read_code
