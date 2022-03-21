program buffer_code
! 03/20/2022 10:15 PM added ability to insert line
! 03/20/2022 06:46 PM converts "imp" to "implicit none"
! 03/20/2022 06:44 PM use - to delete last line
! 03/20/2022 06:42 PM allows a line to be replaced
! 03/20/2022 06:42 PM allows deletion of a line or range of lines
! 03/20/2022 01:02 PM reads source file as optional command line argument
! 03/20/2022 09:07 AM stores the lines of code in type(buffer)
! 03/20/2022 00:28 AM numbered the lines of code in the listing -- also has a "barelist"
! 03/19/2022 06:38 PM branched from xread_code.f90
! 03/19/2022 09:45 AM allows the user to run executable again without recompiling
! 03/18/2022 10:25 PM read Fortran code from terminal, compile and run it
use buffer_mod, only: buffer,buffer_from_file,write_buffer_to_file,write_buffer, &
                      buffer_outside_range,num_lines,add_line,deallocate,replace_line, &
                      remove_last,last,insert
implicit none
character (len=*), parameter :: src="temp.f90", code_end = "end", run_char = ";"
character (len=1000)         :: input_src
integer, parameter           :: nlen = 132
character (len=:), allocatable :: new_code_line,executable
character (len=nlen)         :: text
character (len=1)            :: dum
integer                      :: stat,iline_1,iline_2,ierr,ipos_code,line_num, &
                                nlen_trim,ierr_input_src,iu,ipos1
type(buffer)                 :: code_buffer
logical                      :: is_unix
is_unix = unix()
executable = merge("a.out","a.exe",is_unix)
open (newunit=iu,file=src,iostat=stat)
if (stat == 0) close (iu,status="delete")
call get_command_argument(1,value=input_src,status=ierr_input_src)
if (ierr_input_src == 0) then
   code_buffer = buffer_from_file(input_src)
   print "('read ',i0,' lines of code from ',a)",num_lines(code_buffer),trim(input_src)
end if
get_line: do
   write (*,"('> ')",advance="no")
   read (*,"(a)") text
   nlen_trim = len_trim(text)
   if (nlen_trim == 0) then
      continue
   else if (text == "quit" .or. text == "q") then
      call write_buffer_to_file(code_buffer,src,lines_after=[code_end])
      stop "done -- code saved in " // trim(src)
   else if (text == "help" .or. text == "h") then
      print*,"enter a line of Fortran code. Otherwise, ..."
      print*,"quit or q to quit"
      print*,"run or r to compile and run executable"
      print*,"runold or ro to run executable again without recompiling"
      print*,"compile to compile without running executable"
      print*,"list or l for a program listing with line numbers"
      print*,"barelist or bl for a program listing without line numbers"
      print*,"del to delete all code; del i to delete line i ; del i j to delete lines i to j"
      print*,"line n <code> to replace the current line n with <code>"
      print*,"insert n <code> to move lines n on down by 1 and put <code> on line n"
   else if (text == "-") then ! remove last line
      call remove_last(code_buffer)
   else if (text == "-r") then ! remove last line, compile, and run
      call remove_last(code_buffer)
      call compile_and_run()
   else if (text == "list" .or. text == "l") then ! list code with line numbes
      call write_buffer(code_buffer,numbered=.true.)
   else if (text == "barelist" .or. text == "bl") then ! list code without line numbers
      call write_buffer(code_buffer,numbered=.false.)
   else if (text == "del") then ! delete code
      call deallocate(code_buffer)
   else if (text(1:4) == "del ") then ! delete one line or a range of lines
      read (text,*,iostat=ierr) dum,iline_1,iline_2
      if (ierr == 0) then ! delete lines from iline_1 to iline_2
         code_buffer = buffer_outside_range(code_buffer,iline_1,iline_2)
         call write_buffer(code_buffer,numbered=.true.)
         if (text(nlen_trim:nlen_trim) == run_char) call compile_and_run()
      else ! try to read a single line to delete
         read (text,*,iostat=ierr) dum,iline_1
         if (ierr == 0) then
            code_buffer = buffer_outside_range(code_buffer,iline_1,iline_1)
            call write_buffer(code_buffer,numbered=.true.)
         else
            call add_line(code_buffer,text)
         end if
      end if
   else if (text(1:5) == "line ") then ! edit specified line
      read (text(6:),*,iostat=ierr) line_num ! see if input is 'line <n> ...'
      if (ierr /= 0) then
         call add_line(code_buffer,text) ! if not, treat it as a line of code
      else if (line_num < 1) then
         print*,"need line# > 0"
      else if (line_num > num_lines(code_buffer)) then
         print "(a,i0)","need line# <= ",num_lines(code_buffer)
      else ! replace line line_num of code
         text = text(6:)
         ipos_code = index(text," ")
         if (ipos_code > 0) then
            new_code_line = trim(text(ipos_code+1:nlen))
            call replace_line(code_buffer,new_code_line,line_num)
            print "(i0,':',1x,a)",line_num,new_code_line
         end if
      end if
   else if (text(1:7) == "insert ") then ! insert line at specified position
      ipos1 = 8
      read (text(ipos1:),*,iostat=ierr) line_num ! see if input is 'insert <n> ...'
      if (ierr /= 0) then
         call add_line(code_buffer,text) ! if not, treat it as a line of code
      else
         text = text(ipos1:)
         ipos_code = index(text," ")
         if (ipos_code > 0) then
            new_code_line = trim(text(ipos_code+1:nlen))
            call insert(code_buffer,line_num,new_code_line)
            print "(i0,':',1x,a)",line_num,new_code_line
         end if
      end if
   else if (text == "run" .or. text == "r") then
      call compile_and_run()
   else if (text == "compile") then
      call compile()
   else if (text == "runold" .or. text == "ro") then
      if (file_exists(executable)) then
         call run_exec()
      else
         print "(a,' does not exist')",trim(executable)
      end if
   else if (nlen_trim > 0 .and. text(nlen_trim:nlen_trim) == run_char) then
      ! if line has a trailing semicolon, add the line of code, without the trailing semicolon, and then compile and run the program
      text = text(1:nlen_trim-1)
      call add_line(code_buffer,text)
      call compile_and_run()
   else if (text == "imp") then
      call add_line(code_buffer,"implicit none")
   else
      call add_line(code_buffer,text)
   end if
end do get_line
contains
!
subroutine compile()
integer :: stat, ku
character (len=100) :: last_line
character (len=:), allocatable :: last_line_adjustl
logical :: add_code_end
last_line = last(code_buffer)
last_line_adjustl = adjustl(last_line)
if (last_line_adjustl(1:6) == "end do" .or. last_line_adjustl(1:6) == "end if") then
   add_code_end = .true.
else if (last_line == code_end .or. last_line_adjustl(1:4) == (code_end // " ")) then
   add_code_end = .false.
else ! write an extra code_end on the last line
   add_code_end = .true.
end if
if (add_code_end) then
   call write_buffer_to_file(code_buffer,src,lines_after=[code_end])
else
   call write_buffer_to_file(code_buffer,src)
end if
open (newunit=ku,file=executable,iostat=stat)
if (stat == 0) close(ku,status="delete") ! delete executable if it exists
call execute_command_line("gfortran " // trim(src))
end subroutine compile
!
subroutine run_exec()
if (file_exists(executable)) then
   ! print*,trim(executable)," exists, is_unix =",is_unix !! debug
   if (is_unix) then
      call execute_command_line("./" // executable)
   else
      call execute_command_line(executable)
   end if
end if
end subroutine run_exec
!
subroutine compile_and_run()
call compile()
call run_exec()
end subroutine compile_and_run
!
function file_exists(file_name) result(exists)
character (len=*), intent(in) :: file_name
logical                       :: exists
inquire(file=file_name,exist=exists)
end function file_exists
!
function unix() result(is_unix)
! test if the operating system is Unix
logical :: is_unix
character (len=100) :: path
call get_environment_variable("PATH",path)
is_unix = path(1:1) == "/"
end function unix
!
end program buffer_code
