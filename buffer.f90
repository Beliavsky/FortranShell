module buffer_mod
use iso_fortran_env, only: output_unit
implicit none
private
public :: buffer, num_lines, add_line, add_lines, write_buffer, allocate, &
          deallocate, write_buffer_to_file, buffer_outside_range, replace_line, &
          buffer_from_file, remove_last, allocated, last, insert
interface buffer
   module procedure buffer_from_lines
end interface buffer
!
interface insert
   module procedure insert_line
end interface insert
!
interface allocated
   module procedure allocated_buffer
end interface allocated
!
interface allocate
   module procedure allocate_buffer
end interface allocate
!
interface deallocate
   module procedure deallocate_buffer
end interface deallocate
!
integer, parameter :: nlen = 132, nlines_inc = 1000
type :: buffer
   private
   character (len=nlen), allocatable :: lines(:)
   integer :: nlines = 0 ! # of lines with data
end type buffer
contains
!
pure subroutine insert_line(dt,pos,line)
! move lines from position pos on down by 1 and insert line at pos
type(buffer)     , intent(in out)  :: dt
integer          , intent(in)      :: pos
character (len=*), intent(in)      :: line
integer                            :: nlines
character (len=nlen)               :: line_
line_ = line
nlines = num_lines(dt)
if (nlines == 0) then
   dt = buffer_from_lines([line])
else if (pos < 2) then
   dt = buffer_from_lines([line_,dt%lines(:nlines)])
else if (pos > nlines) then
   dt = buffer_from_lines([dt%lines(:nlines),line_])
else
   dt = buffer_from_lines([dt%lines(1:pos-1),line_,dt%lines(pos:nlines)])
end if
end subroutine insert_line
!
elemental function last(dt) result(line)
! return last line of buffer, "" if buffer empty
type(buffer), intent(in) :: dt
character (len=nlen) :: line
if (num_lines(dt) > 0) then
   line = dt%lines(num_lines(dt))
else
   line = ""
end if
end function last
!
elemental function allocated_buffer(dt) result(tf)
type(buffer), intent(in) :: dt
logical                  :: tf
tf = allocated(dt%lines)
end function allocated_buffer
!
pure subroutine remove_last(dt)
! remove last line of buffer
type(buffer), intent(in out) :: dt
integer :: nlines
nlines = num_lines(dt)
if (nlines < 1) return
dt%lines(nlines) = ""
dt%nlines = nlines - 1
end subroutine remove_last
!
pure subroutine replace_line(dt,line_new,line_num)
! if line line_num of buffer exists, replace it with line_new
type(buffer)     , intent(in out) :: dt
character (len=*), intent(in)     :: line_new
integer          , intent(in)     :: line_num
if (line_num > 0 .and. line_num <= num_lines(dt)) dt%lines(line_num) = line_new
end subroutine replace_line
!
function buffer_from_file(infile) result(dt)
! read buffer from file infile
character (len=*), intent(in) :: infile
type(buffer)                  :: dt
integer                       :: iu,ierr_open,ierr_read
character (len=10000)         :: text          
open (newunit=iu,file=trim(infile),action="read",status="old",iostat=ierr_open)
if (ierr_open /= 0) return
do
   read (iu,"(a)",iostat=ierr_read) text
   if (ierr_read /= 0) exit
   call add_line(dt,trim(text))
end do
end function buffer_from_file
!
pure function buffer_outside_range(dt,i1,i2) result(dt_out)
! return a buffer with lines i1 to i2 removed
type(buffer)     , intent(in)  :: dt
integer          , intent(in)  :: i1,i2
type(buffer)                   :: dt_out
integer                        :: i,nlines
integer          , allocatable :: iuse(:)
nlines = dt%nlines
allocate (iuse(nlines))
forall (i=1:nlines) iuse(i) = merge(i,0,i < i1 .or. i > i2)
iuse = pack(iuse,iuse > 0)
dt_out = buffer(dt%lines(iuse))
end function buffer_outside_range
!
pure function buffer_from_lines(lines) result(dt)
! create a buffer from lines(:)
character (len=*), intent(in) :: lines(:)
type(buffer)                  :: dt
integer                       :: i,n
n = size(lines)
call allocate(dt,n)
do i=1,n
   dt%lines(i) = lines(i)
end do
dt%nlines = max(n,0)
end function buffer_from_lines
!
subroutine write_buffer(dt,outu,numbered,title)
! write buffer to unit outu
type(buffer), intent(in) :: dt
integer     , intent(in), optional :: outu
logical     , intent(in), optional :: numbered
character (len=*), intent(in), optional :: title
integer                  :: i,n,outu_
logical                  :: numbered_
outu_ = output_unit ; if (present(outu)) outu_ = outu
numbered_ = .false. ; if (present(numbered)) numbered_ = numbered
n = num_lines(dt)
if (present(title)) write (outu_,"(a)") title
do i=1,n
   if (numbered_) then
      write (outu_,"(i0,':',1x,a)") i,trim(dt%lines(i))
   else
      write (outu_,"(1x,a)") trim(dt%lines(i))
   end if
end do
! write (outu_,*) !! debug
end subroutine write_buffer
!
subroutine write_buffer_to_file(dt,out_file,status,position,lines_before,lines_after)
! write buffer to file out_file
type(buffer)     , intent(in)           :: dt
character (len=*), intent(in)           :: out_file
character (len=*), intent(in), optional :: status, position
character (len=*), intent(in), optional :: lines_before(:),lines_after(:)
integer                                 :: i,outu
character (len=20)                      :: status_,position_
status_ = "replace"  ; if (present(status)) status_ = status
position_ = "rewind" ; if (present(position)) position_ = position
open (newunit=outu,file=out_file,status=status_,position=position_)
if (present(lines_before)) then
   do i=1,size(lines_before)
      write (outu,"(a)") trim(lines_before(i))
   end do
end if
do i=1,num_lines(dt)
   write (outu,"(a)") trim(dt%lines(i))
end do
if (present(lines_after)) then
   do i=1,size(lines_after)
      write (outu,"(a)") trim(lines_after(i))
   end do
end if
close (outu)
end subroutine write_buffer_to_file
!
elemental function num_lines(dt) result(n)
type(buffer), intent(in) :: dt
integer                  :: n
n = dt%nlines
end function num_lines
!
pure subroutine add_line(dt,line)
type(buffer)     , intent(in out) :: dt
character (len=*), intent(in)     :: line
integer                           :: nlines_new
! print*,"in add_line, size_buffer(dt), dt%nlines =",size_buffer(dt),dt%nlines, &
!       " adding '" // trim(line) // "'" !! debug
if (size_buffer(dt) <= dt%nlines) call expand_buffer(dt,nlines_inc)
nlines_new = dt%nlines + 1
! print*,"in add_line, dt%nlines, nlines_new =",dt%nlines,nlines_new !! debug
! print*,"nlines_new =",nlines_new
dt%lines(nlines_new) = line
dt%nlines = nlines_new
end subroutine add_line
!
subroutine add_lines(dt,lines)
type(buffer)     , intent(in out) :: dt
character (len=*), intent(in)     :: lines(:)
integer                           :: i
! print*,"in add_lines, size(lines), num_lines(dt),allocated(dt%lines) =",size(lines),num_lines(dt),allocated(dt%lines) !! debug
do i=1,size(lines)
!   print*,"in add_lines, i=",i !! debug
   call add_line(dt,lines(i))
end do
end subroutine add_lines
!
pure subroutine expand_buffer(dt,nlines_add)
! increase the capacity of dt by nlines_add
type(buffer), intent(in out) :: dt
integer     , intent(in)     :: nlines_add
type(buffer)                 :: dt_copy
integer                      :: nlines_new,nlines_old
if (nlines_add < 1) return
nlines_old = dt%nlines
dt_copy = dt
nlines_new = nlines_old + nlines_add
call allocate(dt,nlines_new)
if (allocated(dt_copy%lines)) dt%lines(:nlines_old) = dt_copy%lines(:nlines_old)
dt%lines(nlines_old+1:nlines_new) = ""
dt%nlines = nlines_old
end subroutine expand_buffer
!
pure subroutine allocate_buffer(dt,n)
type(buffer), intent(out) :: dt
integer     , intent(in)  :: n
allocate (dt%lines(n))
dt%nlines = 0
end subroutine allocate_buffer
!
pure subroutine deallocate_buffer(dt)
type(buffer), intent(out) :: dt
end subroutine deallocate_buffer
!
pure function size_buffer(dt) result(nsize)
type(buffer), intent(in) :: dt
integer                  :: nsize
if (allocated(dt%lines)) then
   nsize = size(dt%lines)
else
   nsize = 0
end if
end function size_buffer
end module buffer_mod
