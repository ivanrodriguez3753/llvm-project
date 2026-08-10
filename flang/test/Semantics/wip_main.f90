!flang is currently misreporting unused variables. in certain contexts,
!a 'read' does not count as a use. This program reports 3 errors. 
!current fix doesn't seem right... but i fixed it. 
program p
  implicit none
  integer :: aBound = 3      ! used ONLY as ALLOCATE bound
  integer :: doLim  = 3      ! used ONLY as DO upper bound
  integer :: ifThr  = 1      ! used ONLY in IF condition
  integer :: subIdx = 1      ! used ONLY as array subscript in an expression (control)
  integer, allocatable :: arr(:)
  integer :: acc, i
  allocate(arr(aBound))
  arr = 0
  acc = 0
  do i = 1, doLim
    acc = acc + i
  end do
  if (acc > ifThr) print *, 'big'
  print *, arr(subIdx), acc
end program
