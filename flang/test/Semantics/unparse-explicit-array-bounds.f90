! RUN: %flang_fc1 -fdebug-unparse %s 2>&1 | FileCheck %s

! Test unparse of ExplicitShapeBoundsSpec (rank-1 integer array bounds).

! Upper bounds only: SHAPE(src)
subroutine ub_only(src)
  integer, intent(in) :: src(:,:)
  integer :: a(SHAPE(src))
  a = 1
end subroutine
!CHECK: INTEGER a([INTEGER(4)::int(size(src,dim=1,kind=8),kind=4),int(size(src,dim=2,kind=8),kind=4)])

! Lower and upper bounds: lb:ub
subroutine lb_and_ub(lb, ub)
  integer, intent(in) :: lb(2), ub(2)
  integer :: a(lb:ub)
  a = 1
end subroutine
!CHECK: INTEGER a(lb:ub)

! Expression bounds: 2*SHAPE(src)
subroutine expr_bounds(src)
  integer, intent(in) :: src(:,:,:)
  integer :: a(2*SHAPE(src))
  a = 1
end subroutine
!CHECK: INTEGER a([INTEGER(4)::2_4*int(size(src,dim=1,kind=8),kind=4),2_4*int(size(src,dim=2,kind=8),kind=4),2_4*int(size(src,dim=3,kind=8),kind=4)])

! Parameter bounds
subroutine param_bounds()
  integer, parameter :: dims(3) = [2, 3, 4]
  integer :: a(dims)
  a = 1
end subroutine
!CHECK: INTEGER a([INTEGER(4)::2_4,3_4,4_4])
