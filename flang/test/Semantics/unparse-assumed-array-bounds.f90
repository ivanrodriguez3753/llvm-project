! RUN: %flang_fc1 -fdebug-unparse %s 2>&1 | FileCheck %s

! Test unparse of AssumedShapeBoundsSpec (rank-1 integer array lower bounds).

! Dummy rank-1 integer array as assumed-shape lower bounds: reproduces lbs:
subroutine dummy_lb(lbs, a)
  integer, intent(in) :: lbs(3)
  real :: a(lbs:)
  a = 1
end subroutine
!CHECK: REAL a(lbs:)

! Expression lower bounds: the whole rank-1 expression is preserved
subroutine expr_lb(lbs, a)
  integer, intent(in) :: lbs(2)
  integer :: two = 2
  real :: a(two*lbs:)
  a = 1
end subroutine
!CHECK: REAL a(two*lbs:)

! PARAMETER rank-1 array (foldable) as lower bounds
subroutine param_lb(a)
  integer, parameter :: lbs(2) = [0, 3]
  real :: a(lbs:)
  a = 1
end subroutine
!CHECK: REAL a([INTEGER(4)::0_4,3_4]:)
