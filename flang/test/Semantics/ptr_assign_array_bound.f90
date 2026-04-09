! RUN: %python %S/test_errors.py %s %flang_fc1 -Wautomatic-in-main-program -Wsaved-local-in-spec-expr
! C1021 If lower-bounds-expr appears in a pointer-assignment-stmt but not upper-bounds-expr, it shall be a
! rank-one array of constant size equal to the rank of data-pointer-object.
program main 
  real, pointer, dimension(:) :: x, y, z
  real, pointer, dimension(:,:) :: xx, yy
  integer :: rank2_const_size_arr(1,1)
  integer, allocatable :: rank2_nonconst_size_arr(:,:)
  integer :: l_s = 1, l_a1(1) = [1], l_a2(2) = [2,2]
  integer, allocatable :: nonconst_size_arr(:)
  allocate(x(4:9))
  !valid cases
  y(l_a1:) => x
  z(l_a1 * 2 + 1:) => x
  allocate(xx(4:9, 4:9))
  yy(l_a2:) => xx
  yy(l_a2 * 2 + 1:) => xx

  !invalid cases
  !size mismatch 
  !ERROR: Pointer 'y' has rank 1 but the number of bounds specified is 2
  y(l_s:, l_s:) => x
  ! Checking that we don't reuse error message from bounds-spec-list (above) for bounds-bounds-spec
  !ERROR: Pointer 'y' has rank 1 but the extent of bounds array is 2
  y(l_a2:) => x

  !ERROR: Rank-1 integer array used as lower bounds in POINTER-ASSIGNMENT must have constant size
  y(nonconst_size_arr:) => x
  !ERROR: Integer array used as lower bounds in POINTER-ASSIGNMENT must be rank-1 but is rank-2
  y(rank2_const_size_arr:) => x
  !ERROR: Integer array used as lower bounds in POINTER-ASSIGNMENT must be rank-1 but is rank-2
  !ERROR: Rank-1 integer array used as lower bounds in POINTER-ASSIGNMENT must have constant size
  y(rank2_nonconst_size_arr:) => x
  !ERROR: Must have INTEGER type, but is REAL(4)
  y([1.2]:) => x
end program 
