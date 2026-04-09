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
  !TODO: Don't reuse error message from bounds-spec-list (above) for bounds-bounds-spec
  !ERROR: Pointer 'y' has rank 1 but the number of bounds specified is 2
  y(l_a2:) => x

  !ERROR: Pointer lower-bounds rank-1 expression must have known constant extent
  y(nonconst_size_arr:) => x
  !ERROR: Subscript expression has rank 2 greater than 1
  y(rank2_const_size_arr:) => x
  !ERROR: Subscript expression has rank 2 greater than 1
  !ERROR: Pointer lower-bounds rank-1 expression must have known constant extent
  y(rank2_nonconst_size_arr:) => x
  !ERROR: Must have INTEGER type, but is REAL(4)
  y([1.2]:) => x
end program 
