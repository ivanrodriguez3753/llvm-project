! RUN: %python %S/test_errors.py %s %flang_fc1 -Wautomatic-in-main-program -Wsaved-local-in-spec-expr
program main 
  real, allocatable, dimension(:,:), target :: x
  real, pointer, dimension(:,:) :: y, z, a
  integer :: l_a(2) = [1,1], u_a(2) = [6,6]
  integer :: l_s = 1, u_s = 6
  integer, allocatable :: nonconst_size_arr(:)
  integer :: const_size_arr(2), rank3_array(1,1,1)


  !valid cases
  y(l_a:u_a) => x
  
  y(l_s:u_a) => x
  z(l_a:u_s) => x
  a(1:u_a) => x

  !negative cases 
  !ERROR: POINTER-ASSIGNMENT bounds integer rank-1 arrays must have the same size; lower bounds has 1 elements, upper bounds has 2 elements
  a([1] : [6,6]) => x
  !ERROR: Pointer 'a' has rank 2 but the extent of bounds array is 1
  a([1] : [6]) => x
  !ERROR: Pointer 'a' has rank 2 but the extent of bounds array is 1
  a([1] : 6) => x
  !ERROR: Pointer 'a' has rank 2 but the extent of bounds array is 1
  a(1 : [6]) => x

  !ERROR: POINTER-ASSIGNMENT lower bounds rank-1 expression must have known constant extent
  a(nonconst_size_arr : const_size_arr) => x
  !ERROR: POINTER-ASSIGNMENT upper bounds rank-1 expression must have known constant extent
  a(const_size_arr : nonconst_size_arr) => x
  !ERROR: POINTER-ASSIGNMENT lower bounds rank-1 expression must have known constant extent
  !ERROR: POINTER-ASSIGNMENT upper bounds rank-1 expression must have known constant extent
  a(nonconst_size_arr : nonconst_size_arr) => x
  !ERROR: POINTER-ASSIGNMENT lower bounds rank-1 expression must have known constant extent
  a(nonconst_size_arr : 6) => x
  !ERROR: POINTER-ASSIGNMENT upper bounds rank-1 expression must have known constant extent
  a(1 : nonconst_size_arr) => x

  !ERROR: Integer array used as lower bounds in POINTER-ASSIGNMENT must be rank-1 but is rank-3
  a(rank3_array : const_size_arr) => x
  !ERROR: Integer array used as upper bounds in POINTER-ASSIGNMENT must be rank-1 but is rank-3
  a(const_size_arr : rank3_array) => x
  !ERROR: Integer array used as lower bounds in POINTER-ASSIGNMENT must be rank-1 but is rank-3
  !ERROR: Integer array used as upper bounds in POINTER-ASSIGNMENT must be rank-1 but is rank-3
  a(rank3_array : rank3_array) => x

  !ERROR: Integer array used as lower bounds in POINTER-ASSIGNMENT must be rank-1 but is rank-3
  a(rank3_array : 6) => x
  !ERROR: Integer array used as upper bounds in POINTER-ASSIGNMENT must be rank-1 but is rank-3
  a(1 : rank3_array) => x
  !ERROR: POINTER-ASSIGNMENT lower bounds rank-1 expression must have known constant extent
  a(nonconst_size_arr : 6) => x
  !ERROR: POINTER-ASSIGNMENT upper bounds rank-1 expression must have known constant extent
  a(1 : nonconst_size_arr) => x
  !ERROR: Must have INTEGER type, but is REAL(4)
  a(1 : [3.2]) => x

end program 