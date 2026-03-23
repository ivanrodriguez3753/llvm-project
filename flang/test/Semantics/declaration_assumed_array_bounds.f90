! RUN: %python %S/test_errors.py %s %flang_fc1 
program main  
  implicit none 
contains 
  subroutine good(x, y, z, dim_a)
    integer :: dim_a(1)
    !valid cases
    !simple rank-1 integer array reference
    integer :: x(dim_a:) 
    !rank-1 integer array + scalar = rank-1 integer array
    integer :: y(dim_a + 2:)
    !rank-1 integer array via array constructor literal, with some non-const values
    integer :: z([1,2,dim_a(1) + x(dim_a(1))]:)
  end subroutine 

  subroutine bad(x, y, z, dim1_assumed, dim2_assumed)
    integer :: dim3(3, 3, 3)
    integer :: dim1_assumed(:)
    integer :: dim2_assumed(:,:)
    ! ERROR: Rank-1 integer array used as lower bounds in DECLARATION must have constant size
    integer :: x(dim1_assumed:)
    ! ERROR: Integer array used as lower bounds in DECLARATION must be rank-1 but is rank-3
    integer :: y(dim3:)
    ! Combining both errors in one declaration, plus integer-check from 
    ! type wrapper 
    ! ERROR: Integer array used as lower bounds in DECLARATION must be rank-1 but is rank-2
    ! ERROR: Rank-1 integer array used as lower bounds in DECLARATION must have constant size
    ! ERROR: Must have INTEGER type, but is REAL(4)
    integer :: z(dim2_assumed + 3.7:)
  end subroutine 
end program 
