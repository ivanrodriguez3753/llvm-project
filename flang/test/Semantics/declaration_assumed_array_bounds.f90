program main  
  implicit none 
contains 
  subroutine foo_a(x, y, z, dim_a)
    integer :: dim_a(1)
    !valid cases
    !simple rank-1 integer array reference
    integer :: x(dim_a:) 
    !rank-1 integer array + scalar = rank-1 integer array
    integer :: y(dim_a + 2:)
    !rank-1 integer array via array constructor literal, with some non-const values
    integer :: z([1,2,dim_a(1) + x(dim_a(1))])
  end subroutine 
end program 
