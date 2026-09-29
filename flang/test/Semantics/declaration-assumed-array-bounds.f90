! RUN: %python %S/test_errors.py %s %flang_fc1 -Wsaved-local-in-spec-expr
module impure_bounds
contains
  impure integer function impf()
    impf = 1
  end function
  impure function impureZeroSize() result(r)
    integer :: r(0)
  end function
  impure function impureNonZero() result(r)
    integer :: r(2)
  end function
end module
! Rank-1 integer function result used as assumed-shape lower bounds.
module assumed_getter
contains
  pure function get_bounds() result(r)
    integer :: r(2)
    r = [8, 9]
  end function
  subroutine func_return_bound(from_func)
    ! Function result (rank-1 integer array) as assumed-shape lower bounds
    integer :: from_func(get_bounds():)
  end subroutine
end module
! Rank-1 integer PARAMETER array to be USE-associated as assumed-shape bounds.
module assumed_provider
  implicit none
  integer, parameter :: dims(3) = [5, 5, 5]
end module
program main
  implicit none
contains
  subroutine good(x, y, z, dim_a, scalar1, scalar2)
    integer :: dim_a(1)
    !valid cases
    !simple rank-1 integer array reference
    integer :: x(dim_a:)
    !rank-1 integer array + scalar = rank-1 integer array
    integer :: y(dim_a + 2:)
    !rank-1 integer array via array constructor literal, with some non-const values
    integer :: z([1,2,dim_a(1) + x(dim_a(1))]:)
    !rank-1 zero sized array is valid and should declare a scalar
    integer :: scalar1([integer::]:)
    integer :: empty_arr(0) = [integer::]
    !PORTABILITY: specification expression refers to local object 'empty_arr' (initialized and saved) [-Wsaved-local-in-spec-expr]
    integer :: scalar2(empty_arr:)
  end subroutine

  subroutine bad(x, y, z, dim1_assumed, dim2_assumed)
    integer :: dim3(3, 3, 3)
    integer :: dim1_assumed(:)
    integer :: dim2_assumed(:,:)
    ! invalid cases:
    !ERROR: Rank-1 integer array used as lower bounds in DECLARATION must have constant size
    integer :: x(dim1_assumed:)
    !ERROR: Integer array used as lower bounds in DECLARATION must be rank-1 but is rank-3
    integer :: y(dim3:)
    ! A non-INTEGER lower-bounds array is rejected by the Integer<> type wrapper
    ! before the rank/constant-size checks are reached.
    !ERROR: Must have INTEGER type, but is REAL(4)
    integer :: z(dim2_assumed + 3.7:)
  end subroutine

  ! Assumed-shape-bounds has a single (lower) bound, so the explicit-shape
  ! scenario that motivates droppedBoundsToCheck_ cannot arise here: there, a
  ! zero-size bounds array makes the entity scalar and the *other*, scalar bound
  ! -- which would have broadcast across the dimensions -- is dropped and must
  ! still be validated, e.g. `array([integer::] : someScalar)` drops someScalar.
  ! With only one bound there is no such bound to drop.  What can still leave a
  ! bound expression out of the shape yet requiring validation is:
  !   - a zero-size bounds array makes the entity a scalar (rank 0), so the lone
  !     lower-bound expression is not part of any shape.  Its value is trivially
  !     the empty array, but the expression itself -- e.g. an impure function
  !     reference -- is still checked (droppedBoundsToCheck_); or
  !   - an entity-decl's own array-spec overrides a DIMENSION attribute, so the
  !     attribute's bound expressions are not in the shape but are still checked
  !     (overriddenAttrArraySpecBounds).  This override path is not specific to
  !     assumed bounds -- whether the overridden DIMENSION uses rank-1 array
  !     bounds (RankOneBoundElement) or not is irrelevant.
  subroutine dropped_bounds(c)
    use impure_bounds
    ! Zero-size bounds array -> scalar; the lower-bound expression (not its
    ! trivial empty-array value) is validated, catching the impure reference.
    !ERROR: Invalid specification expression: reference to impure function 'impurezerosize'
    real :: a(impureZeroSize():)
    ! Same, but the zero-size assumed bounds come from a DIMENSION attribute.
    !ERROR: Invalid specification expression: reference to impure function 'impurezerosize'
    real, dimension(impureZeroSize():) :: b
    ! A non-zero assumed-bounds DIMENSION overridden by the entity-decl's own
    ! array-spec: the overridden RankOneBoundElement bounds are still validated.
    !ERROR: Invalid specification expression: reference to impure function 'impurenonzero'
    real, dimension(impureNonZero():) :: c([1,2,3]:)
    ! The override path is the same for an ordinary (non-assumed, non-ROBE)
    ! invalid DIMENSION bound.
    !ERROR: Invalid specification expression: reference to impure function 'impf'
    real, dimension(impf():) :: d([1,2,3]:[2,3,4])
  end subroutine

  ! USE-associated rank-1 integer parameter array as assumed-shape lower bounds.
  subroutine use_assoc(arr_ua)
    use assumed_provider, only: dims
    integer :: arr_ua(dims:)
  end subroutine

  ! Vector subscripts and array slices that yield rank-1 constant-size arrays are
  ! valid as assumed-shape lower bounds.
  subroutine subscripts_and_slices(vs, sl)
    integer, parameter :: p(3) = [5, 6, 7]
    ! Vector subscript producing a rank-1 constant-size array of bounds
    integer :: vs(p([1,3]):)
    ! Array slice producing a rank-1 constant-size array of bounds
    integer :: sl(p(1:3:2):)
  end subroutine

  ! The rank implied by the bounds array's constant size is subject to the
  ! maximum supported rank, and must not overflow when narrowed.
  subroutine maxrank_and_overflow(maxrank)
    ! A rank exactly at the maximum (15) is valid.
    integer, parameter :: b15(15) = 0
    integer :: maxrank(b15:)
    ! One past the maximum is rejected.
    integer, parameter :: b16(16) = 0
    !ERROR: DECLARATION rank-1 integer array bound(s) imply rank 16, which is greater than the maximum supported rank 15
    integer :: maxrank_n(b16:)
    ! A rank between the maximum and 32-bit overflow is rejected gracefully.
    integer :: n(2147483648_8) !2^31
    !ERROR: DECLARATION rank-1 integer array bound(s) imply rank 2147483648, which is greater than the maximum supported rank 15
    integer :: too_big(n:)
  end subroutine
end program
