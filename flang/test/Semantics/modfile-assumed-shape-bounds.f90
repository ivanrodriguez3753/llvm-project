! RUN: %python %S/test_modfile.py %s %flang_fc1
! Test mod-file generation for F2023 assumed-shape lower bounds using rank-1
! integer arrays (AssumedShapeBoundsSpec / RankOneBoundElement).
!
! An assumed-shape array has ONLY a lower bound; the upper bound is assumed
! (rendered as a bare `:`). Each dimension's bound IS stored as a
! RankOneBoundElement, exactly as for explicit-shape -- a symbol dump renders
! them as `__builtin_rank1_bound_element(...,dim=N):` per dimension (see
! rank1-bound-element-symbols.f90). What is specific to the MOD FILE is that
! PutShape/HasRankOneBound recover the ROBE's whole base expression and write
! that instead (e.g. `a(__builtin_int(lbs,kind=8):)`), so the synthetic
! spelling never reaches a mod file for an assumed-shape array.
!
! That is a structural guarantee, not an accident of the cases below. The
! synthetic spelling reaches a mod file only when a LONE ROBE is formed as some
! other entity's bound and cannot be reduced -- for explicit-shape that happens
! via `ubound(a,dim)`, which folds to the stored per-dimension ROBE (see
! mmergefallback in modfile-explicit-shape-bounds.f90). Assumed-shape has no
! such path: `lbound(a,dim)` never folds to a ROBE, because the dimension can
! never be proven nonempty (the upper bound is always `:`), so GetLBOUND
! declines and the inquiry stays live. The exact analog of mmergefallback --
! an irreducible `merge(n,m,c)` base plus `b(lbound(a,2))` -- emits zero
! occurrences of the synthetic spelling.
!
! A subsequent `lbound(a,dim)` inquiry on such an array is a separate matter
! from how `a` itself is declared: it stays as a live `lbound()` inquiry (see
! module mlbinquiry) UNLESS the declared lower bound happens to be the
! compile-time constant 1 in that dimension, in which case it folds to `1`
! regardless of extent -- LBOUND is always 1 on an empty dimension anyway, so
! substituting the declared value 1 is safe even without proving the dimension
! nonempty (see module mlbconstfold). This mirrors the per-dimension scalar
! lower-bound spelling (`a(lbs(1):, lbs(2):)`), which folds the same way; the
! rank-1 array spelling doesn't fold any less than that, it just needs the
! folding to happen when `lbound()` is queried rather than when `a` itself is
! declared, since the per-dimension RankOneBoundElement is deliberately left
! unfolded at declaration time (mod-file printing above depends on finding it
! intact). Coverage here therefore focuses on the variety of base-expression
! forms that can appear in the lower-bound position and how each round-trips
! through the mod file.

! Rank-1 dummy as assumed-shape lower bounds
module m1
contains
subroutine sub1(lbs, a)
  integer, intent(in) :: lbs(3)
  real :: a(lbs:)
end subroutine
end module

!Expect: m1.mod
!module m1
!contains
!subroutine sub1(lbs,a)
!integer(4),intent(in)::lbs(1_8:3_8)
!real(4)::a(__builtin_int(lbs,kind=8):)
!end
!end

! Elementwise arithmetic (multiply) lower bounds -> 2_4*lbs stays inside the
! conversion
module m2
contains
subroutine sub2(lbs, a)
  integer, intent(in) :: lbs(2)
  real :: a(2*lbs:)
end subroutine
end module

!Expect: m2.mod
!module m2
!contains
!subroutine sub2(lbs,a)
!integer(4),intent(in)::lbs(1_8:2_8)
!real(4)::a(__builtin_int(2_4*lbs,kind=8):)
!end
!end

! PARAMETER rank-1 array lower bounds -> folds to a constant array constructor
module mparam
  integer, parameter :: dims(3) = [5, 10, 15]
contains
  subroutine s(a)
    real :: a(dims:)
  end subroutine
end module

!Expect: mparam.mod
!module mparam
!integer(4),parameter::dims(1_8:3_8)=[INTEGER(4)::5_4,10_4,15_4]
!contains
!subroutine s(a)
!real(4)::a([INTEGER(8)::5_8,10_8,15_8]:)
!end
!end

! Elementwise arithmetic (add) lower bounds -> reduces to n+1_4
module marith_add
contains
  subroutine s(n, a)
    integer, intent(in) :: n(2)
    real :: a(n+1:)
  end subroutine
end module

!Expect: marith_add.mod
!module marith_add
!contains
!subroutine s(n,a)
!integer(4),intent(in)::n(1_8:2_8)
!real(4)::a(__builtin_int(n+1_4,kind=8):)
!end
!end

! Explicit kind conversion base int(n,4) -> nested conversion
module mkindconv
contains
  subroutine s(n, a)
    integer(8), intent(in) :: n(2)
    real :: a(int(n,4):)
  end subroutine
end module

!Expect: mkindconv.mod
!module mkindconv
!contains
!subroutine s(n,a)
!integer(8),intent(in)::n(1_8:2_8)
!real(4)::a(__builtin_int(__builtin_int(n,kind=4),kind=8):)
!end
!end

! Array SECTION base n(1:2) -> section survives as a subscript triplet
module msection
contains
  subroutine s(n, a)
    integer, intent(in) :: n(4)
    real :: a(n(1:2):)
  end subroutine
end module

!Expect: msection.mod
!module msection
!contains
!subroutine s(n,a)
!integer(4),intent(in)::n(1_8:4_8)
!real(4)::a(__builtin_int(n(1_8:2_8:1_8),kind=8):)
!end
!end

! Vector-subscripted array base n(idx)
module mvecsub
contains
  subroutine s(n, idx, a)
    integer, intent(in) :: n(4)
    integer, intent(in) :: idx(2)
    real :: a(n(idx):)
  end subroutine
end module

!Expect: mvecsub.mod
!module mvecsub
!contains
!subroutine s(n,idx,a)
!integer(4),intent(in)::n(1_8:4_8)
!integer(4),intent(in)::idx(1_8:2_8)
!real(4)::a(__builtin_int(n(__builtin_int(idx,kind=8)),kind=8):)
!end
!end

! Array CONSTRUCTOR base [n(1),n(2)]
module marrcons
contains
  subroutine s(n, a)
    integer, intent(in) :: n(2)
    real :: a([n(1), n(2)]:)
  end subroutine
end module

!Expect: marrcons.mod
!module marrcons
!contains
!subroutine s(n,a)
!integer(4),intent(in)::n(1_8:2_8)
!real(4)::a([INTEGER(8)::__builtin_int(n(1_8),kind=8),__builtin_int(n(2_8),kind=8)]:)
!end
!end

! Elemental INTRINSIC base abs(n)
module melemintrin
contains
  subroutine s(n, a)
    integer, intent(in) :: n(2)
    real :: a(abs(n):)
  end subroutine
end module

!Expect: melemintrin.mod
!module melemintrin
!contains
!subroutine s(n,a)
!integer(4),intent(in)::n(1_8:2_8)
!real(4)::a(__builtin_int(abs(n),kind=8):)
!end
!end

! Array CONSTRUCTOR with an IMPLIED-DO base [(n(i),i=1,2)]
module mimplieddo
contains
  subroutine s(n, a)
    integer, intent(in) :: n(4)
    real :: a([(n(i), i=1,2)]:)
  end subroutine
end module

!Expect: mimplieddo.mod
!module mimplieddo
!contains
!subroutine s(n,a)
!integer(4),intent(in)::n(1_8:4_8)
!real(4)::a(__builtin_int([INTEGER(4)::(n(__builtin_int(__builtin_int(i,kind=4),kind=8)),INTEGER(8)::i=1_8,2_8,1_8)],kind=8):)
!end
!end

! Elemental intrinsic with a non-integer array arg -> merge(n,m,c)
module mmerge
contains
  subroutine s(n, m, c, a)
    integer, intent(in) :: n(2), m(2)
    logical, intent(in) :: c(2)
    real :: a(merge(n, m, c):)
  end subroutine
end module

!Expect: mmerge.mod
!module mmerge
!contains
!subroutine s(n,m,c,a)
!integer(4),intent(in)::n(1_8:2_8)
!integer(4),intent(in)::m(1_8:2_8)
!logical(4),intent(in)::c(1_8:2_8)
!real(4)::a(__builtin_int(merge(n,m,c),kind=8):)
!end
!end

! User/specification FUNCTION result base gb()
module muserfunc
contains
  pure function gb() result(r)
    integer :: r(2)
    r = [3, 4]
  end function
  subroutine s(a)
    real :: a(gb():)
  end subroutine
end module

!Expect: muserfunc.mod
!module muserfunc
!contains
!pure function gb() result(r)
!integer(4)::r(1_8:2_8)
!end
!subroutine s(a)
!real(4)::a(__builtin_int(gb(),kind=8):)
!end
!end

! Whole array COMPONENT base x%c
module mcomp
  type t
    integer :: c(2)
  end type
contains
  subroutine s(x, a)
    type(t), intent(in) :: x
    real :: a(x%c:)
  end subroutine
end module

!Expect: mcomp.mod
!module mcomp
!type::t
!integer(4)::c(1_8:2_8)
!end type
!contains
!subroutine s(x,a)
!type(t),intent(in)::x
!real(4)::a(__builtin_int(x%c,kind=8):)
!end
!end

! Scalar COMPONENT of an array parent base x%c (parent x(2))
module mcompparent
  type t
    integer :: c
  end type
contains
  subroutine s(x, a)
    type(t), intent(in) :: x(2)
    real :: a(x%c:)
  end subroutine
end module

!Expect: mcompparent.mod
!module mcompparent
!type::t
!integer(4)::c
!end type
!contains
!subroutine s(x,a)
!type(t),intent(in)::x(1_8:2_8)
!real(4)::a(__builtin_int(x%c,kind=8):)
!end
!end

! Inquiry-valued base, constant: lbound() WITHOUT dim= is rank-1, so it is a
! bounds base (with dim= it would be scalar, i.e. an ordinary lower-bound-spec
! that never reaches this feature at all).  Here it folds to the constant [1],
! so the subsequent lbound(a,1) folds to 1 -- see mlbconstfold for why a
! declared lower bound of exactly 1 folds without proving the dimension
! nonempty.
module minqconst
contains
  subroutine s(a, b)
    real :: a(lbound([1,2]):)
    real :: b(lbound(a,1):)
    b(1) = 1.0
  end subroutine
end module

!Expect: minqconst.mod
!module minqconst
!contains
!subroutine s(a,b)
!real(4)::a([INTEGER(8)::1_8]:)
!real(4)::b(1_8:)
!end
!end

! Same base spelling, but not constant: lbound(x) on an assumed-shape x folds
! to an array constructor of per-dimension descriptor inquiries rather than to
! constants, so nothing about a's lower bound is known at compile time and the
! subsequent lbound(a,1) stays a live inquiry.
module minqdyn
contains
  subroutine s(x, a, b)
    real :: x(:,:)
    real :: a(lbound(x):)
    real :: b(lbound(a,1):)
    b(1) = 1.0
  end subroutine
end module

!Expect: minqdyn.mod
!module minqdyn
!contains
!subroutine s(x,a,b)
!real(4)::x(:,:)
!real(4)::a([INTEGER(8)::lbound(x,dim=1,kind=8),lbound(x,dim=2,kind=8)]:)
!real(4)::b(__builtin_int(lbound(a,1_4),kind=8):)
!end
!end

! An inquiry into the assumed-shape lower bound: lbound(a,dim) does NOT fold to
! a single RankOneBoundElement (contrast explicit-shape ubound, which does). It
! stays as the `lbound()` inquiry, so no `__builtin_rank1_bound_element(...)`
! spelling is ever produced for an assumed-shape array.
module mlbinquiry
contains
  subroutine s(lbs, a, b)
    integer, intent(in) :: lbs(2)
    real :: a(lbs:)
    real :: b(lbound(a,2):)
    b(1) = 1.0
  end subroutine
end module

!Expect: mlbinquiry.mod
!module mlbinquiry
!contains
!subroutine s(lbs,a,b)
!integer(4),intent(in)::lbs(1_8:2_8)
!real(4)::a(__builtin_int(lbs,kind=8):)
!real(4)::b(__builtin_int(lbound(a,2_4),kind=8):)
!end
!end

! Same shape of inquiry as mlbinquiry, but the rank-1 base is a PARAMETER
! (compile-time constant), and its first element is 1. lbound(a,1) folds to
! 1 per-dimension, same as the per-dimension scalar spelling
! a(lbs(1):, lbs(2):) would; lbound(a,2) does not fold, since 5 is not
! provably nonempty (assumed-shape never has a static upper bound to check).
!
! b and c declare the SAME two lower bounds by the two different routes, and
! agree on which of them folds:
!   b -- two scalar lower-bound-specs, so plain AssumedShapeSpec, no ROBE.
!        Each dimension is printed separately: `1_8:` and `lbound(a,2):`.
!   c -- one rank-1 array constructor, so AssumedShapeBoundsSpec/ROBE.
!        HasRankOneBound fires and the WHOLE base is printed once, with the
!        per-element fold already baked in: `[1_8,lbound(a,2)]:`.
! The constructor is mixed -- element 1 folded to a constant, element 2 stayed
! a live inquiry -- because Analyze(AssumedShapeBoundsSpec&) folds the base as
! a whole before wrapping each dimension in a RankOneBoundElement. So the
! mod-file spelling differs between b and c even though the bounds agree.
!
! d says the same thing again as a whole-array inquiry, and folds LESS than c
! does, for a reason worth pinning: GetLBOUNDs resolves dim 1 (to 1) but not
! dim 2, and AsExtentArrayExpr is all-or-nothing -- one unresolved dimension
! discards the whole array -- so lbound(a) does not fold at all, where c's
! explicitly element-wise spelling folded the element it could. d is also the
! strongest negative control in this file for the guarantee described at the
! top: LBOUND with no DIM= is an inquiry, not an elemental intrinsic, so
! ExtractRankOneElement rejects it at its "transformational intrinsic" fallback
! and the RankOneBoundElement is genuinely irreducible -- a symbol dump shows
! it surviving as __builtin_rank1_bound_element(...lbound(a)...,dim=N). It
! still reaches the mod file as the plain base expression. (mmergefallback
! below fails reduction differently: merge() IS elemental, and is rejected only
! for its LOGICAL argument.)
!
! e is d's positive counterpart, and shows the all-or-nothing threshold from
! the other side: a2's bounds are all 1, so GetLBOUNDs resolves EVERY dimension
! and AsExtentArrayExpr succeeds, folding lbound(a2) the whole way down to
! [1_8,1_8]. Nothing about the base expression differs between d and e -- both
! are a bare whole-array lbound() -- only whether every dimension happened to
! be foldable. e is also the sharpest regression test here for folding the
! RankOneBoundElement at the point of the lbound() query: without that, no
! dimension folds, AsExtentArrayExpr bails, and e would render like d does.
module mlbconstfold
contains
  subroutine s(a, a2, b, c, d, e)
    integer, parameter :: lbs(2) = [1, 5]
    integer, parameter :: lbs_1s(2) = [1,1]
    real :: a(lbs:)
    real :: a2(lbs_1s:)
    real :: b(lbound(a,1):, lbound(a,2):)
    real :: c([lbound(a,1), lbound(a,2)]:)
    real :: d(lbound(a):)
    real :: e(lbound(a2):)
    b(1,5) = 1.0
  end subroutine
end module

!Expect: mlbconstfold.mod
!module mlbconstfold
!contains
!subroutine s(a,a2,b,c,d,e)
!real(4)::a([INTEGER(8)::1_8,5_8]:)
!real(4)::a2([INTEGER(8)::1_8,1_8]:)
!real(4)::b(1_8:,__builtin_int(lbound(a,2_4),kind=8):)
!real(4)::c([INTEGER(8)::1_8,__builtin_int(lbound(a,2_4),kind=8)]:)
!real(4)::d(__builtin_int(lbound(a),kind=8):)
!real(4)::e([INTEGER(8)::1_8,1_8]:)
!end
!end

! Direct analog of mmergefallback in modfile-explicit-shape-bounds.f90, which
! is the case where explicit-shape DOES emit the synthetic
! `__builtin_rank1_bound_element(...)` spelling into a mod file: an irreducible
! base (elemental merge() with a LOGICAL mask, which the reduction helper does
! not recurse into) plus a bound inquiry on the resulting array. Assumed-shape
! has no corresponding path -- lbound(a,2) does not fold to a lone ROBE, it
! stays a live inquiry -- so no synthetic spelling is emitted here. This is the
! negative control for the structural guarantee described at the top.
module mmergefallback
contains
  subroutine s(n, m, c, a, b)
    integer, intent(in) :: n(2), m(2)
    logical, intent(in) :: c(2)
    real :: a(merge(n, m, c):)
    real :: b(lbound(a,2):)
    b(1) = 1.0
  end subroutine
end module

!Expect: mmergefallback.mod
!module mmergefallback
!contains
!subroutine s(n,m,c,a,b)
!integer(4),intent(in)::n(1_8:2_8)
!integer(4),intent(in)::m(1_8:2_8)
!logical(4),intent(in)::c(1_8:2_8)
!real(4)::a(__builtin_int(merge(n,m,c),kind=8):)
!real(4)::b(__builtin_int(lbound(a,2_4),kind=8):)
!end
!end
