! Test lowering of explicit-shape bounds using rank-1 integer arrays
! (RankOneBoundElement in the evaluate representation).
! RUN: bbc -emit-hlfir -o - %s 2>&1 | FileCheck %s

! Test with PARAMETER rank-1 bounds (should fold to scalar constants via array indexing)
module test_param
contains
  subroutine test_param_bounds()
    integer, parameter :: dims(3) = [2, 3, 4]
    real :: a(dims)
    a(1,1,1) = 1.0
  end subroutine
end module
! CHECK-LABEL: func.func @_QMtest_paramPtest_param_bounds()
! CHECK:  hlfir.designate {{.*}} (%c1{{.*}}) : ({{.*}}, index) -> !fir.ref<i64>
! CHECK:  fir.load {{.*}} : !fir.ref<i64>
! CHECK:  fir.convert {{.*}} : (i64) -> index

! Test with rank-1 dummy as upper bounds only.
module test_dummy_upper
contains
  subroutine test_dummy_upper_bounds(n)
    integer, intent(in) :: n(3)
    real :: a(n)
    a(1,1,1) = 1.0
  end subroutine
end module
! CHECK-LABEL: func.func @_QMtest_dummy_upperPtest_dummy_upper_bounds(
! CHECK:  hlfir.elemental {{.*}} -> !hlfir.expr<3xi64>
! CHECK:  ^bb0(%arg{{.*}}: index):
! CHECK:    hlfir.designate {{.*}} (%arg{{.*}}) : ({{.*}}, index) -> !fir.ref<i32>
! CHECK:    fir.load {{.*}} : !fir.ref<i32>
! CHECK:    fir.convert {{.*}} : (i32) -> i64
! CHECK:  %[[C1:.*]] = arith.constant 1 : index
! CHECK:  hlfir.apply {{.*}}, %[[C1]] : (!hlfir.expr<3xi64>, index) -> i64
! CHECK:  %[[C2:.*]] = arith.constant 2 : index
! CHECK:  hlfir.apply {{.*}}, %[[C2]] : (!hlfir.expr<3xi64>, index) -> i64
! CHECK:  %[[C3:.*]] = arith.constant 3 : index
! CHECK:  hlfir.apply {{.*}}, %[[C3]] : (!hlfir.expr<3xi64>, index) -> i64

! Test with both lower and upper rank-1 bounds.
module test_dummy_both
contains
  subroutine test_dummy_both_bounds(lb, ub)
    integer, intent(in) :: lb(2), ub(2)
    real :: a(lb:ub)
    a(1,1) = 1.0
  end subroutine
end module
! CHECK-LABEL: func.func @_QMtest_dummy_bothPtest_dummy_both_bounds(
! CHECK:  hlfir.elemental {{.*}} -> !hlfir.expr<2xi64>
! CHECK:  %[[C1:.*]] = arith.constant 1 : index
! CHECK:  hlfir.apply {{.*}}, %[[C1]] : (!hlfir.expr<2xi64>, index) -> i64
! CHECK:  hlfir.elemental {{.*}} -> !hlfir.expr<2xi64>
! CHECK:  %[[C1_1:.*]] = arith.constant 1 : index
! CHECK:  hlfir.apply {{.*}}, %[[C1_1]] : (!hlfir.expr<2xi64>, index) -> i64
! CHECK:  hlfir.elemental {{.*}} -> !hlfir.expr<2xi64>
! CHECK:  %[[C2:.*]] = arith.constant 2 : index
! CHECK:  hlfir.apply {{.*}}, %[[C2]] : (!hlfir.expr<2xi64>, index) -> i64
! CHECK:  hlfir.elemental {{.*}} -> !hlfir.expr<2xi64>
! CHECK:  %[[C2_1:.*]] = arith.constant 2 : index
! CHECK:  hlfir.apply {{.*}}, %[[C2_1]] : (!hlfir.expr<2xi64>, index) -> i64

! Test broadcast of scalar lower bound with rank-1 upper bounds.
module test_broadcast
contains
  subroutine test_broadcast_bounds(ub)
    integer, intent(in) :: ub(2)
    real :: a(0:ub)
    a(0,0) = 1.0
  end subroutine
end module
! CHECK-LABEL: func.func @_QMtest_broadcastPtest_broadcast_bounds(
! CHECK:  hlfir.elemental {{.*}} -> !hlfir.expr<2xi64>
! CHECK:  %[[U1:.*]] = arith.constant 1 : index
! CHECK:  hlfir.apply {{.*}}, %[[U1]] : (!hlfir.expr<2xi64>, index) -> i64
! CHECK:  %[[U2:.*]] = arith.constant 2 : index
! CHECK:  hlfir.apply {{.*}}, %[[U2]] : (!hlfir.expr<2xi64>, index) -> i64
! CHECK:  fir.shape_shift {{.*}} : (index, index, index, index) -> !fir.shapeshift<2>
