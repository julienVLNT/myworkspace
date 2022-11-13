PROGRAM INTRINSEQUE
    IMPLICIT NONE

    REAL :: u(4), m(3, 4)
    INTEGER :: i
    
    u = (/ -1.0, 0.0, 8.0, 3.14159 /)
    m = RESHAPE( (/ 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 1.0, 10.0, 11.0, 12.0 /), (/3, 4/) )

    PRINT *, "u"
    PRINT *, u
    PRINT *, ""
    PRINT *, "Min(u) =", MINVAL(u)
    PRINT *, "iMin(u) =", MINLOC(u)
    PRINT *, "Min(u > 0) =", MINVAL(u, MASK=u.gt.0.0)
    PRINT *, "iMin(u > 0) =", MINLOC(u, MASK=u.gt.0.0)
    PRINT *, "Max(u) =", MAXVAL(u)
    PRINT *, "iMax(u) =", MAXLOC(u)
    PRINT *, ""
    PRINT *, "mat"
    DO i = 1, SIZE(m, 1)
        PRINT *, m(i, :)
    END DO
    PRINT *, ""
    PRINT *, "m(1, 2) =", m(1, 2)
    PRINT *, "m(2, :) =", m(2, :)
    PRINT *, "m(:, 3) =", m(:, 3)
    PRINT *, "m(1:3, ::2) =", m(1:2, ::2)
    PRINT *, "Max(m) =", MAXVAL(m)
    PRINT *, "Max(m <= 10) =", MAXVAL(m, MASK=m.le.10.0)
    PRINT *, "Max(m, dim=2) =", MAXVAL(m, DIM=2)
    PRINT *, "iMax(m, dim=2) =", MAXLOC(m, DIM=2)
    PRINT *, "iMax(m) =", MAXLOC(m)
    PRINT *, "Min(m) =", MINVAL(m)
    PRINT *, "Min(m > 3) =", MINVAL(m, MASK=m.ge.3.0)
    PRINT *, "iMin(m) =", MINLOC(m)
    PRINT *, "iMin(m, reverse) =", MINLOC(m, BACK=.true.)
    PRINT *, ""
    PRINT *, "matmul(m, u) ="
    PRINT *, MATMUL(m, u)

END PROGRAM INTRINSEQUE
