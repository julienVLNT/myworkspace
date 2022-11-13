PROGRAM INTERPOLATION
    IMPLICIT NONE

    DOUBLE PRECISION, ALLOCATABLE :: xdata(:), fdata(:)
    DOUBLE PRECISION :: x, y
    INTEGER :: i, j, N

    OPEN(UNIT=10, STATUS="unknown", FILE="interpolation.dat", ACTION="read")
    READ(10,*) N
    ALLOCATE(xdata(N))
    ALLOCATE(fdata(N))
    DO i=1,N
        READ(10,*) xdata(i), fdata(i)
    END DO

    OPEN(UNIT=11, STATUS="unknown", FILE="out.dat", ACTION="write")
    DO i=1,1000
        x = 1980.0D0 + (i-1)*(2000.0D0-1980.0D0)/(1000-1)
        CALL lagrange(N, xdata, fdata, x, y)
        WRITE(11,*) x, y
    END DO


    DEALLOCATE(xdata)
    DEALLOCATE(fdata)

    STOP

    CONTAINS

    SUBROUTINE lagrange(N, xdata, fdata, x, y)
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: N
        DOUBLE PRECISION, DIMENSION(N), INTENT(IN) :: xdata, fdata
        DOUBLE PRECISION, INTENT(IN) :: x
        DOUBLE PRECISION, INTENT(OUT) :: y
        DOUBLE PRECISION :: L
        INTEGER :: i, j
        
        y = 0.0D0
        DO i=1,N
            L = 1.0D0
            DO j=1,N
                IF(i == j) THEN
                    L = L*1.0D0
                ELSE
                    L = L * (x-xdata(j))/(xdata(i)-xdata(j))
                END IF
            END DO
            y = y + L*fdata(i)
        END DO
        RETURN
    END SUBROUTINE lagrange

END PROGRAM INTERPOLATION
