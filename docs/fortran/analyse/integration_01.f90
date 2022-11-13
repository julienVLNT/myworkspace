PROGRAM RECTANGLE
    IMPLICIT NONE

    DOUBLE PRECISION, PARAMETER :: a=0.0D0, b=2.0D0*(4D0*atan (1.0D0))
    DOUBLE PRECISION :: h, int
    INTEGER :: i, N

    WRITE(*,*) "Nombre de points de la discretisation : "
    READ(*,*) N
    
    h = (b-a)/N

    int = 0
    DO i = 1, N
        int = int + h*COS(0.5D0*(a+i*h+a+(i+1)*h))
    END DO

    WRITE(*,*) "L'integrale de cos() entre 0 et 2pi vaut ", int

END PROGRAM RECTANGLE