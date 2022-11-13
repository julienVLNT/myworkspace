PROGRAM NEWTON   
    IMPLICIT NONE

    DOUBLE PRECISION, PARAMETER :: rtol=1.0D-10, xinit=1.0
    DOUBLE PRECISION :: x, xtmp
    INTEGER :: i

    x = xinit
    xtmp = xinit

    i = 0
    DO
        x = xtmp - f(xtmp)/fprime(xtmp)
        i = i + 1
        IF(ABS(x-xtmp)/xtmp < rtol) EXIT
        xtmp = x
    END DO

    WRITE(*,*) "Nombre d'iterations : ", i
    WRITE(*,*) "Valeur de la racine :", x
    WRITE(*,*) "Erreur absolue      :", ABS(x)
    WRITE(*,*) "Evaluation f(x*)    :", f(x) 

    CONTAINS

    FUNCTION f(x) RESULT(y)
        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(IN) :: x
        DOUBLE PRECISION :: y

        y = x**3 + 4*x**2 - 10.0

        RETURN
    END FUNCTION

    FUNCTION fprime(x) RESULT(y)
        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(IN) :: x
        DOUBLE PRECISION :: y

        y = 3*x**2 + 8*x

        RETURN
    END FUNCTION

END PROGRAM NEWTON
