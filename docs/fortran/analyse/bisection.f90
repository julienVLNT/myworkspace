PROGRAM BISECTION
    IMPLICIT NONE

    DOUBLE PRECISION, PARAMETER :: rtol=1.0D-10, u1init=1.0D0, u2init=2.0D0
    DOUBLE PRECISION :: u1, u2, u3, u3tmp, y
    INTEGER :: i

    u1 = u1init
    u2 = u2init

    IF(f(u1)*f(u2) > 0) THEN
        WRITE(*,*) "L'intervalle considere ne contient aucune racine pour f !"
    END IF

    u3 = (u1 + u2) / 2.0
    u3tmp = u3

    i = 0
    DO
        IF(f(u1)*f(u3) < 0) THEN
            u2 = u3
            u3 = (u1 + u2) / 2.0
        ELSE IF(f(u1)*f(u3) > 0) THEN
            u1 = u3
            u3 = (u1 + u2) / 2.0
        END IF

        y = ABS((u3tmp - u3)) / u3tmp
        
        IF(y < rtol) EXIT
        
        i = i +1
        u3tmp = u3
    END DO

    WRITE(*,*) "Nombre d'terations  :", i
    WRITE(*,*) "Valeur de la racine :", u3
    WRITE(*,*) "Erreur relative     :", y
    WRITE(*,*) "Evaluation de f(x*) :", f(u3)

    CONTAINS

    FUNCTION f(x) RESULT(y)
        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(IN) :: x
        DOUBLE PRECISION :: y

        y = x**3 + 4*x**2 - 10.0

        RETURN
    END FUNCTION

END PROGRAM BISECTION
