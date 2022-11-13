PROGRAM PRINTER
    IMPLICIT NONE

    INTEGER :: i, j
    REAL :: vec(3), mat(3, 3)
    
    vec = (/ 1.0, 1.0, 1.0 /)
    
    DO i = 1, 3
        DO j = 1, 3
            IF(i==j) THEN
                mat(i, j) = 1.0
            ELSE
                mat(i, j) = 0.0
            ENDIF
        END DO
    END DO

    CALL printvec("vec", vec)
    CALL printmat("mat", mat)

    CONTAINS

    SUBROUTINE printvec(msg, vec)
        IMPLICIT NONE
        
        CHARACTER(*), INTENT(IN) :: msg
        REAL, INTENT(IN) :: vec(:)
        
        PRINT *, msg
        PRINT *, vec
        
        RETURN
    END SUBROUTINE printVec

    SUBROUTINE printmat(msg, mat)
        IMPLICIT NONE

        CHARACTER(*), INTENT(IN) :: msg
        REAL, INTENT(IN) :: mat(:,:)
        INTEGER :: i
        
        PRINT *, msg
        DO i = 1, SIZE(mat, 1)
            PRINT *, mat(i, :)
        END DO
        
        RETURN
    END SUBROUTINE

END PROGRAM PRINTER
