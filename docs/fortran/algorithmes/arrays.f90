!  First programs in Fortran
!  Julien VALENTIN (2022)  julien.vlnt@gmail.com
!  Basic array manipulations in Fortran.

program arrays

    ! Declare a simple array of integers.
    integer, dimension(5) :: vec

    ! Declare a matrix now
    integer, dimension(5,3) :: imat
    
    ! These ones will be initialized later
    integer, dimension (:,:), allocatable :: iMat2, iMat3
    integer :: m = 3, n = 3

    ! One can also specify custom upper and lower bounds for the index
    integer, dimension(-4:4, 5) :: freak

    ! A few words about the attributes of an array.
    ! `rank` is the number of axis of the structure
    print *, "Rank of vec :", rank(vec)
    print *, "Rank of mat :", rank(imat)
    ! `size` is the number of elements in the structure
    print *, "Size of vec :", size(vec)
    print *, "Size of mat :", size(imat)
    print *, "Number of rows in mat :", size(imat(:,0))
    print *, "Number of columns in mat :", size(imat(0,:))

    ! Let get the upper and lower values of freak's index
    print *, "lbound(freak) = ", lbound(freak)
    print *, "ubound(freak) = ", ubound(freak)
    print *, "lower bound of the rows = ", lbound(freak(0,:))
    print *, "upper bound of the columns = ", ubound(freak(:,0))


    ! Allocate memory space for your object
    allocate (iMat2(m, n))

    do i = 1, m
        do j = 1, n
           iMat2(i,j) = (-1)**(i+j)
           print*, "iMat2(",i,",",j,") = ", iMat2(i,j)           
        end do      
    end do

    ! Always free what was allocated !
    deallocate(iMat2)

    print *, "-------------------------------------------------------------------------------"

    ! Pass a matrix as argument of a function
    allocate(iMat3(m,n))
    
    call FillIn(iMat3)

    do i = 1, m
        do j = 1, n
           print*, "iMat3(",i,",",j,") = ", iMat3(i,j)           
        end do      
    end do
    deallocate(iMat3)

contains
    subroutine fillIn(m)
        ! Function that initializes a matrix with i*j, inplace
        integer, intent(out) :: m(:,:)
        integer :: i, j
        do i= 1, size(m,2)
            do j=1, size(m,1)
                m(i,j) = i*j
            end do
        end do
    end subroutine

end program