module ArrayHandler
  use walkers
  implicit none

  contains
    subroutine printArray(array, n)
      implicit none
      integer, intent(in) :: n
      integer :: i, j
      character(len=60) :: form = '(a3, i3, a3, i2, a3, i2, a9, i1)'
      type (walker), intent(in) :: array(n,n)
      type (walker) :: iterable

      do i = 1, n
        do j = 1, n
          iterable = array(i,j)
          if (iterable%x >= 0) then
            print form, "id:", iterable%id, " X:", iterable%x, " Y:", iterable%y, " Health: ", iterable%health_status
          end if
        end do
      end do

    end subroutine printArray

end module ArrayHandler

!print '(a2, i2, a3, i2, a8, i2)',  "X:", iterable%x, " Y:", iterable%y, " Health:", iterable%health_status