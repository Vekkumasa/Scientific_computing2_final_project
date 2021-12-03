module ArrayHandler
  use walkers
  implicit none

  contains
    subroutine printWalkerArray(array, n)
      implicit none
      integer, intent(in) :: n
      integer :: i
      character(len=60) :: form = '(a3, i3, a3, i2, a3, i2, a9, i1)'
      type (walker), intent(in) :: array(n)
      type (walker) :: iterable

      do i = 1, n
        iterable = array(i)
        if (iterable%id > 0) then
          print form, "id:", iterable%id, " X:", iterable%x, " Y:", iterable%y, " Health: ", iterable%health_status
        end if
      end do

    end subroutine printWalkerArray

end module ArrayHandler