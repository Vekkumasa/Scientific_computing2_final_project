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

    subroutine printStatistics(array, n)
      implicit none
      type (walker) :: w
      integer :: sick, immune, i 
      integer, intent(in) :: n
      type (walker), intent(in) :: array(n)
      character(len=60) :: form = '(a10, i2, a9, i2)'
      sick = 0
      immune = 0
      do i = 1, n
        w = array(i)
        if (w%health_status == 2) then
          sick = sick +1
        else if (w%health_status == 3) then
          immune = immune +1
        end if
      end do
      print form, "Infected: ", sick, " Immune: ", immune
    end subroutine printStatistics

end module ArrayHandler