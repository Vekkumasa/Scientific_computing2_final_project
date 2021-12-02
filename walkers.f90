module walkers
  implicit none

  type :: walker
    integer :: x,y
    integer :: health_status ! 1 = Healthy, 2 = Sick, 3 = Immune
  end type walker
  
  contains
    type (walker) function moveDown(w, n)
      implicit none
      type (walker), intent(in) :: w
      integer, intent(in) :: n

      if (w%y +1 > n) then
        moveDown%y = 0
      else
        moveDown%y = w%y +1
      end if

      moveDown%x = w%x
      moveDown%health_status = w%health_status
    end function moveDown

    type (walker) function moveUp(w, n)
      implicit none
      type (walker), intent(in) :: w
      integer, intent(in) :: n
      if (w%y -1 < 0) then
        moveUp%y = n
      else
        moveUp%y = w%y -1
      end if
      moveUp%x = w%x
      moveUp%health_status = w%health_status
    end function moveUp

    type (walker) function moveLeft(w, n)
      implicit none
      type (walker), intent(in) :: w
      integer, intent(in) :: n
      if (w%x -1 < 0) then
        moveLeft%x = n
      else
        moveLeft%x = w%x -1
      end if
      moveLeft%y = w%y
      moveLeft%health_status = w%health_status
    end function moveLeft

    type (walker) function moveRight(w, n)
      implicit none
      type (walker), intent(in) :: w
      integer, intent(in) :: n
      if (w%x + 1 > n) then
        moveRight%x = 0
      else
        moveRight%x = w%x +1
      end if
      moveRight%y = w%y
      moveRight%health_status = w%health_status
    end function moveRight



end module walkers