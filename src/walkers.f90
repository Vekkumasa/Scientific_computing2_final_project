module walkers
  implicit none

  type :: walker
    integer :: id, x, y, health_status ! 1 = Healthy, 2 = Sick, 3 = Immune
  end type walker
  
  contains
    type (walker) function createWalker(n, id, infected, vaccinated)
      implicit none
      integer, intent(in) :: n, id
      integer :: x,y,health_status
      logical, intent(in) :: infected, vaccinated
      real :: random
      call random_number(random)
      x = floor(n * random)
      call random_number(random)
      y = floor(n * random)

      if (infected) then
        health_status = 2
      else if (vaccinated) then
        health_status = 3
      else
        health_status = 1
      end if

      createWalker = walker(id, x, y, health_status)
    end function createWalker

    type (walker) function handleInfection(w, probability)
      implicit none
      real, intent(in) :: probability
      real :: randomNumber
      type (walker), intent(in) :: w
      handleInfection%id = w%id
      handleInfection%x = w%x
      handleInfection%y = w%y
      call random_number(randomNumber)
      if (randomNumber < probability .or. w%health_status == 2) then
        handleInfection%health_status = 2
      else
        handleInfection%health_status = 1
      end if

    end function handleInfection

    type (walker) function handleMove(w, arraySize)
      implicit none
      type (walker), intent(in) :: w
      real :: probability
      integer, intent(in) :: arraySize
      call random_number(probability)
    !  print '(f16.4)', probability

      if (probability <= 0.25) then
        handleMove = moveDown(w, arraySize)
      else if (probability > 0.25 .and. probability <= 0.5) then
        handleMove = moveUp(w, arraySize)
      else if (probability > 0.5 .and. probability <= 0.75) then
        handleMove = moveLeft(w, arraySize)
      else
        handleMove = moveRight(w, arraySize)
      end if
    end function handleMove


    type (walker) function moveDown(w, n)
      implicit none
      type (walker), intent(in) :: w
      integer, intent(in) :: n

      if (w%y -1 < 0) then
        moveDown%y = n
      else
        moveDown%y = w%y -1
      end if

      moveDown%x = w%x
      moveDown%health_status = w%health_status
      moveDown%id = w%id
    end function moveDown

    type (walker) function moveUp(w, n)
      implicit none
      type (walker), intent(in) :: w
      integer, intent(in) :: n
      if (w%y +1 > n) then
        moveUp%y = 0
      else
        moveUp%y = w%y +1
      end if
      moveUp%x = w%x
      moveUp%health_status = w%health_status
      moveUp%id = w%id
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
      moveLeft%id = w%id
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
      moveRight%id = w%id
    end function moveRight

    type (walker) function heal(w, probability)
    ! 1 = Healthy, 2 = Sick, 3 = Immune
      implicit none
      type (walker), intent(in) :: w
      real, intent(in) :: probability
      real :: randomNumber
      call random_number(randomNumber)
      if (randomNumber < probability) then
        heal%health_status = 3
      else
        heal%health_status = w%health_status
      end if
      heal%x = w%x
      heal%y = w%y
      heal%id = w%id
    end function heal

    function setArray(n) result (return_value)
      implicit none
      integer, intent(in) :: n
      type (walker), dimension(n) :: return_value
      type (walker) :: w
      w = walker(-1,-1,-1, -1)
      return_value = w

    end function setArray


end module walkers