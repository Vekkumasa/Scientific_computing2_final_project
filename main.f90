program main
  use walkers
  use arrayHandler
  implicit none
  type (walker) :: testi
  type (walker), allocatable :: walkerArray(:,:)
  integer :: argsCount, arraySize, ios, timeSteps, id = 0
  character(len=20) :: argu
  argsCount = command_argument_count()

  if (argsCount == 1) then
    call get_command_argument(1, argu)
    read(argu, *, iostat=ios) arraySize
      if (ios == 0) then
        read(argu, *) arraySize
      else
        arraySize = 5
      end if
  else
    arraySize = 5
  end if
  if (arraySize < 0) arraySize = 5

  walkerArray = setArray(arraySize)

  testi = walker(1,2, 1, id)
  testi = heal(testi, 20.0)
  walkerArray(1,1) = testi
  testi = handleMove(testi, arraySize)
  call printArray(walkerArray, arraySize)
end program main