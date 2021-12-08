program main
  use walkers
  use arrayhandler
  use walkersimulation
  implicit none
  type (walker) :: w
  type (walker), allocatable :: walkerArray(:)
  integer(kind=4) :: argsCount, arraySize, timeSteps, walkerCount, infected, vaccinated, vaccinatedWalkerCount = 0, id = 1
  integer(kind=4) :: i = 0
  argsCount = command_argument_count()

  if (argsCount /= 5) then
      write(0,'(a,a,a)') &
      'Please enter 5 integers: arraySize, timeSteps, walkerCount, Infected, Vaccinated'
      stop
  end if

  arraySize = readLine(1)
  timeSteps = readLine(2)
  walkerCount = readLine(3)
  infected = readLine(4)
  vaccinated = readLine(5)

  walkerArray = setArray(walkerCount)
  
  print *, arraySize, timeSteps, walkerCount, infected, vaccinated
  do i = 1, walkerCount
    if (i < infected) then
      w = createWalker(arraySize, id, .true., .false.)
    else if (vaccinatedWalkerCount < vaccinated) then
      w = createWalker(arraySize, id, .false., .true.)
      vaccinatedWalkerCount = vaccinatedWalkerCount + 1
    else
      w = createWalker(arraySize, id, .false., .false.)
    end if  
    walkerArray(id) = w
    id = id +1
  end do


  print *, 'simulate:', i
  call simulate(timeSteps, walkerCount, walkerarray, arraySize)

  contains
    integer function readLine(n)
      implicit none
      integer :: ios
      integer, intent(in) :: n
      character(len=20) :: argu

      call get_command_argument(n, argu)
      read(argu, *, iostat=ios) readLine
      if (ios == 0) then
        read(argu, *) readLine
        if (readLine < 0) then
          write(0,'(a,a,a)') &
            'Only positive integers'
          stop
        end if
      else
        write(0,'(a,a,a)') &
          'Please enter 5 integers: arraySize, timeSteps, walkerCount, Infected, Vaccinated'
        stop
      end if
    end function readLine
end program main