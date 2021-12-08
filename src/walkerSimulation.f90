module walkerSimulation
  use walkers
  use arrayHandler
  implicit none

  contains
    subroutine simulate(timeSteps, walkerCount, walkerArray, arraySize)
      implicit none
      type (walker) :: w1, w2
      integer :: i,j,k
      integer(kind=4), intent(in) :: timeSteps, walkerCount, arraySize
      type (walker) :: walkerArray(walkerCount)

      do i = 1, timeSteps
        j = 1
        k = 1     
        do j = 1, walkerCount
          w1 = walkerArray(j)
          w1 = handleMove(w1, arraySize)
          if (w1%health_status == 2) then
            w1 = heal(w1, 0.05)
          end if
          walkerArray(j) = w1
          do k = 1, walkerCount
            w2 = walkerArray(k)
            if (w1%id /= w2%id) then
              if (w1%x == w2%x .and. w1%y == w2%y) then
                if (w1%health_status == 1 .and. w2%health_status == 2) then
                  w1 = handleInfection(w1, 0.5)
                  walkerArray(j) = w1
                end if
              end if
            end if
          end do
        end do

        print *, ''
        print '(i4)', walkerCount
        print '(a10, i3)', "#Timestep:", i
        call printWalkerArray(walkerArray, walkerCount)
        call printStatistics(walkerArray, walkerCount)
      end do

    end subroutine simulate

end module walkerSimulation