program main
  use walkers
  implicit none

  type (walker) :: testi
  testi = walker(1,2)
  print *, testi
end program main