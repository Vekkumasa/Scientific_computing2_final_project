program main
  use walkers
  implicit none
  type (walker) :: testi
  testi = walker(1,2, 1)
  print *, testi
  testi = moveDown(testi, 2)
  print *, testi
  testi = moveDown(testi, 2)
  print *, testi
  testi = moveDown(testi, 2)
  print *, testi
  testi = moveDown(testi, 2)
  
end program main