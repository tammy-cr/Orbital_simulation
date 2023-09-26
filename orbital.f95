program Orbital
  !Thammy Carmona
  !CSI501
  !HW 12
  !11/30/2022
  !Write a program that simulates a planet going around the star

 implicit none
 
  real*8 :: tE, PE, KE, kinetic, potential, h, TTime,t
  integer :: i, N, j
  double precision :: Planet(4),fP(4)

  !put planets inside an array of size 4
  !single planet going around a massive star

   !Planet (1)=  x-coordinate of planet
   !Planet (2)=  x-velocity of planet
   !Planet (3)=  y-coordinate of planet
   !Planet (4)=  y-velocity of planet
 
 
    print*, "What is x-position?"
    read(*,*) planet (1)
    print*, "What is the y-position"
    read(*,*) planet (3)
    print*, "What is x-velocity?"
    read(*,*) planet (2)
    print*, "What is the y-velocity"
    read(*,*) planet (4)
    print*, "What is the time increment?"
    read(*,*) h
    print*, "What is the total time"
    read(*,*) TTime
   

   
 !2 dimensional problem, no Z
 !do loop with forward euler for 2-d gravity

    open (20, file='orbital4.txt')
    write (20, *) '#x-coordinate,y-coordinate,PE,KE,total energy, t'
    
    
 !use the kinetic and potential energy functions before to get the initial energies

    KE= kinetic (Planet)
    PE= potential (Planet)
    tE= KE+PE

    print*, "Initial kinetic is", KE
    print*, " Initial potential energy is",PE 
    print*, "Initial total energy is", tE

    t=0.0
    N= TTime/h
    do j= 1,N
      call fprime (Planet, fP)

      do i= 1,4
       Planet (i)= Planet (i) + h*fP(i)
      enddo
      KE= kinetic (planet)
      PE = potential (planet)
       t= t + h 
       tE= KE+PE
       write (20, *) Planet(1),Planet(3),PE,KE,tE,t
    enddo
        


    print*, ""
    print*, "Final kinetic energy is",KE 
    print*, "Final potential energy is", PE 
    print*, "Final total energy is", tE


  close(20)
 end program Orbital

  !using subroutine to compute 
  subroutine fprime (planet, fP)
  implicit none

   real*8 :: Planet (4), fP (4), r
  
   !eliminate g and M and use fake units
 
   r= sqrt (Planet (1) **2 + Planet (3) **2)
   fP(1)= Planet (2)
   fP(2)= -Planet (1)/r**3
   fP (3)= Planet (4)
   fP (4)= -Planet (3)/ r**3

  end subroutine fprime

  function kinetic (planet) result (KE)
   implicit none

   real*8:: Planet(4), KE
   
   KE=(1.0/2.0)*(Planet (2)**2 +Planet(4)**2)
  
   end function kinetic

  function potential (planet) result (PE)
   implicit none

   real*8:: Planet(4), PE
   
   PE= -1.0/(sqrt(planet (1)**2 + planet(3)**2))
  
   end function potential


  
