program main
  implicit none
  integer::i,j,k,iarg,nlines
  real::E,B,dt,v_xi,v_yi,v_zi,v_z,v_y,m,q,f_z,a_z,L_y,L_z,x_z,x_y,x,t
  character(len=300)::arg,fname

 !E: strength of the electric field
 !B: strength of the magnetic field
 !dt: size of the time step 
 !v_xi: initial value of the x component of the velocity
 !v_yi: initial value of the y component of the velocity
 !v_zi: initial value of the z component of the velocity
 !v_z: z component of the velocity (updated)
 !v_y: y component of the velocity (updated)
 !m: mass
 !q: charge
 !f_z: force in the z direction
 !a_z: acceleration in the z direction
 !L_y: Length of the box in the y direction
 !L_z: length of the box in the z direction
 !x_z: position (z-axis)
 !x_y: position (y-axis)
 !x: position (x-axis)
 

  iarg=command_argument_count()
  if (iarg/=3) then
     call get_command_argument(0,arg)
     write(0,'(a,a,a)') &
      'usage: ',trim(arg),' Electric field strength (E), Magnetic field strength (B) and time step (dt)'
     stop
  end if

  call get_command_argument(1,arg)
  read(arg,*) E
  call get_command_argument(2,arg)
  read(arg,*) B
  call get_command_argument(3,arg)
  read(arg,*)dt


  open (unit=15, file="inputdata.txt", status='old',    &
             access='sequential', form='formatted', action='read' ) !Open the data file created earlier

!The following do loop reads how many lines are in a file. This is needed so that file reading later in the do loop on doesn't get interrupted due to 'end of fileä error
nlines = 0
  do
    read (15,*,end=100)
    nlines = nlines + 1
  end do
  100 close (15) 

!Reopen the same file, for some reason fortran tends to accesses a different file in upcoming operations after the above do loop
  open (unit=14, file="inputdata.txt", status='old',    &
             access='sequential', form='formatted', action='read' )




  open(13, file = 'outputdata.txt', status = 'new')   !Open a new file for the output data
  write(13,*) 'No.  v_x  v_y  v_z  x  y  z  mass  charge' !The output data 
  
  do j=1,nlines-2    !Do loop allows us to acces the input data one line at a time (one partical at a time)

    read(14,*)    i, v_xi, v_yi,v_zi, m, q !reading the values of the input file 

    L_y=0.076  
    L_z=0.019  
    x_z=0      
    x_y=0      
    x=0      
    v_z=v_zi   
    t=0


    do while(t<8e-6.and.x_y<L_y.and.abs(x_z)<=L_z/2) !conditions stops the loop as soon as the particle hits a wall or goes through the other end
    
    f_z=q*E-q*v_yi*B
    a_z=f_z/m
    v_z=v_z+a_z*dt
    x_z=x_z+v_z*dt
    x_y=x_y+v_yi*dt
    t=t+dt
    
      if (x_y>=L_y.and.abs(x_z)<L_z/2) then !seperates which particals go through
      write(13,*)i,v_xi,v_yi,v_z,x,x_y,x_z,m,q !Writes which particals go through to the previously opened output file
      end if 

    end do
  end do
close(13)



open (unit=12, file="visualizeddata.txt", status='old',    &
             access='sequential', form='formatted', action='read' )
             
  do k=1,2,1
    write(fname, '(A, I3.3, A)') "Trajectory", k, ".xyz"
    open(k, file = fname , status = 'new')
      
    if (k==1) then 
      write(k,*) '182' !number of lines, calculated separately
    else 
      write(k,*) '634' !number of lines, calculated separately
    end if
    
    write(k,*) '0.019 ','0.0095 ','0.076'           
    read(12,*) i, v_xi, v_yi,v_zi, m, q
    L_y=0.076
    L_z=0.019
    x_z=0
    v_z=v_zi
    x_y=0
    t=0
    x=0.0095
    
    do while(t<8e-6.and.x_y<L_y.and.abs(x_z)<=L_z/2)  
      f_z=q*E-q*v_yi*B
      a_z=f_z/m
      v_z=v_z+a_z*dt
      x_z=x_z+v_z*dt
      x_y=x_y+v_yi*dt
      t=t+dt
      write(k,*) x*1000,x_y*1000,x_z*1000 !multiply by 1000 to produce bigger picture in Ovito software
    end do
    
  close(k)
  end do             
             
end program main  
  
  
