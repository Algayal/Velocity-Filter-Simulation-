!This program creats the input data file  

program data_table
  implicit none

  real::v_x,v_y,v_z,mass,charge,proton,e
  integer::i
  
  !v_x: x component of the initial velocity
  !v_y: y component of the initial velocity
  !v_z: z component of the initial velocity
  
  !Defining the mass of proton and the elementary charge 
   proton=1.6726219e-27
   e=1.602176634e-19 
  
  open(10,file='inputdata.txt')     !creating a file inputdata.txt


 !Do loop generates the particles to be simulated with varying values    
  do i=1,20 
    v_x=0*i
    v_y=i*10000 !Changing the value of the number multiplied i affects which particles go through the field
    v_z=0*i
    mass=i*proton
    charge=e*(-1)**i
    write(10,*)i,v_x,v_y,v_z,mass,charge
  end do
  
  write(10,*)'          No.    v_x            v_y             v_z               mass               charge'    
  write(10,*)'The total number of particles simulated is',i-1
   
 
 !Creating another set of particles into different folders
 open(12,file='visualizeddata.txt')    
   do i=1,2
  v_x=0*i
  v_y=i*120000*0.5
  v_z=0*i
  mass=i*proton
  charge=e*(-1)**i
  write(12,*)i,v_x,v_y,v_z,mass,charge
  end do

   
end program data_table

  
 
