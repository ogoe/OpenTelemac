!**********************************************************************
module sel2vtk
!**********************************************************************
! sel2vtk V0.1 initial version
!          0.2 enhanced error treatment
!          0.3 improved output-file naming
!          0.4 writing shear-velocity as vector (not as scalar)
!          0.4.1 bugfix in write_vtk_scalar: variables were not
!              assigned correctly (3d case only)
!          0.5 added writing to binary file (G95_ENDIAN=BIG,LITTLE,
!              NATIVE!)
!          0.5.1 bugfix in sel2vtk_loop: 3d values were assigned in a
!                2d case (memory leak)
!          0.5.2 improved name handling in input_data
!          0.5.3 added file counter for result animation 
!          0.5.4 bugfix in open_vtk for 3d file -> wrong name scheme
!          0.5.5 added standard result directory pv in input function
!
!! sel2vtk: This module converts a telemac (2d/3d) selafin result file 
!! into the vtk (vtk.org) legacy/xml file format (triangle and wedge-
!! elements)
!! 
!! It is absolutely necessary that in 2d the velocities and in 3d
!! velocities
!! and elevation are written to the telemac result file.
!! The results from the telemac file will be converted and each time
!! step will be written in a single file in vtk legacy/xml format.
!! The directory where the vtk files are saved must be specified and
!! must exist.
! 
!  All tests an verifications have been done with binaries compiled
!  by g95.org!
!
!  Paraview can be found here: http://www.paraview.org
!
!  Help can be found here http://www.paraview.org/Wiki and
!  here http://wiki.vizworld.com/index.php/ParaView
!
!
!
!Copyright (C) 2007  Oliver Goethel (born2code@ogoethel.de)
!
! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License (Version
! 2 or later)
! as published by the Free Software Foundation.
!
!This program is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.
!
!You should have received a copy of the GNU General Public License
!along with this program; if not, write to the Free Software
!Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
!
!**********************************************************************
implicit none
!
private
!**********************************************************************
!
  integer, pointer :: elements(:,:) 

  integer :: npoin
  integer :: nelements
  integer :: nvar
  integer :: elementtype
  integer :: nplanes
  integer :: fu_s=20
  integer :: fu_v=25
  integer :: file_counter=0

!Version:

  character (LEN=1) :: vm='0'
  character (LEN=1) :: vs='5' 
  character (LEN=1) :: vk='5'
                    
                      
  real, pointer  :: coord(:,:)
  real, pointer  :: value(:,:)

  character (len=80) :: fname
  character (len=80) :: title
  character (len=80) :: teldat
  character (len=80) :: vtkdat
  character (len=80) :: vtkdir
  character (len=32),pointer :: var_names(:)

  logical :: binout=.false.      !.true.
  logical :: test=.FALSE.       !test xml
  logical :: file_counter_active

!**********************************************************************
interface convert
 module procedure convert_f
end interface
!
!**********************************************************************
public :: convert
!
!**********************************************************************
contains
!**********************************************************************
!
!**********************************************************************
function convert_f() result(error)
!**********************************************************************
  integer :: error
!**********************************************************************
  error=0
!
!
  write(*,*) 'SEL2VTK '//VM//'.'//VS//'.'//VK// &
           ' - CONVERTING SELAFIN TO VTK'
!
!
  error=input_data_f()
  if(error/=0) then
      call error_msg(error)
      goto 100
  end if
!
  error=open_selafin_f()
  if(error/=0) then
      call error_msg(error)
      goto 100
  end if
!
  error=read_selafin_mesh_f()
  if(error/=0) then
      call error_msg(error)
      goto 100
  end if
!
  if(test) then
      error=open_xml_gridfile_f()
      error=write_xml_gridfile_f()
      error=close_xml_gridfile_f()
      goto 100
  end if
!
  error=sel2vtk_loop_f()
  if(error/=0) then
      call error_msg(error)
      goto 100
  end if
!
  error=close_selafin_f()
  if(error/=0) then
      call error_msg(error)
      goto 100
  end if
!
100 continue
!
  error=end_f()
!
!**********************************************************************
end function convert_f
!**********************************************************************
!
!
!**********************************************************************
function open_selafin_f() result(error)
!**********************************************************************
!
integer :: error,f_error
!**********************************************************************
  error=0
  fname='open_selafin_f'
!
  open(fu_s,file=trim(teldat),  &
            form='UNFORMATTED', &
            status='old',       &
!F. Decung NATIVE, BIG, LITTLE...?
            convert='BIG_ENDIAN', &
!            access='SEQUENTIAL',  &
            iostat=f_error)

  error=f_error
  if(error/=0) then
    write(*,*) 'Error opening selafin file ',trim(teldat), &
               '! Please check!'
    write(*,*)
  endif
!
return
!
!**********************************************************************
end function open_selafin_f
!**********************************************************************
!
!
!
!**********************************************************************
function close_selafin_f() result(error)
!**********************************************************************
integer :: error,f_error
!**********************************************************************
  error=0
  fname='close_selafin_f'
!
  close(fu_s,iostat=f_error)
  if(f_error/=0) then
    error=f_error
    write(*,*) 'Error closing selafin file ',trim(teldat),'!'
  endif
!
return 
!
!**********************************************************************
end function close_selafin_f
!**********************************************************************
!
!
!
!**********************************************************************
function open_vtk_f(time) result(error)
!**********************************************************************
  real               :: time
  character(len=5)   :: ntime
  character(len=15)  :: ctime
  character(len=120) :: path
  character(len=12 ) :: fformat
  integer            :: error,f_error
!**********************************************************************

  error=0
  fname='open_vtk_f'

 if(file_counter_active) then
   ctime = ''
   file_counter=file_counter+1
   write(ntime,'(I5)') file_counter
   if(ntime(1:1)==' ') ntime(1:1)='0'
   if(ntime(2:2)==' ') ntime(2:2)='0'
   if(ntime(3:3)==' ') ntime(3:3)='0'
   if(ntime(4:4)==' ') ntime(4:4)='0'
   if(ntime(5:5)==' ') ntime(5:5)='0'
   path = trim(vtkdir)//'/'//trim(vtkdat)//'.'// &
          trim(ntime)//'.vtk'
  else
    ctime = ''
    write(ctime,'(EN15.6)') time
    path = trim(vtkdir)//'/'//trim(vtkdat)//'.'// &
           trim(adjustl(ctime))//'.vtk'
  endif
 
  if(binout) then
    fformat = 'UNFORMATTED'
  else
    fformat = 'FORMATTED'
  endif
  !
  open(fu_v,file=trim(path),&
  !form='FORMATTED',status='replace',iostat=f_error)
  form = fformat,status='replace',iostat=f_error)
  error = f_error
  if(error/=0) then
    write(*,*) 'Error opening vtk file ',path,'!'
    write(*,*)
  endif
!
return 
!
!**********************************************************************
end function open_vtk_f
!**********************************************************************
!
!
!
!**********************************************************************
function close_vtk_f() result(error)
!**********************************************************************
!
integer :: error,f_error
!**********************************************************************
  error=0
  fname='close_vtk_f'
!
  close(fu_v,iostat=f_error)
  if(f_error/=0) then
    error=f_error
    write(*,*) 'Error closing vtk file!'
  endif
!
return 
!
!**********************************************************************
end function close_vtk_f
!**********************************************************************
!
!
!
!
! FUNCTION i2c(          n,size) ! e.g. i2c(1,2) is '01'
!i2c(n,size) converts integer n ( n>=0, n<10**size ) to character*size    
!    INTEGER,INTENT(IN):: n,size
!    CHARACTER(size)   :: i2c
!    INTEGER           :: digit,signless,i
!  INTEGER,  PARAMETER:: i0 = iachar('0')
!  CHARACTER,PARAMETER:: cdigit(0:9) = (/(achar(n),n=i0,i0+9)/) 
!
!    signless = abs(n)
!    DO i = size,1,-1
!       digit = mod(signless,10) ! one of 0 1 ... 9
!       signless = (signless - digit)/10
!       i2c(i:i) = cdigit(digit)
!    END DO
!  END FUNCTION i2c 
!
!
!
!
!**********************************************************************
function input_data_f() result(error)
!**********************************************************************
integer :: error,f_error
character (LEN=1) :: file_c
!**********************************************************************
  error=0
  fname='input_data_f'
!
  write(*,*) 'NAME OF THE TELEMAC RESULT-FILE ?'
  read(*,'(A)',iostat=f_error) teldat
  write(*,*) teldat
  write(*,*) ''
  write(*,*) 'NAME OF VTK RESULT DIRECTORY (MUST EXIST!) [pv]?'
  read(*,'(A)',iostat=f_error) vtkdir
  if(vtkdir=='') vtkdir='pv'
  write(*,*) vtkdir
  write(*,*) ''
!
  write(*,*) 'NAME OF VTK RESULT-FILES ? [',trim(teldat),']'
  read(*,'(A)',iostat=f_error) vtkdat
  if(vtkdat=='') vtkdat=teldat
  write(*,*) vtkdat
  write(*,*) ''
!
  write(*,*) 'CONTINOUS FILE NUMBERING? (Y/[N])'
  read(*,'(A)',iostat=f_error) file_c
  !
  !
  if(file_c=='Y' .or. file_c=='y') then
    file_counter_active=.true.
  else
    file_counter_active=.false.
  endif
!
  error=f_error
!
return 
!
!**********************************************************************
end function input_data_f
!**********************************************************************
!
!
!
!**********************************************************************
function read_selafin_mesh_f() result(error)
!**********************************************************************
!
  integer :: error
  integer :: f_error
  integer :: ivar
  integer :: idummy
  integer :: ielement
  integer :: i
  integer :: iparam(10)
!**********************************************************************
  error=0
  fname='read_selafin_mesh_f'
!
  read(fu_s,iostat=f_error) title
  if(f_error/=0) then
      write(*,*) 'Error reading case title!'
      error=f_error
      goto 110
  end if
  !
  read(fu_s,iostat=f_error) nvar
  if(f_error/=0) then
      write(*,*) 'Error reading number of variables!'
      error=f_error
      goto 110
  end if
  !
  allocate( var_names(nvar) )
  !
  DO ivar = 1,nvar
    read(fu_s,iostat=f_error) var_names(ivar)
    if(f_error/=0) then
      write(*,*) 'Error reading name of variable!'
      error=f_error
      goto 110
    endif
  END DO
      !
  error=modify_var_names_f()
  !print*,var_names
  !
  read(fu_s,iostat=f_error) (iparam(i),i=1,10)
  if(f_error/=0) then
      write(*,*) 'Error reading table iparam!'
      error=f_error
      goto 110
  end if
      !
  read(fu_s,iostat=f_error) nelements,npoin,elementtype,idummy
  if(f_error/=0) then
      write(*,*) 'Error reading mesh attributes!'
      error=f_error
      goto 110
  end if
  !print*,nelements,npoin,elementtype
  !
  if(elementtype==6) nplanes=iparam(7)
  !
  if(elementtype/=3 .and. elementtype/=6) then
      write(*,*) 'Only triangle and wedge elements allowed!'
      stop
  end if
      !
!
!     elements
!
  allocate( elements(nelements,elementtype) )
      !
  read(fu_s,iostat=f_error)  &
      ((elements(ielement,i),i=1,elementtype),ielement=1,nelements)
  if(f_error/=0) then
      write(*,*) 'Error reading table of elements!'
      error=f_error
      goto 110
  end if
      !
!
!     boundary
!
  read(fu_s,iostat=f_error)(idummy,i=1,npoin)
  if(f_error/=0) then
      write(*,*) 'Error reading boundary points!'
      error=f_error
      goto 110
  end if

!     X-, Y-, (Z-)coordinates
!
  if(elementtype==3) then
      allocate( coord(npoin,3) )
      coord(:,3)=0.
  else
      allocate( coord(npoin,3) )
  end if
      !
  read(fu_s,iostat=f_error)(coord(i,1),i=1,npoin)
  if(f_error/=0) then
      write(*,*) 'Error reading x-coordinates!'
      error=f_error
      goto 110
  end if
  read(fu_s,iostat=f_error)(coord(i,2),i=1,npoin)
  if(f_error/=0) then
      write(*,*) 'Error reading y-coordinates!'
      error=f_error
      goto 110
  end if
!
110 continue
!
return 
!
!**********************************************************************
end function read_selafin_mesh_f
!**********************************************************************
!
!
!
!**********************************************************************
function modify_var_names_f() result(error)
!**********************************************************************
integer :: error,ivar,i,k,nwhite,length
!**********************************************************************
error=0
!
!
  do ivar=1,nvar
      nwhite=0
      length=len_trim(var_names(ivar))
      do i=1,length
          if(var_names(ivar)(i:i)==' ') nwhite=nwhite+1
      end do
      k=0
      do i=1,length
         if(var_names(ivar)(i:i)/=' ') then
              k=k+1
              if(k<=length-nwhite)  &
                  var_names(ivar)(k:k)=var_names(ivar)(i:i)
          end if
          if(i>length-nwhite) var_names(ivar)(i:i)=' '
      end do
  end do
!
return
!**********************************************************************
end function modify_var_names_f
!**********************************************************************
!
!
!
!**********************************************************************
function sel2vtk_loop_f() result(error)
!**********************************************************************
  integer           :: error
  integer           :: ivar
  integer           :: i
  integer           :: start
  integer           :: f_error
  integer           :: vars(3)
  character(len=15) :: vname
  real              :: at
!**********************************************************************
  error=0
  fname='sel2vtk_loop_f'

  allocate( value(npoin,nvar) )
  do
      read(fu_s,iostat=f_error,end=999) at
      if(f_error/=0) then
          write(*,*) 'Error reading time-step!'
          error=f_error
          goto 999
      end if
      write(*,*) at
      do ivar = 1,nvar
          read(fu_s,iostat=f_error,end=999) (value(i,ivar),i=1,npoin)
          if(f_error/=0) then
              write(*,*) 'Error reading variable result ', &
                          var_names(ivar),' in time-step ',at,'!'
              error=f_error
              goto 999
          end if
      end do
  !
      !it is assumed that in 3d the first data set is the elevation
      !(not sure if this is true in any case)
      if(elementtype==6 .and. (var_names(1)(1:5)=='ELEVA' .OR. &
          var_names(1)(1:4)=='COTE') )then
          coord(:,3)=value(:,1)
      end if
  !
      error=open_vtk_f(at)
      if(error/=0) then
          write(*,*) 'Error opening vtk-file for time-step ',at,'!'
          goto 999
      end if
  !
      error=write_vtk_header_f()
      if(error/=0) then
           write(*,*) 'Error writing vtk-file header at time-step ', &
                      at,'!'
           goto 999
       end if
  !
  !
  ! writing the velocities as a vector
      if (elementtype==3) then
       vars(1)=1
       vars(2)=2
       vars(3)=0
      elseif(elementtype==6) then
       vars(1)=2
       vars(2)=3
       vars(3)=4
      endif
      vname='Velocity'
      error=write_vtk_vector_f(vname,vars)
      if(error/=0) then
          write(*,*)'Error writing vtk-file velocities at time-step '&
                     ,at,'!'
          goto 999
      end if
  !
      select case(elementtype)
       case(3)
        start=3
       case(6)
        start=5
      end select
      !
      ! writing all other variables as a scalar
      if(nvar>=start) then
          do ivar=start,nvar
              if(trim(var_names(ivar))=='SHEARVELOCITYX') then
                  vars(1)=ivar
                  vars(2)=ivar+1
                  vars(3)=ivar+2
                  vname='ShearVelocity'
                  error=write_vtk_vector_f(vname,vars)
              else if(trim(var_names(ivar))=='SHEARVELOCITYY'.OR. &
                      trim(var_names(ivar))=='SHEARVELOCITYZ') then
                  goto 200
              else
                  error=write_vtk_scalar_f( ivar )
                  if(error/=0) then
                      write(*,*) 'Error writing result ', &
                        trim(var_names(ivar)),' at time-step ',at,'!'
                      goto 999
                  end if
              end if
200  continue
          end do
      end if
      !
      error=close_vtk_f()
      !
      !
  end do
  !
999 continue
!
return
!**********************************************************************
end function sel2vtk_loop_f
!**********************************************************************
!
!
!**********************************************************************
function write_vtk_scalar_f( ivar ) result(error)
!**********************************************************************
  integer          :: error
  integer          :: ivar
  integer          :: i
  integer          :: f_error
  character(len=1) :: lf
!**********************************************************************
  error=0
  lf=char(10)
!
  if(binout) then
      write(fu_v,iostat=f_error) 'SCALARS ', &
                                trim(var_names(ivar)),' float'//lf
  else
      write(fu_v,'(A7,1X,A16)',iostat=f_error) 'SCALARS', &
                               trim(var_names(ivar)),'float'
  endif
  if(f_error/=0) then
      error=f_error
      goto 130
  end if
  !
  if(binout) then
      write(fu_v,iostat=f_error) 'LOOKUP_TABLE default'
  else
      write(fu_v,*,iostat=f_error) 'LOOKUP_TABLE default'
  endif
  if(f_error/=0) then
      error=f_error
      goto 130
  end if
  !
  do i=1,npoin
      if(binout) then
          write(fu_v,iostat=f_error) value(i,ivar)
      else
          write(fu_v,*,iostat=f_error) value(i,ivar)
      end if
      if(f_error/=0) then
          error=f_error
          goto 130
      end if
  end do
  !
!
130 continue
!
return
!**********************************************************************
end function write_vtk_scalar_f
!**********************************************************************
!
!
!**********************************************************************
function write_vtk_vector_f(vname,vars) result(error)
!**********************************************************************
  integer           :: error
  integer           :: i
  integer           :: f_error
  integer           :: vars(3)
  character(len=15) :: vname
  character(len=1)  :: lf
!**********************************************************************
  error=0
  fname='write_vtk_vector_f'
  lf=char(10)
!
!
!
!
  if(binout) then
      write(fu_v,iostat=f_error) 'VECTORS ',trim(vname),' float'//lf
  else
      write(fu_v,'(A7,1X,A16,1X,A5)',iostat=f_error) 'VECTORS', &
                 trim(vname),'float'
  endif  
  if(f_error/=0) then
      error=f_error
      goto 140
  end if
!
  do i=1,npoin
      if(elementtype==3) then
          if(binout) then
              write(fu_v,iostat=f_error) &
                   value(i,vars(1)),value(i,vars(2)),'0.0'//lf
          else
              write(fu_v,'(2(E16.8,1X),A3)',iostat=f_error) &
                   value(i,vars(1)),value(i,vars(2)),'0.0'
          end if
          if(f_error/=0) then
              error=f_error
              goto 140
          end if
      else
          if(binout) then
              write(fu_v,iostat=f_error) value(i,vars(1)), &
                                         value(i,vars(2)), &
                                         value(i,vars(3))
          else
              write(fu_v,'(3(E16.8,1X))',iostat=f_error)   &
                                         value(i,vars(1)), &
                                         value(i,vars(2)), &
                                         value(i,vars(3))
          end if
          if(f_error/=0) then
              error=f_error
              goto 140
          end if
      end if
  end do
!
140 continue
!
return
!**********************************************************************
end function write_vtk_vector_f
!**********************************************************************
!
!
!
!**********************************************************************
function write_vtk_header_f() result(error)
!**********************************************************************
  integer           :: error
  integer           :: i
  integer           :: ielement
  integer           :: cell_type
  integer           :: f_error
  character(len=32) :: el_format
  character(len=1)  :: lf
!**********************************************************************
  error=0
  fname='write_vtk_header_f'
  lf=char(10)
!
  if(binout) then
      write(fu_v,iostat=f_error)'# vtk DataFile Version 3.0'//lf
  else
      write(fu_v,'(A26)',iostat=f_error)'# vtk DataFile Version 3.0'
  endif
  if(f_error/=0) then
      error=f_error
      goto 150
  end if
  !
  !
  if(binout) then
      write(fu_v,iostat=f_error)trim(title)//lf
  else
      write(fu_v,'(A32)',iostat=f_error)trim(title)
  end if
  if(f_error/=0) then
      error=f_error
      goto 150
  end if
  !
  if(binout) then
      write(fu_v,iostat=f_error)'ASCII'//lf
  else
      write(fu_v,'(A5)',iostat=f_error)'ASCII'
  endif
  if(f_error/=0) then
      error=f_error
      goto 150
  end if
 !
 if(binout) then
     write(fu_v,iostat=f_error)lf
 else
     write(fu_v,'(1X)',iostat=f_error)
 end if
 if(f_error/=0) then
     error=f_error
     goto 150
 end if
!
 if(binout) then
      write(fu_v,iostat=f_error)'DATASET UNSTRUCTURED_GRID'//lf
  else
      write(fu_v,'(A25)',iostat=f_error)'DATASET UNSTRUCTURED_GRID'
  end if
  if(f_error/=0) then
      error=f_error
      goto 150
  end if
!
!
  if(binout) then
      write(fu_v,iostat=f_error)'POINTS ',npoin,' float'//lf
  else
      write(fu_v,'(A7,I15,A6)',iostat=f_error)'POINTS ',npoin,' float'
  end if
  if(f_error/=0) then
     error=f_error
     goto 150
  end if
  !
  DO I=1,npoin
      IF(elementtype==3) THEN
      !
          if(binout) then
              write(fu_v,iostat=f_error)coord(I,1),coord(I,2),0.E0 
          else
              write(fu_v,'(3(E16.8,1X))',iostat=f_error) &
                                        coord(I,1),coord(I,2),0.E0
          end if
          if(f_error/=0) then
              error=f_error
              goto 150
          end if
      else
    !
          if(binout) then
              write(fu_v,iostat=f_error)coord(I,1), &
                                        coord(I,2),coord(I,3) !//lf
          else
              write(fu_v,'(3(E16.8,1X))',iostat=f_error) &
                              coord(I,1),coord(I,2),coord(I,3)
          end if
          if(f_error/=0) then
              error=f_error
              goto 150
          end if
      END IF
  END DO
!
  !write(*,*) elementtype
!
  if(binout) then
      write(fu_v,iostat=f_error)'CELLS ',  &
                                nelements, &
                                nelements*elementtype+nelements !//lf
  else
      write(fu_v,'(A6,I15,1X,I8)',iostat=f_error)'CELLS ',&
                                nelements,               &
                                nelements*elementtype+nelements
  end if
  if(f_error/=0) then
      error=f_error
      goto 150
  end if 
  !
  write(el_format,*) '(I2,',elementtype,'(1X,I15))'
  if(f_error/=0) then
      error=f_error
      goto 150
  end if
  !
  DO I=1,nelements
   !
     if(binout) then
         write(fu_v,iostat=f_error)&
                  elementtype,     &
                  (elements(I,ielement)-1,ielement=1,elementtype) !//lf
      else
          write(fu_v,el_format,iostat=f_error)&
          elementtype,(elements(I,ielement)-1,ielement=1,elementtype)
      end if
      if(f_error/=0) then
          error=f_error
          goto 150
      end if
  END DO
!
  !write(fu_v,*)
!
! 
  if(binout) then
      write(fu_v,iostat=f_error)'CELL_TYPES ',nelements !//lf
  else
      write(fu_v,'(A10,1X,I15)',iostat=f_error)'CELL_TYPES',nelements
  end if
  if(f_error/=0) then
      error=f_error
      goto 150
  end if
!
  SELECT CASE(elementtype)
    case(3)
          cell_type=5
    case(6)
          cell_type=13
     case default
          write(*,*) 'mod_vtk: Element type not supported!',elementtype
          stop
  END SELECT
!
     
  DO I=1,nelements
  !
      if(binout) then
          write(fu_v,iostat=f_error) cell_type !//lf
      else
          write(fu_v,'(I2)',iostat=f_error) cell_type
      end if
      if(f_error/=0) then
          error=f_error
          goto 150
      end if
  END DO
!
  if(binout) then
      write(fu_v,iostat=f_error)lf
  else
      write(fu_v,'(1X)',iostat=f_error)
  end if
  if(f_error/=0) then
      error=f_error
      goto 150
  end if
!
!
  if(binout) then
      write(fu_v,iostat=f_error) 'POINT_DATA ',npoin !//lf
  else
      write(fu_v,'(A10,1X,I15)',iostat=f_error) 'POINT_DATA',npoin
  end if
  if(f_error/=0) then
      error=f_error
      goto 150
  end if
!
150 continue
!
return
!**********************************************************************
end function write_vtk_header_f
!**********************************************************************
!
!
!
!**********************************************************************
function open_xml_gridfile_f() result(error)
!**********************************************************************
real :: time
character(len=13)  :: ctime
character(len=120) :: path
character(len=12 ) :: format
integer :: error,f_error,i,k
!**********************************************************************
  error=0
  fname='open_xml_gridfile_f'
! 
  write(ctime,'(EN13.4)') time
 !
  k=1
  do i=1,4
      if(ctime(i:i).eq.' ') k=k+1
  end do
 !
  path=trim(vtkdir)//'/'//trim(vtkdat)//'.vtu'
  write(6,*)'vtk grid file : ',path
 !
  if(binout) then
      format='UNFORMATTED'
  else
      format='FORMATTED'
  end if
 !
  open(fu_v,file=trim(path), &
            form=format,     &
            status='replace',&
            iostat=f_error)
  error=f_error
  if(error/=0) then
      write(*,*) 'Error opening vtu-gridfile ',path,'!'
      write(*,*)
  end if
!
return 
!
!**********************************************************************
end function open_xml_gridfile_f
!**********************************************************************
!
!
!
!**********************************************************************
function close_xml_gridfile_f() result(error)
!**********************************************************************
  integer :: error
  integer :: f_error
!**********************************************************************
  error=0
  fname='close_xml_gridfile_f'
!
  close(fu_v,iostat=f_error)
  if(f_error/=0) then
      error=f_error
      write(*,*) 'Error closing vtu-gridfile!'
  end if
!
return
!**********************************************************************
end function close_xml_gridfile_f
!**********************************************************************
!
!
!
!**********************************************************************
function write_xml_gridfile_f() result(error)
!**********************************************************************
integer :: error
integer :: f_error
integer :: i
integer :: k
integer :: cell_type
integer :: offset(nelements)
character(len=200) :: buffer
character(len=20)  :: celem
character(len=20)  :: cpoints
!**********************************************************************
  error=0
  fname='write_xml_gridfile_f'
!
buffer='<?xml version="1.0"?>'
write(fu_v,'(A21)',iostat=f_error) trim(buffer)
buffer='<VTKFile type="UnstructuredGrid" version="0.1" byte_order="LittleEndian">'
write(fu_v,*,iostat=f_error) trim(buffer)
!
buffer=' <UnstructuredGrid>'
write(fu_v,*,iostat=f_error) trim(buffer)
!
write(cpoints,*) npoin
write(celem,*) nelements
write(*,*)npoin,cpoints
write(*,*)nelements,celem
buffer='  <Piece NumberOfPoints="'//trim(cpoints)//'" NumberOfCells="'//trim(celem)//'">'
write(fu_v,*,iostat=f_error) trim(buffer)
!
buffer='   <PointData>'
write(fu_v,*,iostat=f_error) trim(buffer)
buffer='   </PointData>'
write(fu_v,*,iostat=f_error) trim(buffer)
!
buffer='   <CellData>'
write(fu_v,*,iostat=f_error) trim(buffer)
buffer='   </CellData>'
write(fu_v,*,iostat=f_error) trim(buffer)
!
buffer='   <Points>'
write(fu_v,*,iostat=f_error) trim(buffer)
buffer='   <DataArray type="Float32" NumberOfComponents="3" format="ascii">'
write(fu_v,*,iostat=f_error) trim(buffer)
write(fu_v,*,iostat=f_error) ((coord(i,k),k=1,3),i=1,npoin)
buffer='   </DataArray>'
write(fu_v,*,iostat=f_error) trim(buffer)
buffer='   </Points>'
write(fu_v,*,iostat=f_error) trim(buffer)
!
buffer='   <Cells>'
write(fu_v,*,iostat=f_error) trim(buffer)
buffer='   <DataArray type="Int32" Name="connectivity" format="ascii">'
write(fu_v,*,iostat=f_error) trim(buffer)
write(fu_v,*,iostat=f_error) ((elements(i,k)-1,k=1,elementtype),i=1,nelements)
buffer='   </DataArray>'
write(fu_v,*,iostat=f_error) trim(buffer)
buffer='   <DataArray type="Int32" Name="offsets" format="ascii">'
write(fu_v,*,iostat=f_error) trim(buffer)
k=3
do i=1,nelements;offset(i)=k;k=k+3;enddo
write(fu_v,*,iostat=f_error) (offset(i),i=1,nelements)
buffer='   </DataArray>'
write(fu_v,*,iostat=f_error) trim(buffer)
buffer='   <DataArray type="UInt8" Name="types" format="ascii">'
write(fu_v,*,iostat=f_error) trim(buffer)
SELECT CASE(elementtype)
   case(3)
    cell_type=5
   case(6)
    cell_type=13
   case default
    write(*,*) 'mod_vtk: Element type not supported!',elementtype
    stop
  END SELECT
write(fu_v,*,iostat=f_error) (cell_type,i=1,nelements)
buffer='   </DataArray>'
write(fu_v,*,iostat=f_error) trim(buffer)
buffer='   </Cells>'
write(fu_v,*,iostat=f_error) trim(buffer)
!
buffer='  </Piece>'
write(fu_v,*,iostat=f_error) trim(buffer)
!
buffer=' </UnstructuredGrid>'
write(fu_v,*,iostat=f_error) trim(buffer)
!
buffer='</VTKFile>'
write(fu_v,*,iostat=f_error) trim(buffer)
!
!
155 continue
return
!**********************************************************************
end function write_xml_gridfile_f
!**********************************************************************
!
!
!
!**********************************************************************
function end_f() result(error)
!**********************************************************************
!
integer :: error
!**********************************************************************
error=0
fname='end_f'
!
if(associated(var_names)) deallocate( var_names )
if(associated(elements )) deallocate( elements  )
if(associated(coord    )) deallocate( coord     )
if(associated(value    )) deallocate( value     )
!
!
return 
!
!**********************************************************************
end function end_f
!**********************************************************************
!
!
!
!**********************************************************************
subroutine error_msg(error)
!**********************************************************************
integer :: error
!**********************************************************************
!
write(*,*) 'Error in function "',trim(fname),'"'
write(*,*) 'ErrorCode :',error
stop 
!
!**********************************************************************
end subroutine error_msg
!**********************************************************************
!
!
!**********************************************************************
end module sel2vtk
!**********************************************************************
!**********************************************************************
!**********************************************************************
!
!
!**********************************************************************
program main
!**********************************************************************
!
use sel2vtk
integer :: error
!**********************************************************************
!
error=convert()
!
!**********************************************************************
end program main
!**********************************************************************
