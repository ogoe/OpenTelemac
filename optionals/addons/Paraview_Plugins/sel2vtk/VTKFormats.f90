!=============================================================================================================================
!
!          VERY NAIVE FORTRAN90 CODE TO WRITE TWO TRIANGLES IN VTK BINARY FORMATS
!
!=============================================================================================================================
!
! This simple program was written with a didatic purpose of helping some poor Fortran 
! programmers write their own VTK files without having to construct any interface with 
! the huge and complex C++ VTK library (let's leave the C++ stuff for the experts...)
!
! The program is very easy to understand, compile and run (at least I think so...) and 
! can be easily extended for other more complex cases. 
!
! Fortran Compiler: Compaq Visual Fortran 6.6.0 (nowadays Intel Fortran Compiler...)
!
! tests performed for the following softwares/versions:
!
! - ParaView 2.9.9
! - ParaView 2.6.0
! - ParaView 2.4.4
! - ParaView 2.2.1
! - VisIt 1.5.5
!
! Any comment, bug or extension please contact me: 
!
! Renato N. Elias
! http://www.nacad.ufrj.br/~rnelias
! High Performance Computing Center (NACAD)
! Federal University of Rio de Janeiro (UFRJ)
!
!
program VTKBinaryFormats

implicit none

integer, parameter  :: ndim  = 3, & ! number of spatial dimension
                       nnos  = 4, & ! number of nodes
                       nnoel = 3, & ! number of nodes per element
                       nel   = 2    ! number of elements

real*4    :: xyz (ndim ,nnos) = (/ 0.,0.,0.,   1.,0.,0.,  1.,1.,0. , 0.,1.,0./)
real*4    :: vec (ndim ,nnos) = (/ 0.,0.,1.,   0.,0.,1.,  0.,0.,1. , 0.,0.,1./)
real*4    :: scal(      nnos) = (/ 0.,1.,2.,1. /)
integer   :: ien (nnoel,nel ) = (/ 0,1,2, 3,0,2/)
integer   :: etype = 5 ! Element Type 5 = triangle, 10 = tetrahedra 
                       ! (for other elements see http://www.vtk.org/pdf/file-formats.pdf, page 9)

   call WriteLegacyFormat(xyz, ien, scal, vec, ndim, nnoel, nnos, nel, etype)

   call WriteXMLFormat(xyz, ien, scal, vec, ndim, nnoel, nnos, nel, etype)

end program

!--------------------------------------------------------------------------------------------------------------------------

subroutine WriteLegacyFormat(xyz, ien, scal, vec, ndim, nnoel, nnos, nel, etype)

integer   :: ndim, nnoel, nnos, nel
real*4    :: xyz (ndim ,*)
integer   :: ien (nnoel,*)
real*4    :: scal(      *)
real*4    :: vec (ndim ,*)
integer   :: etype

character :: buffer*80, lf*1, str1*8, str2*8
integer   :: ivtk = 9, int
real*4    :: float

lf = char(10) ! line feed character

open(unit=ivtk,file='test_legacy_bin.vtk',form='binary',convert='BIG_ENDIAN')

buffer = '# vtk DataFile Version 3.0'//lf                                             ; write(ivtk) trim(buffer)
buffer = 'vtk output'//lf                                                             ; write(ivtk) trim(buffer)
buffer = 'BINARY'//lf                                                                 ; write(ivtk) trim(buffer)
buffer = 'DATASET UNSTRUCTURED_GRID'//lf//lf                                          ; write(ivtk) trim(buffer)

! POINTS SECTION
write(str1(1:8),'(i8)') nnos
buffer = 'POINTS '//str1//'  float'//lf                                               ; write(ivtk) trim(buffer)
write(ivtk) ((xyz(i,j),i=1,ndim),j=1,nnos)

! CELLS SECTION
write(str1(1:8),'(i8)') nel            ! number of elements (cells)
write(str2(1:8),'(i8)') nel*(1+nnoel)  ! size of the following element list (nel*(nnoel+1))
buffer = lf//lf//'CELLS '//str1//' '//str2//lf                                        ; write(ivtk) trim(buffer)
write(ivtk) (nnoel,(ien(i,j),i=1,nnoel),j=1,nel)

! CELL_TYPES SECTION
write(str1(1:8),'(i8)') nel   ! number of elements (cells)
buffer = lf//lf//'CELL_TYPES'//str1//lf                                               ; write(ivtk) trim(buffer)
write(ivtk) (etype,i=1,nel)

! POINT_DATA SECTION
write(str1(1:8),'(i8)') nnos
buffer = lf//lf//'POINT_DATA '//str1//lf                                              ; write(ivtk) trim(buffer)

buffer = 'SCALARS scalars float'//lf                                                  ; write(ivtk) trim(buffer)
buffer = 'LOOKUP_TABLE default'//lf                                                   ; write(ivtk) trim(buffer)
write(ivtk) (scal(i),i=1,nnos)

buffer = lf//lf//'VECTORS vectors float'//lf                                          ; write(ivtk) trim(buffer)
write(ivtk) ((vec(i,j),i=1,nnoel),j=1,nnos)

close(ivtk)

end subroutine

!--------------------------------------------------------------------------------------------------------------------------

subroutine WriteXMLFormat(xyz, ien, scal, vec, ndim, nnoel, nnos, nel, etype)

integer   :: ndim, nnoel, nnos, nel
real*4    :: xyz (ndim ,*)
integer   :: ien (nnoel,*)
real*4    :: scal(      *)
real*4    :: vec (ndim ,*)
integer   :: etype

character :: buffer*200, lf*1, offset*8, str1*8, str2*8
integer   :: ivtk = 9, int
real*4    :: float

lf = char(10) ! line feed character

! Layout for the Appended Data Section
!
! _ length | SCAL |      length | VEC |     lenght | XYZ |     lenght | IEN |     lenght | OFFSET |    lenght | ETYPE

nbytes_scal   =         nnos * sizeof(float)
nbytes_vec    = ndim  * nnos * sizeof(float)
nbytes_xyz    = ndim  * nnos * sizeof(float)
nbytes_ien    = nnoel * nel  * sizeof(  int)
nbytes_offset =         nel  * sizeof(  int)
nbytes_etype  =         nel  * sizeof(  int)

ioff0 = 0                                   ! scal
ioff1 = ioff0 + sizeof(int) + nbytes_scal   ! vec
ioff2 = ioff1 + sizeof(int) + nbytes_vec    ! xyz
ioff3 = ioff2 + sizeof(int) + nbytes_xyz    ! ien
ioff4 = ioff3 + sizeof(int) + nbytes_ien    ! offset
ioff5 = ioff4 + sizeof(int) + nbytes_offset ! etype


open(unit=ivtk,file='test_xml_bin.vtu',form='binary')

buffer = '<?xml version="1.0"?>'//lf                                                                                                  ; write(ivtk) trim(buffer)
buffer = '<VTKFile type="UnstructuredGrid" version="0.1" byte_order="LittleEndian">'//lf                                                                          ; write(ivtk) trim(buffer)
buffer = '  <UnstructuredGrid>'//lf                                                                                                   ; write(ivtk) trim(buffer)
write(str1(1:8),'(i8)') nnos
write(str2(1:8),'(i8)') nel
buffer = '    <Piece NumberOfPoints="'//str1//'" NumberOfCells="'//str2//'">'//lf                                                                       ; write(ivtk) trim(buffer)
buffer = '      <PointData> '//lf                                                                                                     ; write(ivtk) trim(buffer)
write(offset(1:8),'(i8)') ioff0
buffer = '         <DataArray type="Float32" Name="scalars" format="appended" offset="'//offset//'"           />'//lf                 ; write(ivtk) trim(buffer)
write(offset(1:8),'(i8)') ioff1
buffer = '         <DataArray type="Float32" Name="vectors" NumberOfComponents="3" format="appended" offset="'//offset//'" />'//lf    ; write(ivtk) trim(buffer)
buffer = '      </PointData>'//lf                                                                                                     ; write(ivtk) trim(buffer)
buffer = '      <CellData>  </CellData>'//lf                                                                                          ; write(ivtk) trim(buffer)
buffer = '      <Points>'//lf                                                                                                         ; write(ivtk) trim(buffer)
write(offset(1:8),'(i8)') ioff2
buffer = '        <DataArray type="Float32" Name="coordinates" NumberOfComponents="3" format="appended" offset="'//offset//'" />'//lf ; write(ivtk) trim(buffer)
buffer = '      </Points>'//lf                                                                                                        ; write(ivtk) trim(buffer)
buffer = '      <Cells>'//lf                                                                                                          ; write(ivtk) trim(buffer)
write(offset(1:8),'(i8)') ioff3
buffer = '        <DataArray type="Int32" Name="connectivity" format="appended" offset="'//offset//'" />'//lf                         ; write(ivtk) trim(buffer)
write(offset(1:8),'(i8)') ioff4
buffer = '        <DataArray type="Int32" Name="offsets" format="appended" offset="'//offset//'" />'//lf                              ; write(ivtk) trim(buffer)
write(offset(1:8),'(i8)') ioff5
buffer = '        <DataArray type="Int32" Name="types" format="appended" offset="'//offset//'" />'//lf                                ; write(ivtk) trim(buffer)
buffer = '      </Cells>'//lf                                                                                                         ; write(ivtk) trim(buffer)
buffer = '    </Piece>'//lf                                                                                                           ; write(ivtk) trim(buffer)
buffer = '  </UnstructuredGrid>'//lf                                                                                                  ; write(ivtk) trim(buffer)
buffer = '  <AppendedData encoding="raw">'//lf                                                                                        ; write(ivtk) trim(buffer)
buffer = '_'                                                                                                                          ; write(ivtk) trim(buffer)
write(ivtk) nbytes_scal  , (scal(i),i=1,nnos)
write(ivtk) nbytes_vec   , ((vec(i,j),i=1,ndim),j=1,nnos)
write(ivtk) nbytes_xyz   , ((xyz(i,j),i=1,ndim),j=1,nnos)
write(ivtk) nbytes_ien   , ((ien(i,j),i=1,nnoel),j=1,nel)
write(ivtk) nbytes_offset, (i,i=nnoel,nnoel*nel,nnoel)
write(ivtk) nbytes_etype , (etype,i=1,nel)
buffer = lf//'  </AppendedData>'//lf                                                                                                  ; write(ivtk) trim(buffer)
buffer = '</VTKFile>'//lf                                                                                                             ; write(ivtk) trim(buffer)

close(ivtk)

end subroutine

!-------------------------------------------------------------------------------------------------------------------




