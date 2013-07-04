!                      ******************************
                       module DECLARATIONS_POSTEL3D
!                      ******************************
!
!***********************************************************************
!  POSTEL3D VERSION 6.0
!***********************************************************************
!=======================================================================
! Telemac-3D best version number
! fortran95 version         march 1999        Jacek A. Jankowski pinxit
!=======================================================================
!
!  declaration of the global data structure in Telemac-3D
!
        use BIEF_DEF
!
!       note: this module is organised in 10 parts
!
!       (1) vectors (will be declared as bief_obj structures)
!       (2) matrices (will be declared as bief_obj structures)
!       (3) blocks (will be declared as bief_obj structures)
!       (4) integers
!       (5) logical values
!       (6) reals
!       (7) strings
!       (8) slvcfg structures
!       (9) mesh structure
!      (10) aliases
!
!-----------------------------------------------------------------------
! (1) vectors (real and integer)
!-----------------------------------------------------------------------
!
! 3D velocity components
!
        type(bief_obj), target :: u, v, w
!
!-----------------------------------------------------------------------
! (2) matrices
!-----------------------------------------------------------------------
! none
!-----------------------------------------------------------------------
! (3) blocks
!-----------------------------------------------------------------------
!
!
        type(bief_obj), target :: tab
!
!
! 2D output compatibility - output variables organised in blocks
!th pour bientot, avec le nouveau format
!th        type(bief_obj), target :: varsor, varcl
!
!-----------------------------------------------------------------------
! (4) integers
!-----------------------------------------------------------------------
! key words and parameters
!
!       maximum de variables de sortie
        integer, parameter :: maxvar = 100
!
! previous common mitint: integer steering parameters
!
      integer nplan  , ntrac  , ntrpa , nvar(2), nva3
      integer nr3d , ncou2 , nenre
!
      integer ielm3, ielm2h, ielm2v
      integer ielm0, ielmh, ielmu, ielm1, ielmx
      integer sorg3d
      integer im,jm,nplint
      integer nuprso,pesogr,nc2dh,nc2dv
      integer nplref(9),nseg(9)
!
!      nombre max de coupes
        integer, parameter :: maxcou = 9
!      nombre max de points pour les coupes verticales
        integer, parameter :: maxpts = 50
!
!-----------------------------------------------------------------------
! (5) logical values
!-----------------------------------------------------------------------
!
      logical sigmag
      logical spheri
      logical varsub
!
!-----------------------------------------------------------------------
! (6) reals
!-----------------------------------------------------------------------
!
! previous common mitrea, real steering parameters plus new ones
!
      double precision hmin,  cotint
!
!th  a voir si on met le parametre
!th  en dur pour l'instant
!      double precision href(maxcou),distor(maxcou)
!      double precision x2dv(maxpts,maxcou),y2dv(maxpts,maxcou)
      double precision href(9),distor(9)
!th      double precision zstar(5)
      double precision x2dv(50,9),y2dv(50,9)
!
!-----------------------------------------------------------------------
! (7) strings
!-----------------------------------------------------------------------
!
! previous mitcar
! changes: nomsui -> nompre ; nomr3d -> nomres ; nomr2d -> nomrbi
!     (see module for Telemac definitions)
! consequently binr3d -> binres ;  binsui -> binpre ; binr2d -> binrbi
!
!th  on laisse tout en attendant
!
      character(len=72) titcas, sort3d, sort2d, varimp
      character(len=3)  bingeo, binres, binpre, binrbi , binr3d , bincou
!
      character(len=20) equa
      character(len=32) varcla(10), texte(maxvar), textpr(maxvar)
      character(len=32) textlu(100)
!
!
!-----------------------------------------------------------------------
! (8) slvcfg structures
!-----------------------------------------------------------------------
! none
!-----------------------------------------------------------------------
! (9) mesh structure(s)
!-----------------------------------------------------------------------
! two separate meshes, 2D as usual and 3D with sigma-mesh specific
! features, see almesh.f
!
        type(bief_mesh) :: mesh2D, mesh3D
!
!-----------------------------------------------------------------------
! (10) aliases
!-----------------------------------------------------------------------
! declaration of pointers for aliases
! targets are allocated and pointed to in POINT_POSTEL3D.
!
! useful pointers for often used components in 2d and 3D mesh structures
!
! x,y,z node coordinates: base mesh and 3D sigma mesh
!
        type(bief_obj), pointer :: x2, y2, z2, x3, y3, z3
!
!th surement plein de choses a virer
!th
        type(bief_obj), pointer :: xnebor2, ynebor2
        type(bief_obj), pointer :: xnebor3, ynebor3, znebor3
!
! 2D and 3D lateral boundary normal vectors defined
! per boundary segment (2D) or boundary element (3D)
!
        type(bief_obj), pointer :: xsgbor2, ysgbor2
        type(bief_obj), pointer :: xsgbor3, ysgbor3, zsgbor3
!
! connectivity tables 2D and 3D
! (element number and local node number) --> global node number
!
        type(bief_obj), pointer :: ikle2, ikle3
!
! tables connecting (node boundary number) --> global node number
!
        type(bief_obj), pointer :: nbor2, nbor3
!
! real field pointers for node coordinates
!
        double precision, dimension(:), pointer :: x,y,z
!
! a number of extremely useful integers describing the mesh structure
! see almesh.f and point_telemac3d.f
!
        integer, pointer :: nelem2, nelem3
!
        integer, pointer :: nelmax2
        integer, pointer :: nelmax3 ! previously nelma3
!
        integer, pointer :: nptfr2  ! previously simply nptfr
        integer, pointer :: nptfr3
        integer, pointer :: neleb, nelebx
!
        integer, pointer :: nptfrx2, nptfrx3
        integer, pointer :: dim2, dim3
        integer, pointer :: typelm2, typelm3
        integer, pointer :: npoin2, npoin3
        integer, pointer :: npmax2, npmax3
        integer, pointer :: mxptvs2, mxptvs3
        integer, pointer :: mxelvs2, mxelvs3
!
!     NEW FILE FORMATS
!
      TYPE(BIEF_FILE) :: POS_FILES(100)
      INTEGER POSPRE,POSHOR,POSVER,POSGEO
!
!-----------------------------------------------------------------------
! save all - important
!
      SAVE
!
      END MODULE DECLARATIONS_POSTEL3D