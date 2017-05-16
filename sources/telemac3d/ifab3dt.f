!                    ******************
                     SUBROUTINE IFAB3DT
!                    ******************
!
     &(IFABOR,IFABOR2D,LIUBOF,LIUBOL,LIUBOS,KP1BOR,NELBOR,NULONE,
     & IKLE2,IKLE,NELEM2,NELMAX,
     & NPOIN2,NPTFR,NPLAN,NETAGE,KLOG,TRANSF)
!
!***********************************************************************
! TELEMAC3D   V6P3                                   10/09/2012
!***********************************************************************
!
!brief    Fills IFABOR of MESH3D for prisms cut into tetrahedra. After
!+        to VOISIN31, all boundaries are quoted solid (-1), here we
!+        treat liquid boundaries with the help of IFABOR2D
!
!warning  Does not allow liquid and solid on the same vertical...
!
!history  J-M HERVOUET (LNHE)
!+        10/097/2012
!+        V6P3
!+      First version.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IFABOR         |<->| THE ELEMENT BEHIND A FACE OF AN ELEMENT (IN 3D)
!| IFABOR2D       |-->| THE ELEMENT BEHIND A FACE OF AN ELEMENT (IN 2D)
!| IKLE           |-->| GLOBAL NUMBERS OF POINTS IN 3D ELEMENTS
!| IKLE2          |-->| GLOBAL NUMBERS OF POINTS IN 2D ELEMENTS
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KP1BOR         |-->| GIVES THE NEXT BOUNDARY POINT IN A CONTOUR
!| LIUBOF         |-->| TYPE OF BOUNDARY CONDITIONS ON U AT THE BOTTOM
!| LIUBOL         |-->| TYPE OF BOUNDARY CONDITIONS ON U ON THE LATERAL WALLS
!| LIUBOS         |-->| TYPE OF BOUNDARY CONDITIONS ON U AT THE SURFACE
!| NELBOR         |-->| FOR THE KTH BOUNDARY EDGE, GIVES THE CORRESPONDING
!|                |   | ELEMENT.
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS IN 3D
!| NETAGE         |-->| NUMBER OF PLANES - 1
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPTFR          |-->| NUMBER OF 2D BOUNDARY POINTS
!| NULONE         |-->| GOES WITH ARRAY NELBOR. NELBOR GIVES THE
!|                |   | ADJACENT ELEMENT, NULONE GIVES THE LOCAL
!|                |   | NUMBER OF THE FIRST NODE OF THE BOUNDARY EDGE
!|                |   | I.E. 1, 2 OR 3 FOR TRIANGLES.
!|                |   | HERE IT IS MESH2D%NULONE%I  !!!!!!!!!!!!!!
!| TRANSF         |-->| TRANSFORMATION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NELEM2,NPOIN2,NETAGE,NPLAN,TRANSF
      INTEGER, INTENT(IN)          :: NPTFR,KLOG,NELMAX
      INTEGER, INTENT(INOUT)       :: IFABOR(NELMAX,4)
      INTEGER, INTENT(IN)          :: IFABOR2D(NELEM2,3)
      INTEGER, INTENT(IN)          :: LIUBOF(NPOIN2),LIUBOS(NPOIN2)
      INTEGER, INTENT(IN)          :: LIUBOL(NPTFR,NPLAN)
      INTEGER, INTENT(IN)          :: IKLE2(NELEM2,3),IKLE(NELMAX,4)
      INTEGER, INTENT(IN)          :: NULONE(NPTFR,2)
      INTEGER, INTENT(IN)          :: KP1BOR(NPTFR), NELBOR(NPTFR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IELEM2,IETAGE,IFACE,IT1,IT2,IT3,IT
      INTEGER I1,I2,ITET,IFAC
      LOGICAL OK
!
      INTEGER :: ISUI(3)
!##> SEB @ HRW: NO DATA STATEMENT FOR TYPES WITH ALLOCATABLE COMPONENTS
!      DATA ISUI/2,3,1/
      PARAMETER ( ISUI = (/ 2,3,1 /) )
!##< SEB @ HRW
!
!     DEFINES THE FOUR TRIANGLES OF THE TETRAHEDRON: THE FIRST
!     DIMENSION IS THE NUMBER OF THE TRIANGLE, THE SECOND GIVES
!     THE NODE NUMBERS OF THE NODES OF TETRAHEDRONS WHICH DEFINE IT.
!
      INTEGER :: SOMFAC(3,4)
!##> SEB @ HRW: NO DATA STATEMENT FOR TYPES WITH ALLOCATABLE COMPONENTS
!      DATA SOMFAC /  1,2,3 , 1,4,2 , 2,4,3 , 1,3,4   /
      PARAMETER ( SOMFAC = RESHAPE( (/
     &       1,2,3 , 1,4,2 , 2,4,3 , 1,3,4   /), SHAPE=(/ 3,4 /) ) )
!##< SEB @ HRW
!
!=======================================================================
!
!     IFABOR ON THE LATERAL WALLS, ALREADY INITIALISED AT -1
!     NOW LOOKING AT LIQUID BOUNDARIES WITH THE HELP OF IFABOR2D
!
!=======================================================================
!
      DO IELEM2 = 1,NELEM2
        DO IFACE=1,3
!
          IF(IFABOR(IELEM2,IFACE).EQ.0) THEN
!           ONE LIQUID BOUNDARY DETECTED
!           ALL CORRESPONDING VERTICAL FACES MUST BE SET TO 0
!           LOOP OVER ALL TETRAHEDRA IN THE COLUMN OF PRISMS OVER
!           TRIANGLE IELEM2
            DO IETAGE=1,NETAGE
              DO IT=1,3
                ITET=3*NELEM2*(IETAGE-1)+(IT-1)*NELEM2+IELEM2
!               NOW LOOKING ALL FACES WITH IFABOR=-1
                DO IFAC=1,4
                  IF(IFABOR(ITET,IFAC).EQ.-1) THEN
!                   THE TWO POINTS OF THE 2D SEGMENT
                    I1=IKLE2(IELEM2,IFACE)
                    I2=IKLE2(IELEM2,ISUI(IFACE))
                    OK=.FALSE.
!                   TO BE TAKEN INTO ACCOUNT
!                   THIS FACE MUST HAVE ALL POINTS ABOVE I1 OR I2
!                   THE 3 POINTS OF THE TETRAHEDRON
                    IT1=IKLE(ITET,SOMFAC(1,IFAC))
                    IT2=IKLE(ITET,SOMFAC(2,IFAC))
                    IT3=IKLE(ITET,SOMFAC(3,IFAC))
                    IF( (MOD(IT1-1,NPOIN2)+1.EQ.I1.OR.
     &                   MOD(IT1-1,NPOIN2)+1.EQ.I2    ).AND.
     &                  (MOD(IT2-1,NPOIN2)+1.EQ.I1.OR.
     &                   MOD(IT2-1,NPOIN2)+1.EQ.I2    ).AND.
     &                  (MOD(IT3-1,NPOIN2)+1.EQ.I1.OR.
     &                   MOD(IT3-1,NPOIN2)+1.EQ.I2    ) ) THEN
                      IFABOR(ITET,IFAC)=0
                    ENDIF
                  ENDIF
                ENDDO
              ENDDO
            ENDDO
          ENDIF
!
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
!     IFABOR ON HORIZONTAL FACES OF THE BOTTOM AND FREE SURFACE
!     ALSO INITIALISED AT -1 IN VOISIN31, BUT REDONE HERE
!
      DO IELEM2 = 1,NELEM2
!
!       ADDRESS OF FIRST TETRAHEDRON OF THE BOTTOM LAYER
!       ITS FIRST FACE IS THE BOTTOM FACE
        ITET=IELEM2
        IFABOR(ITET,1) = -1
        IF(LIUBOF(IKLE2(IELEM2,1)).NE.KLOG .AND.
     &     LIUBOF(IKLE2(IELEM2,2)).NE.KLOG .AND.
     &     LIUBOF(IKLE2(IELEM2,3)).NE.KLOG) IFABOR(ITET,1)=0
!
!       ADDRESS OF SECOND TETRAHEDRON OF THE TOP LAYER
!       ITS FIRST FACE IS THE FREE SURFACE FACE
        ITET=(NETAGE-1)*NELEM2*3+NELEM2+IELEM2
        IFABOR(ITET,1) = -1
        IF(LIUBOS(IKLE2(IELEM2,1)).NE.KLOG .OR.
     &     LIUBOS(IKLE2(IELEM2,2)).NE.KLOG .OR.
     &     LIUBOS(IKLE2(IELEM2,3)).NE.KLOG) IFABOR(ITET,1)=0
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
