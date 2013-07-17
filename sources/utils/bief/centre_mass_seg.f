!                       **************************
                        SUBROUTINE CENTRE_MASS_SEG
!                       **************************
!
     & (X,Y,COORD_G,IKLE,NPOIN,ELTSEG,
     & ORISEG,NELEM,NSEG,JMI,CMI,GLOSEG,
     & IFABOR,MESH)
!
!***********************************************************************
! BIEF   V6P3                                                18/12/2012
!***********************************************************************
!
!brief    GIVES COORDINATES OF CENTRE OF GRAVITY OF TRIANGLES RIGHT AND 
!           LEFT OF SEGMENTS I.E. FOR A SEGMENT ISEG:
!            - COORD_G(1,ISEG) AND COORD_G(2,ISEG) ARE THE COORDINATES OF
!              THE CENTRE OF GRAVITY OF THE FIRST NEIGHBORING TRIANGLE 
!            - COORD_G(3,ISEG) AND COORD_G(4,ISEG) ARE THE COORDINATES OF
!              THE CENTRE OF GRAVITY OF THE SECOND NEIGHBORING TRIANGLE 
!         GIVES COORDINATES OF THE MIDDLE POINT OF SEGMENT G1G2 (STOCKED 
!             AT CMI(1,ISEG) AND CMI(2,ISEG)) AND THE ELEMENT NUMBER TO
!             TO WHICH BELONGS CMI
!
!history  R.ATA
!+        18/12/12
!+        V6P3
!+
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CMI            |<--| COORDINATES OF MID-INTERFACE POINTS 
!| COORD_G        |<--| CENTER OF MASS OF ELEMENTS NEIGHBORS OF AN EDGE
!|                |   |  COORD_G(1,ISEG) AND COORD_G(2,ISEG) ARE X1 AND Y1   
!|                |   |  COORD_G(3,ISEG) AND COORD_G(4,ISEG) ARE X2 AND Y2
!| ELTSEG         |-->| SEGMENTS FORMING AN ELEMENT
!| GLOSEG         |-->|  GLOBAL NUMBERS OF VERTICES OF SEGMENTS
!| IKLE           |-->| CONNECTIVITY TABLE.
!| IFABOR         |-->| IFABOR(IEL,I) IS THE ELEMENT BEHIND THE EDGE I OF
!|                |---|  ELEMENT IEL, OTHERWISE
!|                |---|     IFABOR(IEL,I) = -2 : THIS IS INTERFACE EDGE
!|                |---|     IFABOR(IEL,I) = 0  : THIS IS BOUNDARY EDGE
!|                |---|     IFABOR(IEL,I) = -1 : THIS IS LIQUID BOUNDARY EDGE
!| JMI            |<--| NUMBER OF TRIANGLE TO WHICH BELONGS THE 
!|                |   |       MID-INTERFACE POINT.
!| MESH           |-->| MESH STRUCTURE
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!|                |---| 
!| ORISEG         |-->| ORIENTATION OF SEGMENTS FORMING AN 
!|                |   |        ELEMENT 1:ANTI 2:CLOCKWISE
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, ONLY : INPOLY
      USE BIEF_DEF
      IMPLICIT  NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEG,NELEM,NPOIN
      INTEGER, INTENT(IN)             :: IKLE(NELEM,3)
      INTEGER, INTENT(IN)             :: ELTSEG(NELEM,3)
      INTEGER, INTENT(IN)             :: ORISEG(NELEM,3)
      INTEGER, INTENT(INOUT)          :: JMI(NSEG)
      INTEGER, INTENT(IN)             :: GLOSEG(NSEG,2)
      DOUBLE PRECISION, INTENT(INOUT) :: CMI(2,NSEG)
      DOUBLE PRECISION, INTENT(INOUT) :: COORD_G(NSEG,4)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      INTEGER, INTENT(IN)             :: IFABOR(NELEM,3) 
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! 
      INTEGER ISEG,NB1,NB2
      INTEGER IELEM,I,I1,I2,I3
      DOUBLE PRECISION XEL,YEL,XG1,XG2,YG1,YG2,EPS
      DOUBLE PRECISION XSOM(3),YSOM(3) 
      DOUBLE PRECISION X_MIDPOINT,Y_MIDPOINT
      LOGICAL YESNO(NSEG)
!
!-----------------------------------------------------------------------
!   INITIALIZATION OF VARIABLES
! 
      DO I=1,NSEG
        COORD_G(I,1) = 0.D0
        COORD_G(I,2) = 0.D0
        COORD_G(I,3) = 0.D0
        COORD_G(I,4) = 0.D0
        CMI(1,I)     = 0.D0
        CMI(2,I)     = 0.D0
        JMI(I)       = 0
        YESNO(I)     =.FALSE.
      ENDDO
      IF(NCSIZE.GT.1)THEN
        DO I=1,NSEG
          MESH%MSEG%X%R(I)=0.D0
        ENDDO
      ENDIF
!    RECUPERATE THE COORDINATES OF CENTER OF GRAVITY OF THE TRIANGLES 
!    AT THE RIGHT AND AT THE LEFT OF THE SEGMENT BETWEEN TWO NODES
!
      DO IELEM=1, NELEM 
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
!       
        XEL = (X(I1)+X(I2)+X(I3))/3.0D0
        YEL = (Y(I1)+Y(I2)+Y(I3))/3.0D0           
        DO I = 1,3
          ISEG = ELTSEG(IELEM,I)
          IF(ORISEG(IELEM,I).EQ.1)THEN ! G IS LEFT OF THE EDGE
            COORD_G(ISEG,1) =XEL
            COORD_G(ISEG,2) =YEL
          ELSEIF(ORISEG(IELEM,I).EQ.2)THEN !G IS RIGHT OF THE EDGE 
            COORD_G(ISEG,3) =XEL
            COORD_G(ISEG,4) =YEL
          ENDIF
!        
        ENDDO
      ENDDO
! 
      IF(NCSIZE.GT.1) THEN
       CALL PARCOM2_SEG(COORD_G(1:NSEG,1),
     &                  COORD_G(1:NSEG,2),
     &                  COORD_G(1:NSEG,2), ! NO EFFECT FOR THIS ONE
     &                  NSEG,1,1,2,MESH,1,11)
!
       CALL PARCOM2_SEG(COORD_G(1:NSEG,3),
     &                  COORD_G(1:NSEG,4),
     &                  COORD_G(1:NSEG,4), ! NO EFFECT FOR THIS ONE
     &                  NSEG,1,1,2,MESH,1,11)

      ENDIF
! 
!     SECOND PART:
!     RETRIEVE CMI: CMI(1,ISEG) AND CMI(2,ISEG) ARE X AND Y OF THE
!                   MID-POINT G1G2
!     RETRIEVE JMI: JMI(ISEG) IS THE NUMBER IF TRIANGLE TO WHICH 
!                   BELONGS CMI
!
      DO IELEM=1,NELEM
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
        DO I = 1,3
          ISEG = ELTSEG(IELEM,I)
          NB1 = GLOSEG(ISEG,1)
          NB2 = GLOSEG(ISEG,2)
          IF(.NOT.YESNO(ISEG))THEN
           XG1 = COORD_G(ISEG,1)
           YG1 = COORD_G(ISEG,2)
           XG2 = COORD_G(ISEG,3)
           YG2 = COORD_G(ISEG,4)
           ! CENTER OF SEGMENT G1G2
           X_MIDPOINT = 0.5D0*(XG1+XG2)
           Y_MIDPOINT = 0.5D0*(YG1+YG2) 
           ! JMI: IN WHICH ELEMENT IS CMI
           ! WE USE INPOLY FUNCTION (SEE INPOLY.F)
           XSOM(1)=X(I1)
           XSOM(2)=X(I2)
           XSOM(3)=X(I3)
!
           YSOM(1)=Y(I1)
           YSOM(2)=Y(I2)
           YSOM(3)=Y(I3)
           IF(INPOLY(X_MIDPOINT,Y_MIDPOINT,XSOM,YSOM,3 ))THEN 
              IF(NCSIZE.GT.1)THEN
                MESH%MSEG%X%R(ISEG)=IELEM ! I USE THIS STUPID WAY BECAUSE WE CAN 
                                          ! NOT USE PARCOM2_SEG FOR INTEGERS
              ENDIF
              CMI(1,ISEG) = X_MIDPOINT
              CMI(2,ISEG) = Y_MIDPOINT 
              JMI(ISEG)= IELEM
              YESNO(ISEG)=.TRUE.
           ENDIF
          ENDIF
        ENDDO
      ENDDO
!   FOR PARALLELISM
!+--+-+-+-+-+-+-+-+-+-+-
      IF(NCSIZE.GT.1) THEN
       CALL PARCOM2_SEG(CMI(1,1:NSEG),
     &                  CMI(2,1:NSEG),
     &                  MESH%MSEG%X%R(1:NSEG), 
     &                  NSEG,1,1,3,MESH,1,11)
!     !RECOVER THE INTEGER 
       DO ISEG=1,NSEG
         I1       = NINT(MESH%MSEG%X%R(ISEG))
!         JMI(ISEG)= I1         ! IN THIS WAY, JMI(I)=0 FOR INTERFACE SEGMENT
                                ! IF CMI IS NOT IN THIS SUBDOMAIN
         IF(I1.NE.0)YESNO(ISEG)=.TRUE. !TO SAY THAT INTERFACE SEGMENTS ARE ALREADY TREATED
       ENDDO
      ENDIF   
!
! BOUNDARY SEGMENTS
!
      EPS= 1.E-14
      DO ISEG=1,NSEG
         YESNO(ISEG)=.FALSE.
      ENDDO
      DO IELEM=1,NELEM
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
        DO I = 1,3
          ISEG = ELTSEG(IELEM,I)
          IF(.NOT.YESNO(ISEG).AND.(IFABOR(IELEM,1).EQ.-1
     &                        .OR. IFABOR(IELEM,1).EQ. 0)) THEN  !BOUNDARY SEG
            NB1=GLOSEG(ISEG,1)
            NB2=GLOSEG(ISEG,2)
            ! CMI
             CMI(1,ISEG)=0.5D0*(X(NB1)+X(NB2))
             CMI(2,ISEG)=0.5D0*(Y(NB1)+Y(NB2))
            !JMI
             JMI(ISEG)=IELEM
            ! ISEG HAS BEEN TREATED
             YESNO(ISEG)=.TRUE.
          ENDIF
         ENDDO
      ENDDO
!
!---------------------------------------------------------------------
!
      RETURN
      END
