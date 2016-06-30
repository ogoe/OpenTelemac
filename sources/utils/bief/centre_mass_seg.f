!                       **************************
                        SUBROUTINE CENTRE_MASS_SEG
!                       **************************
!
     &(X,Y,COORD_G,IKLE,NPOIN,ELTSEG,ORISEG,NELEM,NSEG,JMI,CMI,GLOSEG,
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
!history  R. ATA & J-M HERVOUET
!+        19/11/2013
!+        V6P3
!+    Optimisation and simplification.
!
!history  R. ATA & J-M HERVOUET
!+        01/12/2013
!+        V6P3
!+    add verification tests
!
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
      INTEGER ISEG,NB1,NB2,IELEM,I,I1,I2,I3
      DOUBLE PRECISION XEL,YEL,XG1,XG2,YG1,YG2,XSOM(3),YSOM(3)
      DOUBLE PRECISION X_MIDPOINT,Y_MIDPOINT
      LOGICAL DEJA
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
        ENDDO
      ENDDO
!
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM2_SEG(COORD_G(1,1),
     &                   COORD_G(1,2),
     &                   COORD_G(1,2), ! NO EFFECT FOR THIS ONE
     &                   NSEG,1,1,2,MESH,1,11)
!
        CALL PARCOM2_SEG(COORD_G(1,3),
     &                   COORD_G(1,4),
     &                   COORD_G(1,4), ! NO EFFECT FOR THIS ONE
     &                   NSEG,1,1,2,MESH,1,11)

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
          YSOM(1)=Y(I1)
          YSOM(2)=Y(I2)
          YSOM(3)=Y(I3)
          IF(INPOLY(X_MIDPOINT,Y_MIDPOINT,XSOM,YSOM,3 ))THEN
            CMI(1,ISEG) = X_MIDPOINT
            CMI(2,ISEG) = Y_MIDPOINT
            JMI(ISEG)= IELEM
          ENDIF
        ENDDO
      ENDDO
!
!     FOR PARALLELISM
!
      IF(NCSIZE.GT.1) THEN
!       NOTE JMH: CMI(1,1:NSEG) HERE IMPLIES A TEMPORARY COPY BY THE COMPILER
!                 AND THEN A REDISTRIBUTION
        CALL PARCOM2_SEG(CMI(1,1:NSEG),
     &                   CMI(2,1:NSEG),
     &                   CMI,
     &                   NSEG,1,1,2,MESH,1,11)
      ENDIF
!
!     TEST TO SEE IF ALL INTERNAL EDGES ARE WELL TREATED
!     NOTE RA: FOR MALPASSET SMALL, MESH IS VERY POOR AND WITHOUT
!     THE FOLLOWING TEST, IT CRASHES
      DEJA =.FALSE.
      DO IELEM=1,NELEM
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
        DO I = 1,3
          ISEG = ELTSEG(IELEM,I)
          IF(JMI(ISEG).EQ.0.AND.
     &       IFABOR(IELEM,I).NE.-1.AND.IFABOR(IELEM,I).NE.0) THEN
            IF(.NOT.DEJA)THEN
              WRITE(LU,*)'MESH WITH POOR QUALITY '
              WRITE(LU,*)'FOR INSTANCE, SEE ELEMENT :',IELEM
              WRITE(LU,*)'WITH NODES',I1,I2,I3
              DEJA=.TRUE.
            ENDIF
          ENDIF
          CMI(1,ISEG)=0.5D0*(COORD_G(ISEG,1)+COORD_G(ISEG,3))
          CMI(2,ISEG)=0.5D0*(COORD_G(ISEG,2)+COORD_G(ISEG,4))
          JMI(ISEG)=IELEM
        ENDDO
      ENDDO
!
! BOUNDARY SEGMENTS
!
      DO IELEM=1,NELEM
        I1 = IKLE(IELEM,1)
        I2 = IKLE(IELEM,2)
        I3 = IKLE(IELEM,3)
        DO I = 1,3
          ISEG = ELTSEG(IELEM,I)
          IF(IFABOR(IELEM,I).EQ.-1.OR. IFABOR(IELEM,I).EQ. 0) THEN
!           BOUNDARY SEGMENT
            NB1=GLOSEG(ISEG,1)
            NB2=GLOSEG(ISEG,2)
!           CMI
            CMI(1,ISEG)=0.5D0*(X(NB1)+X(NB2))
            CMI(2,ISEG)=0.5D0*(Y(NB1)+Y(NB2))
!           JMI
            JMI(ISEG)=IELEM
!           ISEG HAS BEEN TREATED
          ENDIF
        ENDDO
      ENDDO
!
! TO VERIFY THAT IT IS WELL DONE
!
      IF(NCSIZE.LE.1)THEN
        DO ISEG=1,NSEG
          IF(JMI(ISEG).EQ.0)THEN
            WRITE(LU,*)'CENTRE_MASS_SEG: PROBLEM JMI NOT GOOD'
            WRITE(LU,*)'FOR SEGMENT :',ISEG
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
      ENDIF
!
!---------------------------------------------------------------------
!
      RETURN
      END

