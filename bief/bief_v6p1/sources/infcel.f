!                    *****************
                     SUBROUTINE INFCEL
!                    *****************
!
     & ( XX , YY , IKLE , NUBO , VNOIN , NPOIN ,
     &   NVMAX , NELEM , NELMAX , NSEG ,CMI ,JMI ,AIRST)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES NUBO, VNOIN, AIRS, JMI, CMI, AIRST.
!
!history
!+        18/06/03
!+        V5P4
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AIRST          |<--| AREAS OF CELLS
!| CMI            |<--| COORDINATES OF MID-INTERFACE POINTS
!| IKLE           |-->| CONNECTIVITY TABLE.
!| JMI            |<--| NUMBER OF TRIANGLE TO WHICH BELONGS THE 
!|                |   | MID-INTERFACE POINT.
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NSEG           |-->| NUMBER OF SEGMENTS
!| NUBO           |<--| FIRST AND SECOND POINT OF SEGMENTS (GLOSEG ?)
!| NVMAX          |-->| MAXIMUM NUMBER OF NEIGHBOURS OF A POINT.
!|                |   | (MXPTVS IN THE REST OF TELEMAC)
!| VNOIN          |<--| NORMAL TO THE INTERFACE
!|                |   | (2 FIRST COMPONENTS) AND
!|                |   | SEGMENT LENGTH (3RD COMPONENT)
!| XX             |-->| ABSCISSAE OF POINTS IN THE MESH
!| YY             |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT  NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSEG,NELMAX,NPOIN,NVMAX,NELEM
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,*)
      INTEGER, INTENT(INOUT)          :: JMI(*)
      INTEGER, INTENT(INOUT)          :: NUBO(2,NSEG)
      DOUBLE PRECISION, INTENT(IN)    :: XX(NPOIN),YY(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: VNOIN(3,NSEG),CMI(2,*)
      DOUBLE PRECISION, INTENT(INOUT) :: AIRST(2,*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NB1,NB2,ISEG,IS,KV
      INTEGER I1,I2,I3,IS1,IS2,KV1,KV2,J1
      INTEGER IEL,JARET,ERR
!
      INTEGER NU(3),NEX(3),NUB1
!
      DOUBLE PRECISION EPS
      DOUBLE PRECISION UNSIX
      DOUBLE PRECISION X1,Y1,X2,Y2,X3,Y3,RNORM,ENTOPB(2,3)
      DOUBLE PRECISION XI1,YI1,XI2,YI2,XI3,YI3
      DOUBLE PRECISION XG,YG
      DOUBLE PRECISION XS1,YS1,XS2,YS2,XG1,YG1
!
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: JVOIS
!
      INTRINSIC ABS,SQRT
!
!-----------------------------------------------------------------------
!
!  JVOIS: ARRAY OF THE VERTICES (1) AND SEGMENTS (2) NEIGHBOURING A VERTEX
!
      ALLOCATE(JVOIS(NPOIN,NVMAX,2),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'INFCEL: MAUVAISE ALLOCATION DE JVOIS'
        IF(LNG.EQ.2) WRITE(LU,*) 'INFCEL: WRONG ALLOCATION OF JVOIS'
        STOP
      ENDIF
!
      EPS=1.D-5
!
!  NEX: POINTER OF THE SUCCESSORS OF THE VERTICES IN AN ELEMENT
!
!------
! 1. INITIALISES
!------
!
      DO ISEG = 1 , NSEG
        VNOIN(1,ISEG) = 0.D0
        VNOIN(2,ISEG) = 0.D0
        VNOIN(3,ISEG) = 0.D0
        CMI(1,ISEG)   = 0.D0
        CMI(2,ISEG)   = 0.D0
        JMI(ISEG)     = 0
      ENDDO
!
      DO IS = 1 , NPOIN
        DO KV = 1 , NVMAX
          JVOIS(IS,KV,1) = 0
          JVOIS(IS,KV,2) = 0
        ENDDO
      ENDDO
!
      ISEG = 0
      UNSIX = 1.D0/6.D0
!
!------
! 2. BUILDS ARRAYS : JVOIS , NUBO
!------
!
!  -->  ORIENTATION
!
      NEX (1) = 2
      NEX (2) = 3
      NEX (3) = 1
!
!     LOOP ON THE ELEMENTS
!     ***********************
!
      DO 30  IEL = 1 , NELEM
!
        I1 = IKLE(IEL,1)
        I2 = IKLE(IEL,2)
        I3 = IKLE(IEL,3)
!
        X1 = XX( I1 )
        Y1 = YY( I1 )
!
        X2 = XX( I2 )
        Y2 = YY( I2 )
!
        X3 = XX( I3 )
        Y3 = YY( I3 )
!
         XG= (X1+X2+X3)/3.D0
         YG= (Y1+Y2+Y3)/3.D0
!
        ENTOPB(1,1) = Y2 - Y3
        ENTOPB(2,1) = X3 - X2
!
        ENTOPB(1,2) = Y3 - Y1
        ENTOPB(2,2) = X1 - X3
!
        ENTOPB(1,3) = Y1 - Y2
        ENTOPB(2,3) = X2 - X1
!
        NU(1) = I1
        NU(2) = I2
        NU(3) = I3
!
!       LOOP ON THE 3 VERTICES OF ELEMENT IEL
! ----  *******************************************
!
        DO 40  IS = 1 , 3
          IS1 = NU(IS)
          IS2 = NU(NEX(IS))
!
!         IS IS2 NEIGHBOURING IS1? ---> LOOP ON IS1 NEIGHBOURS
!
          DO 41  KV1 = 1 , NVMAX
            IF (JVOIS (IS1, KV1, 1) .EQ. 0) GO TO 43
            IF (JVOIS (IS1, KV1, 1) .EQ. IS2) GO TO 44
41        CONTINUE
!
 43       CONTINUE
          JVOIS (IS1, KV1, 1) = IS2
!
!         IS IS1 NEIGHBOURING IS2? ---> LOOP ON IS2 NEIGHBOURS
!
          DO 45  KV2 = 1 , NVMAX
            IF (JVOIS (IS2, KV2, 1) .EQ. 0) GO TO 46
 45       CONTINUE
!
 46       CONTINUE
!
!  -->    BUILDS THE SEGMENTS AND NEIGHBOURS
!
          ISEG = ISEG + 1
!
          JVOIS(IS2,KV2,1) = IS1
          JVOIS(IS1,KV1,2) = ISEG
          JVOIS(IS2,KV2,2) = ISEG
!
          JARET = ISEG
          NUBO (1, ISEG) = -IS1
          NUBO (2, ISEG) = IS2
!
          UNSIX = ABS(UNSIX)
          JMI(ISEG)=IEL
          GO TO 48
!
 44       CONTINUE
!         THE SEGMENT HAS ALREADY BEEN TREATED
!
          JARET = JVOIS (IS1, KV1, 2)
          NUBO(1,JARET) = - NUBO(1,JARET)
          NB1 = NUBO(1,JARET)
          NB2 = NUBO(2,JARET)
          IF(NB1.EQ.IS1.AND.NB2.EQ.IS2) THEN
            UNSIX = ABS(UNSIX)
          ELSE
            IF(NB2.EQ.IS1.AND.NB1.EQ.IS2) THEN
              UNSIX = - ABS(UNSIX)
            ENDIF
          ENDIF
!
!         END OF LOOP ON THE VERTICES
! ----    *********************************
!
!       BUILDS THE CONTRIBUTION TO VECTOR NIJ
!
 48      VNOIN(1,JARET) = VNOIN(1,JARET) +
     &   UNSIX * ( ENTOPB(1,NEX(IS)) - ENTOPB(1,IS))
!
         VNOIN(2,JARET) = VNOIN(2,JARET) +
     &   UNSIX * ( ENTOPB(2,NEX(IS)) - ENTOPB(2,IS))
!
!   COORDINATES OF POINT M, MIDDLE OF SEGMENT GG1 BELONGING TO
!   THE CELL BOUNDARY
!
!
         CMI(1,JARET)= CMI(1,JARET)+0.5D0*XG
         CMI(2,JARET)= CMI(2,JARET)+0.5D0*YG
!
         IF(JMI(JARET).NE.IEL) THEN
!
!  COMPUTES SURFACES I1GG1 AND I2GG1
!
!
!  BEWARE IS1 # NUBO(1,JARET) (TRAVELS ON THE EDGE FOR THE 2ND TIME
!   IN THE OTHER DIRECTION)
!
        NB1= NUBO(1,JARET)
        NB2= NUBO(2,JARET)
!
         XS1 = XX(NB1)
         YS1 = YY(NB1)
         XS2 = XX(NB2)
         YS2 = YY(NB2)
!
         J1=JMI(JARET)
!
         XG1=(XX(IKLE(J1,1))+XX(IKLE(J1,2))+XX(IKLE(J1,3)))/3.D0
         YG1=(YY(IKLE(J1,1))+YY(IKLE(J1,2))+YY(IKLE(J1,3)))/3.D0
!
!     AIRST : SURFACE OF THE SUB-TRIANGLES I1GG1 AND I2GG1
!
           AIRST(1,JARET)=  0.5D0*ABS((XG1-XS1) * (YG-YS1)
     &                              - (YG1-YS1) * (XG-XS1))
           AIRST(2,JARET)=  0.5D0*ABS((XG1-XS2) * (YG-YS2)
     &                              - (YG1-YS2) * (XG-XS2))
!
! IF M BELONGS TO TRIANGLE IEL, JMI(JARET) =IEL
!
           XI1=X1-CMI(1,JARET)
           XI2=X2-CMI(1,JARET)
           XI3=X3-CMI(1,JARET)
           YI1=Y1-CMI(2,JARET)
           YI2=Y2-CMI(2,JARET)
           YI3=Y3-CMI(2,JARET)
           IF((XI1*YI2-XI2*YI1).LT.EPS) GOTO 40
           IF((XI2*YI3-XI3*YI2).LT.EPS) GOTO 40
           IF((XI3*YI1-XI1*YI3).LT.EPS) GOTO 40
           JMI(JARET) =IEL
!
         ENDIF
!
 40      CONTINUE
 30   CONTINUE
!
!   BUILDS THE BOUNDARY SEGMENTS
!
      DO 50  ISEG =1, NSEG
        NUB1 = NUBO(1,ISEG)
        IF(NUB1.LT.0) THEN
!
!           THIS IS A BOUNDARY EDGE
!
          NUBO(1,ISEG) = - NUBO(1,ISEG)
          IS1=NUBO(1,ISEG)
          IS2=NUBO(2,ISEG)
!
         XS1 = XX(IS1)
         YS1 = YY(IS1)
         XS2 = XX(IS2)
         YS2 = YY(IS2)
         CMI(1,ISEG) = 0.5D0* (XS1 +XS2)
         CMI(2,ISEG) = 0.5D0* (YS1 +YS2)
!
!  COMPUTES SURFACES I1GM AND I2GM
!
         J1=JMI(ISEG)
         XG= (XX(IKLE(J1,1))+XX(IKLE(J1,2))
     &                     + XX(IKLE(J1,3)))/3.D0
         YG= (YY(IKLE(J1,1))+YY(IKLE(J1,2))
     &                     + YY(IKLE(J1,3)))/3.D0
!
!     AIRST : SURFCAE OF THE SUB-TRIANGLES I1GM AND I2GM
!
          AIRST(1,ISEG)= 0.5D0*ABS((CMI(1,ISEG)-XS1) * (YG-YS1)
     &                           - (CMI(2,ISEG)-YS1) * (XG-XS1))
          AIRST(2,ISEG)= 0.5D0*ABS((CMI(1,ISEG)-XS2) * (YG-YS2)
     &                           - (CMI(2,ISEG)-YS2) * (XG-XS2))
!
        ENDIF
50    CONTINUE
!
!   BUILDS THE NORMALS
!
      DO 31 ISEG=1,NSEG
        RNORM = SQRT(VNOIN(1,ISEG)**2 + VNOIN(2,ISEG)**2)
        VNOIN(3,ISEG) = RNORM
        VNOIN(1,ISEG) = VNOIN(1,ISEG) / RNORM
        VNOIN(2,ISEG) = VNOIN(2,ISEG) / RNORM
31    CONTINUE
!
!---------------------------------------------------------------------
!
      DEALLOCATE(JVOIS)
!
!---------------------------------------------------------------------
!
      RETURN
      END
