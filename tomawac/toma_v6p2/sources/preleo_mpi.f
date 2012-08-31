!                    *********************
                     SUBROUTINE PRELEO_MPI
!                    *********************
!
     &(XLEO,YLEO,NLEO,X,Y,IKLE,SURDET,NPOIN2,NELEM2,NOLEO,ISLEO)
!
!***********************************************************************
! TOMAWAC   V6P1                                   22/06/2011
!***********************************************************************
!
!brief    SELECTS THE COMPUTATION NODES CLOSEST
!+                TO THE REQUESTED OUTPUT POINTS.
!
!history  F. MARCOS (LNH)
!+        01/02/95
!+        V1P0
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
!history  G.MATTAROLO (EDF - LNHE)
!+        22/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  G.MATTAROLO (EDF - LNHE)
!+        22/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLE           |-->| CONNECTIVITY TABLE
!| ISLEO          |<--| ARRAY OF LOGICAL
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
!| NLEO           |-->| NUMBER OF SPECTRUM PRINTOUT POINTS
!| NOLEO          |---| NUMBERS OF THE SPECTRUM PRINTOUT POINTS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| SURDET         |-->| 1/DET. OF ELEMENTS 2D FOR ISOPARAM. TRANSF.
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XLEO           |-->| ABSCISSAE OF SPECTRUM PRINTOUT POINTS
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| YLEO           |-->| ORDINATES OF SPECTRUM PRINTOUT POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      COMMON/ECRSPE_MPI/SPE_SEND
      INTEGER SPE_SEND
!
      INTEGER I,ILEO,NLEO,NPOIN2,NELEM2,IELEM,NOELEM
!
      DOUBLE PRECISION X(NPOIN2)  , Y(NPOIN2)
      DOUBLE PRECISION XLEO(NLEO)  , YLEO(NLEO)
      DOUBLE PRECISION SURDET(NELEM2)
      DOUBLE PRECISION DIST,DIST2,SHP1,SHP2,SHP3
!
      INTEGER NOLEO(NLEO),N1G,N2G,N3G
      INTEGER IKLE(NELEM2,3)
      LOGICAL ISLEO(NLEO)
!
!-----------------------------------------------------------------------
!
!       DO 10 ILEO=1,NLEO
!         DIST=1.D99
!         DO 20 I=1,NPOIN2
!          DIST2=(XLEO(ILEO)-X(I))**2+(YLEO(ILEO)-Y(I))**2
!          IF (DIST2.LT.DIST) THEN
!              DIST=DIST2
!              NOLEO(ILEO)=I
!          ENDIF
! 20      CONTINUE
! 10    CONTINUE
       SPE_SEND = 0
       DO ILEO = 1,NLEO
          NOLEO(ILEO) = 1
          ISLEO(ILEO) = .FALSE.
          NOELEM = 0
          DO 20 IELEM = 1,NELEM2
             N1G=IKLE(IELEM,1)
             N2G=IKLE(IELEM,2)
             N3G=IKLE(IELEM,3)
               SHP1 = ((X(N3G)-X(N2G))*(YLEO(ILEO)-Y(N2G))
     &               -(Y(N3G)-Y(N2G))*(XLEO(ILEO)-X(N2G)))*SURDET(IELEM)
               SHP2 = ((X(N1G)-X(N3G))*(YLEO(ILEO)-Y(N3G))
     &               -(Y(N1G)-Y(N3G))*(XLEO(ILEO)-X(N3G)))*SURDET(IELEM)
               SHP3 = ((X(N2G)-X(N1G))*(YLEO(ILEO)-Y(N1G))
     &               -(Y(N2G)-Y(N1G))*(XLEO(ILEO)-X(N1G)))*SURDET(IELEM)
             IF ((SHP1.GE.0.D0).AND.(SHP2.GE.0.D0)
     &                                        .AND.(SHP3.GE.0.D0)) THEN
               ISLEO(ILEO) = .TRUE.
               NOELEM = IELEM
               IF (SHP2>SHP1) THEN
                  NOLEO(ILEO) = N2G
                  IF (SHP3>SHP2) NOLEO(ILEO) = N3G
               ELSE
                  NOLEO(ILEO) = N1G
                  IF (SHP3>SHP1) NOLEO(ILEO) = N3G
               ENDIF
                   SPE_SEND=SPE_SEND+1
               GOTO 30
              ENDIF
20         CONTINUE
30       CONTINUE
       ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
