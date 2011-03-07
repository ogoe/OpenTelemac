!                    *****************
                     SUBROUTINE SMTRAC
!                    *****************
!
     &(NPOIN,DIMT,AT,DT,SMTR,SMH,NREJET,ISCE,TSCE2,MAXSCE,MAXTRA,ITRAC)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE SECOND MEMBER FOR THE TRACER.
!
!history  INRIA
!+        
!+        V5P8
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
!| AT             |-->| TEMPS
!| DIMT           |-->| DIMENSION DU TRACEUR
!| DT             |-->| PAS DE TEMPS HYDRO
!| ISCE           |-->| NUMEROS GLOBAUX DES POINTS SOURCES
!| ITRAC          |---| 
!| MAXSCE         |---| 
!| MAXTRA         |---| 
!| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
!| NREJET         |-->| NOMBRE DE SOURCES/PUITS
!| SMH            |-->| TERMES SOURCES DE L'EQUATION DE CONTINUITE
!| SMTR           |---| TERMES SOURCES DU TRACEUR
!| TSCE2          |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,NREJET,ISCE(*),DIMT,ITRAC
      INTEGER, INTENT(IN) :: MAXSCE,MAXTRA
      DOUBLE PRECISION, INTENT(IN)    :: AT,DT,SMH(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: TSCE2(MAXSCE,MAXTRA)
      DOUBLE PRECISION, INTENT(INOUT) :: SMTR(DIMT)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IS
!
!-----------------------------------------------------------------------
!
      IF(NREJET.NE.0) THEN
        DO I=1,NREJET
          IS =ISCE(I)
          SMTR(IS) = SMTR(IS) + DT*SMH(IS) * TSCE2(I,ITRAC)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END