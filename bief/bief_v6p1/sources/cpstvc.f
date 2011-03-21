!                    *****************
                     SUBROUTINE CPSTVC
!                    *****************
!
     &( X , Y )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COPIES A VECTOR STRUCTURE ONTO ANOTHER.
!
!history  J-M HERVOUET (LNH)
!+        01/03/95
!+        V5P1
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
!| X              |-->| STRUCTURE TO BE COPIED
!| Y              |<--| STRUCTURE THAT RECEIVES X ATTRIBUTES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_CPSTVC => CPSTVC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(IN)    :: X
      TYPE(BIEF_OBJ), INTENT(INOUT) :: Y
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER SIZEX,SIZEY
!
!-----------------------------------------------------------------------
!  TREATS ONLY VECTORS HERE :
!-----------------------------------------------------------------------
!
      IF(Y%TYPE.NE.2.OR.X%TYPE.NE.2) THEN
        IF(LNG.EQ.1) WRITE(LU,200) X%NAME,X%TYPE,Y%NAME,Y%TYPE
        IF(LNG.EQ.2) WRITE(LU,201) X%NAME,X%TYPE,Y%NAME,Y%TYPE
 200    FORMAT(1X,'CPSTVC : CAS NON PREVU POUR X ET Y :',/,1X,
     &            'X=',A6,' TYPE :',1I6                 ,/,1X,
     &            'Y=',A6,' TYPE :',1I6)
 201    FORMAT(1X,'CPSTVC : FORBIDDEN CASE FOR X AND Y:',/,1X,
     &            'X=',A6,' TYPE :',1I6                 ,/,1X,
     &            'Y=',A6,' TYPE :',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      SIZEX = X%DIM1*X%DIM2
      SIZEY = Y%MAXDIM1*Y%MAXDIM2
!
      IF(SIZEX.GT.SIZEY) THEN
        IF(LNG.EQ.1) WRITE(LU,300) X%NAME,SIZEX,Y%NAME,SIZEY
        IF(LNG.EQ.2) WRITE(LU,301) X%NAME,SIZEX,Y%NAME,SIZEY
 300    FORMAT(1X,'CPSTVC : CAS NON PREVU POUR X ET Y:',/,1X,
     &            'X=',A6,' TAILLE         :',1I6,/,1X,
     &            'Y=',A6,' TAILLE MAXIMUM :',1I6)
 301    FORMAT(1X,'CPSTVC : FORBIDDEN CASE FOR X AND Y:',/,1X,
     &            'X=',A6,' SIZE        :',1I6,/,1X,
     &            'Y=',A6,' MAXIMUM SIZE:',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!     DISCRETISATION
      IF(Y%ELM.NE.X%ELM.AND.Y%STATUS.EQ.1) THEN
        IF(LNG.EQ.1) WRITE(LU,400) X%NAME,Y%NAME
        IF(LNG.EQ.2) WRITE(LU,401) X%NAME,Y%NAME
400     FORMAT(1X,'CPSTVC : COPIE DE ',A6,' INTERDITE SUR ',A6)
401     FORMAT(1X,'CPSTVC : COPY OF ',A6,' FORBIDDEN ON ',A6)
        CALL PLANTE(1)
        STOP
      ELSE
        Y%ELM = X%ELM
      ENDIF
!     FIRST VECTOR DIMENSION
      Y%DIM1 = X%DIM1
!     SECOND VECTOR DIMENSION
      Y%DIM2 = X%DIM2
!     CASE OF DISCONTINUOUS VECTORS
      Y%DIMDISC = X%DIMDISC
!
!-----------------------------------------------------------------------
!
      RETURN
      END
