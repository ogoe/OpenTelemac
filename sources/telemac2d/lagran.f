!                    *****************
                     SUBROUTINE LAGRAN
!                    *****************
!
     &(NLAG,DEBLAG,FINLAG)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES FIRST AND FINAL TIMESTEPS FOR THE LAGRANGIAN DRIFTS.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!code
!+  EXAMPLE :
!+
!+      DO ILAG=1,NLAG
!+         DEBLAG(ILAG) = 1
!+         FINLAG(ILAG) = 299
!+      ENDDO ! ILAG
!warning  TWO DRIFTS CANNOT COMPLETE IN THE SAME TIMESTEP (ONLY THE 1ST WILL BE WRITTEN TO FILE)
!warning  THE RESULTS MUST BE SAVED BETWEEN TWO DRIFT COMPUTATION ENDS
!
!history  J-M JANIN (LNH)
!+        17/08/1994
!+        V5P2
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
!| DEBLAG         |<--| TIME STEP AT THE BEGINNING
!| FINLAG         |<--| TIME STEP AT THE END
!| NLAG           |-->| NUMBER OF LAGRANGIAN DRIFTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NLAG
      INTEGER, INTENT(INOUT) :: DEBLAG(NLAG) , FINLAG(NLAG)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     INTEGER ILAG
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE FIRST AND FINAL TIMESTEPS FOR THE COMPUTATION OF
!   LAGRANGIAN DRIFT - BY DEFAULT NOTHING IS DONE
!
!-----------------------------------------------------------------------
!
!     THIS WARNING AND THE CALL TO PLANTE MUST BE REMOVED IF
!     SOMETHING IS IMPLEMENTED BY THE USER BELOW
!
      IF(LNG.EQ.1) WRITE(LU,20)
      IF(LNG.EQ.2) WRITE(LU,21)
20    FORMAT(1X,'ATTENTION, VOUS APPELEZ LE SOUS-PROGRAMME LAGRAN',/,1X,
     &          'DE LA BIBLIOTHEQUE.   COMME VOUS CALCULEZ UN OU',/,1X,
     &          'PLUSIEURS CHAMPS DE DERIVES LAGRANGIENNES, VOUS',/,1X,
     &          'DEVEZ RAPATRIER "LAGRAN" DANS VOTRE FORTRAN, ET',/,1X,
     &          'LE COMPLETER',/////)
21    FORMAT(1X,'ATTENTION, YOU CALL SUBROUTINE LAGRAN OF THE LIBRARY.',
     &     /,1X,'AS YOU COMPUTE ONE OR MORE FIELDS OF LAGRANGIAN',/,1X,
     &          'DRIFTS, YOU NEED TO COPY THIS SUBROUTINE IN YOUR',/,1X,
     &          'OWN FORTRAN FILE AND COMPLETE IT.',/////)
!
      CALL PLANTE(1)
!
!-----------------------------------------------------------------------
!
!  EXAMPLE :
!
!      DO ILAG=1,NLAG
!         DEBLAG(ILAG) = 1
!         FINLAG(ILAG) = 299
!      ENDDO ! ILAG
!
!-----------------------------------------------------------------------
!
      RETURN
      END
