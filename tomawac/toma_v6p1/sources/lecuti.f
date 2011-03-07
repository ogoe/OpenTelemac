!                    *****************
                     SUBROUTINE LECUTI
!                    *****************
!
     &(X,Y,NPOIN,NCOU,XRELV,YRELV,UR,VR,TRA01,NP,NPMAX)
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    READS THE CURRENTS / WINDS IN A USER-DEFINED FILE FORMAT.
!
!note     - THE DATA READ WILL BE STORED IN ARRAYS XRELV, YRELV, UR
!+        AND VR.
!+
!+    - INTERPOLATION ON THE COMPUTATION MESH IS AUTOMATICALLY DONE
!+        AT THE END OF THE SUBROUTINE.
!+
!+    - REMEMBER TO SUPPLY NP FOR THE INTERPOLATION AND TO COMMENT
!+        OUT THE WRITE STATEMENT.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER TO READ IN THE CURRENT/WIND FILE
!
!history  F.MARCOS (LNH)
!+        30/08/95
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NCOU           |-->| NUMERO D'UNITE LOGIQUE DU FICHIER DES COURA.
!| NP             |<--| NOMBRE DE POINTS RELEVES
!| NPMAX          |-->| NOMBRE DE POINTS RELEVES MAXIMUM
!| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
!| TRA01          |<->| TABLEAU DE TRAVAIL DE DIMENSION NPMAX
!| UR,VR          |<->| TABLEAU DES COURANTS OU VENTS RELEVES
!| X,Y            |-->| COORDONNEES DU MAILLAGE
!| XRELV          |<->| TABLEAU DES ABSCISSES DES POINTS RELEVES
!| YRELV          |<->| TABLEAU DES ORDONNEES DES POINTS RELEVES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
      INTEGER NPMAX,NP
!
      INTEGER NCOU,NPOIN
!
      DOUBLE PRECISION X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION XRELV(NPMAX),YRELV(NPMAX),UR(NPMAX),VR(NPMAX)
      DOUBLE PRECISION TRA01(NPMAX)
!
!-----------------------------------------------------------------------
!
      WRITE(LU,*)'***********************************************'
      IF (LNG.EQ.1) THEN
        WRITE(LU,*)'  VOUS FAITES APPEL A LA PROCEDURE LECUTI      '
        WRITE(LU,*)'  (FORMAT DU FICHIER DES COURANTS OU VENTS = 4)'
        WRITE(LU,*)'     MAIS VOUS NE L''AVEZ PAS MODIFIEE         '
      ELSE
        WRITE(LU,*)'     YOU CALL SUBROUTINE LECUTI                '
        WRITE(LU,*)'  (FORMAT OF CURRENT OF WIND FILE = 4)         '
        WRITE(LU,*)'     BUT YOU DID NOT CHANGE IT                 '
      ENDIF
      WRITE(LU,*)'***********************************************'
      CALL PLANTE(0)
      RETURN
      END