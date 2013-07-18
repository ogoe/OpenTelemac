!                       ************************
                        SUBROUTINE FRICTION_USER
!                       ************************
!
!
!***********************************************************************
! TELEMAC2D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    DEFINES FRICTION ZONES (BY NODE).
!+        The file format for giving a zone number to zones is here:
!+        node-number   zone-number
!+        for all the nodes, example:
!+        1  123
!+        2  123
!+        3  47
!+        4  47
!+        etc.
!+           
!
!history  F. HUVELIN
!+        15/04/2004
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE FRICTION_DEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      IMPLICIT NONE      
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      !1/ Global variables
      !-------------------

      !2/ Local variables
      !------------------
      INTEGER       :: I,K,IVAL1,IVAL2,NFILE
      INTEGER       :: NPOIN_GLOB,P_ISUM
      CHARACTER*144 :: NOMFILE

      EXTERNAL P_ISUM
!
!=======================================================================!
!=======================================================================!
!                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!
      NFILE   = T2D_FILES(T2DFO1)%LU
      NOMFILE = T2D_FILES(T2DFO1)%NAME

      ! File Read in order to know the number of node in the mesh
      ! (this information is unknown for a parallel computation
      ! ---------------------------------------------------------
      
      NPOIN_GLOB = 0
      DO 
        READ(NFILE,*,END=666)
        NPOIN_GLOB = NPOIN_GLOB + 1
      ENDDO
666   CONTINUE      
      WRITE(LU,*) 'FRICTION_USER: ',NPOIN_GLOB,' POINTS IN FILE'
      REWIND(NFILE)

      ! File Read
      ! ---------
      DO I = 1,NPOIN_GLOB
         READ(NFILE,*,END=999,ERR=998) IVAL1, IVAL2
         IF (NCSIZE>1) THEN
            K = MESH%KNOGL%I(I)
         ELSE
            K = I
         ENDIF
         KFROPT%I(K) = IVAL2
      ENDDO
      GOTO 997

      ! --------------------------------------------------------------- !
      !             ERROR MESSAGE DURING THE READING                    !
      ! --------------------------------------------------------------- !
999   CONTINUE
      IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'FICHIER DE DONNEES FORMATE : ',NOMFILE
         WRITE(LU,*) 'FIN DE FICHIER ANORMALE'
      ENDIF
      IF(LNG.EQ.2) THEN
         WRITE(LU,*) 'FORMATTED DATA FILE : ',NOMFILE
         WRITE(LU,*) 'ABNORMAL END OF FILE'
      ENDIF
      CALL PLANTE(1)
      STOP

998   CONTINUE
      IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'FICHIER DE DONNEES FORMATE : ',NOMFILE
         WRITE(LU,*) 'ERREUR DE LECTURE'
      ENDIF
      IF(LNG.EQ.2) THEN
         WRITE(LU,*) 'FORMATTED DATA FILE : ',NOMFILE
         WRITE(LU,*) 'READ ERROR'
      ENDIF
      CALL PLANTE(1)
      STOP
      ! --------------------------------------------------------------- !
      ! --------------------------------------------------------------- !

997   CONTINUE
!
!=======================================================================!
!=======================================================================!
!
      RETURN
      END
