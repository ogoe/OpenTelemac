!                       ************************
                        SUBROUTINE FRICTION_USER
!                       ************************
!
!
!***********************************************************************
! TELEMAC2D   V7P0                                   21/08/2010
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
!+        V5P4
!+   First version.
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        24/01/2014
!+        V7P0
!+   Provisionnal version, with ZONES FILE, but this file has yet to be
!+   treated in partel. KNOGL replaced by GLOBAL_TO_LOCAL_POINT.
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
!
!------------------------------------------------------------------------
!
      INTEGER            :: I,K,IVAL1,IVAL2,NFILE
      INTEGER            :: NPOIN_GLOB,P_ISUM
      CHARACTER(LEN=144) :: NOMFILE
      CHARACTER(LEN=144) :: tmp_name
      integer,allocatable :: test(:)
!
      EXTERNAL P_ISUM
!
!=======================================================================!
!=======================================================================!
!                               PROGRAMME                               !
!=======================================================================!
!=======================================================================!
!
      NFILE   = T2D_FILES(T2DZFI)%LU
      NOMFILE = T2D_FILES(T2DZFI)%NAME
!
!
!     File Read in order to know the number of node in the mesh
!     this information is unknown for a parallel computation
! 
!      
!
!     Reading File
! 
      DO K=1,NPOIN
        READ(NFILE,*,END=997,ERR=998) I, IVAL2
        KFROPT%I(I) = IVAL2
      ENDDO
      GOTO 997
!
!---------------------------------------------------------------------
!             ERROR WHEN READING                          
!---------------------------------------------------------------------
!
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
!
!-----------------------------------------------------------------------
!
997   CONTINUE
!
!=======================================================================
!=======================================================================
!
      RETURN
      END
