!                    *********************
                     PROGRAM HOMERE_GRETEL
!                    *********************
!
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    MERGES THE RESULTS OF A PARALLEL COMPUTATION
!+                TO WRITE A SINGLE FILE.
!
!history  Y. AUDOUIN (EDF)
!+        09/2012
!+        V7P0
!+        Reorganizing reading of parameters
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_PARTEL
      IMPLICIT NONE
      INTEGER LNG,LU
      INTEGER LI
      COMMON/INFO/LNG,LU
!
!-------------------------------------------------------------------------
!
      LOGICAL IS
      CHARACTER(LEN=250) :: GEO
      CHARACTER(LEN=8)   :: GEOFORMAT,RESFORMAT
      CHARACTER(LEN=250) :: RES
      INTEGER            :: NPARTS, NPLAN
!
!-------------------------------------------------------------------------
!
      LI=5
      LU=6
      LNG=2
!
!----------------------------------------------------------------------
! INTRODUCE YOURSELF
!
      WRITE(LU,*) ' '
      WRITE(LU,*) '+-------------------------------------------------+'
      WRITE(LU,*) '  GRETEL: TELEMAC MERGER'
      WRITE(LU,*) '                                                   '
      WRITE(LU,*) '  HOLGER WEIL BEER (BAW)'
      WRITE(LU,*) '  JEAN-MICHEL HERVOUET (LNHE)'
      WRITE(LU,*) '  YOANN AUDOUIN        (LNHE) '
      WRITE(LU,*) '  GRETEL (C) COPYRIGHT 2003-2012 '
      WRITE(LU,*) '  BUNDESANSTALT FUER WASSERBAU, KARLSRUHE'
      WRITE(LU,*) ' '
      WRITE(LU,*) '+-------------------------------------------------+'
      WRITE(LU,*) ' '
      WRITE(LU,*) ' '
      WRITE(LU,*) '  MAXIMUM NUMBER OF PARTITIONS: ',MAXNPROC
      WRITE(LU,*) ' '
      WRITE(LU,*) '+--------------------------------------------------+'
      WRITE(LU,*) ' '
!
      WRITE (LU,*) '--GLOBAL GEOMETRY FILE: '
      READ(LI,*) GEO
!
      IF (GEO.EQ.' ') THEN
        WRITE (LU,*) ' NO FILENAME ' 
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) 'INPUT: ',TRIM(GEO)
      ENDIF  

      INQUIRE (FILE=GEO,EXIST=IS)
      IF (.NOT.IS) THEN 
        WRITE (LU,*)' FILE DOES NOT EXIST: ',TRIM(GEO)
        CALL PLANTE(1)
        STOP
      ENDIF  
!
      WRITE(LU,*)
     & '--GEOMETRY FILE FORMAT <FFORMAT> [MED,SERAFIN,SERAFIND]: ' 
      READ(LI,*) GEOFORMAT                
      IF ( (GEOFORMAT .NE. 'MED     ') .AND. 
     &     (GEOFORMAT .NE. 'SERAFIN ') .AND.
     &     (GEOFORMAT .NE. 'SERAFIND') ) THEN
        WRITE(LU,*)
     &  ' FILE FORMAT MUST BE "MED" OR "SERAFIN" OR "SERAFIND" ' 
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) ' INPUT: ', GEOFORMAT
      ENDIF 
!
      WRITE(LU,*) '--RESULT FILE: '
      READ(LI,*) RES
!
      IF (RES.EQ.' ') THEN
        WRITE (LU,*) ' NO FILENAME ' 
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) 'INPUT: ',TRIM(RES)
      ENDIF  
!
      WRITE(LU,*)
     & '--RESULT FILE FORMAT <FFORMAT> [MED,SERAFIN,SERAFIND]: ' 
      READ(LI,*) RESFORMAT                
      IF ( (RESFORMAT .NE. 'MED     ') .AND. 
     &     (RESFORMAT .NE. 'SERAFIN ') .AND.
     &     (RESFORMAT .NE. 'SERAFIND') ) THEN
        WRITE(LU,*)
     &  ' FILE FORMAT MUST BE "MED" OR "SERAFIN" OR "SERAFIND" ' 
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*) ' INPUT: ', RESFORMAT
      ENDIF 
!
      WRITE(LU,FMT='(A,I6,A)')
     &  '--NUMBER OF PARTITIONS <NPARTS> [2 -',MAXNPROC,']: ' 
      READ(LI,*) NPARTS                 
!
      IF ( (NPARTS > MAXNPROC) .OR. (NPARTS < 2) ) THEN
        WRITE(LU,FMT='(A,I6,A)')
     &  ' NUMBER OF PARTITIONS MUST BE IN [2 -',MAXNPROC,']' 
        CALL PLANTE(1)
        STOP
      ELSE
        WRITE(LU,*)' INPUT: ', NPARTS
      ENDIF 
!
      WRITE (LU,*) '--NUMBER OF PLANES: '
      READ (LI,*) NPLAN
      WRITE (LU,*) ' INPUT: ',NPLAN
!
      CALL GRETEL_AUTOP(GEO,GEOFORMAT,RES,RESFORMAT,NPARTS,NPLAN)
!
      STOP 0
      END PROGRAM
