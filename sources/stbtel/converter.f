!                       ***************** 
                        SUBROUTINE CONVERTER 
!                       *****************
     &(LOC_INPFILE,LOC_LOGFILE,LOC_BNDFILE,
     & LOC_OUTFILE,LOC_OUTLOGFILE,LOC_OUTBNDFILE)
!
!***********************************************************************
! STBTEL   V6P1                                   11/07/2011
!***********************************************************************
!
!BRIEF    WRITE A FILE OF MED FORMAT WITH THE MESH OBJECT
!+        INFORMATIONS
!                        
!HISTORY  Y.AUDOUIN (EDF)
!+        11/07/2011
!+        V6P1
!+   CREATION OF THE FILE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LOC_INPFILE        |-->| NAME OF THE INPUT FILE
!| LOC_LOGFILE        |-->| NAME OF THE LOG FILE
!| LOC_BNDFILE        |-->| NAME OF THE BOUNDARY FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_STBTEL
      USE CONV_VTK
      USE CONV_SERAFIN
      USE CONV_MED
      USE CONV_LIM
      USE CONV_CGNS
      USE CONV_UNV
!      
      IMPLICIT NONE
      ! LANGAE AND OUTPUT VALUE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      ! NAME IN THE TEMPORARY FOLDER OF THE FILES :
      ! EQUAL ' ' IF FILE NOT AVAILABLE
      CHARACTER(LEN=MAXLENHARD), INTENT(IN) :: LOC_INPFILE
      CHARACTER(LEN=MAXLENHARD), INTENT(IN) :: LOC_LOGFILE
      CHARACTER(LEN=MAXLENHARD), INTENT(IN) :: LOC_BNDFILE
      CHARACTER(LEN=MAXLENHARD), INTENT(IN) :: LOC_OUTFILE
      CHARACTER(LEN=MAXLENHARD), INTENT(IN) :: LOC_OUTLOGFILE
      CHARACTER(LEN=MAXLENHARD), INTENT(IN) :: LOC_OUTBNDFILE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!======================================================================= 
!     READING MESH FILE AND FILLING MESH_OBJ 
!======================================================================= 
!
      IF(LNG.EQ.1) THEN
        IF(DEBUG) WRITE(LU,*) '---INFORMATION SUR LES FICHIERS---'
        IF(DEBUG) WRITE(LU,*) 'FICHIER D ENTREE        : ',INFILE
        IF(DEBUG) WRITE(LU,*) 'FORMAT DU FICHIER D ENTREEE  : ',INFMT
        IF(DEBUG) WRITE(LU,*) 'FICHIER DES CONDITIONS LIMITES: ',
     &                         BOUNDFILE
        IF(DEBUG) WRITE(LU,*) 'FICHIER LOG       : ',LOGFILE
        IF(DEBUG) WRITE(LU,*) 'FICHIER DE SORTIE   : ',OUTFILE
        IF(DEBUG) WRITE(LU,*) 'FORMAT DU FICHIER DE SORTIE : ',OUTFMT
        IF(DEBUG) WRITE(LU,*) 
     &      'FICHIER DES CONDITIONS LIMITES EN SORTIE : ',OUTBNDFILE
        IF(DEBUG) WRITE(LU,*) 'FICHIER LOG EN SORTIE  : ',OUTLOGFILE
      ENDIF
      IF(LNG.EQ.2) THEN
        IF(DEBUG) WRITE(LU,*) '---FILES INFORMATION---'
        IF(DEBUG) WRITE(LU,*) 'INPUT FILE        : ',INFILE
        IF(DEBUG) WRITE(LU,*) 'INPUT FORMAT      : ',INFMT
        IF(DEBUG) WRITE(LU,*) 'BOUNDARY FILE     : ',BOUNDFILE
        IF(DEBUG) WRITE(LU,*) 'LOG FILE          : ',LOGFILE
        IF(DEBUG) WRITE(LU,*) 'OUTPUT FILE       : ',OUTFILE
        IF(DEBUG) WRITE(LU,*) 'OUTPUT FORMAT     : ',OUTFMT
        IF(DEBUG) WRITE(LU,*) 'OUT BOUNDARY FILE : ',OUTBNDFILE
        IF(DEBUG) WRITE(LU,*) 'OUT LOG FILE      : ',OUTLOGFILE
      ENDIF
!
      ! INITIALISE MESH VALUES
      CALL INI_MESH
!      
      IF(INFMT .EQ. 'SERAFIN') THEN
        CALL READ_SERAFIN(LOC_INPFILE,LOC_BNDFILE,.FALSE.)
      ELSEIF(INFMT .EQ. 'SERAFIND') THEN
        CALL READ_SERAFIN(LOC_INPFILE,LOC_BNDFILE,.TRUE.)
      ELSEIF(INFMT .EQ. 'MED') THEN
        CALL READ_MED(LOC_INPFILE,LOC_BNDFILE)
      ELSEIF(INFMT . EQ. 'UNV') THEN
        CALL READ_UNV(LOC_INPFILE,LOC_LOGFILE)
      ELSEIF(INFMT . EQ. 'CGNS') THEN
        CALL READ_CGNS(LOC_INPFILE)
      ELSE
        IF(LNG.EQ.2) WRITE(LU,*) 'UNKNOW INPUT FILE TYPE:',INFMT
        IF(LNG.EQ.1) WRITE(LU,*) 'FORMAT DU FICHIER D ENTREE INCONNU :',
     &                            INFMT
      ENDIF

      ! CHECK IF THE MESH IS COMPLETE
      IF(DEBUG) CALL CHECK_MESH
!
!======================================================================= 
!     WRITING MESH_OBJ INTO FILE
!======================================================================= 
!
      IF(OUTFMT .EQ. 'SERAFIN') THEN
        CALL WRITE_SERAFIN(LOC_OUTFILE,LOC_OUTBNDFILE,.FALSE.)
      ELSEIF(OUTFMT .EQ. 'SERAFIND') THEN
        CALL WRITE_SERAFIN(LOC_INPFILE,LOC_BNDFILE,.TRUE.)
      ELSEIF(OUTFMT .EQ. 'MED') THEN
        CALL WRITE_MED(LOC_OUTFILE)
      ELSEIF(OUTFMT . EQ. 'UNV') THEN
        CALL WRITE_UNV(LOC_OUTFILE,LOC_OUTLOGFILE)
      ELSEIF(OUTFMT . EQ. 'VTK') THEN
        CALL WRITE_VTK(LOC_OUTFILE)
      ELSEIF(OUTFMT . EQ. 'CGNS') THEN
        CALL WRITE_CGNS(LOC_OUTFILE)
      ELSE
        IF(LNG.EQ.2) WRITE(LU,*) 'UNKNOW OUTPUT FILE TYPE:',OUTFMT
        IF(LNG.EQ.1) WRITE(LU,*) 'FORMAT DU FICHIER DE SORTIE INCONNU:',
     &                            OUTFMT
      ENDIF
      ! DEALLOCATING ARRAY
      CALL FREE_MESH
!
!-----------------------------------------------------------------------
!
      END
