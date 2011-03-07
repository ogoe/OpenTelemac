!                    *****************
                     SUBROUTINE WRIHYD
!                    *****************
!
     &(TITRE , ITSTRT , ITSTOP , ITSTEP , NPOIN2 , MBND   ,
     & NSEG  , NOLAY  , NOMGEO , NOMLIM ,
     & F     , NSTEPA , NOMSOU , NOSUIS , NOMCOU ,
     & NOMINI, NOMVEB , NORSED , NOMSAL , NOMTEM , NOMVEL , NOMVIS ,
     & NHYD,
     & SALI_DEL,TEMP_DEL,VELO_DEL,DIFF_DEL,MARDAT,MARTIM)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    WRITES OUT THE HYDRODYNAMIC FILE FOR DELWAQ (.HYD).
!
!history  CHARLES MOULINEC
!+        20/03/2007
!+        V6P0
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
!| DIFF_DEL       |---| 
!| F              |---| 
!| ITSTEP         |---| 
!| ITSTOP         |---| 
!| ITSTRT         |---| 
!| MARDAT         |---| 
!| MARTIM         |---| 
!| MBND           |---| 
!| NHYD           |---| 
!| NOLAY          |---| 
!| NOMCOU         |---| 
!| NOMGEO         |---| 
!| NOMINI         |---| 
!| NOMLIM         |---| 
!| NOMSAL         |---| 
!| NOMSOU         |---| 
!| NOMTEM         |---| 
!| NOMVEB         |---| 
!| NOMVEL         |---| 
!| NOMVIS         |---| 
!| NORSED         |---| 
!| NOSUIS         |---| 
!| NPOIN2         |---| 
!| NSEG           |---| 
!| NSTEPA         |---| 
!| SALI_DEL       |---| 
!| TEMP_DEL       |---| 
!| TITRE          |---| 
!| VELO_DEL       |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN) :: NHYD,ITSTRT,ITSTOP,ITSTEP,NPOIN2
      INTEGER,          INTENT(IN) :: NSEG,NOLAY,NSTEPA,MBND
      INTEGER,          INTENT(IN) :: MARDAT(3),MARTIM(3)
      CHARACTER(*),     INTENT(IN) :: TITRE,NOMGEO,NOMLIM
      CHARACTER(*),     INTENT(IN) :: NOMSOU,NOSUIS,NOMCOU,NOMSAL,NOMTEM
      CHARACTER(*),     INTENT(IN) :: NOMINI,NOMVEB,NORSED,NOMVEL,NOMVIS
      DOUBLE PRECISION, INTENT(IN) :: F(NPOIN2,NOLAY)
      LOGICAL,          INTENT(IN) :: SALI_DEL,TEMP_DEL
      LOGICAL,          INTENT(IN) :: VELO_DEL,DIFF_DEL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ILAY,IWAQ
!
!-----------------------------------------------------------------------
!
      WRITE ( NHYD, '(A)' )
     &    "TASK      FULL-COUPLING                              "
      WRITE ( NHYD, '(A)' )
     &    "                                                     "
      WRITE ( NHYD, '(A)' )
     &    "#                                                    "
      WRITE ( NHYD, '(A)' )
     &    "# TELEMAC DATA                                       "
      WRITE ( NHYD, '(A)' )
     &    "#                                                    "
      WRITE ( NHYD, '(A)' )
     &    "                                                     "
      WRITE ( NHYD, '(A)' )
     &    "GEOMETRY  FINITE-ELEMENTS                            "
      WRITE ( NHYD, '(A)' )
     &    "                                                     "
      WRITE ( NHYD, '(A)' )
     &    "HORIZONTAL-AGGREGATION       NO                      "
      WRITE ( NHYD, '(A)' )
     &    "MINIMUM-VERT-DIFFUSION-USED  NO                      "
      WRITE ( NHYD, '(A)' )
     &    "VERTICAL-DIFFUSION           CALCULATED              "
      WRITE ( NHYD, '(A)' )
     &    "DESCRIPTION                                          "
      IWAQ = LEN_TRIM(TITRE)
      WRITE ( NHYD, '(A,A,A)' )
     &    "   '",TITRE(1:IWAQ),"'"
      WRITE ( NHYD, '(A)' )
     &    "   '                                    '            "
      WRITE ( NHYD, '(A)' )
     &    "   '                                    '            "
      WRITE ( NHYD, '(A)' )
     &    "END-DESCRIPTION                                      "
      WRITE ( NHYD, '(A,I4,I2,I2,I2,I2,I2,A)' )
     &"REFERENCE-TIME           '",MARDAT(1),MARDAT(2),MARDAT(3),
     &                             MARTIM(1),MARTIM(2),MARTIM(3),"'"
      WRITE ( NHYD, '(A,I14,A)' )
     &    "HYDRODYNAMIC-START-TIME  '",ITSTRT,"'"
      WRITE ( NHYD, '(A,I14,A)' )
     &    "HYDRODYNAMIC-STOP-TIME   '",ITSTOP,"'"
      WRITE ( NHYD, '(A,I14,A)' )
     &    "HYDRODYNAMIC-TIMESTEP    '",NSTEPA,"'"
      WRITE ( NHYD, '(A,I14,A)' )
     &    "CONVERSION-REF-TIME      '",ITSTRT,"'"
      WRITE ( NHYD, '(A,I14,A)' )
     &    "CONVERSION-START-TIME    '",ITSTRT,"'"
      WRITE ( NHYD, '(A,I14,A)' )
     &    "CONVERSION-STOP-TIME     '",ITSTOP,"'"
      WRITE ( NHYD, '(A,I14,A)' )
     &    "CONVERSION-TIMESTEP      '",NSTEPA,"'"
      WRITE ( NHYD, '(A,I6)'  )
     &    "GRID-CELLS-FIRST-DIRECTION ",NPOIN2
      WRITE ( NHYD, '(A,I6,A)')
     &    "GRID-CELLS-SECOND-DIRECTION",NSEG+MBND," # NR OF EXCHANGES!"
      WRITE ( NHYD, '(A,I6)' )
     &    "NUMBER-HYDRODYNAMIC-LAYERS ",NOLAY
      WRITE ( NHYD, '(A,I6)' )
     &    "NUMBER-WATER-QUALITY-LAYERS",NOLAY
      IWAQ = LEN_TRIM(NOMGEO)
      WRITE ( NHYD, '(A,A,A)' )
     &    "HYDRODYNAMIC-FILE        '",NOMGEO(1:IWAQ),"'"
      WRITE ( NHYD, '(A)' )
     &    "AGGREGATION-FILE         NONE                        "
      WRITE ( NHYD, '(A,A,A)' )
     &    "GRID-INDICES-FILE        '",NOMGEO(1:IWAQ),"'"
      IWAQ = LEN_TRIM(NOMLIM)
      WRITE ( NHYD, '(A,A,A)' )
     &    "GRID-COORDINATES-FILE    '",NOMLIM(1:IWAQ),"'"
      IWAQ = LEN_TRIM(NOMSOU)
      WRITE ( NHYD, '(A,A,A)' )
     &    "VOLUMES-FILE             '",NOMSOU(1:IWAQ),"'"
      IWAQ = LEN_TRIM(NOSUIS)
      WRITE ( NHYD, '(A,A,A)' )
     &    "AREAS-FILE               '",NOSUIS(1:IWAQ),"'"
      IWAQ = LEN_TRIM(NOMCOU)
      WRITE ( NHYD, '(A,A,A)' )
     &    "FLOWS-FILE               '",NOMCOU(1:IWAQ),"'"
      IWAQ = LEN_TRIM(NOMVEB)
      WRITE ( NHYD, '(A,A,A)' )
     &    "POINTERS-FILE            '",NOMVEB(1:IWAQ),"'"
      IWAQ = LEN_TRIM(NORSED)
      WRITE ( NHYD, '(A,A,A)' )
     &    "LENGTHS-FILE             '",NORSED(1:IWAQ),"'"
      IF(SALI_DEL) THEN
        IWAQ = LEN_TRIM(NOMSAL)
        WRITE ( NHYD, '(A,A,A)' )
     &    "SALINITY-FILE            '",NOMSAL(1:IWAQ),"'"
      ELSE
      WRITE ( NHYD, '(A)' )
     &    "SALINITY-FILE            NONE                        "
      ENDIF
      IF(TEMP_DEL) THEN
        IWAQ = LEN_TRIM(NOMTEM)
        WRITE ( NHYD, '(A,A,A)' )
     &    "TEMPERATURE-FILE         '",NOMTEM(1:IWAQ),"'"
      ELSE
      WRITE ( NHYD, '(A)' )
     &    "TEMPERATURE-FILE         NONE                        "
      ENDIF
      IF(DIFF_DEL) THEN
        IWAQ = LEN_TRIM(NOMVIS)
        WRITE ( NHYD, '(A,A,A)' )
     &    "VERT-DIFFUSION-FILE      '",NOMVIS(1:IWAQ),"'"
      ELSE
      WRITE ( NHYD, '(A)' )
     &    "VERT-DIFFUSION-FILE      NONE                        "
      ENDIF
      IF(VELO_DEL) THEN
        IWAQ = LEN_TRIM(NOMVEL)
        WRITE ( NHYD, '(A,A,A)' )
     &    "VELOCITY-FILE            '",NOMVEL(1:IWAQ),"'"
      ELSE
      WRITE ( NHYD, '(A)' )
     &    "VELOCITY-FILE            NONE                        "
      ENDIF
      IWAQ = LEN_TRIM(NOMINI)
      WRITE ( NHYD, '(A,A,A)' )
     &    "SURFACES-FILE            '",NOMINI(1:IWAQ),"'"
      WRITE ( NHYD, '(A)' )
     &    "TOTAL-GRID-FILE          NONE                        "
      WRITE ( NHYD, '(A)' )
     &    "DISCHARGES-FILE          NONE                        "
      WRITE ( NHYD, '(A)' )
     &    "CHEZY-COEFFICIENTS-FILE  NONE                        "
      WRITE ( NHYD, '(A)' )
     &    "SHEAR-STRESSES-FILE      NONE                        "
      WRITE ( NHYD, '(A)' )
     &    "WALKING-DISCHARGES-FILE  NONE                        "
      IF ( NOLAY .GT. 1 ) THEN
         WRITE ( NHYD, '(A)' )
     &       "MINIMUM-VERT-DIFFUSION                            "
         WRITE ( NHYD, '(A)' )
     &       "   UPPER-LAYER       0.0000E+00                   "
         WRITE ( NHYD, '(A)' )
     &       "   LOWER-LAYER       0.0000E+00                   "
         WRITE ( NHYD, '(A)' )
     &       "   INTERFACE-DEPTH   0.0000E+00                   "
         WRITE ( NHYD, '(A)' )
     &       "END-MINIMUM-VERT-DIFFUSION                        "
      ENDIF
      WRITE ( NHYD, '(A)' )
     &    "CONSTANT-DISPERSION                                  "
      WRITE ( NHYD, '(A)' )
     &    "   FIRST-DIRECTION    0.0000                         "
      WRITE ( NHYD, '(A)' )
     &    "   SECOND-DIRECTION   0.0000                         "
      WRITE ( NHYD, '(A)' )
     &    "   THIRD-DIRECTION    0.0000                         "
      WRITE ( NHYD, '(A)' )
     &    "END-CONSTANT-DISPERSION                              "
      WRITE ( NHYD, '(A)' )
     &    "HYDRODYNAMIC-LAYERS                               "
      DO ILAY=1,NOLAY
         WRITE ( NHYD, '(F10.4)' ) F(1,ILAY)
      ENDDO
      WRITE ( NHYD, '(A)' )
     &    "END-HYDRODYNAMIC-LAYERS                           "
      WRITE ( NHYD, '(A)' )
     &    "WATER-QUALITY-LAYERS                              "
      DO ILAY=1,NOLAY
         WRITE ( NHYD, '(F10.4)' ) 1.0
      ENDDO
      WRITE ( NHYD, '(A)' )
     &    "END-WATER-QUALITY-LAYERS                          "
      WRITE ( NHYD, '(A)' )
     &    "DISCHARGES                                           "
      WRITE ( NHYD, '(A)' )
     &    "END-DISCHARGES                                       "
!
!-----------------------------------------------------------------------
!
      RETURN
      END