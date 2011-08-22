!                    *****************
                     SUBROUTINE WRIHYD
!                    *****************
!
     &(TITRE , ITSTRT , ITSTOP , ITSTEP , NPOIN2 , MBND   ,
     & NSEG  , NOLAY  , NOMGEO , NOMLIM ,
     & F     , NSTEPA , NOMSOU , NOMMAB , NOMCOU ,
     & NOMINI, NOMVEB , NOMMAF , NOMSAL , NOMTEM , NOMVEL , NOMVIS ,
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
!| DIFF_DEL       |-->| IF YES, WRITES DIFFUSION FILE FOR DELWAQ
!| F              |-->| ARRAY TO STORE FRACTION OF DEPTH PER LAYER
!| ITSTEP         |-->| TIME STEP
!| ITSTOP         |-->| STOP TIME
!| ITSTRT         |-->| START TIME
!| MARDAT         |-->| DATE (YEAR, MONTH,DAY)
!| MARTIM         |-->| TIME (HOUR, MINUTE,SECOND)
!| MBND           |-->| SEQUENTIAL COUNTER OPEN BOUNDARIES
!| NHYD           |-->| DELWAQ STEERING FILE CANAL
!| NOLAY          |-->| NUMBER OF PLANES
!| NOMCOU         |-->| FLUX FILE
!| NOMGEO         |-->| RESULT FILE OF THE SIMULATION
!| NOMINI         |-->| HORIZONTAL SURFACE FILE
!| NOMLIM         |-->| BOUNDARY FILE OF THE SIMULATION
!| NOMMAB         |-->| AREA FILE
!| NOMMAF         |-->| NODE DISTANCE FILE
!| NOMSAL         |-->| SALINITY FOR DELWAQ FILE
!| NOMSOU         |-->| VOLUME FILE
!| NOMTEM         |-->| TEMPERATURE FOR DELWAQ FILE
!| NOMVEB         |-->| NODE EXCHANGE FILE
!| NOMVEL         |-->| VELOCITY FILE
!| NOMVIS         |-->| DIFFUSION FILE
!| NPOIN2         |-->| NUMBER OF 2D POINTS IN THE MESH
!| NSEG           |-->| NUMBER OF 2D SEGMENTS IN THE MESH
!| NSTEPA         |-->| NUMBER OF TIME-STEPS FOR TIME AGGREGATION
!| SALI_DEL       |-->| IF YES, THERE IS SALINITY
!| TEMP_DEL       |-->| IF YES, THERE IS TEMPERATURE
!| TITRE          |-->| TITLE OF STUDY
!| VELO_DEL       |-->| IF YES, WRITES VELOCITY FILE FOR DELWAQ
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
      CHARACTER(*),     INTENT(IN) :: NOMSOU,NOMMAB,NOMCOU,NOMSAL,NOMTEM
      CHARACTER(*),     INTENT(IN) :: NOMINI,NOMVEB,NOMMAF,NOMVEL,NOMVIS
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
     &    "task      full-coupling                              "
      WRITE ( NHYD, '(A)' )
     &    "                                                     "
      WRITE ( NHYD, '(A)' )
     &    "#                                                    "
      WRITE ( NHYD, '(A)' )
     &    "# telemac data                                       "
      WRITE ( NHYD, '(A)' )
     &    "#                                                    "
      WRITE ( NHYD, '(A)' )
     &    "                                                     "
      WRITE ( NHYD, '(A)' )
     &    "geometry  finite-elements                            "
      WRITE ( NHYD, '(A)' )
     &    "                                                     "
      WRITE ( NHYD, '(A)' )
     &    "horizontal-aggregation       no                      "
      WRITE ( NHYD, '(A)' )
     &    "minimum-vert-diffusion-used  no                      "
      WRITE ( NHYD, '(A)' )
     &    "vertical-diffusion           calculated              "
      WRITE ( NHYD, '(A)' )
     &    "description                                          "
      IWAQ = LEN_TRIM(TITRE)
      WRITE ( NHYD, '(A,A,A)' )
     &    "   '",TITRE(1:IWAQ),"'"
      WRITE ( NHYD, '(A)' )
     &    "   '                                    '            "
      WRITE ( NHYD, '(A)' )
     &    "   '                                    '            "
      WRITE ( NHYD, '(A)' )
     &    "end-description                                      "
      WRITE ( NHYD, '(A,I4,I2,I2,I2,I2,I2,A)' )
     &"reference-time           '",MARDAT(1),MARDAT(2),MARDAT(3),
     &                             MARTIM(1),MARTIM(2),MARTIM(3),"'"
      WRITE ( NHYD, '(A,I14,A)' )
     &    "hydrodynamic-start-time  '",ITSTRT,"'"
      WRITE ( NHYD, '(A,I14,A)' )
     &    "hydrodynamic-stop-time   '",ITSTOP,"'"
      WRITE ( NHYD, '(A,I14,A)' )
     &    "hydrodynamic-timestep    '",NSTEPA,"'"
      WRITE ( NHYD, '(A,I14,A)' )
     &    "conversion-ref-time      '",ITSTRT,"'"
      WRITE ( NHYD, '(A,I14,A)' )
     &    "conversion-start-time    '",ITSTRT,"'"
      WRITE ( NHYD, '(A,I14,A)' )
     &    "conversion-stop-time     '",ITSTOP,"'"
      WRITE ( NHYD, '(A,I14,A)' )
     &    "conversion-timestep      '",NSTEPA,"'"
      WRITE ( NHYD, '(A,I6)'  )
     &    "grid-cells-first-direction ",NPOIN2
      WRITE ( NHYD, '(A,I6,A)')
     &    "grid-cells-second-direction",NSEG+MBND," # nr of exchanges!"
      WRITE ( NHYD, '(A,I6)' )
     &    "number-hydrodynamic-layers ",NOLAY
      WRITE ( NHYD, '(A,I6)' )
     &    "number-water-quality-layers",NOLAY
      IWAQ = LEN_TRIM(NOMGEO)
      WRITE ( NHYD, '(A,A,A)' )
     &    "hydrodynamic-file        '",NOMGEO(1:IWAQ),"'"
      WRITE ( NHYD, '(A)' )
     &    "aggregation-file         none                        "
      WRITE ( NHYD, '(A,A,A)' )
     &    "grid-indices-file        '",NOMGEO(1:IWAQ),"'"
      IWAQ = LEN_TRIM(NOMLIM)
      WRITE ( NHYD, '(A,A,A)' )
     &    "grid-coordinates-file    '",NOMLIM(1:IWAQ),"'"
      IWAQ = LEN_TRIM(NOMSOU)
      WRITE ( NHYD, '(A,A,A)' )
     &    "volumes-file             '",NOMSOU(1:IWAQ),"'"
      IWAQ = LEN_TRIM(NOMMAB)
      WRITE ( NHYD, '(A,A,A)' )
     &    "areas-file               '",NOMMAB(1:IWAQ),"'"
      IWAQ = LEN_TRIM(NOMCOU)
      WRITE ( NHYD, '(A,A,A)' )
     &    "flows-file               '",NOMCOU(1:IWAQ),"'"
      IWAQ = LEN_TRIM(NOMVEB)
      WRITE ( NHYD, '(A,A,A)' )
     &    "pointers-file            '",NOMVEB(1:IWAQ),"'"
      IWAQ = LEN_TRIM(NOMMAF)
      WRITE ( NHYD, '(A,A,A)' )
     &    "lengths-file             '",NOMMAF(1:IWAQ),"'"
      IF(SALI_DEL) THEN
        IWAQ = LEN_TRIM(NOMSAL)
        WRITE ( NHYD, '(A,A,A)' )
     &    "salinity-file            '",NOMSAL(1:IWAQ),"'"
      ELSE
      WRITE ( NHYD, '(A)' )
     &    "salinity-file            none                        "
      ENDIF
      IF(TEMP_DEL) THEN
        IWAQ = LEN_TRIM(NOMTEM)
        WRITE ( NHYD, '(A,A,A)' )
     &    "temperature-file         '",NOMTEM(1:IWAQ),"'"
      ELSE
      WRITE ( NHYD, '(A)' )
     &    "temperature-file         none                        "
      ENDIF
      IF(DIFF_DEL) THEN
        IWAQ = LEN_TRIM(NOMVIS)
        WRITE ( NHYD, '(A,A,A)' )
     &    "vert-diffusion-file      '",NOMVIS(1:IWAQ),"'"
      ELSE
      WRITE ( NHYD, '(A)' )
     &    "vert-diffusion-file      none                        "
      ENDIF
      IF(VELO_DEL) THEN
        IWAQ = LEN_TRIM(NOMVEL)
        WRITE ( NHYD, '(A,A,A)' )
     &    "velocity-file            '",NOMVEL(1:IWAQ),"'"
      ELSE
      WRITE ( NHYD, '(A)' )
     &    "velocity-file            none                        "
      ENDIF
      IWAQ = LEN_TRIM(NOMINI)
      WRITE ( NHYD, '(A,A,A)' )
     &    "surfaces-file            '",NOMINI(1:IWAQ),"'"
      WRITE ( NHYD, '(A)' )
     &    "total-grid-file          none                        "
      WRITE ( NHYD, '(A)' )
     &    "discharges-file          none                        "
      WRITE ( NHYD, '(A)' )
     &    "chezy-coefficients-file  none                        "
      WRITE ( NHYD, '(A)' )
     &    "shear-stresses-file      none                        "
      WRITE ( NHYD, '(A)' )
     &    "walking-discharges-file  none                        "
      IF ( NOLAY .GT. 1 ) THEN
         WRITE ( NHYD, '(A)' )
     &       "minimum-vert-diffusion                            "
         WRITE ( NHYD, '(A)' )
     &       "   upper-layer       0.0000E+00                   "
         WRITE ( NHYD, '(A)' )
     &       "   lower-layer       0.0000E+00                   "
         WRITE ( NHYD, '(A)' )
     &       "   interface-depth   0.0000E+00                   "
         WRITE ( NHYD, '(A)' )
     &       "end-minimum-vert-diffusion                        "
      ENDIF
      WRITE ( NHYD, '(A)' )
     &    "constant-dispersion                                  "
      WRITE ( NHYD, '(A)' )
     &    "   first-direction    0.0000                         "
      WRITE ( NHYD, '(A)' )
     &    "   second-direction   0.0000                         "
      WRITE ( NHYD, '(A)' )
     &    "   third-direction    0.0000                         "
      WRITE ( NHYD, '(A)' )
     &    "end-constant-dispersion                              "
      WRITE ( NHYD, '(A)' )
     &    "hydrodynamic-layers                               "
      DO ILAY=1,NOLAY
         WRITE ( NHYD, '(F10.4)' ) F(1,ILAY)
      ENDDO
      WRITE ( NHYD, '(A)' )
     &    "end-hydrodynamic-layers                           "
      WRITE ( NHYD, '(A)' )
     &    "water-quality-layers                              "
      DO ILAY=1,NOLAY
         WRITE ( NHYD, '(F10.4)' ) 1.0
      ENDDO
      WRITE ( NHYD, '(A)' )
     &    "end-water-quality-layers                          "
      WRITE ( NHYD, '(A)' )
     &    "discharges                                           "
      WRITE ( NHYD, '(A)' )
     &    "end-discharges                                       "
!
!-----------------------------------------------------------------------
!
      RETURN
      END
