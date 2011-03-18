C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WRITES OUT THE HYDRODYNAMIC FILE FOR DELWAQ (.HYD).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DIFF_DEL, F, ITSTEP, ITSTOP, ITSTRT, MARDAT, MARTIM, MBND, NHYD, NOLAY, NOMCOU, NOMGEO, NOMINI, NOMLIM, NOMSAL, NOMSOU, NOMTEM, NOMVEB, NOMVEL, NOMVIS, NORSED, NOSUIS, NPOIN2, NSEG, NSTEPA, SALI_DEL, TEMP_DEL, TITRE, VELO_DEL
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ILAY, IWAQ
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TEL4DEL()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 6.0                                       </center>
!> </td><td> 20/03/2007
!> </td><td> CHARLES MOULINEC
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DIFF_DEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ITSTEP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ITSTOP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ITSTRT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MARDAT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MARTIM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MBND
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NHYD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NOLAY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NOMCOU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NOMGEO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NOMINI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NOMLIM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NOMSAL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NOMSOU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NOMTEM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NOMVEB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NOMVEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NOMVIS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NORSED
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NOSUIS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSTEPA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SALI_DEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TEMP_DEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TITRE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VELO_DEL
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                           SUBROUTINE WRIHYD
     &(TITRE , ITSTRT , ITSTOP , ITSTEP , NPOIN2 , MBND   ,
     & NSEG  , NOLAY  , NOMGEO , NOMLIM ,
     & F     , NSTEPA , NOMSOU , NOSUIS , NOMCOU ,
     & NOMINI, NOMVEB , NORSED , NOMSAL , NOMTEM , NOMVEL , NOMVIS ,
     & NHYD,
     & SALI_DEL,TEMP_DEL,VELO_DEL,DIFF_DEL,MARDAT,MARTIM)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DIFF_DEL       |---| 
C| F             |---| 
C| ITSTEP         |---| 
C| ITSTOP         |---| 
C| ITSTRT         |---| 
C| MARDAT         |---| 
C| MARTIM         |---| 
C| MBND           |---| 
C| NHYD           |---| 
C| NOLAY          |---| 
C| NOMCOU         |---| 
C| NOMGEO         |---| 
C| NOMINI         |---| 
C| NOMLIM         |---| 
C| NOMSAL         |---| 
C| NOMSOU         |---| 
C| NOMTEM         |---| 
C| NOMVEB         |---| 
C| NOMVEL         |---| 
C| NOMVIS         |---| 
C| NORSED         |---| 
C| NOSUIS         |---| 
C| NPOIN2         |---| 
C| NSEG           |---| 
C| NSTEPA         |---| 
C| SALI_DEL       |---| 
C| TEMP_DEL       |---| 
C| TITRE          |---| 
C| VELO_DEL       |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER,          INTENT(IN) :: NHYD,ITSTRT,ITSTOP,ITSTEP,NPOIN2
      INTEGER,          INTENT(IN) :: NSEG,NOLAY,NSTEPA,MBND
      INTEGER,          INTENT(IN) :: MARDAT(3),MARTIM(3)
      CHARACTER(*),     INTENT(IN) :: TITRE,NOMGEO,NOMLIM
      CHARACTER(*),     INTENT(IN) :: NOMSOU,NOSUIS,NOMCOU,NOMSAL,NOMTEM
      CHARACTER(*),     INTENT(IN) :: NOMINI,NOMVEB,NORSED,NOMVEL,NOMVIS
      DOUBLE PRECISION, INTENT(IN) :: F(NPOIN2,NOLAY)
      LOGICAL,          INTENT(IN) :: SALI_DEL,TEMP_DEL
      LOGICAL,          INTENT(IN) :: VELO_DEL,DIFF_DEL
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ILAY,IWAQ
C
C-----------------------------------------------------------------------
C
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
      IWAQ = LEN_TRIM(NOSUIS)
      WRITE ( NHYD, '(A,A,A)' )
     &    "areas-file               '",NOSUIS(1:IWAQ),"'"
      IWAQ = LEN_TRIM(NOMCOU)
      WRITE ( NHYD, '(A,A,A)' )
     &    "flows-file               '",NOMCOU(1:IWAQ),"'"
      IWAQ = LEN_TRIM(NOMVEB)
      WRITE ( NHYD, '(A,A,A)' )
     &    "pointers-file            '",NOMVEB(1:IWAQ),"'"
      IWAQ = LEN_TRIM(NORSED)
      WRITE ( NHYD, '(A,A,A)' )
     &    "lengths-file             '",NORSED(1:IWAQ),"'"
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
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C