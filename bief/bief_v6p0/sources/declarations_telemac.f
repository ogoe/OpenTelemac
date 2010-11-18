C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief    DECLARATIONS COMMON TO ALL PROGRAMS<br>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFOS : RVVP, ICVP, IFVP, IVVP<hr>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ADV_CAR, ADV_LPO, ADV_LPO_TF, ADV_NSC, ADV_NSC_NC, ADV_NSC_TF, ADV_PSI, ADV_PSI_NC, ADV_PSI_TF, ADV_SUP, COUPLING, KADH, KDDL, KDIR, KDRAIN, KENT, KENTU, KINC, KLOG, KMIX, KNEU, KOND, KSORT, NAMECODE, NNAMECODE, TMCOD_ARTEMIS, TMCOD_ESTEL2D, TMCOD_ESTEL3D, TMCOD_MATISSE, TMCOD_POSTEL3D, TMCOD_RUBENS, TMCOD_SISYPHE, TMCOD_SPARTACUS2D, TMCOD_STBTEL, TMCOD_SUBIEF2D, TMCOD_SUBIEF3D, TMCOD_TEL2DE3D, TMCOD_TEL2DSIS, TMCOD_TEL3DSIS, TMCOD_TELEMAC2D, TMCOD_TELEMAC3D, TMCOD_TOMAWAC
!>   </td></tr>
!>     </table>

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
      MODULE DECLARATIONS_TELEMAC
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C----------------------------------------------------------------------
C
C 1./ INTEGER VALUES TO DESCRIBE BOUNDARY CONDITIONS:
C
C
C     FOR THE BOUNDARY CONDITIONS FILE:
C
C     ENTRANCE: PRESCRIBED VALUES (SAVE VELOCITIES)
      INTEGER, PARAMETER :: KENT  =  5
C
C     VELOCITY IMPOSED (INSTEAD OF DISCHARGE)
      INTEGER, PARAMETER :: KENTU =  6
C
C     FREE OUTPUT
      INTEGER, PARAMETER :: KSORT =  4
C
C     NO-SLIP CONDITION
      INTEGER, PARAMETER :: KADH  =  0
C
C     WALL WITH OR WITHOUT FRICTION
      INTEGER, PARAMETER :: KLOG  =  2
C
C     OPEN BOUNDARY WITH INCIDENT WAVE
      INTEGER, PARAMETER :: KINC  =  1
C
C     ESTEL-2D : FREE DRAINAGE
      INTEGER, PARAMETER :: KDRAIN  =  3
C
C     ESTEL-2D : MIXED CONDITION
      INTEGER, PARAMETER :: KMIX  =  4
C
C     DEPENDING ON ALGORITHMS AND CASES, THESE VALUES WILL BE
C     TRANSFORMED INTO:
C
C     TECHNICAL BOUNDARY CONDITIONS
C
C     NEUMANN
      INTEGER, PARAMETER :: KNEU  =  1
C
C     DIRICHLET
      INTEGER, PARAMETER :: KDIR  =  2
C
C     DEGREE OF FREEDOM
      INTEGER, PARAMETER :: KDDL  =  3
C
C     INCIDENT WAVE
      INTEGER, PARAMETER :: KOND  =  4
C
C----------------------------------------------------------------------
C
C 2./ INTEGER VALUES TO DESCRIBE ADVECTION SCHEMES:
C
C     CHARACTERISTICS
      INTEGER, PARAMETER :: ADV_CAR     =  1
C     SUPG
      INTEGER, PARAMETER :: ADV_SUP     =  2
C     LEO POSTMA
      INTEGER, PARAMETER :: ADV_LPO     =  3
C     DISTRIBUTIVE N-SCHEME
      INTEGER, PARAMETER :: ADV_NSC     =  4
C     DISTRIBUTIVE PSI SCHEME
      INTEGER, PARAMETER :: ADV_PSI     =  5
C     NON CONSERVATIVE EQUATION, DISTRIBUTIVE PSI SCHEME
      INTEGER, PARAMETER :: ADV_PSI_NC  =  6
C     NON CONSERVATIVE EQUATION, DISTRIBUTIVE N SCHEME
      INTEGER, PARAMETER :: ADV_NSC_NC  =  7
C     LEO POSTMA, EDGE-BASED FOR TIDAL FLATS
      INTEGER, PARAMETER :: ADV_LPO_TF  = 13
C     DISTRIBUTIVE N SCHEME, EDGE-BASED FOR TIDAL FLATS
      INTEGER, PARAMETER :: ADV_NSC_TF  = 14
C     DISTRIBUTIVE PSI SCHEME, EDGE-BASED FOR TIDAL FLATS
      INTEGER, PARAMETER :: ADV_PSI_TF  = 15
C
C-----------------------------------------------------------------------
C
C 3./ CODE COUPLING
C
      CHARACTER*144 COUPLING
C
C 4./ NAME OF CURRENT CODE (SEE BIEF_OPEN_FILES AND CONFIG_CODE)
C
      CHARACTER(LEN=24) :: NAMECODE,NNAMECODE(3)
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
C
C 2B./ NUMBER FOR EACH CODE IN TELEMAC SYSTEM (THIS WAS FOR PROTECTION)
C
      INTEGER, PARAMETER :: TMCOD_ARTEMIS      = 1
      INTEGER, PARAMETER :: TMCOD_TOMAWAC      = 2
      INTEGER, PARAMETER :: TMCOD_ESTEL2D      = 3
      INTEGER, PARAMETER :: TMCOD_ESTEL3D      = 4
      INTEGER, PARAMETER :: TMCOD_MATISSE      = 5
      INTEGER, PARAMETER :: TMCOD_RUBENS       = 6
      INTEGER, PARAMETER :: TMCOD_SISYPHE      = 7
      INTEGER, PARAMETER :: TMCOD_STBTEL       = 8
      INTEGER, PARAMETER :: TMCOD_SUBIEF2D     = 9
      INTEGER, PARAMETER :: TMCOD_SUBIEF3D     =10
      INTEGER, PARAMETER :: TMCOD_TELEMAC2D    =11
      INTEGER, PARAMETER :: TMCOD_TELEMAC3D    =12
      INTEGER, PARAMETER :: TMCOD_POSTEL3D     =13
      INTEGER, PARAMETER :: TMCOD_TEL2DSIS     =13
      INTEGER, PARAMETER :: TMCOD_TEL3DSIS     =14
      INTEGER, PARAMETER :: TMCOD_TEL2DE3D     =15
      INTEGER, PARAMETER :: TMCOD_SPARTACUS2D  =16
C
C 2C./ GLOBAL DATA (THIS WAS FOR PROTECTION)
C
      INTEGER IFVP,ICVP,IVVP
      DOUBLE PRECISION RVVP
      COMMON/INFOS/RVVP,ICVP,IFVP,IVVP
C
C OTHER FUNCTIONS (THIS WAS FOR PROTECTION)
C
      INTERFACE
        INTEGER FUNCTION ISQRT0 (IVAL)
          IMPLICIT NONE
          INTEGER  , INTENT(IN) :: IVAL
        END FUNCTION
      END INTERFACE
C
      INTERFACE
        INTEGER FUNCTION ISQRT (IVAL)
          IMPLICIT NONE
          INTEGER  , INTENT(IN) :: IVAL
        END FUNCTION
      END INTERFACE
C
      INTERFACE
        DOUBLE PRECISION FUNCTION RSQRT (RVAL)
          IMPLICIT NONE
          DOUBLE PRECISION , INTENT(IN) :: RVAL
        END FUNCTION
      END INTERFACE
C
      INTERFACE
        INTEGER FUNCTION ISQRTF (IVAL)
          IMPLICIT NONE
          INTEGER , INTENT(IN) :: IVAL
        END FUNCTION
      END INTERFACE
C
C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------
C
      CONTAINS
C                       ****************************
                        CHARACTER*11 FUNCTION EXTENS
C                       ****************************
C
     &(N,IPID)
C
C***********************************************************************
C  BIEF RELEASE 5.9       26/05/2008 J-M HERVOUET (LNHE)  01 30 87 80 18
C
C***********************************************************************
C
C      FUNCTIONS: EXTENDS THE FILES ON EACH PROCESSOR
C      ==========
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________|
C |     N          | -->| NOMBRE DE PROCESSEURS MOINS UN = NCSIZE-1
C |     IPID       | -->| NUMERO DU PROCESSEUR
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), (DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C CALLED BY :
C
C CALLS : --
C
C**********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER, INTENT(IN) :: IPID,N
C
C-----------------------------------------------------------------------
C
      IF(N.GT.0) THEN
C
        EXTENS='00000-00000'
C
        IF(N.LT.10) THEN
          WRITE(EXTENS(05:05),'(I1)') N
        ELSEIF(N.LT.100) THEN
          WRITE(EXTENS(04:05),'(I2)') N
        ELSEIF(N.LT.1000) THEN
          WRITE(EXTENS(03:05),'(I3)') N
        ELSEIF(N.LT.10000) THEN
          WRITE(EXTENS(02:05),'(I4)') N
        ELSE
          WRITE(EXTENS(01:05),'(I5)') N
        ENDIF
C
        IF(IPID.LT.10) THEN
          WRITE(EXTENS(11:11),'(I1)') IPID
        ELSEIF(IPID.LT.100) THEN
          WRITE(EXTENS(10:11),'(I2)') IPID
        ELSEIF(IPID.LT.1000) THEN
          WRITE(EXTENS(09:11),'(I3)') IPID
        ELSEIF(IPID.LT.10000) THEN
          WRITE(EXTENS(08:11),'(I4)') IPID
        ELSE
          WRITE(EXTENS(07:11),'(I5)') IPID
        ENDIF
C
      ELSE
C
        EXTENS='       '
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END FUNCTION
C
C-----------------------------------------------------------------------
C
      END MODULE DECLARATIONS_TELEMAC

C
C#######################################################################
C