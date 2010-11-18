C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPLEMENTS A VECTOR AT THE INTERFACES BETWEEN
!>                SUB-DOMAINS.
!><br>            X CAN BE A BLOCK OF VECTORS. IN THIS CASE, ALL THE
!>                VECTORS IN THE BLOCK ARE TREATED.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT : FROM RELEASE 5.9 ON, IDENTICAL VALUES ARE
!>                     ENSURED AT INTERFACE POINTS SO THAT DIFFERENT
!>                     PROCESSORS WILL ALWAYS MAKE THE SAME DECISION
!>                     IN TESTS ON REAL NUMBERS.

!>  @warning  IF THE VECTORS HAVE A SECOND DIMENSION, IT IS
!>            IGNORED FOR THE TIME BEING

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ICOM, MESH, X
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NPTIR NPTIR@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IAN, NP11, NPLAN, NPOIN, NSEG, X2, X3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_PARCOM, X2, X3
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> NBPTS(), PARCOM2(), PARCOM2_SEG(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_EFFPNT(), BEDLOAD_NERBED_VF(), BEDLOAD_SECCURRENT(), BEDLOAD_SOLVS_VF(), BYPASS_CRUSHED_POINTS_EBE(), BYPASS_CRUSHED_POINTS_SEG(), CARACT(), CFLPSI(), CFLVF(), CHECK_DIGITS(), CHECK_DOT(), CONDIS_SISYPHE(), CORMAR(), CVDF3D(), CVDFTR(), CVTRVF(), CVTRVF_POS(), DIFF3D(), FILTER(), FILTER_H(), FRICTI(), FSGRAD(), GRAD2D(), HVF(), INITAB(), MASBAS2D(), MASKTO(), MATRBL(), MAXSLOPE(), MESH_PROP(), MURD3D(), MURD3D_POS(), PARCOM_BORD(), PARMOY(), POSITIVE_DEPTHS(), PRECD1(), PRECD4(), PRECD9(), PREDIV(), PROPAG(), PROSOU(), RADIAT(), SISYPHE(), SMAGO3D(), SOLVE(), SOUKEP(), SOUKOM(), SOURCES_SINKS(), STREAMLINE(), STREAMLINE_TOMAWAC(), TELEMAC2D(), TRIDW2(), TRISOU(), TVF(), VELRES(), VGFPSI(), WAVE_EQUATION(), WCTURB(), WSTARW()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 24/10/08
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td> AFTER REINHARD HINKELMANN (HANNOVER UNI.)
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ICOM
!></td><td>--></td><td>COMMUNICATION MODE
!>                  = 1 : VALUE WITH MAXIMUM ABSOLUTE VALUE TAKEN
!>                  = 2 : CONTRIBUTIONS ADDED
!>                  = 3 : MAXIMUM CONTRIBUTION RETAINED
!>                  = 4 : MINIMUM CONTRIBUTION RETAINED
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>MAILLAGE.
!>    </td></tr>
!>          <tr><td>X
!></td><td><-></td><td>VECTEUR OU BLOC DE VECTEURS.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PARCOM
     &( X , ICOM , MESH )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ICOM           |-->| COMMUNICATION MODE
C|                |   | = 1 : VALUE WITH MAXIMUM ABSOLUTE VALUE TAKEN
C|                |   | = 2 : CONTRIBUTIONS ADDED
C|                |   | = 3 : MAXIMUM CONTRIBUTION RETAINED
C|                |   | = 4 : MINIMUM CONTRIBUTION RETAINED
C| MESH           |-->| MAILLAGE.
C| X             |<->| VECTEUR OU BLOC DE VECTEURS.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_PARCOM => PARCOM
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C-----------------------------------------------------------------------
C
      INTEGER, INTENT(IN) :: ICOM
C
C     STRUCTURES: VECTORS OR BLOCKS
C
      TYPE(BIEF_MESH), INTENT(INOUT)   :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT) :: X
C
C-----------------------------------------------------------------------
C
      TYPE(BIEF_OBJ), POINTER  :: X2,X3
      INTEGER NPOIN,NPLAN,IAN,NP11,NSEG
C
C***********************************************************************
C
C  OF NO USE IF A SUB-DOMAIN IS DISCONNECTED FROM THE OTHERS
C
      IF(NPTIR.EQ.0) RETURN
C
C-----------------------------------------------------------------------
C
      NPOIN = MESH%NPOIN
      NPLAN = 1
      IF(MESH%DIM.EQ.3) THEN
        NPOIN = NBPTS(11)
        NPLAN = MESH%NPOIN/NPOIN
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(X%TYPE.EQ.2) THEN
C
C     VECTOR STRUCTURE
C
      IAN = 1
      CALL PARCOM2(X%R,X%R,X%R,NPOIN,NPLAN,ICOM,IAN,MESH)
C
      IF(X%ELM.EQ.13) THEN
        NP11=NBPTS(11)
        NSEG=MESH%NSEG
        CALL PARCOM2_SEG(X%R(NP11+1:NP11+NSEG),
     &                   X%R(NP11+1:NP11+NSEG),
     &                   X%R(NP11+1:NP11+NSEG),
     &                   NSEG,1,ICOM,IAN,MESH,1)
      ENDIF
C
      ELSEIF(X%TYPE.EQ.4) THEN
C
C     BLOCK STRUCTURE
C
C     BEWARE: NUMBER LIMITED TO 3 |||||||||||||||||||||||||
      IAN = X%N
      IF(IAN.EQ.1) THEN
        X2 => X%ADR(1)%P
        X3 => X%ADR(1)%P
      ELSEIF(IAN.EQ.2) THEN
        X2 => X%ADR(2)%P
        X3 => X%ADR(2)%P
      ELSEIF(IAN.EQ.3) THEN
        X2 => X%ADR(2)%P
        X3 => X%ADR(3)%P
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'PARCOM PREVU JUSQU''A 3 VECTEURS'
        IF(LNG.EQ.2) WRITE(LU,*) 'PARCOM: NO MORE THAN 3 VECTORS'
        CALL PLANTE(1)
        STOP
      ENDIF
C
      CALL PARCOM2(X%ADR(1)%P%R,X2%R,X3%R,NPOIN,NPLAN,ICOM,IAN,MESH)
C
C     PROVISIONNALY 1 BY 1, COULD BE OPTIMISED
C
      IF(X%ADR(1)%P%ELM.EQ.13) THEN
        NP11=NBPTS(11)
        NSEG=MESH%NSEG
        CALL PARCOM2_SEG(X%ADR(1)%P%R(NP11+1:NP11+NSEG),
     &                   X%ADR(1)%P%R(NP11+1:NP11+NSEG),
     &                   X%ADR(1)%P%R(NP11+1:NP11+NSEG),
C    *                   NSEG,1,ICOM,IAN,MESH)
     &                   NSEG,1,ICOM,1  ,MESH,1)
      ENDIF
      IF(IAN.GE.2.AND.X2%ELM.EQ.13) THEN
        NP11=NBPTS(11)
        NSEG=MESH%NSEG
        CALL PARCOM2_SEG(X2%R(NP11+1:NP11+NSEG),
     &                   X2%R(NP11+1:NP11+NSEG),
     &                   X2%R(NP11+1:NP11+NSEG),
C    *                   NSEG,1,ICOM,IAN,MESH)
     &                   NSEG,1,ICOM,1  ,MESH,1)
      ENDIF
      IF(IAN.EQ.3.AND.X3%ELM.EQ.13) THEN
        NP11=NBPTS(11)
        NSEG=MESH%NSEG
        CALL PARCOM2_SEG(X3%R(NP11+1:NP11+NSEG),
     &                   X3%R(NP11+1:NP11+NSEG),
     &                   X3%R(NP11+1:NP11+NSEG),
C    *                   NSEG,1,ICOM,IAN,MESH)
     &                   NSEG,1,ICOM,1  ,MESH,1)
      ENDIF
C
      ELSE
C
C     ERROR ON THE STRUCTURE
C
      IF (LNG.EQ.1) WRITE(LU,50) X%NAME,X%TYPE
      IF (LNG.EQ.1) WRITE(LU,53)
50    FORMAT(1X,'PARCOM (BIEF) : NOM DE X : ',A6,'  TYPE : ',1I6)
53    FORMAT(1X,'                CAS NON PREVU')
      IF (LNG.EQ.2) WRITE(LU,51) X%NAME,X%TYPE
      IF (LNG.EQ.2) WRITE(LU,54)
51    FORMAT(1X,'PARCOM (BIEF): NAME OF X: ',A6,'  TYPE : ',1I6)
54    FORMAT(1X,'               UNEXPECTED CASE')
      CALL PLANTE(1)
      STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C