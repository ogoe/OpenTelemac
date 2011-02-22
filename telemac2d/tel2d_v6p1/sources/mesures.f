C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS MEASURED H, U AND V AT TIME AT.
!>                GIVES THE CORRESPONDING WEIGHTS ALPHA1, ALPHA2 AND ALPHA3.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  USER SUBROUTINE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ITER, TT
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::ALPHA1 ALPHA1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ALPHA2 ALPHA2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ALPHA3 ALPHA3@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HD HD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MESH MESH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MSK MSK@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NELEM NELEM@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T1 T1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2DREF T2DREF@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2D_FILES T2D_FILES@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T3 T3@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TEXTE TEXTE@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UD UD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VD VD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::W W@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, I, OKH, OKU, OKV, TPS
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CHGDIS(), FIND_IN_SEL(), OS(), PLANTE(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CONDIN_ADJ(), HOMERE_ADJ_T2D(), PROPAG_ADJ()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 17/08/2001
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS DE LA MESURE
!>    </td></tr>
!>          <tr><td>ITER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TT
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MESURES
     &(ITER,TT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS DE LA MESURE
C| ITER           |---| 
C| TT             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: ITER
      DOUBLE PRECISION, INTENT(IN) :: TT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION TPS,C
      LOGICAL OKH,OKU,OKV
      INTEGER I
C
C-----------------------------------------------------------------------
C
      IF(T2D_FILES(T2DREF)%NAME(1:1).NE.' ') THEN
C
C-----------------------------------------------------------------------
C
C       WHEN MEASUREMENTS ARE IN A SELAFIN FILE
C
        CALL FIND_IN_SEL(HD,TEXTE(4)(1:16),T2D_FILES(T2DREF)%LU,
     &                   W,OKH,RECORD=ITER,TIME=TPS)
        CALL FIND_IN_SEL(UD,TEXTE(1)(1:16),T2D_FILES(T2DREF)%LU,
     &                   W,OKU,RECORD=ITER,TIME=TPS)
        CALL FIND_IN_SEL(VD,TEXTE(2)(1:16),T2D_FILES(T2DREF)%LU,
     &                   W,OKV,RECORD=ITER,TIME=TPS)
C
        IF(.NOT.OKH.OR..NOT.OKU.OR..NOT.OKV) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'MESURES : PROBLEME DE LECTURE DE HD, UD OU VD'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'MESURES : PROBLEM WHEN READIND HD, UD, OR VD'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(ABS(TT-TPS).GT.1.D-3) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'MESURES : PROBLEME DE LECTURE DU TEMPS'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'MESURES : PROBLEM WHEN READIND TIME'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
C       UD AND VD MAY BE QUASI-BUBBLE
C       (BUT ALPHA2 AND ALPHA3 WILL BE SET TO 0)
        IF(UD%ELM.EQ.12) THEN
          CALL CHGDIS(UD,11,12,MESH)
          CALL CHGDIS(VD,11,12,MESH)
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSE
C
C-----------------------------------------------------------------------
C
C      CASE TO BE IMPLEMENTED BY USER HERE (OTHER FILE FORMAT, ETC.)
C
       IF(LNG.EQ.1) WRITE(LU,*) 'MESURES A PROGRAMMER DANS MESURES'
       IF(LNG.EQ.2) WRITE(LU,*) 'MEASUREMENTS TO IMPLEMENT IN MESURES'
       CALL PLANTE(1)
       STOP
C
C-----------------------------------------------------------------------
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C     WEIGHT FUNCTIONS FOR ALL THE TIMESTEPS
C
      CALL VECTOR(T1,'=','MASBAS          ',
     &            HD%ELM,1.D0,T3,T3,T3,T3,T3,
     &            T3,MESH,MSK,MASKEL)
      CALL OS( 'X=Y     ' , ALPHA1 , T1 , T1 , C )
C
C     CASE OF QUASI-BUBBLE ELEMENT FOR UD
      IF(HD%ELM.NE.UD%ELM) THEN
        CALL VECTOR(T1,'=','MASBAS          ',
     &              UD%ELM,1.D0,T3,T3,T3,T3,T3,
     &              T3,MESH,MSK,MASKEL)
      ENDIF
C
      CALL OS( 'X=Y     ' , ALPHA2 , T1 , T1 , C )
      CALL OS( 'X=Y     ' , ALPHA3 , T1 , T1 , C )
C
C     CANCELS WEIGHTS FOR QUASI-BUBBLE POINTS
C
      IF(UD%ELM.EQ.12) THEN
        DO I=NPOIN+1,NPOIN+NELEM
          ALPHA2%R(I)=0.D0
          ALPHA3%R(I)=0.D0
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C