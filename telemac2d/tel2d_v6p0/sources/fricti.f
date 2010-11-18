C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FRICTION TERMS IN THEIR IMPLICIT FORM.
!>                THEY WILL BE ADDED TO MATRICES AM2 AND AM3 IN PROCU3.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CF, FUDRAG, FU_IMP, FVDRAG, FV_IMP, HFROT, HN, MASKEL, MESH, MSK, T1, T2, UN, UNSV2D, VERTIC, VN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, H, HHN, HO, IELMH, IELMS, IELMU, N, UNORM
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FRICTI, HHN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CHGDIS(), CPSTVC(), DRAGFO(), OS(), PARCOM(), PLANTE(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>COST_FUNCTION(), PROPAG(), PROPAG_ADJ(), THOMPS()

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
!> </td><td> 03/08/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 31/07/2009
!> </td><td>
!> </td><td> POINTER HHN TO AVOID A COPY
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CF
!></td><td>--></td><td>COEFFICIENT DE FROTTEMENT VARIABLE EN ESPACE
!>    </td></tr>
!>          <tr><td>FU ET FV_IMP
!></td><td><--</td><td>TERMES DE FROTTEMENT, TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>FUDRAG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FU_IMP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FVDRAG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FV_IMP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HFROT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEURS D'EAU A TN
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UN , VN
!></td><td>--></td><td>COMPOSANTES DES VECTEURS VITESSES A TN.
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VERTIC
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FRICTI
     &(FU_IMP,FV_IMP,FUDRAG,FVDRAG,UN,VN,HN,CF,MESH,T1,T2,VERTIC,
     & UNSV2D,MSK,MASKEL,HFROT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CF             |-->| COEFFICIENT DE FROTTEMENT VARIABLE EN ESPACE
C| FU ET FV_IMP   |<--| TERMES DE FROTTEMENT, TABLEAUX DE TRAVAIL
C| FUDRAG         |---| 
C| FU_IMP         |---| 
C| FVDRAG         |---| 
C| FV_IMP         |---| 
C| HFROT          |---| 
C| HN             |-->| HAUTEURS D'EAU A TN
C| MASKEL         |---| 
C| MESH           |---| 
C| MSK            |---| 
C| T1             |---| 
C| T2             |---| 
C| UN , VN        |-->| COMPOSANTES DES VECTEURS VITESSES A TN.
C| UNSV2D         |---| 
C| VERTIC         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_FRICTI => FRICTI
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL, INTENT(IN)                 :: VERTIC,MSK
      INTEGER, INTENT(IN)                 :: HFROT
      TYPE(BIEF_OBJ),  INTENT(IN)         :: UN,VN,CF,UNSV2D,MASKEL
      TYPE(BIEF_OBJ),  INTENT(IN), TARGET :: HN
      TYPE(BIEF_OBJ),  INTENT(INOUT)      :: FU_IMP,FV_IMP,FUDRAG,FVDRAG
      TYPE(BIEF_OBJ),  INTENT(INOUT), TARGET :: T1,T2
      TYPE(BIEF_MESH), INTENT(INOUT)      :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER N,IELMU,IELMH,IELMS
C
      DOUBLE PRECISION UNORM,H,C,HO
C
      INTRINSIC SQRT,MAX
C
      TYPE(BIEF_OBJ), POINTER :: HHN
C
C-----------------------------------------------------------------------
C
      IELMU=UN%ELM
      IELMH=HN%ELM
      IELMS=CF%ELM
C
C     SETUP THE WATE DEPTH WITH THE SAME DISCRETIZATION
C
      IF(IELMH.NE.IELMU) THEN
        CALL OS( 'X=Y     ' , X=T1 , Y=HN )
        CALL CHGDIS( T1 , IELMH , IELMU , MESH )
        HHN=>T1
      ELSE
        HHN=>HN
      ENDIF
C
      IF(IELMS.NE.IELMU) THEN
        IF (LNG.EQ.1) WRITE(LU,200) IELMS,IELMU
        IF (LNG.EQ.2) WRITE(LU,201) IELMS,IELMU
200     FORMAT(1X,'FRICTI : DISCRETISATION DU FROTTEMENT : ',1I6,/,
     &         1X,'DIFFERENTE DE CELLE DE U : ',1I6)
201     FORMAT(1X,'FRICTI: DISCRETIZATION OF FRICTION:',1I6,/,
     &         1X,'DIFFERENT FROM U: ',1I6)
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C     FU_IMP AND FV_IMP ARE WORKING ARRAYS
C
      CALL CPSTVC(UN,FU_IMP)
      CALL CPSTVC(VN,FV_IMP)
C
C-----------------------------------------------------------------------
C
C     BEWARE: HO HIDDEN PARAMETER
C
      HO = 3.D-2
C
      IF(HFROT.EQ.1) THEN
        DO N=1,UN%DIM1
          UNORM = SQRT(UN%R(N)**2+VN%R(N)**2)
          H = MAX(HHN%R(N),1.D-9)
C         MODIFICATION BY JMH ON 06/08/04
C         FOLLOWING LINE TO KEEP A FRICTION ON TIDAL FLATS, IF UNORM=0
C         IDEA : IF TOO SMALL, UNORM PROGRESSIVELY REPLACED BY SQRT(G*H)
C         WHEN H TENDS TO 0. LITTLE CHANGE, BUT BIG EFFECT ON UNORM/H
          IF(H.LT.HO) UNORM=MAX(UNORM,SQRT(9.81*(HO-H)*H/HO))
          FU_IMP%R(N) = - 0.5D0 * CF%R(N) * UNORM / H
          FV_IMP%R(N) = FU_IMP%R(N)
        ENDDO
      ELSEIF(HFROT.EQ.2) THEN
        CALL VECTOR(T2,'=','MASVEC          ',IELMH,
     &              1.D0,HHN,HHN,HHN,HHN,HHN,HHN,MESH,MSK,MASKEL)
        IF(NCSIZE.GT.1) CALL PARCOM(T2,2,MESH)
        DO N=1,UN%DIM1
          UNORM = SQRT(UN%R(N)**2+VN%R(N)**2)
C         SMOOTHED OR AVERAGE DEPTH
          H = MAX(T2%R(N)*UNSV2D%R(N),1.D-9)
          IF(H.LT.HO) UNORM=MAX(UNORM,SQRT(9.81*(HO-H)*H/HO))
          FU_IMP%R(N) = - 0.5D0 * CF%R(N) * UNORM / H
          FV_IMP%R(N) = FU_IMP%R(N)
        ENDDO
      ELSE
        WRITE(LU,*) 'FRICTI : PARAMETRE HFROT INCONNU : ',HFROT
        WRITE(LU,*) 'FRICTI: UNKNOWN PARAMETER HFROT:',HFROT
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(VERTIC) THEN
        CALL CPSTVC(UN,FUDRAG)
        CALL CPSTVC(VN,FVDRAG)
        CALL DRAGFO(FUDRAG,FVDRAG)
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C