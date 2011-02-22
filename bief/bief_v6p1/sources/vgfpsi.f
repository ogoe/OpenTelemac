C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SAME AS VGRADF BUT WITH THE PSI SCHEME AND
!>                SUB-ITERATIONS TO REACH STABILITY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CFLMAX, DT, F, IELM, MASKEL, MESH, MSK, RES, T1, T2, U, V, XMUL
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DDT, IT, NIT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CFLPSI(), CPSTVC(), MAXI(), NBPTS(), OS(), PARCOM(), PLANTE(), P_DMAX(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CVDFTR(), PROPAG()

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
!> </td><td> 18/02/08
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CFLMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F
!></td><td>--></td><td>FONCTION F.
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT DU RESULTAT.
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
!>          <tr><td>RES
!></td><td><--</td><td>VECTEUR RESULTAT.
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPOSANTES DU CHAMP CONVECTEUR.
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VGFPSI
     &(RES,IELM,U,V,F,DT,XMUL,CFLMAX,T1,T2,MESH,MSK,MASKEL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CFLMAX         |---| 
C| DT             |---| 
C| F             |-->| FONCTION F.
C| IELM           |-->| TYPE D'ELEMENT DU RESULTAT.
C| MASKEL         |---| 
C| MESH           |---| 
C| MSK            |---| 
C| RES            |<--| VECTEUR RESULTAT.
C| T1             |---| 
C| T2             |---| 
C| U,V            |-->| COMPOSANTES DU CHAMP CONVECTEUR.
C| XMUL           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF !, EX_VGFPSI => VGFPSI
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      INTEGER, INTENT(IN)           :: IELM
      LOGICAL, INTENT(IN)           :: MSK
C
      DOUBLE PRECISION, INTENT(IN)  :: DT,XMUL
      DOUBLE PRECISION, INTENT(OUT) :: CFLMAX
C
C     STRUCTURES
C
      TYPE(BIEF_OBJ), INTENT(IN)      :: U,V,F,MASKEL
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: RES,T1,T2
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      INTEGER IT,NIT
      DOUBLE PRECISION DDT
C
      INTRINSIC INT
C
      DOUBLE PRECISION P_DMAX
      EXTERNAL         P_DMAX
C
C-----------------------------------------------------------------------
C
C     NUMBER OF NECESSARY SUB-ITERATIONS
C
      CALL CFLPSI(T1,U,V,DT,IELM,MESH,MSK,MASKEL)
      CALL MAXI(CFLMAX,IT,T1%R,BIEF_NBPTS(IELM,MESH))
      IF(NCSIZE.GT.1) CFLMAX=P_DMAX(CFLMAX)
C
      NIT = INT(CFLMAX) + 1
C     WRITE(LU,*) 'VGFPSI : NIT=',NIT,' CFLMAX=',CFLMAX
C
      IF(NIT.GT.100) THEN
        IF(LNG.EQ.1) WRITE(LU,900) NIT
        IF(LNG.EQ.2) WRITE(LU,901) NIT
900     FORMAT(1X,'VGFPSI : ',1I6,' SOUS-ITERATIONS DEMANDEES POUR LE'
     &   ,/,1X,   '         SCHEMA PSI. DIMINUER LE PAS DE TEMPS')
901     FORMAT(1X,'VGFPSI: ',1I6,' SUB-ITERATIONS REQUIRED FOR THE'
     &   ,/,1X,   '        PSI SCHEME. DECREASE THE TIME-STEP')
        CALL PLANTE(1)
        STOP
      ENDIF
C
      DDT = DT/NIT
C
C     T1 WILL TAKE THE SUCCESSIVE VALUES OF F
      CALL OS( 'X=Y     ' , X=T1 , Y=F )
      IF(NIT.GT.1) THEN
        CALL VECTOR(MESH%T,'=','MASBAS          ',IELM,
     &              1.D0,F,F,F,F,F,F,MESH,MSK,MASKEL)
        IF(NCSIZE.GT.1) CALL PARCOM(MESH%T,2,MESH)
        CALL OS('X=1/Y   ',MESH%T,MESH%T,MESH%T,-DDT/XMUL,
     &          IOPT=2,INFINI=0.D0,ZERO=1.D-8)
      ENDIF
C
C     LOOP ON THE SUB-ITERATIONS
C
      CALL CPSTVC(F,RES)
C
      DO IT=1,NIT
C
        IF(NIT.GT.1) THEN
          IF(IT.EQ.1) CALL OS('X=0     ',X=RES)
          CALL VECTOR(T2,'=','VGRADF       PSI',IELM,
     &                XMUL,T1,T1,T1,U,V,V,MESH,MSK,MASKEL)
          CALL OS('X=X+CY  ',X=RES,Y=T2,C=1.D0/NIT)
          IF(NCSIZE.GT.1) CALL PARCOM(T2,2,MESH)
          CALL OS('X=X+CYZ ',T1,T2,MESH%T,-DDT/XMUL)
        ELSE
          CALL VECTOR(RES,'=','VGRADF       PSI',IELM,
     &                XMUL,T1,T1,T1,U,V,V,MESH,MSK,MASKEL)
        ENDIF
C
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
