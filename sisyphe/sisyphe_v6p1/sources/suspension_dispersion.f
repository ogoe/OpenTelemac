C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE DISPERSION PARAMETERS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CODE, DISP, HN, KX, KY, KZ, NPOIN, OPTDIF, T1, T2, T3, TOB, U2D, V2D, VISC_TEL, XKX, XKY, XMVE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> COST, DIMVISC, K, SINT, UETH
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SUSPENSION_DISPERSION
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), OS(), OV_2(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SUSPENSION_MAIN()

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
!>      <td><center> 5.1                                       </center>
!> </td><td> 13/12/2000
!> </td><td> C. MOULIN (LNH)  01 30 87 83 81
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CODE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DISP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KZ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTDIF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VISC_TEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XKX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XKY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVE
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE SUSPENSION_DISPERSION ! (_IMP_)
     &  (TOB, XMVE,HN,  OPTDIF, NPOIN, XKX, XKY,
     &   T1, T2, T3, KX, KY, KZ, DISP,U2D,V2D,VISC_TEL,CODE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CODE           |---| 
C| DISP           |---| 
C| HN             |---| 
C| KX             |---| 
C| KY             |---| 
C| KZ             |---| 
C| NPOIN          |---| 
C| OPTDIF         |---| 
C| T1             |---| 
C| T2             |---| 
C| T3             |---| 
C| TOB            |---| 
C| U2D            |---| 
C| V2D            |---| 
C| VISC_TEL       |---| 
C| XKX            |---| 
C| XKY            |---| 
C| XMVE           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,
     &    EX_SUSPENSION_DISPERSION => SUSPENSION_DISPERSION
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE (BIEF_OBJ),  INTENT(IN)    :: TOB,HN,VISC_TEL
      INTEGER,          INTENT(IN)    :: OPTDIF, NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, XKX, XKY
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T1, T2, T3
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: KX, KY, KZ, DISP
      TYPE (BIEF_OBJ),  INTENT(IN)    :: U2D,V2D
      CHARACTER(LEN=24), INTENT(IN)   :: CODE

      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER                     :: K,DIMVISC
      DOUBLE PRECISION            :: UETH, COST, SINT

!
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      ! ****************************************************** !
      ! IA - CONSTANT DISPERSION OR DISPERSION(I) = ALPHA(I)*H !
      ! ****************************************************** !
      IF (OPTDIF == 2.OR.OPTDIF == 1) THEN
!
C        ADDED ON 19/10/2007 (JMH)
         CALL CPSTVC(U2D,T1)
         CALL CPSTVC(U2D,T2)
!
         IF(OPTDIF == 2) THEN
           DO K = 1, NPOIN
             UETH = SQRT(TOB%R(K)/XMVE)
             T1%R(K) = XKX * UETH * HN%R(K)
             T2%R(K) = XKY * UETH * HN%R(K)
           ENDDO
         ELSE
           CALL OS('X=C     ', X=T1, C=XKX)
           CALL OS('X=C     ', X=T2, C=XKY)
         ENDIF
!
         CALL OS('X=N(Y,Z)', X=T3, Y=U2D, Z=V2D)
!
         DO K=1,NPOIN
!
            IF(T3%R(K).GE.1.D-6) THEN
               COST = U2D%R(K)/T3%R(K)
               SINT = V2D%R(K)/T3%R(K)
            ELSE
               COST = 0.D0
               SINT = 0.D0
            ENDIF
!
            KX%R(K) = (T1%R(K) - T2%R(K))*(COST**2) + T2%R(K)
            KY%R(K) = (T2%R(K) - T1%R(K))*(COST**2) + T1%R(K)
            KZ%R(K) = (T1%R(K) - T2%R(K))*COST*SINT
!
         ENDDO
!
!
      ! *********************************** !
      ! IB - DISPERSION GIVEN BY TELEMAC-2D ! (_IMP_)
      ! *********************************** !
      ELSEIF(OPTDIF == 3) THEN
!
         IF(CODE(1:9).EQ.'TELEMAC2D') THEN
           IF(VISC_TEL%DIM2.EQ.1) THEN
             CALL OS('X=Y     ', X=KX,Y=VISC_TEL)
             CALL OS('X=Y     ', X=KY,Y=KX)
             CALL OS('X=0     ', X=KZ)
           ELSEIF(VISC_TEL%DIM2.EQ.3) THEN
             DIMVISC=VISC_TEL%MAXDIM1
             DO K=1,NPOIN
               KX%R(K)=VISC_TEL%R(K)
               KY%R(K)=VISC_TEL%R(K+  DIMVISC)
               KZ%R(K)=VISC_TEL%R(K+2*DIMVISC)
             ENDDO
           ELSE
             IF(LNG.EQ.1) THEN
               WRITE(LU,*) 'SUSPENSION_DISPERSION:'
               WRITE(LU,*) ' '
               WRITE(LU,*) 'MAUVAISE DIMENSION DE VISC_TEL:',
     &                      VISC_TEL%DIM2
             ENDIF
             IF(LNG.EQ.2) THEN
               WRITE(LU,*) 'SUSPENSION_DISPERSION:'
               WRITE(LU,*) ' '
               WRITE(LU,*) 'UNEXPECTED DIMENSION OF VISC_TEL:',
     &                      VISC_TEL%DIM2
             ENDIF
             CALL PLANTE(1)
             STOP
           ENDIF
         ELSE
           WRITE(LU,*) ' '
           IF(LNG.EQ.1) THEN
             WRITE(LU,*) 'SUSPENSION_DISPERSION:'
             WRITE(LU,*) ' '
             WRITE(LU,*) 'OPTION 3 : DIFFUSION DONNEE PAR TELEMAC'
             WRITE(LU,*) 'NON PROGRAMMEE OU IMPOSSIBLE AVEC ',CODE
           ENDIF
           IF(LNG.EQ.2) THEN
             WRITE(LU,*) 'SUSPENSION_DISPERSION:'
             WRITE(LU,*) ' '
             WRITE(LU,*) 'OPTION 3: DIFFUSIVITY GIVEN BY TELEMAC'
             WRITE(LU,*) 'NOT IMPLEMENTED OR IMPOSSIBLE WITH ',CODE
           ENDIF
           CALL PLANTE(1)
           STOP
         ENDIF
!
      ! ***************************************** !
      ! IC - OPTION FOR DISPERSION NOT CODED UP   ! (_IMP_)
      ! ***************************************** !
      ELSE
         IF (LNG == 1) WRITE(LU,30) OPTDIF
         IF (LNG == 2) WRITE(LU,31) OPTDIF
         CALL PLANTE(1)
         STOP
      ENDIF
!
      CALL OV_2('X=Y     ', DISP%R, 1, KX%R, 1, KX%R, 1, 0.D0,
     &          DISP%MAXDIM1, DISP%DIM1)
      CALL OV_2('X=Y     ', DISP%R, 2, KY%R, 1, KY%R, 1, 0.D0,
     &          DISP%MAXDIM1, DISP%DIM1)
      CALL OV_2('X=Y     ', DISP%R, 3, KZ%R, 1, KZ%R, 1, 0.D0,
     &          DISP%MAXDIM1, DISP%DIM1)

      !----------------------------------------------------------------!
30    FORMAT('DISPERSION : OPTION POUR LA DISPERSION NON PREVUE: ',1I6)
      !----------------------------------------------------------------!
31    FORMAT('DISPERSION: OPTION FOR THE DISPERSION NOT AVAILABLE:',1I6)
      !----------------------------------------------------------------!

!======================================================================!
!======================================================================!

      RETURN
      END SUBROUTINE SUSPENSION_DISPERSION
C
C#######################################################################
C