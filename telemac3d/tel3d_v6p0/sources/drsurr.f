C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES DELTAR = (RHO-RHO0)/RHO0.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  DENLAW =0,1,2,3 (NOTHING, DEPENDS ON TEMP., SALINITY, OR BOTH).

!>  @warning  ASSUMES THAT RHO0 IS GIVEN AT T=15degC AND S=0 MG/L
!>            AND THAT BETA(S)=-1E-3 (L/MG) AT T=15degC

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BETAC, DELTAR, DENLAW, IND_S, IND_T, NTRAC, RHO, RHO0, RHOS, SEDI, T0AC, TA
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ITRAC, NTRACM1
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

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
!> </td><td> **/11/2000
!> </td><td> JMH   AG - LNHE
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/1999
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BETAC
!></td><td>--></td><td>-(1/RHO)*(DRHO/DT) FOR TRACERS WHEN CONSTANT
!>    </td></tr>
!>          <tr><td>DELTAR
!></td><td><--</td><td>(RHO-RHO0)/RHO0
!>    </td></tr>
!>          <tr><td>DENLAW
!></td><td>--></td><td>CHOICE OF DENSITY LAW (SEE ABOVE)
!>    </td></tr>
!>          <tr><td>IND_S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IND_T
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>--></td><td>NUMBER OF ACTIVE TRACERS
!>    </td></tr>
!>          <tr><td>RHO
!></td><td>--></td><td>WATER DENSITY
!>    </td></tr>
!>          <tr><td>RHO0
!></td><td>--></td><td>WATER DENSITY AT REFERENCE CONCENTRATION
!>    </td></tr>
!>          <tr><td>RHOS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SEDI
!></td><td>--></td><td>IF YES, THERE IS SEDIMENT
!>    </td></tr>
!>          <tr><td>T0AC
!></td><td>--></td><td>REFERENCE CONCENTRATION OF TRACERS
!>    </td></tr>
!>          <tr><td>TA
!></td><td>--></td><td>TRACERS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DRSURR
     & (DELTAR, TA, BETAC,T0AC,RHO,RHO0,RHOS,DENLAW,SEDI,NTRAC,
     &  IND_T,IND_S)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BETAC          |-->| -(1/RHO)*(DRHO/DT) FOR TRACERS WHEN CONSTANT
C| DELTAR         |<--| (RHO-RHO0)/RHO0
C| DENLAW         |-->| CHOICE OF DENSITY LAW (SEE ABOVE)
C| IND_S          |---| 
C| IND_T          |---| 
C| NTRAC          |-->| NUMBER OF ACTIVE TRACERS
C| RHO            |-->| WATER DENSITY
C| RHO0           |-->| WATER DENSITY AT REFERENCE CONCENTRATION
C| RHOS           |---| 
C| SEDI           |-->| IF YES, THERE IS SEDIMENT
C| T0AC           |-->| REFERENCE CONCENTRATION OF TRACERS
C| TA             |-->| TRACERS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)           :: NTRAC, DENLAW,IND_T,IND_S
      DOUBLE PRECISION, INTENT(IN)  :: RHO0,RHOS
      DOUBLE PRECISION, INTENT(IN)  :: BETAC(NTRAC), T0AC(NTRAC)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: DELTAR
      TYPE(BIEF_OBJ), INTENT(IN)    :: TA
      TYPE(BIEF_OBJ), INTENT(INOUT) :: RHO
      LOGICAL, INTENT(IN)           :: SEDI
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ITRAC,NTRACM1
!
!***********************************************************************
!
      IF(DENLAW.GE.1.AND.DENLAW.LE.3) THEN
!
        IF(DENLAW.EQ.1) THEN
!
C        LAW ACCORDING TO TEMPERATURE
C        RHO = RHO0(1-(7(T-T0)**2)*1.E-6)
C                                               -3
C        WITH T0=4degC   AND   RHO0=999.972 KG.M
!
C        NOTE: ONLY THE GRADIENT OF DELTAR APPEARS IN EQUATIONS
!
         CALL OS( 'X=Y+C   ',X=RHO,Y=TA%ADR(IND_T)%P,C=-4.D0)
         CALL OS( 'X=XY    ',X=RHO,Y=RHO)
         CALL OS( 'X=CX    ',X=RHO,C=7.D-6 )
         CALL OS( 'X=X+C   ',X=RHO,C=-1.D0 )
         CALL OS( 'X=CX    ',X=RHO,C=-999.972D0)
!
        ELSEIF(DENLAW.EQ.2) THEN
!
C        LAW ACCORDING TO SALINITY S
C        RHO = RHO0(1+750S*1.E-6)
!
C                              -3
C        WITH RHO0=999.972 KG.M
!
         CALL OS( 'X=CY    ',X=RHO,Y=TA%ADR(IND_S)%P,C=750.D-6)
         CALL OS( 'X=X+C   ',X=RHO,C=1.D0)
         CALL OS( 'X=CX    ',X=RHO,C=999.972D0)
!
        ELSEIF(DENLAW.EQ.3) THEN
!
C        LAW ACCORDING TO BOTH TEMPERATURE AND SALINITY
C        RHO = RHO0(1-(7(T-T0)**2-750S)*1.E-6)
C                                               -3
C        WITH T0=4degC   AND   RHO0=999.972 KG.M
!
         CALL OS( 'X=Y+C   ',X=RHO,Y=TA%ADR(IND_T)%P,C=-4.D0  )
         CALL OS( 'X=XY    ',X=RHO,Y=RHO)
         CALL OS( 'X=CX    ',X=RHO,C=7.D-6 )
         CALL OS( 'X=X+CY  ',X=RHO,Y=TA%ADR(IND_S)%P,C=-750.D-6)
         CALL OS( 'X=X+C   ',X=RHO,C=-1.D0)
         CALL OS( 'X=CX    ',X=RHO,C=-999.972D0 )
!
        ENDIF
!
C       COMPUTES DRHO/DRO  = (RHO - RHO0)/RHO0
C       THE VALUE OF RHO0 GIVEN BY THE USER IS TAKEN HERE, IT TAKES INTO
C       ACCOUNT AN AVERAGE TEMPERATURE OR SALINITY IN THE DOMAIN, FOR A
C       BETTER BOUSSINESQ APPROXIMATION
!
        CALL OS( 'X=Y+C   ', X=DELTAR , Y=RHO , C=-RHO0 )
        CALL OS( 'X=CX    ', X=DELTAR , C=1.D0/RHO0 )
!
      ELSEIF(DENLAW.EQ.4) THEN
!
C       COMPUTES DELTAR WITH COEFFICIENTS BETAC GIVEN BY THE USER
!
C       BEWARE : BETA = - (1/RHO0)*(RHO-RHO0)/(TA-T0AC)
C                HENCE - SIGN IN SECOND CALL TO OS
C                BECAUSE DELTAR = (RHO-RHO0)/RHO0
!
C       SEDIMENT (TRACER NUMBER NTRAC) IS NATURALLY INCLUDED
C       IN THIS LOOP
!
        CALL OS( 'X=0     ' , X=DELTAR )
!
        IF(SEDI) THEN
          NTRACM1=NTRAC-1
        ELSE
          NTRACM1=NTRAC
        ENDIF
!
        IF(NTRACM1.GT.0) THEN
          DO ITRAC = 1,NTRACM1
            CALL OS('X=X+CY  ',DELTAR,TA%ADR(ITRAC)%P,
     &                              TA%ADR(ITRAC)%P, -BETAC(ITRAC) )
            CALL OS('X=X+C   ',DELTAR,DELTAR,DELTAR,
     &                              T0AC(ITRAC)*BETAC(ITRAC))
          ENDDO
        ENDIF
!
      ELSEIF(DENLAW.EQ.0) THEN
!
        CALL OS('X=0     ',X=DELTAR)
!
      ELSE
!
        IF(LNG.EQ.1) WRITE(LU,*) 'LOI DE DENSITE INCONNUE DANS DRSURR :'
        IF(LNG.EQ.2) WRITE(LU,*) 'WRONG DENSITY LAW IN DRSURR'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
C     EFFECT OF SEDIMENT IS ALWAYS ADDED
!
C     SEDIMENT (SEDIMENT MUST BE THE LAST TRACER, HENCE NUMBER NTRAC)
C     ADDS UP THE SEDIMENT EFFECT
!
      IF(SEDI) THEN
!
        CALL OS('X=X+CY  ',X=DELTAR,Y=TA%ADR(NTRAC)%P,
     &                       C=(RHOS-RHO0)/(RHO0*RHOS))
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C