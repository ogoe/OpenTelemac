C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       TREATS SIPHONS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALTSCE, ANGSCE, CESCE, CSSCE, DELSCE, DSCE, ENTET, ENTSIP, GRAV, H, ISCE, LSCE, MAXSCE, NSIPH, NTRAC, RELAXS, SECSCE, SORSIP, T, TSCE, U, USCE, V, VSCE, ZF
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
!>    </th><td> CE1, CE2, CS1, CS2, D1, D2, HIR1, HIR2, I1, I2, IR1, IR2, ITRAC, L, N, Q, S1, S2, SEC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> P_DMAX()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!> </td><td> 16/02/2009
!> </td><td> J-M HERVOUET (LNH) 01 30 87 80 18
!> </td><td> CORRECTED 03/2000 CORRECTION (IN PARALLEL MODE)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/2000
!> </td><td> E. DAVID (SOGREAH)
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 03/10/1996
!> </td><td> J.-M. HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 19/04/1996
!> </td><td> V. GUINOT (LHF)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALTSCE
!></td><td>--></td><td>COTE DES BUSES.
!>    </td></tr>
!>          <tr><td>ANGSCE
!></td><td>--></td><td>ANGLE DES BUSES AVEC L'AXE OX.
!>    </td></tr>
!>          <tr><td>CESCE
!></td><td><--</td><td>COEFFICIENTS DE PERTE DE CHARGE
!>                  LORS D'UN FONCTIONNEMENT EN ENTREE.
!>    </td></tr>
!>          <tr><td>CSSCE
!></td><td><--</td><td>COEFFICIENTS DE PERTE DE CHARGE
!>                  LORS D'UN FONCTIONNEMENT EN SORTIE.
!>    </td></tr>
!>          <tr><td>DELSCE
!></td><td>--></td><td>ANGLE DES BUSES AVEC LA VERTICALE
!>    </td></tr>
!>          <tr><td>DSCE
!></td><td><--</td><td>DEBIT DES SOURCES.
!>    </td></tr>
!>          <tr><td>ENTET
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ENTSIP
!></td><td>--></td><td>NUMERO DE L'ENTREE D'UNE BUSE DANS LA
!>                  NUMEROTATION DES SOURCES.
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>PESANTEUR.
!>    </td></tr>
!>          <tr><td>H
!></td><td>---</td><td>HAUTEUR D'EAU.
!>    </td></tr>
!>          <tr><td>ISCE
!></td><td>--></td><td>NUMERO GLOBAL DES POINTS SOURCES.
!>    </td></tr>
!>          <tr><td>LSCE
!></td><td><--</td><td>PERTE DE CHARGE LINEAIRE DE LA CONDUITE.
!>    </td></tr>
!>          <tr><td>MAXSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPSIPH
!></td><td>--></td><td>NOMBRE DE SIPHONS.
!>    </td></tr>
!>          <tr><td>NSIPH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>RELAXS
!></td><td>--></td><td>COEFFICIENT DE RELAXATION.
!>    </td></tr>
!>          <tr><td>SECSCE
!></td><td>--></td><td>SECTION DES BUSES.
!>    </td></tr>
!>          <tr><td>SORSIP
!></td><td>--></td><td>NUMERO DE LA SORTIE D'UNE BUSE DANS LA
!>                  NUMEROTATION DES SOURCES.
!>    </td></tr>
!>          <tr><td>T
!></td><td>--></td><td>TRACEUR.
!>    </td></tr>
!>          <tr><td>TRAC
!></td><td>--></td><td>LOGIQUE INDIQUANT S'IL Y A UN TRACEUR.
!>    </td></tr>
!>          <tr><td>TSCE
!></td><td>--></td><td>VALEUR DU TRACEUR AUX SOURCES.
!>    </td></tr>
!>          <tr><td>U
!></td><td>--></td><td>VITESSE U.
!>    </td></tr>
!>          <tr><td>USCE
!></td><td>--></td><td>VITESSE U DU COURANT AUX SOURCES.
!>    </td></tr>
!>          <tr><td>V
!></td><td>--></td><td>VITESSE V.
!>    </td></tr>
!>          <tr><td>VSCE
!></td><td>--></td><td>VITESSE V DU COURANT AUX SOURCES.
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>---</td><td>COTES DU FOND.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SIPHON
     &(RELAXS,NSIPH,ENTSIP,SORSIP,GRAV,
     & H,ZF,ISCE,DSCE,SECSCE,ALTSCE,CSSCE,CESCE,DELSCE,ANGSCE,LSCE,
     & NTRAC,T,TSCE,USCE,VSCE,U,V,ENTET,MAXSCE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALTSCE         |-->| COTE DES BUSES.
C| ANGSCE         |-->| ANGLE DES BUSES AVEC L'AXE OX.
C| CESCE          |<--| COEFFICIENTS DE PERTE DE CHARGE
C|                |   | LORS D'UN FONCTIONNEMENT EN ENTREE.
C| CSSCE          |<--| COEFFICIENTS DE PERTE DE CHARGE
C|                |   | LORS D'UN FONCTIONNEMENT EN SORTIE.
C| DELSCE         |-->| ANGLE DES BUSES AVEC LA VERTICALE
C| DSCE           |<--| DEBIT DES SOURCES.
C| ENTET          |---| 
C| ENTSIP         |-->| NUMERO DE L'ENTREE D'UNE BUSE DANS LA
C|                |   | NUMEROTATION DES SOURCES.
C| GRAV           |-->| PESANTEUR.
C| H             |---| HAUTEUR D'EAU.
C| ISCE           |-->| NUMERO GLOBAL DES POINTS SOURCES.
C| LSCE           |<--| PERTE DE CHARGE LINEAIRE DE LA CONDUITE.
C| MAXSCE         |---| 
C| NPSIPH         |-->| NOMBRE DE SIPHONS.
C| NSIPH          |---| 
C| NTRAC          |---| 
C| RELAXS         |-->| COEFFICIENT DE RELAXATION.
C| SECSCE         |-->| SECTION DES BUSES.
C| SORSIP         |-->| NUMERO DE LA SORTIE D'UNE BUSE DANS LA
C|                |   | NUMEROTATION DES SOURCES.
C| T             |-->| TRACEUR.
C| TRAC           |-->| LOGIQUE INDIQUANT S'IL Y A UN TRACEUR.
C| TSCE           |-->| VALEUR DU TRACEUR AUX SOURCES.
C| U             |-->| VITESSE U.
C| USCE           |-->| VITESSE U DU COURANT AUX SOURCES.
C| V             |-->| VITESSE V.
C| VSCE           |-->| VITESSE V DU COURANT AUX SOURCES.
C| ZF             |---| COTES DU FOND.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NSIPH,NTRAC,MAXSCE
      INTEGER, INTENT(IN)             :: ENTSIP(*),SORSIP(*),ISCE(*)
      LOGICAL, INTENT(IN)             :: ENTET
      DOUBLE PRECISION, INTENT(IN)    :: RELAXS,GRAV
      DOUBLE PRECISION, INTENT(INOUT) :: USCE(*),VSCE(*),DSCE(*)
      DOUBLE PRECISION, INTENT(INOUT) :: TSCE(MAXSCE,NTRAC)
      DOUBLE PRECISION, INTENT(IN)    :: ANGSCE(*),LSCE(*),CESCE(*)
      DOUBLE PRECISION, INTENT(IN)    :: CSSCE(*),DELSCE(*)
      DOUBLE PRECISION, INTENT(IN)    :: SECSCE(*),ALTSCE(*)
      DOUBLE PRECISION, INTENT(IN)    :: H(*),ZF(*),U(*),V(*)
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: T
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER N,I1,I2,IR1,IR2,ITRAC
C
      DOUBLE PRECISION SEC,L
      DOUBLE PRECISION D1,D2,S1,S2,CE1,CE2,CS1,CS2,Q,HIR1,HIR2
C
      INTRINSIC SQRT,COS,SIN
C
      DOUBLE PRECISION P_DMAX
      EXTERNAL         P_DMAX
C
C-----------------------------------------------------------------------
C
C LOOP OVER THE SIPHONS
C
      DO 10 N=1,NSIPH
C
C     IDENTIFIES ENTRY / EXIT NODES
C
C     NUMBER OF THE CORRESPONDING SOURCES
      I1=ENTSIP(N)
      I2=SORSIP(N)
C     NUMBER OF THE POINTS FOR THESE SOURCES
      IR1=ISCE(I1)
      IR2=ISCE(I2)
C
C     LOADS, TAKEN AS FREE SURFACE ELEVATION
C
      IF(IR1.GT.0) THEN
        S1=H(IR1)+ZF(IR1)
        HIR1=H(IR1)
      ELSE
        S1=-1.D10
        HIR1=-1.D10
      ENDIF
      IF(IR2.GT.0) THEN
        S2=H(IR2)+ZF(IR2)
        HIR2=H(IR2)
      ELSE
        S2=-1.D10
        HIR2=-1.D10
      ENDIF
C     CASE WHERE ONE OF THE ENDS IS NOT IN THE SUB-DOMAIN
      IF(NCSIZE.GT.1) THEN
        S1=P_DMAX(S1)
        S2=P_DMAX(S2)
        HIR1=P_DMAX(HIR1)
        HIR2=P_DMAX(HIR2)
      ENDIF
C
C     COEFFICIENTS FOR COMPUTATION OF PRESSURE LOSS
C
      D1=DELSCE(I1)
      D2=DELSCE(I2)
      CE1=CESCE(I1)
      CE2=CESCE(I2)
      CS1=CSSCE(I1)
      CS2=CSSCE(I2)
      SEC=SECSCE(I1)
      L  =LSCE(I1)
C
C     COMPUTES THE FLOW ACCORDING TO DELTAH
C     IF THE LINEAR PRESSURE LOSS IS NEGLIGIBLE, COULD HAVE DIFFERENT
C     ENTRY / EXIT SECTIONS
C
      IF(S1.GE.S2) THEN
C EDD + CCT 03/2000 (CORRECTED BY JMH 16/02/2009 H(IR1) AND H(IR2)
C                    ARE NOT GUARANTEED TO WORK IN PARALLEL ==>  HIR1 AND HIR2)
C        IF(S1.GT.ALTSCE(I1).AND.S1.GT.ALTSCE(I2)) THEN
        IF(S1.GT.ALTSCE(I1).AND.S1.GT.ALTSCE(I2).AND.
     &     HIR1.GT.0.02D0) THEN
C
          Q = SEC * SQRT( 2.D0*GRAV*(S1-S2)/(CE1+L+CS2) )
        ELSE
          Q=0.D0
        ENDIF
      ELSE
C EDD + CCT 03/2000
C        IF(S2.GT.ALTSCE(I1).AND.S2.GT.ALTSCE(I2)) THEN
        IF(S2.GT.ALTSCE(I1).AND.S2.GT.ALTSCE(I2).AND.
     &     HIR2.GT.0.02D0) THEN
C
          Q = - SEC * SQRT( 2.D0*GRAV*(S2-S1)/(CS1+L+CE2) )
        ELSE
          Q=0.D0
        ENDIF
      ENDIF
C
C     NOTHING HAPPENS IF THE LOADS AT THE 2 ENDS ARE LOWER THAN
C     THE ELEVATION OF THE NOZZLES
C
      IF(S1.LT.ALTSCE(I1).AND.S2.LT.ALTSCE(I2)) Q=0.D0
C
C  FILLS OUT DSCE USING RELAXATION
C
      DSCE(I2)= RELAXS * Q + (1.D0-RELAXS) * DSCE(I2)
      DSCE(I1)=-DSCE(I2)
C
      IF(ENTET) THEN
        WRITE(LU,*) ' '
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'SIPHON ',N,' DEBIT DE ',DSCE(I2),' M3/S'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'CULVERT ',N,' DISCHARGE OF ',DSCE(I2),' M3/S'
        ENDIF
        WRITE(LU,*) ' '
      ENDIF
C
C  TREATS THE VELOCITIES AT THE SOURCES
C  SAME APPROACH FOR VELOCITY AND TRACER
C
      IF(DSCE(I1).GT.0.D0) THEN
        USCE(I1) = ( COS(D1)*DSCE(I1)/SECSCE(I1) ) * COS(ANGSCE(I1))
        VSCE(I1) = ( COS(D1)*DSCE(I1)/SECSCE(I1) ) * SIN(ANGSCE(I1))
      ELSE
        IF(IR1.GT.0) THEN
          USCE(I1) = U(IR1)
          VSCE(I1) = V(IR1)
        ENDIF
      ENDIF
      IF(DSCE(I2).GT.0.D0) THEN
        USCE(I2) = ( COS(D2)*DSCE(I2)/SECSCE(I2) ) * COS(ANGSCE(I2))
        VSCE(I2) = ( COS(D2)*DSCE(I2)/SECSCE(I2) ) * SIN(ANGSCE(I2))
      ELSE
        IF(IR2.GT.0) THEN
          USCE(I2) = U(IR2)
          VSCE(I2) = V(IR2)
        ENDIF
      ENDIF
C
C  TREATS THE TRACER :
C
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          IF(DSCE(I1).GT.0.D0) THEN
            IF(IR2.GT.0) THEN
              TSCE(I1,ITRAC)=T%ADR(ITRAC)%P%R(IR2)
            ELSE
              TSCE(I1,ITRAC)=-1.D10
            ENDIF
            IF(NCSIZE.GT.1) TSCE(I1,ITRAC)=P_DMAX(TSCE(I1,ITRAC))
          ENDIF
          IF(DSCE(I2).GT.0.D0) THEN
            IF(IR1.GT.0) THEN
              TSCE(I2,ITRAC)=T%ADR(ITRAC)%P%R(IR1)
            ELSE
              TSCE(I2,ITRAC)=-1.D10
            ENDIF
            IF(NCSIZE.GT.1) TSCE(I2,ITRAC)=P_DMAX(TSCE(I2,ITRAC))
          ENDIF
        ENDDO
      ENDIF
C
C  END OF THE LOOP OVER THE SIPHONS
C
10    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C