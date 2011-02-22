C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PREPARES THE SOURCES TERMS IN THE DIFFUSION EQUATION
!>                FOR THE TRACER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  BEWARE OF NECESSARY COMPATIBILITIES FOR HPROP, WHICH
!>            SHOULD REMAIN UNCHANGED UNTIL THE COMPUTATION OF THE
!>            TRACER MASS IN CVDFTR

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, DSCE, DT, FAC, HPROP, ISCE, MASSOU, MAXSCE, MAXTRA, NREJTR, NTRAC, TETAT, TEXP, TIMP, TN, TSCE, TSCEXP, YASMI
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
!>    </th><td> DEBIT, I, IR, ITRAC, TRASCE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), P_DSUM()
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
!>      <td><center>                                           </center>
!> </td><td> 01/10/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td> MODIFIED TEST ON ICONVF(3)
!> </td></tr>
!>      <tr>
!>      <td><center> 6.0                                       </center>
!> </td><td> 23/02/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18; C MOULIN (LNH) 30 87 83 81
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS.
!>    </td></tr>
!>          <tr><td>DSCE
!></td><td>--></td><td>DEBITS DES REJETS
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS.
!>    </td></tr>
!>          <tr><td>FAC
!></td><td>--></td><td>IN PARALLEL :
!>                  1/(NUMBER OF SUB-DOMAINS OF THE POINT)
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR.
!>    </td></tr>
!>          <tr><td>H,HN
!></td><td>--></td><td>HAUTEURS D'EAU A TN+1 ET TN
!>    </td></tr>
!>          <tr><td>HPROP
!></td><td>--></td><td>HAUTEUR DE PROPAGATION
!>    </td></tr>
!>          <tr><td>ISCE
!></td><td>--></td><td>POINTS LES PLUS PROCHES DES REJETS.
!>    </td></tr>
!>          <tr><td>ITRAC
!></td><td>---</td><td>TRACER RANK
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE
!>    </td></tr>
!>          <tr><td>MASSOU
!></td><td>---</td><td>MASSE DE TRACEUR AJOUTEE.
!>    </td></tr>
!>          <tr><td>MAXSCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MAXTRA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>BLOC DES ENTIERS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>NREJTR
!></td><td>--></td><td>NOMBRE DE REJETS DE TRACEUR.
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1,2
!></td><td>--></td><td>TABLEAUX DE TRAVAIL.
!>    </td></tr>
!>          <tr><td>TETAH
!></td><td>--></td><td>IMPLICITATION SUR H.
!>    </td></tr>
!>          <tr><td>TETAT
!></td><td>--></td><td>IMPLICITATION DU TRACEUR.
!>    </td></tr>
!>          <tr><td>TEXP
!></td><td>--></td><td>TERME SOURCE EXPLICITE.
!>    </td></tr>
!>          <tr><td>TIMP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TN
!></td><td>--></td><td>TRACEUR AU PAS DE TEMPS PRECEDENT.
!>    </td></tr>
!>          <tr><td>TSCE
!></td><td>--></td><td>VALEURS DES TRACEURS AUX REJETS
!>    </td></tr>
!>          <tr><td>TSCEXP
!></td><td><--</td><td>TERME EXPLICITE VENANT DES SOURCES
!>                  PONCTUELLES DE L'EQUATION DU TRACEUR
!>                  EGAL A TSCE - ( 1 - TETAT ) TN
!>    </td></tr>
!>          <tr><td>UN , VN
!></td><td>--></td><td>COMPOSANTES DES VECTEURS VITESSES A TN.
!>    </td></tr>
!>          <tr><td>W1
!></td><td>--></td><td>TABLEAU DE TRAVAIL.
!>    </td></tr>
!>          <tr><td>XSCE
!></td><td>--></td><td>ABSCISSES DES REJETS.
!>    </td></tr>
!>          <tr><td>YASMI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YSCE
!></td><td>--></td><td>ORDONNEES DES REJETS.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DIFSOU
     &(TEXP,TIMP,YASMI,TSCEXP,HPROP,TN,TETAT,NREJTR,ISCE,DSCE,TSCE,
     & MAXSCE,MAXTRA,AT,DT,MASSOU,NTRAC,FAC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS.
C| DSCE           |-->| DEBITS DES REJETS
C| DT             |-->| PAS DE TEMPS.
C| FAC            |-->| IN PARALLEL :
C|                |   | 1/(NUMBER OF SUB-DOMAINS OF THE POINT)
C| GRAV           |-->| ACCELERATION DE LA PESANTEUR.
C| H,HN           |-->| HAUTEURS D'EAU A TN+1 ET TN
C| HPROP          |-->| HAUTEUR DE PROPAGATION
C| ISCE           |-->| POINTS LES PLUS PROCHES DES REJETS.
C| ITRAC          |---| TRACER RANK
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
C| MASSOU         |---| MASSE DE TRACEUR AJOUTEE.
C| MAXSCE         |---| 
C| MAXTRA         |---| 
C| MESH           |-->| BLOC DES ENTIERS DU MAILLAGE.
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| NREJTR         |-->| NOMBRE DE REJETS DE TRACEUR.
C| NTRAC          |---| 
C| T1,2           |-->| TABLEAUX DE TRAVAIL.
C| TETAH          |-->| IMPLICITATION SUR H.
C| TETAT          |-->| IMPLICITATION DU TRACEUR.
C| TEXP           |-->| TERME SOURCE EXPLICITE.
C| TIMP           |---| 
C| TN             |-->| TRACEUR AU PAS DE TEMPS PRECEDENT.
C| TSCE           |-->| VALEURS DES TRACEURS AUX REJETS
C| TSCEXP         |<--| TERME EXPLICITE VENANT DES SOURCES
C|                |   | PONCTUELLES DE L'EQUATION DU TRACEUR
C|                |   | EGAL A TSCE - ( 1 - TETAT ) TN
C| UN , VN        |-->| COMPOSANTES DES VECTEURS VITESSES A TN.
C| W1             |-->| TABLEAU DE TRAVAIL.
C| XSCE           |-->| ABSCISSES DES REJETS.
C| YASMI          |---| 
C| YSCE           |-->| ORDONNEES DES REJETS.
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
      INTEGER, INTENT(IN)             :: ISCE(*),NREJTR,NTRAC
      INTEGER, INTENT(IN)             :: MAXSCE,MAXTRA
      LOGICAL, INTENT(INOUT)          :: YASMI(*)
      DOUBLE PRECISION, INTENT(IN)    :: AT,DT,TETAT,DSCE(*)
      DOUBLE PRECISION, INTENT(IN)    :: TSCE(MAXSCE,MAXTRA),FAC(*)
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU(*)
      TYPE(BIEF_OBJ), INTENT(IN)      :: TN,HPROP
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: TSCEXP,TEXP,TIMP
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,IR,ITRAC
C
      DOUBLE PRECISION DEBIT,TRASCE
C
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
C
C-----------------------------------------------------------------------
C
C     EXPLICIT SOURCE TERMS (HERE SET TO ZERO)
C
      DO ITRAC=1,NTRAC
        CALL OS('X=0     ',X=TEXP%ADR(ITRAC)%P)
      ENDDO
C
C-----------------------------------------------------------------------
C
C     IMPLICIT SOURCE TERMS (HERE SET TO ZERO)
C
      DO ITRAC=1,NTRAC
C       CALL OS('X=0     ',X=TIMP%ADR(ITRAC)%P)
C       EQUIVALENT A
        YASMI(ITRAC)=.FALSE.
      ENDDO
C
C                                   N+1
C     EXAMPLE WHERE WE ADD -0.0001 T      IN THE RIGHT HAND-SIDE
C     OF THE TRACER EQUATION THAT BEGINS WITH DT/DT=...
C     (T12=SMI WILL BE DIVIDED BY HPROP IN CVDFTR, THE EQUATION IS:
C     DT/DT=...+SMI*T(N+1)/H
C
C     HERE THIS IS DONE FOR TRACER 3 ONLY IN A RECTANGULAR ZONE
C
C     CALL OS('X=0     ',X=TIMP%ADR(3)%P)
C     DO I=1,HPROP%DIM1
C       IF(X(I).GE.263277.D0.AND.X(I).LE.265037.D0) THEN
C       IF(Y(I).GE.379007.D0.AND.Y(I).LE.380326.D0) THEN
C         TIMP%ADR(3)%P%R(I)=-0.00001D0*HPROP%R(I)
C       ENDIF
C       ENDIF
C     ENDDO
C     YASMI(3)=.TRUE.
C
C-----------------------------------------------------------------------
C
C  TAKES THE SOURCES OF TRACER INTO ACCOUNT
C
C-----------------------------------------------------------------------
C
      DO ITRAC=1,NTRAC
C
      MASSOU(ITRAC) = 0.D0
C
      CALL OS('X=0     ',X=TSCEXP%ADR(ITRAC)%P)
C
      IF(NREJTR.NE.0) THEN
C
      DO 10 I = 1 , NREJTR
C
        IR = ISCE(I)
C       TEST IR.GT.0 FOR THE PARALLELISM
        IF(IR.GT.0) THEN
          DEBIT=DSCE(I)
          IF(DEBIT.GT.0.D0) THEN
            TRASCE = TSCE(I,ITRAC)
          ELSE
C           THE VALUE AT THE SOURCE IS THAT OF INDIDE IF THE FLOW
C                                                      IS OUTGOING
            TRASCE = TN%ADR(ITRAC)%P%R(IR)
          ENDIF
C         SOURCE TERM ADDED TO THE MASS OF TRACER
          IF(NCSIZE.GT.1) THEN
C           FAC TO AVOID COUNTING THE POINT SEVERAL TIMES
C           (SEE CALL TO P_DSUM BELOW)
            MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DEBIT*TRASCE*FAC(IR)
          ELSE
            MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DEBIT*TRASCE
          ENDIF
          TRASCE = TRASCE - (1.D0 - TETAT) * TN%ADR(ITRAC)%P%R(IR)
          TSCEXP%ADR(ITRAC)%P%R(IR)=TSCEXP%ADR(ITRAC)%P%R(IR)+TRASCE
C
C         THE IMPLICIT PART OF THE TERM - T * SCE
C         IS DEALT WITH IN CVDFTR.
C
        ENDIF
C
10    CONTINUE
C
      IF(NCSIZE.GT.1) MASSOU(ITRAC)=P_DSUM(MASSOU(ITRAC))
C
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