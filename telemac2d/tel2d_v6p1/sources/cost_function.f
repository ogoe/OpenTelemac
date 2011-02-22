C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PARTIALLY COMPUTES (ONE TIMESTEP) THE COST FUNCTION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC2D, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> JCOUT, MODE, OPTION
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::ALPHA1 ALPHA1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ALPHA2 ALPHA2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ALPHA3 ALPHA3@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CF CF@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CHESTR CHESTR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CV1 CV1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CV2 CV2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CV3 CV3@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ESTIME ESTIME@endlink, 
!> @link DECLARATIONS_TELEMAC2D::GRADJ GRADJ@endlink, 
!> @link DECLARATIONS_TELEMAC2D::GRAV GRAV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::H H@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HD HD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HFROT HFROT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HH HH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HN HN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::KFROT KFROT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MESH MESH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MSK MSK@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NZONE NZONE@endlink, 
!> @link DECLARATIONS_TELEMAC2D::QQ QQ@endlink, 
!> @link DECLARATIONS_TELEMAC2D::RR RR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T1 T1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2 T2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T3 T3@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T4 T4@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T5 T5@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TE3 TE3@endlink, 
!> @link DECLARATIONS_TELEMAC2D::U U@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UD UD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UN UN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UNSV2D UNSV2D@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UU UU@endlink, 
!> @link DECLARATIONS_TELEMAC2D::V V@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VD VD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VERTIC VERTIC@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VN VN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VV VV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ZF ZF@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ZONE ZONE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, I, J
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_COST_FUNCTION
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FRICTI(), INCLU2(), OS(), PLANTE(), SLOPES(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_ADJ_T2D(), PROPAG_ADJ()

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
!> </td><td> 02/10/2000
!> </td><td> A. LEOPARDI (UNINA)
!> </td><td> UPGRADE
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 25/06/1993
!> </td><td> E. BARROS
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALPHA
!></td><td>--></td><td>WEIGHT FUNCTIONS
!>    </td></tr>
!>          <tr><td>JCOUT
!></td><td><-></td><td>COST FUNCTION
!>    </td></tr>
!>          <tr><td>MODE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF POINTS IN THE MESH
!>    </td></tr>
!>          <tr><td>OPTION
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U,V,H
!></td><td>--></td><td>VELOCITY AND DEPTH
!>    </td></tr>
!>          <tr><td>UD,VD,HD
!></td><td>--></td><td>MEASURES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE COST_FUNCTION
     &(JCOUT,OPTION,MODE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALPHA          |-->| WEIGHT FUNCTIONS
C| JCOUT          |<->| COST FUNCTION
C| MODE           |---| 
C| NPOIN          |-->| NUMBER OF POINTS IN THE MESH
C| OPTION         |---| 
C| U,V,H          |-->| VELOCITY AND DEPTH
C| UD,VD,HD       |-->| MEASURES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D, EX_COST_FUNCTION => COST_FUNCTION
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION , INTENT(INOUT) :: JCOUT
      INTEGER , INTENT(IN)             :: OPTION
      CHARACTER(LEN=3) , INTENT(IN)    :: MODE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      COMMON/INFO/LNG,LU
      INTEGER LNG,LU
C
      INTEGER I,J
      DOUBLE PRECISION C
C
C=======================================================================
C
      IF(MODE.EQ.'FCT') THEN
C
C       HERE U,V AND H ARE GIVEN BY A CALL TO LITENR
C
        IF(OPTION.EQ.1) THEN
C
          DO I=1,NPOIN
C
            JCOUT = JCOUT + ALPHA1%R(I) * (H%R(I)-HD%R(I))**2
     &                    + ALPHA2%R(I) * (U%R(I)-UD%R(I))**2
     &                    + ALPHA3%R(I) * (V%R(I)-VD%R(I))**2
C
          ENDDO
C
        ELSEIF(OPTION.EQ.2) THEN
C
          DO I=1,NPOIN
C
            JCOUT = JCOUT + ALPHA1%R(I) * GRAV *
     &              (SQRT(MAX(H%R(I),0.D0))-SQRT(MAX(HD%R(I),0.D0)))**2
     &                    + ALPHA2%R(I) * (U%R(I)-UD%R(I))**2
     &                    + ALPHA3%R(I) * (V%R(I)-VD%R(I))**2
C
          ENDDO
C
        ELSE
C
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'COST_FUNCTION : OPTION NON PREVUE : ',MODE
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'COST_FUNCTION: UNEXPECTED OPTION : ',MODE
          ENDIF
          CALL PLANTE(1)
          STOP
C
C       TEST ON OPTION
        ENDIF
C
C=======================================================================
C
      ELSEIF(MODE.EQ.'GRD') THEN
C
      IF(    INCLU2(ESTIME,'FROTTEMENT')
     &   .OR.INCLU2(ESTIME,'FRICTION'  )  ) THEN
C
C     IDENTIFICATION OF FRICTION
C
      IF (KFROT.EQ.3.OR.KFROT.EQ.2) THEN
C
      CALL SLOPES(TE3,ZF,MESH)
      CALL VECTOR(T1,'=','MASBAS          ',U%ELM,1.D0,
     &            T3,T3,T3,T3,T3,T3,MESH,.TRUE.,TE3)
C
      CALL FRICTI(T3,T4,T2,T2,UN,VN,HN,CF,MESH,T2,T5,VERTIC,UNSV2D,MSK,
     &            MASKEL,HFROT)
C
      CALL OS( 'X=XY    ' , T3 , T1 , T1 , C )
      CALL OS( 'X=XY    ' , T4 , T1 , T1 , C )
C
      CALL OS( 'X=CXYZ  ' , T3 , QQ , UU , -2.D0 )
      CALL OS( 'X=CXYZ  ' , T4 , RR , VV , -2.D0 )
C
      CALL OS( 'X=X+Y   ' , T3 , T4 , T4 , C )
      CALL OS( 'X=Y/Z   ' , T3 , T3 , CHESTR , C )
C
      ELSE
C
       IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'COST_FUNCTION : FROTTEMENT NON TRAITE : ',KFROT
       ENDIF
       IF(LNG.EQ.2) THEN
         WRITE(LU,*) 'COST_FUNCTION: UNEXPECTED FRICTION LAW: ',KFROT
       ENDIF
       CALL PLANTE(1)
       STOP
C
      ENDIF
C
      ELSE
C
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'COST_FUNCTION : PARAMETRE NON PREVU :'
          WRITE(LU,*) ESTIME
          WRITE(LU,*) 'VERIFIER LE MOT CLEF : ESTIMATION DE PARAMETRES'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'COST_FUNCTION: UNEXPECTED PARAMETER :'
          WRITE(LU,*) ESTIME
          WRITE(LU,*) 'CHECK THE KEY-WORD: PARAMETER ESTIMATION'
        ENDIF
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C COMPUTATION OF A GRADIENT FOR EVERY ZONE, AFTER BUILDING
C A GRADIENT VECTOR FOR EVERY POINT (IN T3)
C
      IF(NZONE.GT.0) THEN
C       IF ONE IDENTIFIES ONLY ONE PARAMETER S FOR A SET OF NODES
C       GRADIENT DJ/DS IS THE SUM OF THE GRADIENTS OF EACH
C       NODE OF THE SET
        DO J=1,NZONE
          DO I=1,NPOIN
            IF(ZONE%I(I).EQ.J) GRADJ%R(J)=GRADJ%R(J)+T3%R(I)
          ENDDO
        ENDDO
      ELSE
C       NOTE JMH: HERE IT IS SUPPOSED THAT NPARAM = NPOIN
        CALL OS('X=X+Y   ',GRADJ,T3,T3,0.D0)
      ENDIF
C
C=======================================================================
C
      ELSEIF(MODE.EQ.'RHS') THEN
C
C           IT    IT    IT
C  TERMS 2 W   ( X   - M   ) OR EQUIVALENT DEPENDING ON THE OPTION
C           IP    IP    IP
C
      IF(OPTION.EQ.1) THEN
C
        CALL OS( 'X=Y-Z   ', CV1 , HH , HD , C )
        CALL OS( 'X=Y-Z   ', CV2 , UU , UD , C )
        CALL OS( 'X=Y-Z   ', CV3 , VV , VD , C )
C
        CALL OS( 'X=CXY   ', CV1 , ALPHA1 , ALPHA1 , 2.D0 )
        CALL OS( 'X=CXY   ', CV2 , ALPHA2 , ALPHA2 , 2.D0 )
        CALL OS( 'X=CXY   ', CV3 , ALPHA3 , ALPHA3 , 2.D0 )
C
      ELSEIF(OPTION.EQ.2) THEN
C
C       HERE COST FUNCTION COMPUTED WITH CELERITY INSTEAD OF DEPTH
        CALL OS( 'X=SQR(Y)', T1  , HH , T1 , C   )
        CALL OS( 'X=SQR(Y)', T2  , HD , T2 , C   )
        CALL OS( 'X=Y-Z   ', T3  , T1 , T2 , C )
        CALL OS( 'X=Y/Z   ', CV1 , T3 , T1 , C )
        CALL OS( 'X=Y-Z   ', CV2 , UU , UD , C )
        CALL OS( 'X=Y-Z   ', CV3 , VV , VD , C )
C
        CALL OS( 'X=CXY   ', CV1 , ALPHA1 , ALPHA1 , GRAV )
        CALL OS( 'X=CXY   ', CV2 , ALPHA2 , ALPHA2 , 2.D0 )
        CALL OS( 'X=CXY   ', CV3 , ALPHA3 , ALPHA3 , 2.D0 )
C
      ENDIF
C
C=======================================================================
C
      ELSE
C
C=======================================================================
C
       IF(LNG.EQ.1) WRITE(LU,*) 'COST_FUNCTION : MODE NON PREVU : ',MODE
       IF(LNG.EQ.2) WRITE(LU,*) 'COST_FUNCTION: UNEXPECTED MODE : ',MODE
       CALL PLANTE(1)
       STOP
C
C=======================================================================
C
C     TEST ON MODE
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C