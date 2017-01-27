!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  my_FLUSH                       !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &(ID)
        
#if defined NO_STD_FLUSH || NAGFOR
      USE F90_UNIX_IO, ONLY: FLUSH
#endif
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: ID
      
#ifndef NESTOR_INTERFACES 
      !--------------------- local ----------
      
#if defined(NO_STD_FLUSH)
      CALL FLUSH(ID)
#else
      FLUSH(ID)
#endif

!***                                              ********************************************
!***                                              ********************************************
#endif NESTOR_INTERFACES                         !******************************************** 
      END SUBROUTINE my_FLUSH                    !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
!
!
!      manual:   FieldDump < 1    ==> kein Verklappen
!      manual:   IF( A%MinVolume >= 0.0D0 ) CALL CalcDigVolumeInRadius(...)
!      manual:         IF( A(m)%GrainClass(1) < -0.0000000D0) CYCLE !> A negative value indicates
!                                                     !  that dummy values are used
!
!      manual: in general all dummy values must be negative ( or zero ? )
!      manual:
!      manual:
!      manual:
!      manual:
!      Annahme : ELAY0    = constant
!      Annahme : Timestep = constant
!      Annahme :
!      Annahme :
!      Annahme :
!      Annahme :
!      Annahme :

!      ?Frage: wird  CALL PARCOM(T13,2,MESH) nach meinen Tests nicht benoetigt
!      ?       aber warum wird es in Telemac immer in kombination mit siehe unten
!      ?      aufgerufen
!      ?      CALL VECTOR(T13,'=','MASBAS          '
!      ?     &      ,IELMH_SIS,1.D0,T14,T14,T14,T14,T14,T14,MESH,MSK,MASKEL)
!      ?!         IF(NCSIZE.GT.1) CALL PARCOM(T13,2,MESH)

!
!      to do:  Beruecksichtigung von Ein- , Austrag in action field durch Transport
!
!      to do:  min Baggermenge   Problem ist  die Parallelisierung !! ist erledigt ?
!
!      to do:  testen ob zugabe mit const Dicke noch korret arbeitet
!                ungetestet:  IF( A(m)%tsCount  >  A(m)%nts ) THEN ! The action is over
!                ungetestet:    A(m)%State = 2                     ! 2 = Action for ever inactive
!                ungetestet:  ENDIF
!
!      to do:  RefWSP: aus Dreiecks-Gitter interpolieren
!      to do:  RefWSP: mehrere Ref-horizonte ermoeglichen
!      to do:  SR: Dig_by_Criterion  layCL(:), heapCL(:)   bisher nicht deallociert
!      to do:
!      to do:  EXTERNAL  P_SYNC   wann ist die EXTERNAL Anweisung zwingend ?
!      to do:
!      to do:  restart:  beim Einlesen der Restartdatei auf gleiche Prozessorzahl pruefen und ggf. error message
!      to do:
!      to do:  Fehlermeldung auf Prozessor 0 lenken
!      to do:
!      to do:  Fehler/unplausibles Verhalten wenn "DigDepth" kleiner "CritDepth"
!      to do:
!      to do:
!      to do:  "DumpPlanar =  TRUE" sollte auch beim "Dig_by_criterion" moeglich sein
!      to do:
!      to do:  Bei Einlese-Schleifen IOSTAT = stat ; if(stat.NE.0) testen
!      to do:  bzw. END = 600 einbauen
!      to do:
!      to do:















